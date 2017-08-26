

{-# LANGUAGE GADTs, KindSignatures, FlexibleInstances, FlexibleContexts,
MultiParamTypeClasses, NoMonomorphismRestriction, TypeFamilies #-}

module Language.SPL (
    -- * Built-in constructs
    -- ** Distributions
    uniform, normal, lookup, sample,
    -- ** Processes
    trace, closed, prefix, zip, skip,
    -- * Prelude
    inclusivePrefix,
    iterative, map, time, always, reverse,
    brownian, integral,
    true, false,
    zip3,
    curry, uncurry, curry3, uncurry3,
    lift, lift2, lift3,
    choose, choice,
    fold, maximum_, minimum_, average,
    -- * Types
    Pair (..),
    If_ (..),
    Dist, Process,
    Time,
    Type,
    Ordered (..),
    Boolean (..),
    ToConstant (..)
    ) where

import Syntax
import Prelude hiding (curry, uncurry, zip, zip3, map, lookup, reverse)
import qualified Prelude as H
import Data.Char (toLower)

-------------------- Built-in constructs --------------------

-- |The standard uniform distribution (between 0 and 1, both inclusive).
uniform :: Dist Double
uniform = Uniform

-- |The standard normal distribution (with mean 0 and variance 1).
normal :: Dist Double
normal = Normal

-- |Applies the function to a single sample from the distribution.
-- The sample is a so called degenerate distribution containing one value with probability 1.
sample :: (Type a, Type b) => Dist a -> (Dist a -> Dist b) -> Dist b
sample = Sample

-- |A Process is conceptually a function from time to Dist.
-- Lookup is the manifistation of this concept.
lookup :: Type a => Dist Time -> Process a -> Dist a
lookup = Lookup

-- |Builds a process given function from time to a distribution
closed :: Type a => (Dist Time -> Dist a) -> Process a
closed = Closed

-- |Builds a process by accumulating over another process, given an initial value.
-- It is similar to a scan over a list (or a prefix sum) - however, it also knows
-- the delta time, specifying the time between the current and previous iteration.
-- The distribution at time zero is the function applied to the delta time, the
-- initial distribution and the distribution at time zero in the other process.
prefix :: (Type a, Type b) => (Dist Time -> Dist a -> Dist b -> Dist a) -> Dist a -> Process b -> Process a
prefix = Prefix

-- |Builds a process by pairing each element of two processes.
zip :: (Type a, Type b) => Process a -> Process b -> Process (a, b)
zip = Zip

-- |Applies the function to a single path of the process.
-- The path is a time series (every distribution in the process is degenerate).
trace :: (Type a, Type b) => Process a -> (Process a -> Process b) -> Process b
trace = Trace

-- |Skips ahead in the given process by the specified duration.
skip :: Dist Time -> Process a -> Process a
skip t process = case process of
    Closed f -> Closed (\t' -> f (t + t'))
    Zip p1 p2 -> Zip (skip t p1) (skip t p2)
    TagP tag p -> TagP tag (skip t p)
    Prefix f d0 p -> Prefix f (Lookup t (inclusivePrefix f d0 p)) (skip t p)
    Trace p f -> Trace p (skip t . f)

-------------------- Prelude --------------------

-- |Like prefix, but the whole process is delayed by the delta time
-- and the distribution at time zero is the given initial distribution.
inclusivePrefix :: (Type a, Type b) => (Dist Time -> Dist a -> Dist b -> Dist a) -> Dist a -> Process b -> Process a
inclusivePrefix f v p = map first (prefix (\d a w -> pair (second a) (f d (second a) w)) (pair v v) p)

integral = inclusivePrefix (\dt a v -> a + dt * v) 0

brownian = iterative (\dt a -> a + sqrt dt * normal) 0

time = closed id

true = constant True

false = constant False

always = Closed . const

uncurry :: (Type a, Type b, Type c) =>
    (Dist a -> Dist b -> Dist c) -> Dist (a, b) -> Dist c
uncurry f v = f (first v) (second v)

curry :: (Type a, Type b, Type c) =>
    (Dist (a, b) -> Dist c) -> Dist a -> Dist b -> Dist c
curry f a b = f (pair a b)

uncurry3 :: (Type a, Type b, Type c, Type d) =>
    (Dist a -> Dist b -> Dist c -> Dist d) -> Dist (a, (b, c)) -> Dist d
uncurry3 f v = uncurry (f (first v)) (second v)

curry3 :: (Type a, Type b, Type c, Type d) =>
    (Dist (a, (b, c)) -> Dist d) -> Dist a -> Dist b -> Dist c -> Dist d
curry3 f a b c = f (pair a (pair b c))

zip3 a b c = zip a (zip b c)

lift :: (Type a, Type b) =>
    (Dist a -> Dist b) -> Process a -> Process b
lift = map

lift2 :: (Type a, Type b, Type c) =>
    (Dist a -> Dist b -> Dist c) -> Process a -> Process b -> Process c
lift2 f p1 p2 = map (uncurry f) (zip p1 p2)

lift3 :: (Type a, Type b, Type c, Type d) =>
    (Dist a -> Dist b -> Dist c -> Dist d) -> Process a -> Process b -> Process c -> Process d
lift3 f p1 p2 p3 = map (uncurry3 f) (zip3 p1 p2 p3)

iterative :: (Type a) =>
    (Dist Time -> Dist a -> Dist a) -> Dist a -> Process a
iterative f i = inclusivePrefix (\dt a _ -> f dt a) i time

map :: (Type a, Type b) =>
    (Dist a -> Dist b) -> Process a -> Process b
map f = prefix (\_ _ d -> f d) (error "Accumulator variable should not be used in a map")

reverse :: (Type a) =>
    Dist Time -> Process a -> Process a
reverse end p = closed $ \t -> lookup (end - t) p

choice q d1 d2 = if_ (uniform .<. q) d1 d2

choose [d] = d
choose (d:ds) = choice (1 / fromIntegral (length (d:ds))) d (choose ds)

fold :: (Type a, Type b) => Dist Time -> Dist Time -> (Dist Time -> Dist a -> Dist b -> Dist a) -> (Dist a) -> Process b -> Dist a
fold t1 t2 op init p = lookup (t2 - t1) (prefix op init (skip t1 p))

maximum_ :: Dist Time -> Dist Time -> Process Double -> Dist Double
maximum_ t1 t2 p = fold t1 t2 (const max_) (-1/0) p

minimum_ :: Dist Time -> Dist Time -> Process Double -> Dist Double
minimum_ t1 t2 p = fold t1 t2 (const min_) (1/0) p

average :: Process Double -> Process Double
average process =
    let sumCount = prefix (\_ acc value -> pair (first acc + value) (second acc + 1) ) (pair 0 0) process in
    map (\p -> first p / second p) sumCount

-------------------- Constants --------------------

class ToConstant a where
    type ConstantType a
    constant :: ConstantType a -> a

instance ToConstant (Dist Double) where
    type ConstantType (Dist Double) = Double
    constant = Certain . Double

instance ToConstant (Dist Bool) where
    type ConstantType (Dist Bool) = Bool
    constant = Certain . Bool

instance ToConstant (Process Double) where
    type ConstantType (Process Double) = Double
    constant = always . constant

instance ToConstant (Process Bool) where
    type ConstantType (Process Bool) = Bool
    constant = always . constant

-------------------- Syntactic sugar for pairs --------------------

class Pair a where
    pair :: (Type b, Type c) => a b -> a c -> a (b, c)
    first :: (Type b, Type c) => a (b, c) -> a b
    second :: (Type b, Type c) => a (b, c) -> a c

instance Pair Dist where
    pair = Binary Pair
    first = Unary First
    second = Unary Second

instance Pair Process where
    pair a b = zip a b
    first = lift first
    second = lift second

-------------------- Conditional operator --------------------

class If_ a where
    if_ :: Type b => a Bool -> a b -> a b -> a b

instance If_ Dist where
    if_ = Ternary If

instance If_ Process where
    if_ = lift3 if_

-------------------- Ord emulation --------------------

infix 4 .<.
infix 4 .<=.
infix 4 .>.
infix 4 .>=.
infix 4 .==.
infix 4 ./=.

class Type b => Ordered a b where
    min_ :: a b -> a b -> a b
    max_ :: a b -> a b -> a b
    (.<.) :: a b -> a b -> a Bool
    (.<=.) :: a b -> a b -> a Bool
    (.>.) :: a b -> a b -> a Bool
    (.>=.) :: a b -> a b -> a Bool
    (.==.) :: a b -> a b -> a Bool
    (./=.) :: a b -> a b -> a Bool

infixr 3 .&&.
infixr 2 .||.

class Boolean a where
    (.||.) :: a Bool -> a Bool -> a Bool
    (.&&.) :: a Bool -> a Bool -> a Bool
    not_ :: a Bool -> a Bool

-------------------- Instances for Double --------------------

instance Eq (Dist Double) where
    (==) = error "Eq is not defined for Dist Double, but is still instantiated to provide Num (Dist Double)"

instance Show (Dist a) where
    show = showDist 0

instance Num (Dist Double) where
    fromInteger = constant . fromIntegral
    (+) = Binary Add
    (-) = Binary Sub
    (*) = Binary Mult
    abs = Unary Abs
    signum = Unary Sign
    negate = Unary Negate

instance Fractional (Dist Double) where
    fromRational = constant . fromRational
    (/) = Binary Div
    recip = (1.0 /)

instance Floating (Dist Double) where
    exp = Unary Exp
    pi = constant pi
    log = Unary Log
    sqrt = Unary Sqrt
    sin = Unary Sin
    cos = Unary Cos
    tan = Unary Tan
    asin = Unary Asin
    acos = Unary Acos
    atan = Unary Atan
    sinh = Unary Sinh
    cosh = Unary Cosh
    tanh = Unary Tanh
    asinh = Unary Asinh
    acosh = Unary Acosh
    atanh = Unary Atanh
    (**) = Binary Power
    logBase = Binary LogBase

instance Ordered Dist Double where
    min_ = Binary Min
    max_ = Binary Max
    (.<.) = Binary Less
    (.<=.) = Binary LessEqual
    (.>.) = Binary Greater
    (.>=.) = Binary GreaterEqual
    (.==.) = Binary Equal
    (./=.) = Binary NotEqual

instance Boolean Dist where
    (.||.) = Binary Or
    (.&&.) = Binary And
    not_ = Unary Not

instance Eq (Process Double) where
    (==) = error "Eq is not defined for Process Double, but is still instantiated to provide Num (Process Double)"

instance Show (Process a) where
    show = showProcess 0

instance Num (Process Double) where
    fromInteger = constant . fromInteger
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    abs = lift abs
    signum = lift signum
    negate = lift negate

instance Fractional (Process Double) where
    fromRational = constant . fromRational
    (/) = lift2 (/)
    recip = (1.0 /)

instance Floating (Process Double) where
    exp = lift exp
    pi = constant pi
    log = lift log
    sqrt = lift sqrt
    sin = lift sin
    cos = lift cos
    tan = lift tan
    asin = lift asin
    acos = lift acos
    atan = lift atan
    sinh = lift sinh
    cosh = lift cosh
    tanh = lift tanh
    asinh = lift asinh
    acosh = lift acosh
    atanh = lift atanh
    (**) = lift2 (**)
    logBase = lift2 logBase

instance Ordered Process Double where
    min_ = lift2 min_
    max_ = lift2 max_
    (.<.) = lift2 (.<.)
    (.<=.) = lift2 (.<=.)
    (.>.) = lift2 (.>.)
    (.>=.) = lift2 (.>=.)
    (.==.) = lift2 (.==.)
    (./=.) = lift2 (./=.)

instance Boolean Process where
    (.||.) = lift2 (.||.)
    (.&&.) = lift2 (.&&.)
    not_ = lift not_

-------------------- Auxiliaries --------------------

showDist :: Int -> Dist a -> String
showDist x dist = case dist of
    Certain value -> case value of
        Double value -> show value
        Bool value -> H.map toLower (show value)
    Unary op d1 -> parenthesize $ show op ++ " " ++ showDist x d1
    Binary op d1 d2 -> parenthesize $ showDist x d1 ++ " " ++ show op ++ " " ++ showDist x d2
    Ternary If d1 d2 d3 -> parenthesize $ "if " ++ showDist x d1 ++ " " ++ showDist x d2 ++ " " ++ showDist x d3
    Uniform -> "uniform"
    Normal -> "normal"
    Lookup time process -> parenthesize $ "lookup " ++ showDist x time ++ " " ++ show process
    Sample d f -> parenthesize $ showDist x d ++ " ‘sample‘ " ++ showDistFunction x f
    TagD i -> showDistVar i

showDistVar :: Int -> String
showDistVar x = "d" ++ show x

showDistArgument :: Int -> Dist a -> String
showDistArgument x d | varUsedInDist x d = showDistVar x
showDistArgument x _ = "_"

showDistFunction :: Type a => Int -> (Dist a -> Dist b) -> String
showDistFunction x f = "\\" ++ showDistVar x ++ " -> " ++ showDist (x + 1) (f (TagD x))

showProcess :: Int -> Process a -> String
showProcess x process = case process of
    Closed f | varUsedInDist x (f (TagD x)) -> parenthesize $ "closed \\" ++
        showDistVar x ++ " -> " ++ showDist (x + 1) (f (TagD x))
            | otherwise -> showDist x (f (TagD undefined))
    Prefix f d0 p ->
        let body = f (TagD x) (TagD (x + 1)) (TagD (x + 2)) in
        let a1 = showDistArgument x body in
        let a2 = showDistArgument (x + 1) body in
        let a3 = showDistArgument (x + 2) body in
        let d0' = if varUsedInDist (x + 1) body then showDist x d0 else "_" in
        let p' = if varUsedInDist (x + 2) body then showProcess x p else "_" in
        parenthesize $ "prefix " ++ lambda [a1, a2, a3] (showDist (x + 3) body) ++ " " ++ d0' ++ " " ++ p'
    Zip p1 p2 -> parenthesize $ "zip " ++ showProcess x p1 ++ " " ++ showProcess x p2
    Trace p f ->
        let body = f (TagP x undefined) in
        let a1 = showProcessArgument x body in
        parenthesize $ showProcess x p ++ " ‘trace‘ " ++ lambda [a1] (showProcess (x + 1) body)
    TagP x _ -> showProcessVar x

showProcessVar :: Int -> String
showProcessVar x = "p" ++ show x

showProcessArgument :: Int -> Process a -> String
showProcessArgument x p | varUsedInProcess x p = showProcessVar x
showProcessArgument x _ = "_"

lambda :: [String] -> String -> String
lambda args body = parenthesize $ "\\" ++ (concatMap (++ " ") args) ++ "-> " ++ unparenthesize body

parenthesize s = "(" ++ s ++ ")"

unparenthesize s =
    if length s >= 2 && head s == '(' && s !! (length s - 1) == ')'
    then take (length s - 2) (tail s)
    else s