
{-# LANGUAGE GADTs, KindSignatures, TypeFamilies, OverlappingInstances,
EmptyDataDecls, DeriveDataTypeable, StandaloneDeriving, FlexibleInstances,
ScopedTypeVariables #-}
module Syntax (
module Operator,
module Type,
    Time,
    Dist (..),
    Process (..),
    Type (..),
    varUsedInDist,
    varUsedInProcess,
    examine,
    usesAccumulator
) where

--import Operator
--import Type

import Data.Typeable
import System.Random

type Time = Double

data Dist :: * -> * where
    Normal :: Dist Double
    Uniform :: Dist Double
    Lookup :: (Type a) => Dist Time -> Process a -> Dist a
    Certain :: (Type a) => Constant a -> Dist a
    Sample :: (Type a, Type b) => Dist a -> (Dist a -> Dist b) -> Dist b
    Unary :: (Type a1, Type a2) => UnaryOperator a1 a2 -> Dist a1 -> Dist a2
    Binary :: (Type a1, Type a2, Type a3) => BinaryOperator a1 a2 a3 -> Dist a1 -> Dist a2 -> Dist a3
    Ternary :: (Type a1, Type a2, Type a3, Type a4) => TernaryOperator a1 a2 a3 a4 -> Dist a1 -> Dist a2 -> Dist a3 -> Dist a4
    TagD :: (Type a) => Int -> Dist a

data Process :: * -> * where
    Closed :: (Type a) => (Dist Time -> Dist a) -> Process a
    Prefix :: (Type a, Type b) => (Dist Time -> Dist a -> Dist b -> Dist a) -> Dist a -> Process b -> Process a
    Zip :: (Type a, Type b) => Process a -> Process b -> Process (a, b)
    Trace :: (Type a, Type b) => Process a -> (Process a -> Process b) -> Process b
    TagP :: (Type a) => Int -> Process a -> Process a


-- | Contains the types that we can represent (in ’Dist’ and ’Process’).
class Typeable a => Type a where
    splType :: a -> SPLType
    toDist :: a -> Dist a

instance Type Double where
    splType _ = DoubleType
    toDist = Certain . Double

instance Type Bool where
    splType _ = BoolType
    toDist = Certain . Bool

deriving instance Typeable StdGen

instance Type StdGen where
    splType _ = GeneratorType
    toDist = error "Unimplemented" -- TODO: Fix this (although it cannot happen if we don’t expose toDist)

instance forall a b. (Type a, Type b) => Type (a, b) where
    splType _ = PairType (splType (error "splType" :: a)) (splType (error "splType" :: b))
    toDist (a, b) = Binary Pair (toDist a) (toDist b)

varUsedInDist :: Int -> Dist a -> Bool
varUsedInDist x dist = case dist of
    TagD i -> i == x
    Unary _ d1 -> varUsedInDist x d1
    Binary _ d1 d2 -> varUsedInDist x d1 || varUsedInDist x d2
    Ternary If d1 d2 d3 -> varUsedInDist x d1 || varUsedInDist x d2 || varUsedInDist x d3
    Lookup time process -> varUsedInDist x time || varUsedInProcess x process
    Sample d f -> varUsedInDist x d || varUsedInDist x (f (TagD (x+1)))
    _ -> False

varUsedInProcess :: Int -> Process a -> Bool
varUsedInProcess x process = case process of
    TagP i _ -> x == i
    Closed f -> varUsedInDist x (f (TagD (x+1)))
    Trace p f -> varUsedInProcess x p || varUsedInProcess x (f (TagP (x+1) undefined ))
    Prefix f d0 p ->
        let body = f (TagD (x+1)) (TagD (x+2)) (TagD (x+3)) in
        let usingAccumulator = varUsedInDist (x + 2) body in
        let usedInAccumulator = usingAccumulator && varUsedInDist x d0 in
        varUsedInDist x body || usedInAccumulator || varUsedInProcess x p
    Zip p1 p2 -> varUsedInProcess x p1 || varUsedInProcess x p2

examine :: (Type a1, Type a2, Type a) =>
    (Dist a -> Dist a1 -> Dist a2 -> Dist a3) -> (Bool, Bool, Bool)
examine f =
    let (x1, x2, x3) = (-4, -3, -2) in
    let body = f (TagD x1) (TagD x2) (TagD x3) in
    (varUsedInDist x1 body, varUsedInDist x2 body, varUsedInDist x3 body)

usesAccumulator f = let (_, used, _) = examine f in used





