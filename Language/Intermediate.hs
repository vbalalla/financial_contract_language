{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses, FlexibleContexts,
FlexibleInstances, ScopedTypeVariables, TypeFamilies, EmptyDataDecls #-}
module Language.SPL.Intermediate (
Intermediate (..),
Index (..), Layout (..), peek,
Translate (..), translate, convert,
Accumulator (..),
stochastic
) where
import qualified Language.SPL.Syntax as S
import qualified Language.SPL.Operator as S
import qualified Language.SPL as S
import Language.SPL.SimulationResult (SimulationResult)
import System.Random
import Data.Typeable
import Data.Maybe
import Control.Monad (guard)
data Index :: * -> * -> * where
Zero :: Index (env, a) a
Succ :: Index env a -> Index (env, b) a
data Intermediate :: * -> * -> * where
Uniform :: Intermediate env Double
Normal :: Intermediate env Double
Let :: (S.Type a, S.Type b) =>
Intermediate env a -> Intermediate (env, a) b -> Intermediate env b
Constant :: (S.Type a) =>
(S.Constant a) -> Intermediate env a
Unary :: (S.Type a, S.Type b) =>
(S.UnaryOperator a b) -> Intermediate env a -> Intermediate env b
Binary :: (S.Type a, S.Type b, S.Type c) =>
(S.BinaryOperator a b c) -> Intermediate env a -> Intermediate env b ->
Intermediate env c
If :: (S.Type a) =>
Intermediate env Bool -> Intermediate env a -> Intermediate env a ->
Intermediate env a
Prefix :: (S.Type a) =>
Bool -> Intermediate env S.Time -> Accumulator env a -> Intermediate env a
Variable :: (S.Type a) =>
Index env a -> Intermediate env a
Split :: (S.Type a) =>
Intermediate (env, StdGen) a -> Intermediate env a
Use :: (S.Type a) =>
Index env StdGen -> Intermediate env a -> Intermediate env a
data Accumulator :: * -> * -> * where
Accumulate :: (S.Type a, S.Type b) =>
Intermediate (((env, S.Time), a), b) a ->
Bool -> Maybe (Intermediate env a) -> Maybe (Accumulator env b) ->
Accumulator env a
Zip :: (S.Type a, S.Type b) =>
Accumulator env a -> Accumulator env b -> Accumulator env (a, b)
Expression :: (S.Type a) =>
Bool -> Intermediate (env, S.Time) a -> Accumulator env a
Splitting :: (S.Type a) =>
Accumulator (env, StdGen) a -> Accumulator env a
Using :: (S.Type a) =>
Index env StdGen -> Accumulator env a -> Accumulator env a
data Layout :: * -> * -> * where
Empty :: Layout env ()
Push :: (S.Type a) =>
Layout env env’ -> Index env a -> Layout env (env’, a)
size :: Layout env env’ -> Int
size Empty = 0
size (Push layout _) = size layout + 1
increase :: Layout env env’ -> Layout (env, a) env’
increase Empty = Empty
increase (Push layout index) = Push (increase layout) (Succ index)
project :: (S.Type a) => Int -> Layout env env’ -> Index env a
project _ Empty = error "Cannot project an empty layout"
project 0 (Push _ index) = fromJust (gcast index)
project n (Push layout _) = project (n - 1) layout
peek :: Index env a -> env -> a
peek Zero (_, a) = a
peek (Succ n) (environment, _) = peek n environment
translate :: Translate () a => a -> Intermediate (EnvironmentOf () a) (ResultOf a)
translate = translate’ Empty
class Translate env a where
type EnvironmentOf env a
type ResultOf a
translate’ :: Layout env env -> a -> Intermediate (EnvironmentOf env a) (
ResultOf a)
instance S.Type a => Translate env (S.Dist a) where
type EnvironmentOf env (S.Dist a) = env
type ResultOf (S.Dist a) = a
translate’ = convert’
instance forall env a b. (Translate (env, a) b, S.Type a) => Translate env (S.Dist a
-> b) where
type EnvironmentOf env (S.Dist a -> b) = EnvironmentOf (env, a) b
type ResultOf (S.Dist a -> b) = ResultOf b
translate’ layout f = translate’ layout’ (f tag)
where
(tag, layout’) = distTag layout
instance forall env a b. (Translate (env, a) b, S.Type a) => Translate env (S.
Process a -> b) where
type EnvironmentOf env (S.Process a -> b) = EnvironmentOf (env, a) b
type ResultOf (S.Process a -> b) = ResultOf b
translate’ layout f = translate’ layout’ (f (S.always tag))
where
(tag, layout’) = distTag layout
convert :: S.Dist a -> Intermediate () a
convert = convert’ Empty
convert’ :: Layout env env -> S.Dist a -> Intermediate env a
convert’ layout term = case term of
S.TagD tag -> Variable (project (size layout - tag - 1) layout)
S.Certain constant -> Constant constant
S.Uniform -> Uniform
S.Normal -> Normal
S.Sample e f -> Let (convert’ layout e) (convert’ layout’ (f tag))
where
(tag, layout’) = distTag layout
S.Unary op e1 -> Unary op (convert’ layout e1)
S.Binary op e1 e2 -> Binary op (convert’ layout e1) (convert’ layout e2)
S.Ternary S.If e1 e2 e3 -> If (convert’ layout e1) (convert’ layout e2) (convert
’ layout e3)
S.Lookup e p -> lookup layout e p
where
lookup :: Layout env env -> S.Dist S.Time -> S.Process a -> Intermediate env
a
lookup layout time process = case process of
p@(S.Prefix _ _ p’) ->
Prefix (usesTimeInProcess p’) (convert’ layout time) (accumulator
layout p)
S.Zip p1 p2 ->
Binary S.Pair (lookup layout time p1) (lookup layout time p2)
S.Closed f ->
convert’ layout (f time)
S.Trace p f ->
Split (lookup layout’ time (f tag))
where
(tag, layout’) = processTag layout p
S.TagP tag p ->
Use (project (size layout - tag - 1) layout) (lookup layout time p)
accumulator :: (S.Type a) => Layout env env -> S.Process a -> Accumulator
env a
accumulator layout process = case process of
S.Zip p1 p2 ->
let a1 = accumulator layout p1 in
let a2 = accumulator layout p2 in
Zip a1 a2
S.Prefix f e p ->
let accumulator’ = accumulator layout p in
let initialValue = convert’ layout e in
let (timeTag, layout’) = distTag layout in
let (accumulateTag, layout’’) = distTag layout’ in
let (valueTag, layout’’’) = distTag layout’’ in
let f’ = convert’ layout’’’ (f timeTag accumulateTag valueTag) in
let (useDt, useAccumulator, useProcess) = S.examine f in
let justWhen :: Bool -> a -> Maybe a
justWhen condition a = guard condition >> return a in
Accumulate f’ useDt (justWhen useAccumulator initialValue) (justWhen
useProcess accumulator’)
S.Trace p f -> Splitting (accumulator layout’ (f tag))
where
(tag, layout’) = processTag layout p
S.TagP tag p ->
Using (project (size layout - tag - 1) layout) (accumulator layout p
)
p@(S.Closed _) ->
let (tag, layout’) = distTag layout in
Expression (usesTimeInProcess p) (lookup layout’ tag p)
distTag :: S.Type a =>
Layout env env’ ->
(S.Dist a, Layout (env, a) (env’, a))
distTag layout = (S.TagD (size layout), increase layout ‘Push‘ Zero)
processTag :: S.Type a =>
Layout env env’ ->
S.Process a ->
(S.Process a, Layout (env, StdGen) (env’, StdGen))
processTag layout p = (S.TagP (size layout) p, increase layout ‘Push‘ Zero)
usesTimeInProcess :: S.Process a -> Bool
usesTimeInProcess process = case process of
S.Zip p1 p2 -> usesTimeInProcess p1 || usesTimeInProcess p2
S.Prefix _ _ p -> usesTimeInProcess p
S.Trace p f -> usesTimeInProcess (f p)
S.Closed f -> S.varUsedInDist 0 (f (S.TagD 0))
S.TagP _ p -> usesTimeInProcess p
stochastic :: Intermediate env a -> Bool
stochastic process = case process of
Uniform -> True
Normal -> True
Let e1 e2 -> stochastic e1 || stochastic e2
Constant _ -> False
Unary _ e1 -> stochastic e1
Binary _ e1 e2 -> stochastic e1 || stochastic e2
If e1 e2 e3 -> stochastic e1 || stochastic e2 || stochastic e3
Prefix _ e a -> stochastic e || stochasticAccumulator a
Variable _ -> False
Split e -> stochastic e
Use _ e -> False
stochasticAccumulator :: Accumulator env a -> Bool
stochasticAccumulator accumulator = case accumulator of
Accumulate e1 _ e2 a -> stochastic e1 || maybe False stochastic e2 || maybe
False stochasticAccumulator a
Zip a1 a2 -> stochasticAccumulator a1 || stochasticAccumulator a2
Expression _ e -> stochastic e
Splitting a -> stochasticAccumulator a
Using _ a -> stochasticAccumulator a
instance Show (Intermediate env a) where
show intermediate = case intermediate of
Uniform -> "uniform"
Normal -> "normal"
Let e1 e2 -> "(let " ++ show e1 ++ " in " ++ show e2 ++ ")"
Constant (S.Double value) -> show value
Constant (S.Bool value) -> show value
Unary op e1 -> "(" ++ show op ++ " (" ++ show e1 ++ "))"
Binary op e1 e2 -> "(" ++ show e1 ++ show op ++ show e2 ++ ")"
If e1 e2 e3 -> "(if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show
e3 ++ ")"
Variable index -> "x" ++ show index
Split e -> "(split (" ++ show e ++ "))"
Use index e -> "(use g" ++ show index ++ " within " ++ show e ++ ")"
Prefix usingTime e a -> "(prefix " ++ show usingTime ++ " " ++ show e ++ " "
++ show a ++ ")"
instance Show (Accumulator env a) where
show accumulator = case accumulator of
Accumulate f useDt d0 acc -> "(accumulate " ++ show f ++ " " ++ show useDt
++ " " ++ show d0 ++ " " ++ show acc ++ ")"
Zip acc1 acc2 -> "(accumulateZip " ++ show acc1 ++ " " ++ show acc2 ++ ")"
Expression _ p -> "(accumulateOther " ++ show p ++ ")"
Splitting p -> "(splitting " ++ show p ++ ")"
Using index p -> "(using " ++ show index ++ " " ++ show p ++ ")"
instance Show (Index env a) where
show = show . indexToInt
indexToInt :: Index env a -> Int
indexToInt Zero = 0
indexToInt (Succ n) = indexToInt n + 1

