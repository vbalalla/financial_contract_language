{-# LANGUAGE GADTs #-}
module Semantics where
import Syntax
import Operator
import Language.SPL ()
import Prelude hiding (Real)
import Control.Monad
type Real = Double
delta = 0.1
end = 10
class Monad m => ProbabilityMonad m where
uniform’ :: m Real
normal’ :: m Real
process :: ProbabilityMonad m => Process a -> m [a]
process p = case p of
Closed f -> mapM (distribution . f . Certain . Double) [0, delta .. end]
Prefix f i p | usesAccumulator f -> do
i’ <- distribution i
p’ <- process p
let accumulate a v = distribution (f (toDist delta) (toDist a) (toDist v))
l <- scanM accumulate i’ p’
return (tail l)
Prefix f i p -> do
p’ <- process p
mapM (distribution . f (toDist delta) undefined . toDist) p’
Zip p1 p2 -> do
p1’ <- process p1
p2’ <- process p2
return (zip p1’ p2’)
Trace p f -> do
p’ <- process p
let s = Closed (\(Certain (Double t’)) -> toDist (index t’ p’))
process (f s)
distribution :: ProbabilityMonad m => Dist a -> m a
distribution d = case d of
Uniform -> uniform’
Normal -> normal’
Certain (Double v) -> return v
Certain (Bool v) -> return v
Lookup t p -> do
t’ <- distribution t
p’ <- process p
return (index t’ p’)
Sample d f -> do
d’ <- distribution d
distribution (f (toDist d’))
Unary o d -> do
d’ <- distribution d
return (unaryOperator o d’)
Binary o d1 d2 -> do
d1’ <- distribution d1
d2’ <- distribution d2
return (binaryOperator o d1’ d2’)
Ternary o d1 d2 d3 -> do
d1’ <- distribution d1
d2’ <- distribution d2
d3’ <- distribution d3
return (ternaryOperator o d1’ d2’ d3’)
index t l = l !! floor (t / delta)
scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM f i [] = return [i]
scanM f i (x:xs) = do
i’ <- f i x
is <- scanM f i’ xs
return (i:is)
{-
processAt :: ProbabilityMonad m => Process a -> Time -> m a
processAt p t = case p of
Closed f -> distribution (f (Certain (Double t)))
Prefix f i p | t == 0 -> do
i’ <- distribution i
p’ <- processAt p t
distribution (f (toDist delta) (toDist i’) (toDist p’))
Prefix f i p -> do
a <- processAt (Prefix f i p) (t - delta)
p’ <- processAt p t
distribution (f (toDist delta) (toDist a) (toDist p’))
Zip p1 p2 -> do
p1’ <- processAt p1 t
p2’ <- processAt p2 t
return (p1’, p2’)
-- The case for Trace is wrong: it mixes up values from different time series
Trace p f -> do
l <- sequence [processAt p t’ | t’ <- [0, delta .. end]]
let index t’ = toDist (l !! floor (t’ / delta))
let s = Closed (\(Certain (Double t’)) -> index t’)
processAt (f s) t
-}