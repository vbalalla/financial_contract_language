-- import System.Random
--
-- data Coin = Heads | Tails deriving (Show, Enum, Bounded)
--
-- instance Random Coin where
--   randomR (a, b) g =
--     case randomR (fromEnum a, fromEnum b) g of
--       (x, g') -> (toEnum x, g')
--   random g = randomR (minBound, maxBound) g
--
-- main = do
--   g <- newStdGen
--   print . take 1 $ (randoms g :: [Coin])


import System.IO.Unsafe  -- be careful!
import System.Random

data Dist = Dist Float Float

mean :: Dist -> Float
mean (Dist m sd) = m

std :: Dist -> Float
std (Dist m sd) = sd

-- c :: Float
-- c = unsafePerformIO (getStdRandom (randomR (90, 110))) / 100

-- x :: [IO Float]
-- -- x = 10 : map (* (unsafePerformIO (getStdRandom (randomR (90, 110))) / 100)) x
-- x = (getStdRandom (randomR (90, 110))) : x

-- randList :: Int -> [[Float]]
-- randList 0 = []
-- randList n = (randList (n-1)) : y
--     where
--     y = map (/ 100) (map unsafePerformIO x)
--         where x = (getStdRandom (randomR (90, 110))) : x

-- y1 :: [Float]
-- y1 = map (/ 100) (map unsafePerformIO x1)
--     where x1 = (getStdRandom (randomR (90, 110))) : x1
--
-- y2 :: [Float]
-- y2 = map (/ 100) (map unsafePerformIO x2)
--     where x2 = (getStdRandom (randomR (90, 110))) : x2
--
-- y3 :: [Float]
-- y3 = map (/ 100) (map unsafePerformIO x3)
--     where x3 = (getStdRandom (randomR (90, 110))) : x3
--
-- y4 :: [Float]
-- y4 = map (/ 100) (map unsafePerformIO x4)
--     where x4 = (getStdRandom (randomR (90, 110))) : x4

-- z :: [Float]
-- z = map

interestWalk :: Float -> [Float] -> [Float]
interestWalk = walk
    where walk currentIr (x:xs) = (currentIr * x) : (walk (currentIr * x) xs)

-- z1 = interestWalk 0.01 y1
--
-- z2 = interestWalk 0.01 y2
--
-- z3 = interestWalk 0.01 y3
--
-- z4 = interestWalk 0.01 y4

interestWalkAll :: Float -> Int -> [[Float]]
interestWalkAll a = walkMore
    where
    walkMore 0 = []
    walkMore n = (walkMore (n-1)) ++ [(interestWalk a (map (/ 100) (map unsafePerformIO x)))]
                                                           where x = (getStdRandom (randomR (90, 110))) : x


m = interestWalkAll 0.01 50

d = Dist 3.4 2.4

-- distMean :: [Float] -> [Float] -> [Float] -> [Float] -> Int -> Float
-- distMean z1 z2 z3 z4 t = (sum l) / (fromIntegral (length l) :: Float)
--     where l = (z1 !! t) : (z2 !! t) : (z3 !! t) : (z4 !! t) : []

-- x = distMean z1 z2 z3 z4 5



disc :: [[Float]] -> Float -> Int -> Int -> Float
disc intr p k = discin
    where
    discin 0 = p
    discin n = (discin (n-1))*(1 + (intr!!k!!(n-1)))


discSum :: [[Float]] -> Float -> Int -> Int -> Float
discSum intr p 0 n = disc intr p 0 n
discSum intr p k n = (discSum intr p (k-1) n) + (disc intr p k n)

discMean :: [[Float]] -> Float -> Int -> Float
discMean intr p n = (discSum intr p ((length intr)-1) n)/(fromIntegral (length intr))

discVarSum :: [[Float]] -> Float -> Int -> Int -> Float
discVarSum intr p 0 n = 0
discVarSum intr p k n = (((disc intr p k n) - dmean) * ((disc intr p k n) - dmean)) + (discVarSum intr p (k-1) n)
    where dmean = discMean intr p n

discVar :: [[Float]] -> Float -> Int -> Float
discVar intr p n = (discVarSum intr p ((length intr)-1) n) / (fromIntegral ((length intr)-1))

discAll :: [[Float]] -> Float -> Int -> Dist
discAll intr p n = Dist (discMean intr p n) (discVar intr p n)


v = discAll m 100 4


