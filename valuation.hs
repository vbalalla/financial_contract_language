{-# LANGUAGE FlexibleInstances  #-}

{- Experimental DSL which allows to use relatively friendly syntax
to create financial models operating contracts, amounts, currencies.

Typechecks amounts - amounts in different currencies cannot be added
accidentally.

Defines primitive contracts (european, american, etc.) for convenience.

Knows how to simplify contract and how to convert contract to amount
in the main currency at a given time using simplistic model of exchange rates.

-}


module DSL where
------------------------------------------------------------------------------
import Data.Decimal
import qualified Data.List as L
------------------------------------------------------------------------------
import System.IO.Unsafe  -- be careful!
import System.Random

        -- Contract

------------------------------------------------------------------------------

data Contract = Contract Name Terms deriving Show

name :: Contract -> Name
name (Contract n t) = n

terms :: Contract -> Terms
terms (Contract n t) = t

type Name = String
data Terms =
    Zero
    | One  Amount
    | Give Terms
    | And  Terms Terms
    | Or   Terms Terms
    | Cond    (Obs Bool)   Terms Terms
    | Scale   (Obs Int)    Terms
    | When    (Obs Bool)   Terms
    deriving Show
------------------------------------------------------------------------------


        -- Time, Period, Random value, Observable

------------------------------------------------------------------------------
data PeriodName = Month | Months

type Time   = Integer
type Period = Integer

instance Num (PeriodName -> Time) where
    fromInteger t Month = t::Time
    fromInteger t Months = t

newtype Obs a = Obs (Time -> a)

getValue :: Obs a -> Time -> a
getValue (Obs x) time = x time

instance Show a => Show (Obs a) where
    show (Obs obs) = "(Obs " ++ show (obs 0) ++ ")"

konst :: a -> Obs a
konst k = Obs (\t -> k)

at :: Time -> Obs Bool
at t = Obs (\time -> (time == t))

lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
lift2 f (Obs o1) (Obs o2) = Obs (\t -> f (o1 t) (o2 t))

date :: Obs Time
date = Obs (\t -> t::Time)



-- Compare observables
(%<), (%<=), (%==), (%>=), (%>) :: Ord a => Obs a -> Obs a -> Obs Bool
(%<)  = lift2 (<)
(%>)  = lift2 (>)
(%==) = lift2 (==)
(%>=) = lift2 (>=)
(%<=) = lift2 (<=)

type Term            = [Time]
type PaymentSchedule = [Amount]
------------------------------------------------------------------------------


        -- Currencies and amounts

------------------------------------------------------------------------------
data Currency = AUD | NZD | USD deriving (Eq, Show)
data Amount = Amt Decimal Currency

instance Show Amount where
    show (Amt amt currency) = show amt ++ show currency

instance Num (Currency -> Amount) where
    fromInteger amt c = Amt (Decimal 0 amt) c

instance Num Amount where
    (-) (Amt a1 c1) (Amt a2 c2)
        | (c1 == c2) = Amt (a1-a2) c1
    (+) (Amt a1 c1) (Amt a2 c2)
        | (c1 == c2) = Amt (a1+a2) c1

instance Eq Amount where
    (==) (Amt a1 c1) (Amt a2 c2) = (a1==a2) && (c1==c2)

instance Ord Amount where
    compare (Amt a1 c1) (Amt a2 c2)
        | (c1 == c2) = compare a1 a2

instance Eq Terms where
    (==) (One a1) (One a2) = (a1 == a2)

instance Ord Terms where
    compare (One a1) (One a2) = compare a1 a2
    compare (One a1) Zero = compare a1 0
    compare Zero (One a2) = compare 0 a2
    compare Zero Zero = EQ

instance Eq Contract where
    (==) (Contract n1 t1) (Contract n2 t2) = (t1 == t2)

instance Ord Contract where
    compare (Contract n1 t1) (Contract n2 t2) = compare t1 t2


amountToDecimal (Amt a c) = a
------------------------------------------------------------------------------



        -- Operations on Terms

------------------------------------------------------------------------------
zero :: Terms
zero = Zero

one :: Amount -> Terms
one = One

scale :: Obs Int -> Terms -> Terms
scale = Scale

give :: Terms -> Terms
give = Give

and :: Terms -> Terms -> Terms
and = And

or :: Terms -> Terms -> Terms
or = Or

cond :: Obs Bool -> Terms -> Terms -> Terms
cond = Cond

when :: Obs Bool -> Terms -> Terms
when = When
------------------------------------------------------------------------------


        -- Operations on contracts

------------------------------------------------------------------------------
(.+) :: Contract -> Contract -> Contract
(.+) (Contract n1 t1) (Contract n2 t2) =
    Contract (n1 ++ " _And_ " ++ n2) (t1 `DSL.and` t2)
------------------------------------------------------------------------------


        -- Specifics

------------------------------------------------------------------------------
-- Zero coupon bond
zcb :: Time -> Amount -> Terms
zcb t amt = when (at t) (one amt)

-- European option
european :: Time -> Currency -> Terms -> Terms
european t c u = when (at t) (u `DSL.or` Zero)

-- American option
american :: (Time, Time) -> Terms -> Terms
american (t1, t2) u = when (between t1 t2) u

-- Option
between :: Time -> Time -> Obs Bool
between t1 t2 = lift2 (&&) (date %>= (konst t1)) (date %<= (konst t2))
------------------------------------------------------------------------------



        -- Modelling

------------------------------------------------------------------------------
type ExchangeRate = Double

maxT :: ExchangeRate -> Terms -> Terms -> Terms
maxT exchR Zero Zero = Zero
maxT exchR (One (Amt a1 c1)) Zero = One $ Amt (max a1 0) c1
maxT exchR Zero (One (Amt a2 c2)) = One $ Amt (max 0 a2) c2
maxT exchR (One (Amt a1 c1)) (One (Amt a2 c2)) = One $ Amt (max a1 a21) c1
    where
        a21 = a2 *. (realToFrac exchR)

getCurrency :: Amount -> Currency
getCurrency (Amt amt currency) = currency

convertTo :: Model -> Time -> Currency ->  Terms -> Terms
convertTo m t c0 Zero = Zero
convertTo m t c0 (One (Amt amt c))
    | (c0 == c)  = One $ Amt amt c0 -- No need to convert anything
    | otherwise  = One $ Amt (amt *. currentER) c0
    where
        -- Exchange Rates
        observableER = (exchangeRate m) c c0
        currentER    = realToFrac $ getValue observableER t


data Model = Model {
    mainCurrency :: Currency,
    depositRate :: Obs Decimal,
    loanRate :: Obs Decimal,
    interestRate :: Obs Float ,
    exchangeRate :: Currency -> Currency -> Obs ExchangeRate }

evalContractAt :: Model -> Time -> Contract -> Contract
evalContractAt m t (Contract name terms) = Contract name (evalTermsAt m t terms)

-- Simplify contract, based on the provided time and model
evalTermsAt :: Model -> Time -> Terms -> Terms
evalTermsAt m t = eval
    where
        eval Zero                      = Zero
        eval (One amt)                 = convertTo m t (mainCurrency m) $ One amt
        eval (Give c)                  = eval (scale (Obs (\t -> (-1))) (eval c))
        eval (Zero `And` Zero)         = Zero
        eval (Zero `And` (One amt))    = One amt
        eval ((One amt) `And` Zero)    = One amt
        eval (c1 `And` c2)             = eval ((eval c1) `DSL.and` (eval c2))
        eval (c1 `Or`  c2)             = maxT (getValue ((exchangeRate m) USD USD) t) (eval c1) (eval c2)
        eval (Cond (Obs o) c1 c2)      = if (o t) then (eval c1) else (eval c2)
        eval (When (Obs o) c)          = if (o t) then (eval c) else Zero
        eval (Scale (Obs s) (One (Amt amt cur))) = One $ Amt (amt *. (realToFrac $ s t)) cur

data Dist = Dist Decimal Decimal

mean :: Dist -> Decimal
mean (Dist m sd) = m

std :: Dist -> Decimal
std (Dist m sd) = sd

zeroP :: Obs Dist
zeroP = (konst (Dist 0 0))

oneP :: Amount -> Obs Dist
oneP amt = konst (Dist (amountToDecimal amt) 0)

convertToP :: Model -> Time -> Currency ->  Terms -> Obs Dist
convertToP m t c0 Zero = zeroP
convertToP m t c0 (One (Amt amt c))
    | (c0 == c)  = oneP $ Amt amt c0 -- No need to convert anything
    | otherwise  = oneP $ Amt (amt *. currentER) c0
    where
        -- Exchange Rates
        observableER = (exchangeRate m) c c0
        currentER    = realToFrac $ getValue observableER t

scaleP ::Obs Decimal -> Obs Dist -> Obs Dist
scaleP (Obs o) (Obs p) = Obs (\t -> (Dist ((mean (p t)) * (o t)) ((std (p t)) * (o t))))

andP :: Obs Dist -> Obs Dist -> Obs Dist
-- andP (Obs p1) (Obs p2) = Obs (\t -> (Dist ((mean (p1 t)) + (mean (p2 t))) ((((std (p1 t)) ^^ 2) + ((std (p2 t)) ^^ 2)) ^^ (1 / 2)) ))
andP (Obs p1) (Obs p2) = Obs (\t -> (Dist ((mean (p1 t)) + (mean (p2 t))) ((std (p1 t)) + (std (p2 t))) ))

maxTP :: ExchangeRate -> Time -> Obs Dist -> Obs Dist -> Obs Dist
maxTP exchR t (Obs p1) (Obs p2) = if ((mean (p2 t)) > (mean (p1 t))) then (Obs p2) else (Obs p1)
-- maxTP exchR Zero Zero = Zero
-- maxTP exchR (One (Amt a1 c1)) Zero = One $ Amt (max a1 0) c1
-- maxTP exchR Zero (One (Amt a2 c2)) = One $ Amt (max 0 a2) c2
-- maxTP exchR (One (Amt a1 c1)) (One (Amt a2 c2)) = One $ Amt (max a1 a21) c1
--     where
--         a21 = a2 *. (realToFrac exchR)

-- disc :: Model -> Time -> Obs Dist -> Obs Dist
-- disc m 0 (Obs p) = (Obs p)


evalTermsAtP :: Model -> Time -> Terms -> Obs Dist
evalTermsAtP m t = evalP
    where
        evalP Zero                      = zeroP
        evalP (One amt)                 = convertToP m t (mainCurrency m) $ One amt
        evalP (Give c)                  = scaleP (Obs (\t -> (-1))) (evalP c)
        evalP (Zero `And` Zero)         = zeroP
        evalP (Zero `And` (One amt))    = oneP amt
        evalP ((One amt) `And` Zero)    = oneP amt
        evalP (c1 `And` c2)             = (evalP c1) `andP` (evalP c2)
        evalP (c1 `Or`  c2)             = maxTP (getValue ((exchangeRate m) USD USD) t) t (evalP c1) (evalP c2)
        evalP (Cond (Obs o) c1 c2)      = if (o t) then (evalP c1) else (evalP c2)
--         evalP (When (Obs o) c)          = if (o t) then (discAll m t (evalP c)) else zeroP
        evalP (Scale (Obs s) (One (Amt amt cur))) = oneP $ Amt (amt *. (realToFrac $ s t)) cur

disc :: Model -> Time -> Terms -> Terms
disc m 0 c = c
disc m t c = (disc m (t-1) (scaleT (getValue (interestRate m)) c))

scaleT :: (Time -> Float) -> Terms -> Terms
scaleT o c = c

-- Calculates contract value at a given point of time
toAmountAt :: Model -> Time -> Terms -> Amount
toAmountAt m t = toAmount . (evalTermsAt m t)
    where
        toAmount :: Terms -> Amount
        toAmount Zero      = Amt 0 (mainCurrency m)
        toAmount (One amt) = amt

------------------------------------------------------------------------------
u1 = Contract "1" $ when (at $ 3 Months) (Scale (konst 100) $ Give $ One $ 1 USD)
u2 = Contract "2" $ american (1 Month, 3 Months) (Scale (konst 10) $ One $ 1 NZD)
u3 = Contract "3" $ And (american (1 Month, 3 Months) (Scale (konst 10) $ One $ 1 NZD)) (when (at $ 3 Months) (Scale (konst 100) $ Give $ One $ 1 USD))
u4 = Contract "4" $ when (at $ 4 Months) (Scale (konst 100) $ One $ 1 USD)
u5 = Contract "5" $ american (0 Month, 5 Months)  (Scale (konst 100) $ One $ 1 USD)

u7 = Contract "7" $ cond (between 3 6) (terms u1) (terms u4)

u6 = Contract "6" $ And (when (at $ 3 Months) (Scale (konst 10) $ Give $ One $ 1 USD)) (american (0 Month, 5 Months)  (Scale (konst 100) $ One $ 1 USD))

-- main = zcb

data Risk = Double

revobs :: Obs Bool -> Obs Bool
revobs (Obs o) = Obs (\time -> (if (o time) then False else True))

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . L.group . L.sort

merge :: [Int] -> [Int] -> [Int]
merge xs     []     = rmdups (0:xs)
merge []     ys     = rmdups (0:ys)
merge (x:xs) (y:ys) = rmdups (x : y : merge xs ys)

add :: [Int] -> [Int] -> [Int]
add xs [] = rmdups (0:xs)
add [] ys = rmdups (0:ys)
add (x:xs) (y:ys) = rmdups ((x+y) : (merge (add (x:xs) ys)  (add xs (y:ys))))

mult :: Int -> [Int]-> [Int]
mult x ys = map (x *) ys

--data types

type Event = [Int]
type Calender = Obs Event

--eval

zeroCal :: Calender
zeroCal = konst [0]

oneCal :: Amount -> Calender
oneCal k = konst [1]

scaleCal :: Obs Int -> Calender -> Calender
scaleCal o cal = lift2 mult o cal

zipCalOr :: Calender -> Calender -> Calender
zipCalOr cal1 cal2 = lift2 merge cal1 cal2

zipCalAnd :: Calender -> Calender -> Calender
zipCalAnd cal1 cal2 = lift2 add cal1 cal2

shift :: Calender -> Obs Bool -> Calender
shift cal (Obs o) = Obs (\time -> (if (o time) then (getValue cal time) else (getValue zeroCal time)))

giveCal :: Calender -> Calender
giveCal cal = lift2 mult (konst (-1)) cal

--contract eval calender

evalCalenderAt :: Time -> Terms -> Calender
evalCalenderAt t = calender
    where
        calender Zero                   = zeroCal
        calender (One k)                = oneCal k
        calender (Give c)               = giveCal (calender c)
        calender (o `Scale` c)          = scaleCal o (calender c)
        calender (c1 `And` c2)          = zipCalAnd (calender c1) (calender c2)
        calender (c1 `Or` c2)           = zipCalOr (calender c1) (calender c2)
        calender (Cond o c1 c2)         = zipCalAnd (shift (calender c1) o) (shift (calender c2) (revobs o))
        calender (When o c)             = shift (calender c) o

x = evalCalenderAt 0 (terms u5)

y1 = merge [6,1] [20,6,45]
y2 = add [1] [20]
y3 = revobs (at 6)

exRate :: Currency -> Currency -> Obs ExchangeRate
exRate c1 c2 = konst 0.8

m = Model USD (konst 0.95) (konst 0.95) (konst 0.95) exRate
z = toAmountAt m 2 (american (0 Month, 5 Months)  (One $ 100 USD))

-- interest :: Decimal -> Decimal -> Int -> Obs Int
-- interest up down current= Obs (\t -> (intRate up down current t))
--
-- intRate :: Decimal -> Decimal -> Int -> Time -> Int
-- intRate up down = itr
--     where
--         itr old 0 = old
-- --         itr old t = if ((unsafePerformIO toss) ==0) then (itr (old + up) (t-1)) else (itr (old - down) (t-1))
--         itr old t = itr (old + (unsafePerformIO toss)) (t-1)
--
-- toss :: IO Int
-- toss = getStdRandom (randomR (-99, 99))



k1 :: [Float]
k1 = map (/ 100) (map unsafePerformIO x1)
    where x1 = (getStdRandom (randomR (90, 110))) : x1

k2 :: [Float]
k2 = map (/ 100) (map unsafePerformIO x2)
    where x2 = (getStdRandom (randomR (90, 110))) : x2

-- z :: [Float]
-- z = map

interestWalk :: Float -> [Float] -> [Float]
interestWalk = walk
    where walk currentIr (x:xs) = (currentIr * x) : (walk (currentIr * x) xs)

z1 = interestWalk 0.01 k1

z2 = interestWalk 0.01 k2



