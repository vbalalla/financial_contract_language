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
        -- eval (c1 `Or`  c2)             = maxT ((exchangeRate m) (getCurrency (eval c1)) (getCurrency (eval c2)) time) (eval c1) (eval c2) 
        eval (Cond (Obs o) c1 c2)      = if (o t) then (eval c1) else (eval c2)
        eval (When (Obs o) c)          = if (o t) then (eval c)  else Zero
        eval (Scale (Obs s) (One (Amt amt cur))) = One $ Amt (amt *. (realToFrac $ s t)) cur

-- Calculates contract value at a given point of time
toAmountAt :: Model -> Time -> Terms -> Amount
toAmountAt m t = toAmount . (evalTermsAt m t)
    where
        toAmount :: Terms -> Amount
        toAmount Zero      = Amt 0 (mainCurrency m)
        toAmount (One amt) = amt

------------------------------------------------------------------------------
u1 = Contract "1" $ when (at $ 3 Months) (Give $ One $ 100 USD)
u2 = Contract "2" $ american (1 Month, 3 Months) (One $ 10 NZD)
u3 = Contract "3" $ Or (american (1 Month, 3 Months) (One $ 10 NZD)) (when (at $ 3 Months) (Give $ One $ 100 USD))
-- main = zcb

data Risk = Double

--risk :: Model -> Contract -> Risk
--risk m t c =
--cal = [0..31]
--
--getDate :: Terms -> Time -> Bool
--getDate (When (Obs o) c) t = o t
--getDate (Cond (Obs o) c1 c2) t = o t
----x = 3 Months
--z = getDate (terms u2)
--
--n = map z cal

----------------------------------------------------------------------------------
cal = [0..31]
--getCal ::[Integer] -> Terms -> [Integer]
--getCal calender = calX
--    where   calX Zero                   = calender
--            calX (One k)                = calender
--            calX (Give c)               = calX c
--            calX (o `Scale` c)          = calX c
--            calX (c1 `And` c2)          = calAnd (calX c1) (calX c2)
--            calX (c1 `Or` c2)           = calOr (calX c1) (calX c2)
--            calX (Cond (Obs o) c1 c2)   = map (calCond o) (calAnd (calX c1) (calX c2))
--            calX (When (Obs o) c)       = map (calWhen o) (calX c)

-----------------------------------------------------------------------------------------------------------------

type Calender = Obs String
--data Calender = Calender (Obs String)


--getEvent :: Time ->Terms -> Calender
--getEvent t = calEvent
--    where   calEvent Zero                   = konst "Zero "
--            calEvent (One k)                = Obs (\time -> ((show k) ++ " "))
--            calEvent (Give c)               = Obs (\time -> "give " ++ (getValue (calEvent c) time))
----            calEvent ((Obs o) `Scale` c)    = (show (o t)) ++ (calEvent c)
--            calEvent (c1 `And` c2)          = Obs (\time -> (if (((getValue (calEvent c1) time) /= "") && ((getValue (calEvent c2) time) /= "")) then ((getValue (calEvent c1) time) ++ "and " ++ (getValue (calEvent c2) time)) else (if (((getValue (calEvent c1) time) == "") && ((getValue (calEvent c2) time) == "")) then "" else ((getValue (calEvent c1) time) ++ (getValue (calEvent c2) time)))))
----            calEvent (c1 `Or` c2)           = if (((calEvent c1) /= "") && ((calEvent c2) /= "")) then ((calEvent c1) ++ "and " ++ (calEvent c2)) else (if (((calEvent c1) == "") && ((calEvent c2) == "")) then "" else ((calEvent c1) ++ (calEvent c2)))
----            calEvent (Cond (Obs o) c1 c2)   = if (o t) then (calEvent c1) else (calEvent c2)
--            calEvent (When (Obs o) c)       = Obs (\time -> (if (o time) then (getValue (calEvent c) time) else ""))

calender :: Terms -> Calender

andCal :: Calender -> Calender -> Calender
andCal c1 c2 = Obs (\time -> (if (((getValue c1 time) /= "") && ((getValue c2 time) /= "")) then ((getValue c1 time) ++ "and " ++ (getValue c2 time)) else (if (((getValue c1 time) == "") && ((getValue c2 time) == "")) then "" else ((getValue c1 time) ++ (getValue c2 time)))))

zeroCal :: Calender
zeroCal = Obs (\time -> ("Zero "))

calender Zero                   = zeroCal
calender (One k)                = Obs (\time -> ((show k) ++ " "))
calender (Give c)               = Obs (\time -> "give " ++ (getValue (calender c) time))
--            calEvent ((Obs o) `Scale` c)    = (show (o t)) ++ (calEvent c)
calender (c1 `And` c2)          = andCal (calender c1) (calender c2)
--            calEvent (c1 `Or` c2)           = if (((calEvent c1) /= "") && ((calEvent c2) /= "")) then ((calEvent c1) ++ "and " ++ (calEvent c2)) else (if (((calEvent c1) == "") && ((calEvent c2) == "")) then "" else ((calEvent c1) ++ (calEvent c2)))
--            calEvent (Cond (Obs o) c1 c2)   = if (o t) then (calEvent c1) else (calEvent c2)
calender (When (Obs o) c)       = Obs (\time -> (if (o time) then (getValue (calender c) time) else ""))

--calEvent :: Terms -> Time -> String
--calEvent terms t = getValue (getEvent t terms) t

x = calender (terms u1)

--printCal :: [String] -> x
--printCal ([]) = "\n"
--printCal (x:xs) = x ++ "\n" ++ (printCal xs)


calCond :: (Integer -> Bool) -> Integer -> Integer
calCond o i = if (o i) then -1 else i

calWhen :: (Integer -> Bool) -> Integer -> Integer
calWhen o i = if (o i) then -1 else i

calAnd :: [Integer] -> [Integer] -> [Integer]
calAnd [] [] = []
calAnd (x:xs) (y:ys) = if ((x == y) && (x >= 0)) then x:(calAnd xs ys) else (if (x<0 && y<0) then (x+y):(calAnd xs ys) else (if (x<0) then x:(calAnd xs ys) else y:(calAnd xs ys)) )

calOr :: [Integer] -> [Integer] -> [Integer]
calOr [] [] = []
calOr (x:xs) (y:ys) = if ((x == y) && (x >= 0)) then x:(calOr xs ys) else (if (x<0 && y<0) then (x+y):(calOr xs ys) else (if (x<0) then x:(calOr xs ys) else y:(calOr xs ys)) )


