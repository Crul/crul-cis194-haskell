{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits i
  | i <= 0    = []
  | otherwise = digit : nextDigits
    where digit      = (lastDigit i)
          nextDigits = toRevDigits (dropLastDigit i)

-- Exercise 3 -----------------------------------------

doubleIfEvenIdx :: Integer -> Integer -> Integer
doubleIfEvenIdx idx i = if idx `mod` 2 == 0 then i*2 else i

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith doubleIfEvenIdx [1..]

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits xs = sum allDigits
  where allDigits = concatMap toRevDigits xs

-- Exercise 5 -----------------------------------------

luhnReminder :: Integer -> Integer
luhnReminder = lastDigit . sumDigits . doubleEveryOther . toRevDigits

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (==0) . luhnReminder

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _   _   _   = []
hanoi n src trg aux = srcToAux ++ baseToTrg ++ auxToTrg
  where
    srcToAux  = hanoi (n-1) src aux trg
    baseToTrg = [(src, trg)]
    auxToTrg  = hanoi (n-1) aux trg src

hanoi' :: Integer -> [Move]
hanoi' n = hanoi n "a" "b" "c"

hanoiFourK :: Integer -> Integer
hanoiFourK n = k
  where
    x = fromInteger $ (2::Integer)*n + (1::Integer)
    k = (n + 1) - round (sqrt x :: Double)

hanoiFour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour 0 src trg a _ = hanoi 0 src trg a
hanoiFour n src trg a b = srcToAux ++ baseToTrg ++ auxToTrg
  where
    k         = hanoiFourK n
    srcToAux  = hanoiFour k src a trg b
    baseToTrg = hanoi (n-k) src trg b
    auxToTrg  = hanoiFour k a trg src b

hanoiFour' :: Integer -> [Move]
hanoiFour' n = hanoiFour n "a" "b" "c" "d"
