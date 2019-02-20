{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
-- import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n | n < 2     = 1
      | otherwise = fib (n-2) + fib (n-1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons c s) = c : streamToList s

-- Exercise 4 -----------------------------------------

instance Functor Stream where
--  fmap :: Functor Stream => (a -> b) -> Stream a -> Stream b
    fmap f (Cons x s) = Cons (f x) (fmap f s)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat s = Cons s $ sRepeat s

sIterate :: (a -> a) -> a -> Stream a
sIterate f a = Cons a $ sIterate f (f a)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a sa) sb = Cons a (sInterleave sb sa)

sTake :: Int -> Stream a -> [a]
sTake n _ | n <= 0  = []
sTake n (Cons a sa) = a : sTake (n-1) sa

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
-- https://github.com/wangqr/CIS194/blob/master/HW06.hs
ruler = sInterleave (sRepeat 0) (fmap (+1) ruler)
-- https://github.com/ROKT-CIS-194/cis-194-homework/blob/master/2015/src/CIS194/ClaudioN/HW06.hs
-- ruler = foldr1 sInterleave $ map sRepeat [0..]

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = sIterate nxt seed
  where nxt s = (1103515245 * s + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = undefined

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
