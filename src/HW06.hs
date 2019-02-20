{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
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
{-- ghc HW06.hs -rtsopts -main-is HW06 && HW06.exe +RTS -s -h -i0.001
Just (1096,2147482927)
     610,600,760 bytes allocated in the heap
  11,558,022,056 bytes copied during GC
     105,295,608 bytes maximum residency (209 sample(s))
       6,428,824 bytes maximum slop
             190 MB total memory in use (0 MB lost due to fragmentation)
--}

minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------
{-- ghc HW06.hs -rtsopts -main-is HW06 && HW06.exe +RTS -s -h -i0.001
Just (1096,2147482927)
     552,048,944 bytes allocated in the heap
         494,568 bytes copied during GC
          42,808 bytes maximum residency (173 sample(s))
          26,824 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)
--}

minMax :: [Int] -> Maybe (Int, Int)
minMax []       = Nothing   -- no min or max if there are no elements
-- https://wiki.haskell.org/Foldr_Foldl_Foldl'
minMax (x:xs)   = Just $ foldl' minMax' (x, x) xs
  where minMax' (!mm, !mM) v = (min mm v, max mM v)

{- 4th attempt: 144 MB total memory in use
minMax (x:xs)   = Just $ foldl minMax' (x, x) xs
  where minMax' (!mm, !mM) v = (min mm v, max mM v)
-}

{- 3rd attempt: 302 MB total memory in use
minMax (x:xs)   = Just $ foldr minMax' (x, x) xs
  where minMax' y (!mm, !mM) = (min y mm, max y mM)
-}

{- 2nd attempt: 249 MB total memory in use
minMax (x:xs)   = Just $ foldr minMax' (x, x) xs
  where minMax' y (mm, mM) = (min y mm, max y mM)
-}

{- 1st attempt: 279 MB total memory in use
minMax (x:xs)   = Just ( safeFn min x mins, safeFn max x maxs )
  where
    (mins, maxs) = miiToMiMi $ minMax xs
    miiToMiMi :: Maybe (Int, Int) -> (Maybe Int, Maybe Int)
    miiToMiMi Nothing      = (Nothing, Nothing)
    miiToMiMi (Just (a,b)) = (Just a , Just b)
    safeFn :: (Int -> Int -> Int) -> Int -> Maybe Int -> Int
    safeFn _ a Nothing  = a
    safeFn f a (Just b) = f a b
-}

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532
-- main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix2x2 a = M2x2 a a a a deriving (Eq)

instance Show a => Show (Matrix2x2 a) where
  show (M2x2 a b c d) = "[ " ++ (show a) ++ " " ++ (show b) ++
                        "\n  " ++ (show c) ++ " " ++ (show d) ++ " ]"

instance Num a => Num (Matrix2x2 a) where
    (*) (M2x2 a1 b1 c1 d1) (M2x2 a2 b2 c2 d2) = M2x2 s11 s12 s21 s22
      where s11 = a1*a2 + b1*b2
            s12 = a1*c2 + b1*d2
            s21 = c1*a2 + d1*b2
            s22 = c1*c2 + d1*d2

    -- No meaningful definitions exist
    (+)         = undefined
    negate      = undefined
    fromInteger = undefined
    abs         = undefined
    signum      = undefined

fibMatrix :: Matrix2x2 Integer
fibMatrix = M2x2 1 1
                 1 0

fastFib :: Int -> Integer
fastFib 0 = 1
fastFib n = nFib
  where (M2x2 nFib _ _ _) = fibMatrix ^ n
