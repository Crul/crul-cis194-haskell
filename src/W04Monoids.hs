module W04Monoids where

import Data.Monoid

{-- From Data.Monoid

class Monoid m where
  mempty  :: m
  mappend :: m -> m -> m

  mconcat :: [m] -> m     -- this can be omitted from Monoid instances
  mconcat = foldr mappend mempty

(<>) :: Monoid m => m -> m -> m    -- infix operator for convenience
(<>) = mappend

instance Monoid [a] where
  mempty  = []
  mappend = (++)
--}

intInts :: Monoid m => (Integer -> m) -> m   -- interesting ints!
intInts mk_m = go [1..100]   -- [1..100] is the list of numbers from 1 to 100
  where go [] = mempty
        go (n:ns)
          | let div_by_5 = n `mod` 5 == 0
                div_by_7 = n `mod` 7 == 0
          , (div_by_5 || div_by_7) && (not (div_by_5 && div_by_7))
          = mk_m n <> go ns
          | otherwise
          = go ns

intIntsList :: [Integer]
intIntsList = intInts (:[])
           -- intInts (\x -> [x])

{-- From Data.Monoid

data Product a = Product a
instance Num a => Monoid (Product a) where
  mempty                          = Product 1
  mappend (Product x) (Product y) = Product (x * y)

getProduct :: Product a -> a
getProduct (Product x) = x
--}

intIntsProduct :: Integer
intIntsProduct = getProduct $ intInts Product
