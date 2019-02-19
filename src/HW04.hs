{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P []

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = (clean a) == (clean b)
      where clean = dropWhile (== 0) . reverse

-- Exercise 3 -----------------------------------------

withIndices :: Num a => [a] -> [(Int, a)]
withIndices p = zip [0..] p

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = showPoly p

showPoly :: (Num a, Eq a, Show a) => [a] -> String
showPoly p = zeroIfEmpty poly
  where
    poly  = intercalate " + " $ reverse terms
    terms = filter (not . null) $ map showPolyTerm $ withIndices p

showPolyTerm :: (Num a, Eq a, Show a) => (Int, a) -> String
showPolyTerm (_   , 0)      = ""
showPolyTerm (0   , factor) = show factor
showPolyTerm (1   , factor) = showFactorForX factor ++ "x"
showPolyTerm (ordr, factor) = showFactorForX factor ++ "x^" ++ (show ordr)

showFactorForX :: (Num a, Eq a, Show a) => a -> String
showFactorForX 1    = ""
showFactorForX (-1) = "-"
showFactorForX f    = show f

zeroIfEmpty :: String -> String
zeroIfEmpty "" = "0"
zeroIfEmpty s  = s

-- Exercise 4 -----------------------------------------

expandPoly :: Num a => [a] -> Int -> [a]
expandPoly p l = p ++ (take l $ repeat 0)

plus' :: Num a => [a] -> [a] -> [a]
plus' a b = result
  where
    lengthDff = length b - length a
    a'        = expandPoly a lengthDff
    b'        = expandPoly b (-lengthDff)
    result    = map (uncurry (+)) $ zip a' b'

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P $ plus' a b

-- Exercise 5 -----------------------------------------

timesTerm :: Num a => [a] -> (Int, a) -> [a]
timesTerm p (idx, fctr) = (take idx $ repeat 0) ++ map (*fctr) p

times' :: Num a => [a] -> [a] -> [a]
times' a b = result
  where
    intermdt = map (timesTerm b) (withIndices a)
    result   = foldr plus' [0] intermdt

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = P $ times' a b

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate        = (* P [-1])
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

