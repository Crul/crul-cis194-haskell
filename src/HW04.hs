{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = (clean a) == (clean b)
      where clean = dropWhile (== 0) . reverse

-- Exercise 3 -----------------------------------------

withIndices :: Num a => [a] -> [(Integer, a)]
withIndices p = zip [0..] p

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = showPoly p

showPoly :: (Num a, Eq a, Show a) => [a] -> String
showPoly p = zeroIfEmpty poly
  where
    poly  = intercalate " + " $ reverse terms
    terms = filter (not . null) $ map showPolyTerm $ withIndices p

showPolyTerm :: (Num a, Eq a, Show a) => (Integer, a) -> String
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

expandPoly :: Num a => Bool -> [a] -> Int -> [a]
expandPoly rghtOrLft p l = left ++ right
  where zeros = (take l $ repeat 0)
        left  = if rghtOrLft then p else zeros
        right = if rghtOrLft then zeros else p


expandPolyRight :: Num a => [a] -> Int -> [a]
expandPolyRight = expandPoly True

plus' :: Num a => [a] -> [a] -> [a]
plus' a b = result
  where
    lengthDff = length b - length a
    a'        = expandPolyRight a lengthDff
    b'        = expandPolyRight b (-lengthDff)
    result    = map (uncurry (+)) $ zip a' b'

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P $ plus' a b

-- Exercise 5 -----------------------------------------

expandPolyLeft :: Num a => [a] -> Int -> [a]
expandPolyLeft = expandPoly False

timesTerm :: Num a => [a] -> (Integer, a) -> [a]
timesTerm p (idx, fctr) = (take (fromInteger idx) $ repeat 0) ++ map (*fctr) p

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

applyPTerm :: Num a => a -> (Integer, a) -> a -> a
applyPTerm xVal (idx, fctr) acc = acc + (fctr * xVal ^ idx)

applyP :: Num a => Poly a -> a -> a
applyP (P p) xVal = foldr (applyPTerm xVal) 0 (withIndices p)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n a
      | n <= 0    = a
      | otherwise = nderiv (n-1) (deriv a)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P [])     = P []
    deriv (P (_:ps)) = P dTerms
      where dTerms = map (\(e,t) -> t * fromInteger e) (zip [1..] ps)

