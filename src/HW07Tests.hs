module HW07Tests where
-- https://github.com/ImsungChoi/haskell-test/blob/6ad275f36b4857ee65046851577032cf65692530/src/HW07Tests.hs
-- https://github.com/totahuanocotl/haskell/blob/48225a0aa95237587e13eae068b3e6b69cc84c18/test/LearningHaskell/HW07Tests.hs

import Prelude hiding (mapM)
import Control.Monad.Random hiding (mapM, liftM)

import qualified Data.Vector as V

import HW07 (liftM, swapV, mapM, getElts, randomElt, randomVec, randomVecR, shuffle)
import Testing
import Data.List

evalRnd' r = (evalRand r) (mkStdGen 0)

-- Exercise 1 -----------------------------------------

testLiftMPlus :: (Monad m, Num a, Eq (m a)) => (a, m a, m a) -> Bool
testLiftMPlus (val, ma, mb) = liftM (+val) ma == mb

testLiftMAppend :: (Monad m, Eq (m [a])) => ([a], m [a], m [a]) -> Bool
testLiftMAppend (val, ma, mb) = liftM (++val) ma == mb

testSwapV :: Eq a => (Int, Int, [a], Maybe [a]) -> Bool
testSwapV (i1, i2, vec, (Just res)) = swapV i1 i2 (V.fromList vec) == Just (V.fromList res)
testSwapV (i1, i2, vec, _         ) = swapV i1 i2 (V.fromList vec) == Nothing

ex1Tests :: [Test]
ex1Tests = [ Test "test liftM Int" testLiftMPlus
             [ (1, Just 5, Just 6)
             , (-1, Just 4, Just 3)
             , (1, Nothing, Nothing)
             ]
           , Test "test liftM String" testLiftMAppend
             [ ("Baz", Just "Foo", Just "FooBaz")
             , ("456", Just "123", Just "123456")
             , ("Foo", Nothing, Nothing)
             ]
           , Test "test swapV" testSwapV
             [ (0, 2, [1,2,3], Just [3,2,1])
             , (0, 2, [1,2], Nothing)
             ]
           ]


-- Exercise 2 -----------------------------------------

testMapMMaybe :: Eq a => [a] -> Bool
testMapMMaybe x = mapM Just x == Just x

positive :: Integer -> Maybe Integer
positive x | x > 0     = Just x
           | otherwise = Nothing

testMapMPositive :: ([Integer], Maybe [Integer]) -> Bool
testMapMPositive (x, res) = mapM positive x == res

testGetElts :: ([Int], [Int], Maybe [Int]) -> Bool
testGetElts (idx, vec, res) = getElts idx (V.fromList vec) == res

ex2Tests :: [Test]
ex2Tests = [ Test "test mapM Int" testMapMMaybe
             [([]), ([0..10])]
           , Test "test mapM String" testMapMMaybe
             [([]), (["a", "b", "c"])]
           , Test "test mapM positive" testMapMPositive
             [([], Just []), ([1..10], Just [1..10]), ([1,2,-1], Nothing)]
           , Test "test getElts" testGetElts
             [([], [], Just [])
             , ([0], [], Nothing)
             , ([1,3], [10,11,12,13,14], Just [11,13])
             , ([1,3,7], [10,11,12,13,14], Nothing)
             , ([2], [0,1], Nothing)
             ]
           ]


-- Exercise 3 -----------------------------------------
-- To get deterministic test result:
-- evalRnd' (randomElt (V.fromList [4,2,3,1,5,6,7]))
-- evalRnd' (randomElt (V.fromList [1,2,3]))

testRandomElt :: Eq a => ([a], Maybe a) -> Bool
testRandomElt (vec, res) = evalRnd' (randomElt (V.fromList vec)) == res

ex3Tests :: [Test]
ex3Tests = [ Test "test randomElt" testRandomElt
             [ ([4,2,3,1,5,6,7], Just 4)
             , ([1,2,3], Just 3)
             , ([], Nothing)
             ]
           ]


-- Exercise 4 -----------------------------------------
-- To get deterministic test result:
-- evalRnd' (randomVec 3)
-- evalRnd' (randomVecR 1 (1,9))
-- evalRnd' (randomVecR 7 (100,999))
-- evalRnd' (randomVecR 7 ('a','z'))
-- evalRnd' (randomVecR 40 ('A','F'))

testRandomVec :: (Int, [Integer]) -> Bool
testRandomVec (n, res) = evalRnd' (randomVec n) == V.fromList res

testRandomVecR :: (Random a, Eq a) => (Int, (a, a), [a]) -> Bool
testRandomVecR (n, hilo, res) = evalRnd' (randomVecR n hilo) == V.fromList res

ex4Tests :: [Test]
ex4Tests = [ Test "test randomVec" testRandomVec
             [(0, [])
             , (1, [9106162675347844341])
             , (3, [9106162675347844341,-5782012937088350469,3531325756418318423])
             ]
           , Test "test randomVecR Int" testRandomVecR
             ([(0, (0,0)    , [])
             , (1, (1,9)    , [3])
             , (7, (100,999), [183,393,763,238,200,587,781])
             ] :: [(Int, (Integer, Integer), [Integer])])
           , Test "test randomVecR String" testRandomVecR
             [ (0 , (' ',' '), "")
             , (7 , ('a','z'), "nlrkwld")
             , (40, ('A','F'), "FFDAEBDBBAFEAECBCDDAEAAFEFCDFEFCFCEECEFB")
             ]
           ]


-- Exercise 5 -----------------------------------------
-- To get deterministic test result:
-- evalRnd' (shuffle $ V.fromList [1,2,3,4,5,6])
-- evalRnd' (shuffle $ V.fromList [10,20,30,40,50,60,70])

--shuffle :: Vector a -> Rnd (Vector a)
testShuffle :: Eq a => ([a], [a]) -> Bool
testShuffle (vec, res) = evalRnd' (shuffle $ V.fromList vec) == V.fromList res

ex5Tests :: [Test]
ex5Tests = [ Test "test shuffle" testShuffle
             [ ([], [])
             , ([1,2,3,4,5,6], [2,3,1,5,4,6])
             , ([10,20,30,40,50,60,70], [70,50,20,30,40,60,10])
             ]
           ]


-- All Tests -------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  ]

main :: IO ()
main = putStrLn $ show $ runTests allTests
