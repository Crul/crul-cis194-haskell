module HW07Tests where
-- https://github.com/ImsungChoi/haskell-test/blob/6ad275f36b4857ee65046851577032cf65692530/src/HW07Tests.hs
-- https://github.com/totahuanocotl/haskell/blob/48225a0aa95237587e13eae068b3e6b69cc84c18/test/LearningHaskell/HW07Tests.hs

import Prelude hiding (mapM)

import qualified Data.Vector as V

import HW07 (liftM, swapV, mapM, getElts)
import Testing
import Data.List

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


-- All Tests -------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  ]

main :: IO ()
main = putStrLn $ show $ runTests allTests
