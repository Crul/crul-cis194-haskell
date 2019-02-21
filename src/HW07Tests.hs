module HW07Tests where
-- https://github.com/ImsungChoi/haskell-test/blob/6ad275f36b4857ee65046851577032cf65692530/src/HW07Tests.hs
-- https://github.com/totahuanocotl/haskell/blob/48225a0aa95237587e13eae068b3e6b69cc84c18/test/LearningHaskell/HW07Tests.hs

import qualified Data.Vector as V

import HW07 (liftM, swapV)
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


-- All Tests -------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  ]

main :: IO ()
main = putStrLn $ show $ runTests allTests
