module HW06Tests where

import HW06 (fibs1)
import Testing

-- Exercise 1 -----------------------------------------

testFibs1 :: (Int, [Integer]) -> Bool
testFibs1 (len, fibs) = take len fibs1 == fibs

ex1Tests :: [Test]
ex1Tests = [ Test "test fibs1" testFibs1
             [(14, [1,1,2,3,5,8,13,21,34,55,89,144,233,377])]
           ]


-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  ]

main :: IO ()
main = putStrLn $ show $ runTests allTests
