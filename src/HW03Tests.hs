module HW03Tests where

import HW03
import Testing

import Data.List (foldl')

runStateUpdates :: [(String, Int)] -> State
runStateUpdates = foldl' (\x (var,val) -> extend x var val) empty

-- Exercise 1 -----------------------------------------

testState :: ([(String, Int)], (String, Int)) -> Bool
testState (inputs, (oVar, oVal)) = runStateUpdates inputs oVar == oVal

ex1Tests :: [Test]
ex1Tests = [ Test "state: empty" testState
             [ ([], ("X", 0)) ]
           , Test "state: set" testState
             [ ([("X", 1)], ("X", 1))
             , ([("Y", 1)], ("X", 0)) ]
           , Test "state: multiple updates" testState
             [ ([("X", 1), ("X", 2)], ("X", 2)) ]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = []

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = []

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = []

-- Example Programs -----------------------------------------

progTests :: [Test]
progTests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , progTests
                  ]
