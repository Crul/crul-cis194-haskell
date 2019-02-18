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

testEvalE :: ([(String, Int)], Expression, Int) -> Bool
testEvalE (inputs, expr, result) = evalE (runStateUpdates inputs) expr == result

ex2Tests :: [Test]
ex2Tests = [ Test "evalE: variables" testEvalE
             [ ([], Var "X", 0)
             , ([("X", 1)], Var "X", 1)
             ]
           , Test "evalE: values" testEvalE
             [ ([], Val 0, 0)
             , ([], Val 1, 1)
             ]
           , Test "evalE: arithmetic" testEvalE
             [ ([], Op (Val 2) Plus (Val 3), 5)
             , ([], Op (Val 2) Minus (Val 3), (-1))
             , ([], Op (Val 2) Times (Val 3), 6)
             , ([], Op (Val 20) Divide (Val 3), 6)
             ]
           , Test "evalE: comparison" testEvalE
             [ ([], Op (Val 2) Gt (Val 3), 0)
             , ([], Op (Val 3) Gt (Val 2), 1)
             , ([], Op (Val 2) Lt (Val 3), 1)
             , ([], Op (Val 3) Lt (Val 2), 0)
             , ([], Op (Val 3) Eql (Val 2), 0)
             , ([], Op (Val 2) Eql (Val 2), 1)
             ]
           ]

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
