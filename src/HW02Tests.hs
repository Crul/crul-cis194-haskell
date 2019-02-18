-- CIS 194, Spring 2015
--
-- Test cases for HW 02

module HW02Tests where

import HW02
import Testing


-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = [ testF2 "exactMatches test" exactMatches
             [ ([Red, Blue, Green, Yellow], [Blue, Green, Yellow, Red], 0)
             , ([Red, Blue, Green, Yellow], [Red, Purple, Green, Orange], 2)
             ]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF1 "countColors test" countColors
             [ ([Red, Blue, Yellow, Purple], [1, 0, 1, 1, 0, 1])
             , ([Green, Blue, Green, Orange], [0, 2, 1, 0, 1, 0])
             ]
           , testF2 "matches test" matches
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue], 3) ]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF2 "getMove test" getMove
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue],
               Move [Red, Orange, Orange, Blue] 1 2)
             ]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ testF2 "isConsistent test" isConsistent
             [ (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Yellow, Purple],
               True)
             , (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Red, Purple],
               False)
             ]
           ]

-- Exercise 5 -----------------------------------------

testFilterCodes :: (Move, [Code], [Code]) -> Bool
testFilterCodes (mv, a, b) = filterCodes mv a == b

ex5Tests :: [Test]
ex5Tests = [ Test "filterCodes test" testFilterCodes
             [ (Move [Red, Red, Blue, Green] 1 1
               , [ [Red, Blue, Yellow, Purple], [Red, Blue, Red, Purple] ]
               , [ [Red, Blue, Yellow, Purple] ]
               )
             ]
           ]

-- Exercise 6 -----------------------------------------

testAllCodes :: (Int, [Code]) -> Bool
testAllCodes (n, c) = allCodes addColor n == c

ex6Tests :: [Test]
ex6Tests = [
    Test "allCodes test" testAllCodes
    [ (1, [ [Red], [Green], [Blue], [Yellow], [Orange], [Purple] ] )
    , (2, [ [Red, Red   ], [Green, Red   ], [Blue, Red   ], [Yellow, Red   ], [Orange, Red   ], [Purple, Red   ]
          , [Red, Green ], [Green, Green ], [Blue, Green ], [Yellow, Green ], [Orange, Green ], [Purple, Green ]
          , [Red, Blue  ], [Green, Blue  ], [Blue, Blue  ], [Yellow, Blue  ], [Orange, Blue  ], [Purple, Blue  ]
          , [Red, Yellow], [Green, Yellow], [Blue, Yellow], [Yellow, Yellow], [Orange, Yellow], [Purple, Yellow]
          , [Red, Orange], [Green, Orange], [Blue, Orange], [Yellow, Orange], [Orange, Orange], [Purple, Orange]
          , [Red, Purple], [Green, Purple], [Blue, Purple], [Yellow, Purple], [Orange, Purple], [Purple, Purple]
        ]
      )
    , (0, [[]])
    , (-1, [[]])
    ]
  ]

-- Exercise 7 -----------------------------------------

testSolve :: (Code, [Move]) -> Bool
testSolve (c, ms) = solve c == ms

ex7Tests :: [Test]
ex7Tests = [
    Test "solve test" testSolve
    [ ( [ Red, Blue, Blue ]
      , [ Move [Red,Red,Red] 1 0, Move [Green,Green,Red] 0 1
        , Move [Blue,Red,Blue] 1 2, Move [Red,Blue,Blue] 3 0
        ]
      )
    , ( [ Yellow, Orange, Purple ]
      , [ Move [Red,Red,Red] 0 0, Move [Green,Green,Green] 0 0, Move [Blue,Blue,Blue] 0 0
        , Move [Yellow,Yellow,Yellow] 1 0, Move [Orange,Orange,Yellow] 1 1
        , Move [Orange,Yellow,Purple] 1 2, Move [Yellow,Orange,Purple] 3 0
        ]
      )
    ]
  ]

-- Bonus ----------------------------------------------

testFiveGuessCount :: Code -> Bool
testFiveGuessCount c = length (fiveGuess c) <= 5

codeByIdx :: Int -> Code
codeByIdx = (!!) $ allCodes addColor' 4

idxToTest :: [Int]
idxToTest = [1, 111, 195, 216, 314, 320, 500, 666, 722, 814, 999, 1000, 1020, 1030, 1250, 1295]
-- idxToTest = [0..1295]

testFiveGuess :: (Code, [Move]) -> Bool
testFiveGuess (c, ms) = fiveGuess c == ms

bonusTests :: [Test]
bonusTests = [
    Test "fiveGuess test optimality" testFiveGuessCount $ map codeByIdx idxToTest
  , Test "fiveGuess test solution" testFiveGuess
    [ ( [ Green, Blue, Purple, Blue ]
      , [ Move [Red,Red,Green,Green] 0 1, Move [Green,Blue,Yellow,Yellow] 2 0
        , Move [Red,Orange,Yellow,Orange] 0 0, Move [Red,Red,Blue,Purple] 0 2
        , Move [Green,Blue,Purple,Blue] 4 0
        ]
      )
    , ( [ Orange, Orange, Yellow, Orange ]
      , [ Move [Red,Red,Green,Green] 0 0, Move [Blue,Blue,Yellow,Orange] 2 0
        , Move [Blue,Purple,Blue,Purple] 0 0, Move [Red,Yellow,Yellow,Yellow] 1 0
        , Move [Orange,Orange,Yellow,Orange] 4 0
        ]
      )
    ]
  ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  , bonusTests
                  ]
