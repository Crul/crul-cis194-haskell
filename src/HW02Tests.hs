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
testAllCodes (n, c) = allCodes n == c

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
    , (0, [])
    , (-1, [])
    ]
  ]

-- Exercise 7 -----------------------------------------

ex7Tests :: [Test]
ex7Tests = []

-- Bonus ----------------------------------------------

bonusTests :: [Test]
bonusTests = []

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
