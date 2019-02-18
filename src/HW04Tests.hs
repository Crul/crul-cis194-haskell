module HW04Tests where

import HW04
import Testing

-- Exercise 1 -----------------------------------------

-- Exercise 2 -----------------------------------------

testEqual :: (Num a, Eq a) => (Poly a, Poly a) -> Bool
testEqual (a, b) = a == b

ex2Tests :: [Test]
ex2Tests = [ Test "==" testEqual
             [ (P[1, 2, 3], P[1, 2, 3]),
               (P[1, 2, 0, 0], P[1, 2])]
           , Test "/=" (not . testEqual)
             [ (P[1, 2], P[1, 2, 3])
             , (P[1, 0, 2], P[1, 2]) ]
           ]


-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = []


-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = []


-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = []


-- Exercise 7 -----------------------------------------

ex7Tests :: [Test]
ex7Tests = []


-- Exercise 9 -----------------------------------------

ex9Tests :: [Test]
ex9Tests = []


-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex2Tests,
                    ex3Tests,
                    ex4Tests,
                    ex5Tests,
                    ex7Tests,
                    ex9Tests
                  ]
