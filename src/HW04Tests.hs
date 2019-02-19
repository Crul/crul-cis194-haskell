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
testShow :: (Num a, Eq a, Show a) => (Poly a, String) -> Bool
testShow (p, s) = show p == s

ex3Tests :: [Test]
ex3Tests = [ Test "show" testShow
             [ (P [1, 0, 0, 2], "2x^3 + 1"),
               (P [0, -1, 2], "2x^2 + -x"),
               (P [0], "0"),
               (P [-1], "-1"),
               (P [0, 1], "x"),
               (P [0, -1], "-x"),
               (P [-3, -2], "-2x + -3")]
           ]


-- Exercise 4 -----------------------------------------
testPlus :: (Num a, Eq a) => (Poly a, Poly a, Poly a) -> Bool
testPlus (pa, pb, pc) = pa + pb == pc

ex4Tests :: [Test]
ex4Tests = [ Test "plus" testPlus
             [ (P [5, 0, 1], P [1, 1, 2], P [6, 1, 3]),
               (P [1, 0, 1], P [1, 1], P [2, 1, 1]),
               (P [1, 1], P [2, 0, 3], P [3, 1, 3])]
           ]


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
