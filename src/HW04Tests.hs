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
testTimes :: (Num a, Eq a) => (Poly a, Poly a, Poly a) -> Bool
testTimes (pa, pb, pc) = pa * pb == pc

ex5Tests :: [Test]
ex5Tests = [ Test "times" testTimes
             [ (P [1, 1, 1], P [2, 2], P [2, 4, 4, 2]),
               (P [1], P [1, 1], P [1, 1]),
               (P [0], P [1, 1], P [0])]
           ]


-- Exercise 6 -----------------------------------------
testNegate :: (Num a, Eq a) => (Poly a, Poly a) -> Bool
testNegate (pa, pb) = negate pa == pb

testFromInteger :: (Num a, Eq a) => (Integer, Poly a) -> Bool
testFromInteger (i, p) = p == fromInteger i

testCasting :: (Num a, Eq a) => (Poly a, [a]) -> Bool
testCasting (P p, a) = p == a

ex6Tests :: [Test]
ex6Tests = [ Test "negate" testNegate
             [ (P [2, 4, 4, 2], P [-2, -4, -4, -2]),
               (P [1, 0, 1], P [-1, 0, -1]),
               (P [0], P [0])]
           , Test "fromInteger" testFromInteger
             [ (3, P [3]), (0, P [0]), (-1, P [-1]) ]
           , Test "casting" testCasting
             [ (x^2 + 2*x + 1, [1, 2, 1])
             , (10*x^4 + 13, [13, 0, 0, 0, 10])
             , (x^2 + (-2)*x + (-1), [-1, -2, 1])
             , (7*x^2, [0, 0, 7])
             , (0, [0])
             ]
           ]


-- Exercise 7 -----------------------------------------
testApplyP :: (Num a, Eq a) => (Poly a, a, a) -> Bool
testApplyP (p, a, b) = applyP p a == b

ex7Tests :: [Test]
ex7Tests = [ Test "applyP" testApplyP
             [ (x^2 + 2*x + 1, 1, 4)
             , (x^2 + 2*x + 1, 2, 9)
             , (x^2 + (-2)*x + 5, 2, 5)]
           ]


-- Exercise 9 -----------------------------------------
testNDeriv :: (Num a, Eq a) => (Poly a, Int, Poly a) -> Bool
testNDeriv (p1, n, p2) = nderiv n p1 == p2

ex9Tests :: [Test]
ex9Tests = [ Test "nderiv" testNDeriv
             [ (x^2 + 3*x + 5, 1, 2*x + 3),
               (x^2 + 3*x + 5, 2, 2),
               (3*x^3 + x^2 + 2*x + 1, 3, 18)]
           ]


-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex2Tests,
                    ex3Tests,
                    ex4Tests,
                    ex5Tests,
                    ex6Tests,
                    ex7Tests,
                    ex9Tests
                  ]

main :: IO ()
main = putStrLn $ show $ runTests allTests
