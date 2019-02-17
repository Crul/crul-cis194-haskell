-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, d) = toRevDigits n == d

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
             [(1234, [4,3,2,1]), (314151413, [3,1,4,1,5,1,4,1,3]), (0, []), (-17, [])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n, d) = doubleEveryOther n == d

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [([4,9,5,5], [4,18,5,10]), ([3,14,15], [3,28,15]), ([17], [17]), ([0,0], [0,0])]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (n,d) = sumDigits n == d

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits
             [([10, 5, 18, 4], 19), ([0,0,0], 0)]
           ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, d) = luhn n == d

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
             [(5594589764218858, True), (1234567898765432, False)]
           ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (i, a, b, c, mv) = hanoi i a b c == mv

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi
             [ (2, "a", "b", "c", [("a","c"), ("a","b"), ("c","b")])
             , (3, "a", "b", "c", [ ("a","b"), ("a","c"), ("b","c")  -- x(n-1) a => c
                                  , ("a","b")                        -- (base) a => b
                                  , ("c","a"), ("c","b"), ("a","b")  -- x(n-1) c => b
                                  ]
               )
             ]
           ]

-- Exercise 7 -----------------------------------------

testHanoiFour :: (Integer, Peg, Peg, Peg, Peg, [Move]) -> Bool
testHanoiFour (i, a, b, c, d, mv) = hanoiFour i a b c d == mv

testHanoiFourOptimal :: (Integer, Int) -> Bool
testHanoiFourOptimal (i, mv) = length (hanoiFour i "a" "b" "c" "d") == mv

ex7Tests :: [Test]
ex7Tests = [ Test "hanoiFour test" testHanoiFour
             [ (5, "a", "b", "c", "d",
                [ ("a","b"),("a","d"),("a","c"),("d","c"),("b","c")  -- [3-tower] a => c
                , ("a","d"),("a","b"),("d","b")                      -- [2-tower] a => b
                , ("c","a"),("c","d"),("c","b"),("d","b"),("a","b")  -- [3-tower] c => b
                ]
               )
             ]
           , Test "hanoiFourOptimal test" testHanoiFourOptimal
             [(1, 1), (2, 3), (3, 5), (4, 9), (5, 13), (10, 49), (15, 129)]
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
                  ]
