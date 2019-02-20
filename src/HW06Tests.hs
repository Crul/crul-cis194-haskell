module HW06Tests where

import HW06 (fibs1, fibs2, Stream(..), streamToList, sRepeat, sIterate, sInterleave, sTake, nats, ruler, rand, minMaxSlow, minMax, fastFib)
import Testing

-- Exercise 1 -----------------------------------------

testFibs1 :: (Int, [Integer]) -> Bool
testFibs1 (len, fibs) = take len fibs1 == fibs

ex1Tests :: [Test]
ex1Tests = [ Test "test fibs1" testFibs1
             [(14, [1,1,2,3,5,8,13,21,34,55,89,144,233,377])]
           ]


-- Exercise 2 -----------------------------------------

testFibs2 :: (Int, [Integer]) -> Bool
testFibs2 (len, fibs) = take len fibs2 == fibs

ex2Tests :: [Test]
ex2Tests = [ Test "test fibs2" testFibs2
             [(100, [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368
                    ,75025,121393,196418,317811,514229,832040,1346269,2178309,3524578,5702887,9227465,14930352
                    ,24157817,39088169,63245986,102334155,165580141,267914296,433494437,701408733,1134903170
                    ,1836311903,2971215073,4807526976,7778742049,12586269025,20365011074,32951280099,53316291173
                    ,86267571272,139583862445,225851433717,365435296162,591286729879,956722026041,1548008755920
                    ,2504730781961,4052739537881,6557470319842,10610209857723,17167680177565,27777890035288
                    ,44945570212853,72723460248141,117669030460994,190392490709135,308061521170129,498454011879264
                    ,806515533049393,1304969544928657,2111485077978050,3416454622906707,5527939700884757
                    ,8944394323791464,14472334024676221,23416728348467685,37889062373143906,61305790721611591
                    ,99194853094755497,160500643816367088,259695496911122585,420196140727489673,679891637638612258
                    ,1100087778366101931,1779979416004714189,2880067194370816120,4660046610375530309
                    ,7540113804746346429,12200160415121876738,19740274219868223167,31940434634990099905
                    ,51680708854858323072,83621143489848422977,135301852344706746049,218922995834555169026
                    ,354224848179261915075
                    ])]
           ]


-- Exercise 3 -----------------------------------------

testStreamToList :: (Int, String, [String]) -> Bool
testStreamToList (len, val, res) = res == (take len $ streamToList $ sRepeat val)

ex3Tests :: [Test]
ex3Tests = [ Test "test streamToList" testStreamToList
             [ (0, "Foo", [])
             , (-1, "Foo", [])
             , (5, "Foo", ["Foo","Foo","Foo","Foo","Foo"])
             , (7, "a", ["a","a","a","a","a","a","a"])
             ]
           ]


-- Exercise 4 -----------------------------------------

testStreamFmap :: (String -> String) -> (String, String) -> Bool
testStreamFmap fn (val, res) = expected == result
  where expected = take 5 $ repeat res
        result   = take 5 $ streamToList $ fmap fn $ sRepeat val

testStreamFmapAppend :: (String, String, String) -> Bool
testStreamFmapAppend (val, appVal, res) = testStreamFmap (++ appVal) (val, res)

testStreamFmapFirst :: (String, String) -> Bool
testStreamFmapFirst (val, res) = testStreamFmap ((:[]) . head) (val, res)

ex4Tests :: [Test]
ex4Tests = [ Test "test Stream fmap (append)" testStreamFmapAppend
             [("Foo", "Baz", "FooBaz"), ("1", "2", "12")]
           , Test "test Stream fmap (first)" testStreamFmapFirst
             [("Foo", "F"), ("987654321", "9")]
           ]


-- Exercise 5 -----------------------------------------

testSRepeat :: Eq a => (Int, a, [a]) -> Bool
testSRepeat (len, val, res) = res == (take len $ streamToList $ sRepeat val)

testSIterateAppend :: Eq a => (Int, [a], [a], [[a]]) -> Bool
testSIterateAppend (len, val, valApp, res) = res == (take len $ streamToList $ sIterate (++valApp) val)

testSIterateTail :: Eq a => (Int, [a], [[a]]) -> Bool
testSIterateTail (len, val, res) = res == (take len $ streamToList $ sIterate tail val)

testSInterleave :: Eq a => (Int, Stream a, Stream a, [a]) -> Bool
testSInterleave (len, a, b, res) = res == (take len $ streamToList $ sInterleave a b)

testSTake :: Eq a => (Int, Stream a, [a]) -> Bool
testSTake (len, s, res) = res == (sTake len s)

ex5Tests :: [Test]
ex5Tests = [ Test "test sRepeat" testSRepeat
             [ (0, "Foo", [])
             , (-1, "Foo", [])
             , (5, "Foo", ["Foo","Foo","Foo","Foo","Foo"])
             , (7, "a", ["a","a","a","a","a","a","a"])
             ]
           , Test "test sIterate append" testSIterateAppend
             [ (0, "Foo", "Baz", [])
             , (-1, "Foo", "Baz", [])
             , (1, "Foo", "Baz", ["Foo"])
             , (4, "Foo", "Baz", ["Foo", "FooBaz", "FooBazBaz", "FooBazBazBaz"])
             ]
           , Test "test sIterate tail" testSIterateTail
             [ (0, "Foo", [])
             , (-1, "Foo", [])
             , (1, "Foo", ["Foo"])
             , (4, "FooBaz", ["FooBaz", "ooBaz", "oBaz", "Baz"])
             ]
           , Test "test sInterleave" testSInterleave
             [ (0, sRepeat "a", sRepeat "a", [])
             , (-1, sRepeat "a", sRepeat "b", [])
             , (5, sRepeat "a", sRepeat "b", ["a","b","a","b","a"])
             , (7, sIterate (++"z") "a", sRepeat "b", ["a","b","az","b","azz","b","azzz"])
             ]
           , Test "test sTake" testSTake
             [ (0, sRepeat "a", [])
             , (-1, sRepeat "a", [])
             , (5, sRepeat "a", ["a","a","a","a","a"])
             , (5, sIterate (++"z") "a", ["a","az","azz","azzz","azzzz"])
             ]
           ]


-- Exercise 6 -----------------------------------------

testNats :: (Int, [Integer]) -> Bool
testNats (n, res) = sTake n nats == res

testRuler :: (Int, [Integer]) -> Bool
testRuler (n, res) = sTake n ruler == res

ex6Tests :: [Test]
ex6Tests = [ Test "test nats" testNats
             [ (0, [])
             , (-1, [])
             , (5, [0,1,2,3,4])
             , (7, [0,1,2,3,4,5,6])
             ]
           , Test "test ruler" testRuler
             [ (0, [])
             , (-1, [])
             , (16, [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4])
             ]
           ]


-- Exercise 7 -----------------------------------------

testRand :: (Int, Int, [Int]) -> Bool
testRand (n, seed, res) = sTake n (rand seed) == res

ex7Tests :: [Test]
ex7Tests = [ Test "test rand" testRand
             [ (0, 0, [])
             , (-1, 0, [])
             , (5, 13, [13, 1460808642, 907931091, 761707792, 1824100361])
             , (7, 31564, [31564, 1317918613, 1012031658, 906175643, 526385208, 2134988817, 97377654])
             ]
           ]


-- Exercise 8 -----------------------------------------

-- Exercise 9 -----------------------------------------

testMinMax :: (Int, Int) -> Bool
testMinMax (len, seed) = minMax vals == minMaxSlow vals
  where vals = sTake len $ rand seed

ex9Tests :: [Test]
ex9Tests = [ Test "test minMax" testMinMax
             [(0, 0), (1, 1), (10, 27), (50, 31415926)]
           ]


-- Exercise 10 -----------------------------------------

testFastFib :: (Int, Integer) -> Bool
testFastFib (nth, res) = fastFib nth == res

ex10Tests :: [Test]
ex10Tests = [ Test "test fastFib" testFastFib
             [ (0,1), (1,1), (2,2), (5,8), (10,89), (100,573147844013817084101)
             , (200,453973694165307953197296969697410619233826)
             , (500,225591516161936330872512695036072072046011324913758190588638866418474627738686883405015987052796968498626)
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
                  , ex9Tests
                  , ex10Tests
                  ]

main :: IO ()
main = putStrLn $ show $ runTests allTests
