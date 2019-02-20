module HW05Tests where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 as Char8
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map

import HW05
import Testing
import Parser

-- Exercise 1 -----------------------------------------

testGetSecret :: (ByteString, ByteString, ByteString) -> Bool
testGetSecret (orig, modf, scrt) = getSecret' orig modf == scrt

ex1Tests :: [Test]
ex1Tests = [ Test "test getSecret" testGetSecret
             [ ( Char8.pack "LOREM IPSUM DOLOR SIT AMET"
               , Char8.pack "POREM IPIUM DOLPR SAT AMST"
               , Char8.pack "\FS\SUB\US\b\SYN") ]
           ]


-- Exercise 2 -----------------------------------------

-- Exercise 3 -----------------------------------------

-- Exercise 4 -----------------------------------------

testFindBadTs :: (Maybe [TId], Maybe [Transaction], Maybe [Transaction]) -> Bool
testFindBadTs (tids, trns, bads) = findBadTs tids trns == bads

ex4Tests :: [Test]
ex4Tests = [ Test "test findBadTs" testFindBadTs
             [ ( Nothing, Nothing, Nothing)
             , ( Just [], Nothing, Nothing)
             , ( Nothing, Just [], Nothing)
             , ( Just ["bad1","bad2"], Just [], Just[])
             , ( Just ["bad1","bad2"]
               , Just [Transaction "from" "to" 10 "good1"]
               , Just []
               )
             , ( Just ["bad1","bad2"]
               , Just [Transaction "from" "to" 10 "bad1", Transaction  "from2" "to2" 20 "good1"]
               , Just [Transaction "from" "to" 10 "bad1"]
               )
             ]
           ]


-- Exercise 5 -----------------------------------------

testGetFlow :: ([Transaction], [(String, Integer)]) -> Bool
testGetFlow (trns, flow) = getFlow trns == Map.fromList flow

ex5Tests :: [Test]
ex5Tests = [ Test "test getFlow" testGetFlow
             [ ( [], [] )
             , ( [Transaction "Kelly" "John" 10 ""], [("John", 10), ("Kelly", -10)] )
             , ( [Transaction "Kelly" "John" 10 "", Transaction "John" "Kelly" 15 ""]
               , [("John", -5), ("Kelly", 5)]
               )
             ]
           ]


-- All Tests ------------------------------------------

allTests :: [Test]
allTests = Prelude.concat [ ex1Tests
                          , ex4Tests
                          , ex5Tests
                          ]
