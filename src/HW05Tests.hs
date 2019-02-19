module HW05Tests where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 as Char8

import HW05
import Testing

-- Exercise 1 -----------------------------------------

testGetSecret :: (ByteString, ByteString, ByteString) -> Bool
testGetSecret (orig, modf, scrt) = getSecret' orig modf == scrt

ex1Tests :: [Test]
ex1Tests = [ Test "test getSecret" testGetSecret
             [ ( Char8.pack "LOREM IPSUM DOLOR SIT AMET"
               , Char8.pack "POREM IPIUM DOLPR SAT AMST"
               , Char8.pack "\FS\SUB\US\b\SYN") ]
           ]


-- All Tests ------------------------------------------

allTests :: [Test]
allTests = Prelude.concat [ ex1Tests
                          ]
