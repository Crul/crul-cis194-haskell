{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.Bits as DB
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------
-- getSecret "clues/dog-original.jpg" "clues/dog.jpg"

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret base trgt = do
  fBase <- BS.readFile base
  fTrgt <- BS.readFile trgt
  return $ getSecret' fBase fTrgt

getSecret' :: ByteString -> ByteString -> ByteString
getSecret' base trgt = toBSt xored
  where xored = BS.zipWith DB.xor base trgt
        toBSt = BS.pack . filter (/=0)


-- Exercise 2 -----------------------------------------
{--
:{
do
  key <- getSecret "clues/dog-original.jpg" "clues/dog.jpg"
  let file = "clues/victims.json"
  Prelude.putStrLn "decrypting..."
  decryptWithKey key file
  Prelude.putStrLn $ file ++ " decrypted!"
:}
--}

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey pssw fPth = do
  encF <- BS.readFile $ fPth ++ ".enc"
  let decoded = getSecret' encF (BS.cycle pssw)
  BS.writeFile fPth decoded


-- Exercise 3 -----------------------------------------
-- parseFile "clues/victims.json" :: IO (Maybe [Parser.TId])
-- parseFile "clues/transactions.json" :: IO (Maybe [Parser.Transaction])

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fPth = do
  fData <- BS.readFile $ fPth
  return $ decode fData

-- Exercise 4 -----------------------------------------
{--
:{
do (Just bad) <- getBadTs "clues/victims.json" "clues/transactions.json"
   Prelude.putStr $ show $ Prelude.length bad
   -- Prelude.putStr $ show $ Prelude.map Parser.tid bad
:}
--}

findBadTs :: Maybe [TId] -> Maybe [Transaction] -> Maybe [Transaction]
findBadTs Nothing     _           = Nothing
findBadTs _           Nothing     = Nothing
findBadTs (Just tids) (Just trns) = Just badTs
  where badTs = filter (\t -> (tid t) `elem` tids) trns

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victFPath trnsFPath = do
  victims <- parseFile victFPath :: IO (Maybe [TId])
  transcs <- parseFile trnsFPath :: IO (Maybe [Transaction])
  return $ findBadTs victims transcs


-- Exercise 5 -----------------------------------------
{--
:{
do
  Just trans <- getBadTs "clues/victims.json" "clues/transactions.json"
  return $ getFlow trans
:}
--}

adjustFlow :: Transaction -> Map String Integer -> Map String Integer
adjustFlow (Transaction from to amount _) = addAmount from (-amount) . addAmount to amount
    where addAmount = Map.insertWith (+)

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr adjustFlow Map.empty

{-- First attempt
getFlow trns = Map.fromList finalB
  where
    positv = map (\tr -> (to tr  ,  amount tr)) trns
    negatv = map (\tr -> (from tr, -amount tr)) trns
    allBal = sortBy (compare `on` fst) $ positv ++ negatv
    groupd = groupBy ((==) `on` fst) allBal
    group' = map (\l -> (fst . head $ l, map snd l)) groupd
    finalB = map (\(k,d) -> (k, sum d)) group'
--}


-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = undefined

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

