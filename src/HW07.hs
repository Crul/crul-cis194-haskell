{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random hiding (mapM, liftM)
-- import Data.Functor
-- import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
-- import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do a <- ma
                return $ f a
-- liftM f ma = ma >>= \a -> return $ f a
-- liftM f ma = [ f x | x <- ma ]

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV idx1 idx2 vec = liftM2 assign (vec !? idx1) (vec !? idx2)
  where assign elm1 elm2 = vec // [(idx1, elm2), (idx2, elm1)]


-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f xs = sequence $ map f xs

getElts :: [Int] -> Vector a -> Maybe [a]
getElts idxs vec = mapM (\idx -> vec !? idx) idxs

-- Exercise 3 -----------------------------------------
-- evalRandIO $ randomElt (V.fromList [])
-- evalRandIO $ randomElt (V.fromList [0])

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = (v !?) <$> getRandomR (0, pred $ length v)

-- randomElt v = liftM (v !?) $ getRandomR (0, length v - 1)
-- randomElt v = (v !?) `liftM` getRandomR (0, length v - 1)

{- 1st attempt
randomElt vec = do
  rndIdx <- getRandomR (0, length vec)
  return $ (vec !? rndIdx)
-}

-- Exercise 4 -----------------------------------------
-- evalRandIO $ randomVec 0
-- evalRandIO $ randomVec 3

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.fromList <$> replicateM n getRandom

-- randomVec n = liftM V.fromList $ replicateM n getRandom

-- 1st attempt
-- randomVec n = sequence $ V.fromList $ map (\_ -> getRandom) [1..n]

-- evalRandIO $ randomVecR 0 (0,0)
-- evalRandIO $ randomVecR 3 (1,10)
-- evalRandIO $ randomVecR 3 (10,1)

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n rng = V.fromList <$> replicateM n (getRandomR rng)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle vec = shuffle' (pred $ length vec) vec

shuffle' :: Int -> Vector a -> Rnd (Vector a)
shuffle' (-1) vec = return vec
shuffle'  i   vec = do j <- getRandomR (0, i)
                       let vec' = vec // [(i, vec!j), (j, vec!i)]
                       shuffle' (pred i) vec'

{-
shuffle v = (v //) `liftM` go v (getN $ length v)
  where go :: Vector a -> [Int] -> Rnd ([(Int, a)])
        go v' as = (zip' v' as) `liftM` mapM (\x -> getRandomR $ getPair x) as
-}

{-
shuffle vec = sub vec $ len - 1
  where
    sub v 0 = return v
    sub v i = do
        newV <- liftM (vswap v i) $ getRandomR (0, i)
        sub newV $ i - 1
    vswap v i j = v // [(i, v ! j), (j, v ! i)]
    len = length vec
-}

{-
shuffle v = foldl (\v2 (i, j) -> fromJust $ swapV i j v2) v <$> mapM pair is
  where n  = length v
        is = [n-1,n-2..1]
        pair i = ((,) i) <$> getRandomR (0, i)
-}

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vec idx = (V.filter (<val) vec', val, V.filter (>=val) vec')
  where val  = vec ! idx
        vec' = (V.take idx vec) V.++ (V.drop (succ idx) vec)

{-
partitionAt v i = (lt, p, gte)
  where p   = v ! i
        lt  = V.ifilter (\vi va -> va <  p && vi /= i) v
        gte = V.ifilter (\vi va -> va >= p && vi /= i) v
-}

{-
partitionAt v p = (mfilter (pivot >) v', pivot, mfilter (pivot <=) v')
  where pivot = v!p
        v' = V.take p v V.++ V.drop (p+1) v
-}

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vec | length vec < 2 = vec
          | otherwise      = (qsort less) V.++ cons curr (qsort grtr)
            where (less, curr, grtr) = partitionAt vec 0

-- Exercise 8 -----------------------------------------
-- evalRandIO $ qsortR (V.fromList [5,2,8,7,1,3,4])
-- evalRandIO $ qsortR (V.fromList [1,2,1,2,1,2,1,2,1,2])
-- evalRandIO $ qsortR (V.fromList $ reverse [1..10000])

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vec | len < 2   = return vec
           | otherwise = do
              rndIdx <- getRandomR (0, pred len)
              let (less, curr, grtr) = partitionAt vec rndIdx
              less' <- qsortR less
              grtr' <- qsortR grtr
              return $ less' V.++ (cons curr grtr')
           where len = length vec

-- Exercise 9 -----------------------------------------
-- evalRandIO $ select 3 (V.fromList [4,2,3,1,5,6,7])

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select idx vec | idx >= len = return Nothing
               | otherwise  = do rndIdx <- getRandomR (0, pred len)
                                 retOrSel $ partitionAt vec rndIdx

               where len = length vec
                     retOrSel (less,curr,grtr) | idx == lenL = return $ Just curr
                                               | otherwise = select' lenL less grtr
                                                 where lenL = length less

                     select' lenL less grtr | idx < lenL = select idx less
                                            | otherwise  = select (idx-lenL-1) grtr


-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card lb st | lb <- labels, st <- suits ]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck | V.null deck = Nothing
              | otherwise   = Just (V.head deck, V.tail deck)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = getCards' []
  where getCards' cs 0 deck = Just (cs, deck)
        getCards' cs n deck = addCard (nextCard deck)
          where addCard mbCardDeck = do
                 (card, deck') <- mbCardDeck
                 getCards' (cs ++ [card]) (pred n) deck'

{-- 1st attempt
  where
    getCards' cs 0 deck = Just (cs, deck)
    getCards' cs n deck = addCard (nextCard deck)
      where
        addCard Nothing              = Nothing  <--- NEEDED BECAUSE PATTERN MATCHING INSTEAD OF a <- m a
        addCard (Just (card, deck')) = getCards' (cs ++ [card]) (pred n) deck'
--}

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
