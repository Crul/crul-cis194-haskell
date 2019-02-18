{-# OPTIONS_GHC -Wall #-}
module HW02 where
import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = length . filter (uncurry (==)) $ zip a b
-- https://stackoverflow.com/a/13426526
-- exactMatches a b = length . filter (uncurry (==)) $ zip a b
-- exactMatches a b = length . filter (uncurry (==)) . zip a $ b
-- exactMatches a = length . filter (uncurry (==)) . zip a
-- exactMatches a = (.) (length . filter (uncurry (==))) (zip a)
-- exactMatches a = ((.) (length . filter (uncurry (==)))) . zip $ a
-- exactMatches = ((.) (length . filter (uncurry (==)))) . zip

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map countColor colors
  where countColor :: Peg -> Int
        countColor peg = length $ filter (==peg) code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches a b = sum $ zipWith min countA countB
  where
    countA = countColors a
    countB = countColors b

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exactM nonExM
  where
    exactM = exactMatches secret guess
    allM   = matches secret guess
    nonExM = allM - exactM

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent mv@(Move guess _ _) code = mv == getMove code guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------
addColor :: Code -> [Code]
addColor code = map (:code) colors

addColor' :: Code -> [Code]
addColor' code = map (\color -> code ++ [color]) colors

allCodes :: (Code -> [Code]) -> Int -> [Code]
allCodes _          0 = [[]]
allCodes addColorFn n
  | n > 0     = concatMap addColorFn nxtCodes
  | otherwise = [[]]
  where
    nxtCodes  = allCodes addColorFn $ n-1

-- Exercise 7 -----------------------------------------

solveGuess :: Code -> [Code] -> [Move]
solveGuess _    []       = []
solveGuess scrt (gss:cs) = mv : nxtMoves
  where
    mv       = getMove scrt gss
    filtered = filterCodes mv cs
    nxtMoves = solveGuess scrt filtered

solve :: Code -> [Move]
solve scrt = solveGuess scrt candidates
  where candidates = allCodes addColor $ length scrt

{-- Exercise 7 Alternative Version ---------------------
solve :: Code -> [Move]
solve scrt = solution
  where
    candidates = allCodes addColor $ length scrt
    solution   = solveGuess candidates []

    solveGuess :: [Code] -> [Move] -> [Move]
    solveGuess []         moves = moves
    solveGuess (guess:gs) moves
      | guess == scrt = mv : moves
      | otherwise     = mv : nxtMoves
        where
          mv       = getMove scrt guess
          filtered = filterCodes mv (guess:gs)
          nxtMoves = solveGuess filtered moves
--}

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess scrt = fiveGuessSolver scrt allC allC fstCode
  where
    brdSize  = length scrt
    fstCode  = firstCode brdSize
    allC     = allCodes addColor' brdSize

firstCode :: Int -> Code
firstCode brdSize = [ if x <= brdSize `div` 2
                      then colors!!0
                      else colors!!1
                    | x <- [1..brdSize] ]

fiveGuessSolver :: Code -> [Code] -> [Code] -> Code -> [Move]
-- fiveGuessSolver _    []     _      _    = []
-- fiveGuessSolver []   _      _      _    = []
fiveGuessSolver scrt unused valids code = mv : moves
  where
    mv = getMove scrt code
    moves
      | code == scrt = []
      | otherwise    = fiveGuessSolver scrt unused' valids' code'
        where
          unused' = delete code unused
          valids' = filterCodes mv $ delete code valids
          code'   = minimax unused' valids'

minimax :: [Code] -> [Code] -> Code
-- minimax _      []     = []
-- minimax []     (v:_)  = v
minimax unused valids = head minSorted
  where
    vals         = map (scoreCode valids) unused
    scores       = zip unused vals
    minVal       = minimum vals
    minValScores = filter ((== minVal) . snd) scores
    minValCodes  = map fst minValScores
    validMinVCs  = intersect minValCodes valids
    minSorted    = concat [validMinVCs, minValCodes]

scoreCode :: [Code] -> Code -> Int
scoreCode valids code = maximum scores
  where
    getMove' c = getMove c code
    moves      = map getMove' valids
    uniqueMvs  = nub moves
    scoreMove  = \mv -> length $ filter (== mv) moves
    scores     = map scoreMove uniqueMvs
