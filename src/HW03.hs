module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st a i = st'
  where st' b = if a == b then i else st b

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

toInt :: Bool -> Int
toInt b = if b then 1 else 0

evalE :: State -> Expression -> Int
evalE st (Var str)    = st str
evalE st (Val i)      = i
evalE st (Op a bop b) = op a' b'
  where
    a' = evalE st a
    b' = evalE st b
    op = case bop
         of Plus   -> (+)
            Minus  -> (-)
            Times  -> (*)
            Divide -> div
            Gt     -> (toInt .) . (>)
            Ge     -> (toInt .) . (>=)
            Lt     -> (toInt .) . (<)
            Le     -> (toInt .) . (<=)
            Eql    -> (toInt .) . (==)

{-- Non-point-free version
            Gt     -> boolToIntOp (>)
            Ge     -> boolToIntOp (>=)
            Lt     -> boolToIntOp (<)
            Le     -> boolToIntOp (<=)
            Eql    -> boolToIntOp (==)

    boolToIntOp :: (Int -> Int -> Bool) -> Int -> Int -> Int
    boolToIntOp bFn a b = toInt $ bFn a b
--}

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar Skip = DSkip
desugar (Assign st ex)          = DAssign st ex
desugar (If ex aSt bSt)         = DIf ex (desugar aSt) (desugar bSt)
desugar (While ex st)           = DWhile ex (desugar st)
desugar (Sequence aSt bSt)      = DSequence (desugar aSt) (desugar bSt)
desugar (Incr st)               = DAssign st (Op (Var st) Plus (Val 1))
desugar (For ini cond inc body) = dFor
  where
    ini'    = desugar ini
    body'   = desugar body
    inc'    = desugar inc
    whlBody = DSequence body' inc'
    dWhile  = DWhile cond whlBody
    dFor    = DSequence ini' dWhile

-- Exercise 4 -----------------------------------------

toBool :: Int -> Bool
toBool = (0 /=)

exprIsTrue :: State -> Expression -> Bool
exprIsTrue state ex = toBool $ evalE state ex

evalSimple :: State -> DietStatement -> State
evalSimple state DSkip               = state
evalSimple state (DAssign st ex)     = extend state st (evalE state ex)
evalSimple state (DIf ex aSt bSt)    = evalSimple state (if exprIsTrue state ex then aSt else bSt)
-- evalSimple state (DSequence aSt bSt) = evalSimple (evalSimple state aSt) bSt
evalSimple state (DSequence aSt bSt) = foldl evalSimple state [aSt, bSt]
evalSimple state w@(DWhile ex st)    = if exprIsTrue state ex
                                       then evalSimple state (DSequence st (DWhile ex st))
                                       else state

run :: State -> Statement -> State
run state stmt = evalSimple state (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
