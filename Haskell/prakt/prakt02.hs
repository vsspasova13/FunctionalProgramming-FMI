
data Bool' = True' | False'

----------------------------------------------
-- RPS (Rock, Paper, Scissors) and Booleans --
----------------------------------------------

-- RPS - enum example
-- This will be our type representing Rock, Paper, and Scissors
-- show case here
data RPS = Rock | Paper | Scissors
  deriving (Show)

-- beats function: Implement the logic for which RPS beats another
-- Use pattern matching and ignore some cases to demonstrate the use of _
-- EXAMPLES
-- >>> beats Rock Paper
-- False
-- >>> beats Paper Rock
-- True
beats :: RPS -> RPS -> Bool
beats Rock Scissors = True
beats Paper Rock = True
beats Scissors Paper = True
beats _ _ = False

-- TASK
-- Define the "next" throw you can do in the "usual" ordering of RPS
-- i.e. @next x@ should be the throw that beats x
-- EXAMPLES
-- >>> next Rock
-- Paper
next :: RPS -> RPS
next Rock = Paper
next Paper = Scissors
next Scissors = Rock

next' :: RPS -> RPS
next' x = case x of
  Rock -> Paper
  Paper -> Scissors
  Scissors -> Rock

-- TASK
-- Define what it means for two RPS values to be equal
-- Use pattern matching and use _ matches!
-- EXAMPLES
-- >>> eqRPS Rock Rock
-- True
-- >>> eqRPS Rock Paper
-- False
eqRPS :: RPS -> RPS -> Bool
eqRPS Rock Rock = True
eqRPS Paper Paper = True
eqRPS Scissors Scissors = True
eqRPS _ _ = False


-- TASK
-- Define a shorter version of beats using next and eqRPS
-- EXAMPLES
-- >>> beats' Rock Paper
-- True
-- >>> beats' Paper Scissors
-- True
beats' :: RPS -> RPS -> Bool
beats' x y = eqRPS x (next y)

------------
-- Points --
------------

-- Record syntax for a point in 2D discrete space
data Point = MkPoint Integer Integer
  deriving (Show)

defaultPoint :: Point
defaultPoint = MkPoint 0 0

-- |
-- Check if a point is in the first quadrant (both x and y are positive)
isInFirstQuadrant :: Point -> Bool
isInFirstQuadrant (MkPoint x y) = x > 0 && y > 0  

inWhichQuadrantIsIn :: Point -> Integer
inWhichQuadrantIsIn (MkPoint x y) 
  | x >= 0 && y >= 0 = 1
  | x < 0 && y >= 0 = 2
  | x < 0 && y < 0 = 3
  | x >= 0 && y < 0 = 4
  | otherwise = undefined "opa belq"

-- |
-- Invert a point by swapping the signs of x and y
invert :: Point -> Point
invert (MkPoint x y) = MkPoint (-x) (-y)

----------------------------------------
-- Natural Numbers (Peano Arithmetic) --
----------------------------------------

-- Encoding for natural numbers using Peano arithmetic
data Nat = Zero | Succ Nat
  deriving (Show)

-- TASK
-- Convert an Integer to Nat
-- EXAMPLES
-- >>> integerToNat 3
-- Succ (Succ (Succ Zero))
integerToNat :: Integer -> Nat
integerToNat x =
  if x == 0 
  then Zero
  else Succ (integerToNat (x - 1))

-- TASK
-- Convert a Nat back to an Integer
-- EXAMPLES
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

-- TASK
-- Add two Nats
-- EXAMPLES
-- >>> addNat (Succ Zero) (Succ Zero)
-- Succ (Succ Zero)
addNat :: Nat -> Nat -> Nat
addNat Zero n2 = n2
addNat (Succ n1) n2 =  Succ (addNat n1 n2)

-- TASK
-- Multiply two Nats
-- EXAMPLES
-- >>> multNat (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))
-- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
multNat :: Nat -> Nat -> Nat
multNat Zero n2 = Zero
multNat n1 n2 = n2 `addNat` (n1 `multNat` n2)

-- TASK
-- Compare two Nats, returning an Ordering
-- EXAMPLES
-- >>> compareNat (Succ Zero) Zero
-- GT
compareNat :: Nat -> Nat -> Ordering
compareNat Zero Zero = EQ
compareNat Zero (Succ _) = LT
compareNat (Succ _) Zero = GT
compareNat (Succ n1) (Succ n2) = compareNat n1 n2

-- TASK
-- Return the maximum of two Nats
-- EXAMPLES
-- >>> maxNat (Succ Zero) (Succ (Succ Zero))
-- Succ (Succ Zero)
maxNat :: Nat -> Nat -> Nat
maxNat n1 Zero = n1
maxNat Zero n2 = n2
matNat (Succ n1) (Succ n2) = Succ (maxNat n1 n2) 

-----------------
-- Expressions --
-----------------

-- A simple expression language for a calculator
data Expr
  = Val Integer
  | Plus Expr Expr
  | Mult Expr Expr
  deriving (Show)

infixr 7 `Plus`
infixr 8 `Mult`

-- TASK
-- Evaluate an expression in the Expr language
-- EXAMPLES
-- >>> eval (Val 3)
-- 3
-- >>> eval (Plus (Val 3) (Val 4))
-- 7
-- >>> eval (Mult (Val 3) (Val 4))
-- 12
eval :: Expr -> Integer
eval (Val x) = x
eval (Mult x y) = eval x * eval y
eval (Plus x y) = eval x + eval y
