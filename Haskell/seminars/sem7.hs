module Solutions where

import Prelude hiding (succ, pred)

data Nat = Zero | Succ Nat

succ :: Nat -> Nat
succ = Succ 

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ $ add m n

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ m) n = add n (mult m n) 

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

cmp :: Nat -> Nat -> Ordering
cmp Zero Zero = EQ
cmp Zero _ = LT
cmp _ Zero = GT
cmp (Succ n) (Succ m) = cmp n m

data Tree a = Node a [Tree a]

nodes:: Tree a -> Int
nodes (Node _ []) = 1
nodes (Node _ xs) = 1 + sum (map nodes xs)

leaves :: Tree a -> Int
leaves (Node _ []) = 1
leaves (Node _ xs) = sum (map nodes xs)

contains :: Eq a => a -> Tree a -> Bool
contains x (Node y ys) = x ==y || any (contains x) ys

data BinTree a = Nodee a (BinTree a) (BinTree a) | Empty

