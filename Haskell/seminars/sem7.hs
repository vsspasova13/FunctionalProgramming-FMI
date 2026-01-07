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

---2

data List a = Null | Cons a (List a)

isEmpty :: List a -> Bool
isEmpty Null = True
isEmpty _ = False

headList :: List a -> Maybe a
headList Null = Nothing
headList (Cons a _) = Just a

singleton :: a -> List a
singleton a = Cons a Null

(+++) :: List a -> List a -> List a
(+++) Null xs = xs
(+++) (Cons x xs) ys = Cons x (xs +++ ys)

reverseList :: List a -> List a
reverseList Null = Null
reverseList xs = helper xs Null
    where 
        helper :: List a -> List a -> List a
        helper Null acc = acc
        helper (Cons x xs) acc = helper xs (Cons x acc)

fromList :: [a] -> List a
fromList [] = Null
fromList (x:xs) = (Cons x (fromList xs))

toList :: List a -> [a]
toList Null = []
toList (Cons x xs) = x : toList xs

mapList :: (a -> b) -> List a -> List b
mapList _ Null = Null
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

intersperse :: a -> List a -> List a
intersperse x Null = Null
intersperse _ (Cons y Null) = Cons y Null
intersperse x (Cons y ys) = Cons y (Cons x (intersperse x ys))

--3
data Expr a
  = Constant a
  | Variable String
  | Expr a :+: Expr a
  | Expr a :*: Expr a
  deriving (Show, Eq, Ord)

type Dict k v = [(k,v)]

fromListD :: [(k,v)] -> Dict k v
fromListD = id

lookUp :: Eq k => k -> Dict k v -> Maybe v
lookUp _ [] = Nothing
lookUp key ((k,v):xs)
    | key == k = Just v
    | otherwise = lookUp key xs

eval :: Num a => Dict String a -> Expr a -> Maybe a
eval _ (Constant a) = Just a
eval kvs (Variable v) = lookUp v kvs
eval kvs (e1 :+: e2) =
    case (eval kvs e1, eval kvs e2) of
        (Just v1, Just v2) -> Just (v1+v2)
        _                  -> Nothing
eval kvs (e1 :*: e2) =
    case (eval kvs e1, eval kvs e2) of
        (Just v1, Just v2) -> Just (v1*v2)
        _                  -> Nothing


--4

data Tree a = Node a [Tree a]

nodes:: Tree a -> Int
nodes (Node _ []) = 1
nodes (Node _ xs) = 1 + sum (map nodes xs)

leaves :: Tree a -> Int
leaves (Node _ []) = 1
leaves (Node _ xs) = sum (map nodes xs)

contains :: Eq a => a -> Tree a -> Bool
contains x (Node y ys) = x == y || any (contains x) ys

flatten :: Tree a -> [a]
flatten (Node x []) = [x]
flatten (Node x xs) = x : concatMap flatten xs

--5

data BinTree a = Nodee a (BinTree a) (BinTree a) | Empty
    deriving Show

countLeaves :: BinTree a -> Int
countLeaves Empty = 0
countLeaves (Nodee _ Empty Empty) = 1
countLeaves (Nodee _ t1 t2) = countLeaves t1 + countLeaves t2

height :: BinTree a -> Int;
height Empty = 0
height (Nodee _ t1 t2) = 1 + max (height t1) (height t2)

mapBT :: (a -> b) -> BinTree a -> BinTree b;
mapBT f Empty = Empty
mapBT f (Nodee x t1 t2) = (Nodee (f x) (mapBT f t1) (mapBT f t2))

inorder :: BinTree a -> [a];
inorder Empty = []
inorder (Nodee x t1 t2) = inorder t1 ++ [x] ++ inorder t2

preorder :: BinTree a -> [a];
preorder Empty = []
preorder (Nodee x t1 t2) = x : preorder t1 ++ preorder t2

insertBST :: Ord a => a -> BinTree a -> BinTree a
insertBST x Empty = Nodee x Empty Empty
insertBST x (Nodee y t1 t2) 
    | x < y     = Nodee y (insertBST x t1) t2
    | otherwise = Nodee y t1 (insertBST x t2)

toBST :: Ord a => [a] -> BinTree a;
toBST [] = Empty
toBST (x:xs) = foldl (flip insertBST) Empty (x:xs)

isBST' :: Ord a => a -> a -> BinTree a -> Bool
isBST' _  _ Empty = True
isBST' minV maxV (Nodee x l r) =
     x >= minV && x <= maxV
     && isBST' minV x l
     && isBST' x maxV r

isBST :: (Ord a, Bounded a) => BinTree a -> Bool
isBST = isBST' minBound maxBound 

isBalanced :: BinTree a -> Bool
isBalanced Empty = True
isBalanced (Nodee x l r) = abs (height l - height r) <= 1 && isBalanced l && isBalanced r

isBalancedBST :: (Ord a, Bounded a) => BinTree a -> Bool
isBalancedBST Empty = True
isBalancedBST tree = isBST tree && isBalanced tree

--6

paths :: BinTree a -> [[a]]
paths Empty = []
paths (Nodee x Empty Empty) = [[x]]
paths (Nodee x l r) = map (x:) (paths l ++ paths r)

oddProduct :: [Int] -> Int
oddProduct xs = product [x | x <- xs, x `mod` 2 > 0]

minOddProduct :: [[Int]] -> Int
minOddProduct xs = minimum [oddProduct x | x <- xs]

pathsWithMinOddProd :: [[Int]] -> [[Int]]
pathsWithMinOddProd xs =
    let minProd = minOddProduct xs
    in [p | p <- xs, oddProduct p == minProd]

maxLengthMinOddProd :: BinTree Int -> Int
maxLengthMinOddProd Empty = 0
maxLengthMinOddProd t = 
    let allPaths = paths t
    in maximum (map length (pathsWithMinOddProd allPaths))

tree :: BinTree Int
tree = Nodee 2
          (Nodee 3 Empty Empty)
          (Nodee 5 (Nodee 7 Empty Empty) Empty)