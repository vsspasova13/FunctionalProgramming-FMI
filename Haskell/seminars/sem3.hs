module Solutions where

import Prelude hiding (repeat, Ordering, Maybe, Just, Nothing)

repeat :: a -> [a]
repeat x = x : repeat x

from :: Int -> [Int]
from n = n : from (n + 1)

fibsFrom :: Integer -> Integer -> [Integer]
fibsFrom a b = a : fibsFrom b (a + b)

fibs :: [Integer]
fibs = fibsFrom 0 1

data Ordering = Lower | Equal | Greater 
    deriving (Show, Read, Eq, Ord)

cmpInt :: Int -> Int -> Ordering
cmpInt a b
    | a < b =  Lower
    | a > b = Greater
    | otherwise = Equal

type Sides = Int
type Length = Double

data Shape = Square Length | Triangle Length Length Length | Polygon Sides Length
    deriving (Show, Eq, Ord)

perimeter :: Shape -> Double
perimeter (Triangle a b c) = a + b + c
perimeter (Polygon n a) = fromIntegral n * a
perimeter (Square a) = 4 * a

numberOfSides :: Shape -> Int
numberOfSides (Triangle {})= 3
numberOfSides (Square _ )= 4
numberOfSides (Polygon n _) = n

prettyPrint :: Shape -> String
prettyPrint (Triangle a b c) = "This figure is Triangle with sides " ++ show a ++ ", " ++ show b ++ "and " ++ show c
prettyPrint (Square a) = "This figure is Square with side " ++ show a
prettyPrint (Polygon n a) = "This figure is Polygon with " ++ show n ++ " side, each with length of " ++ show a

data Pair a b =  Pair a b
    deriving (Show, Eq, Ord)

myFst :: Pair a b -> a
myFst (Pair a _) = a

mySnd :: Pair a b -> b
mySnd (Pair _ b) = b

myRev :: Pair a b -> Pair b a
myRev (Pair a b) = Pair b a

pairToTuple :: Pair a b -> (a,b)
pairToTuple (Pair a b) = (a,b)

tupleToPair :: (a,b) -> Pair a b
tupleToPair (a,b) = Pair a b

cmp :: (Ord a) => a -> a -> Ordering
cmp a b
    | a < b =  Lower
    | a > b = Greater
    | a == b = Equal

cmpPair :: (Ord a, Ord b) => Pair a b -> Pair a b -> Ordering
cmpPair (Pair a b) (Pair x y) = 
    case cmp a x of
        Equal -> cmp b y
        res -> res
 
pairsToList :: [Pair a b] -> Pair [a] [b]
pairsToList  = helper [] [] 
    where
        helper :: [a] -> [b] -> [Pair a b] -> Pair [a] [b]
        helper xs ys [] = Pair xs ys
        helper xs ys ((Pair a b) : rest) = helper (xs ++ [a]) (ys ++ [b]) rest

data Maybe a = Just a | Nothing
    deriving (Show, Eq, Ord)

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM (Just a) (Just b) = Just (a + b)
addM _ _ = Nothing

sumM :: [Maybe Int] -> Maybe Int
sumM [] = Just 0
sumM ((Just x) : xs) = 
    case sumM xs of
        Just s -> Just (x + s)
        _ -> Nothing
sumM _ = Nothing

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

fromJust :: Maybe a -> a
fromJust Nothing = error "no value of type a in Nothing"
fromJust (Just a) = a

quickSort :: (Ord a ) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort small ++ [x] ++ quickSort large
    where 
      small = filterSmall xs x
      large = filterLarge xs x

      filterSmall :: (Ord a) => [a] -> a ->[a]
      filterSmall [] _ =[]
      filterSmall (y:ys) piv 
        | y <= piv = y : filterSmall ys piv
        | otherwise = filterSmall ys piv
    
      filterLarge :: (Ord a) => [a] -> a ->[a]
      filterLarge [] _ =[]
      filterLarge (y:ys) piv 
        | y > piv = y : filterLarge ys piv
        | otherwise = filterLarge ys piv
    
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs)
  | x `elem` xs = nub xs
  | otherwise = x : nub xs

newtype Set a = Set [a]
    deriving (Show)

emptySet :: Set a
emptySet = Set []

singletonSet :: a -> Set a
singletonSet x = Set [x]

fromList :: (Ord a, Eq a) => [a] -> Set a
fromList xs = Set (nub (quickSort xs))

toList :: Set a -> [a]
toList (Set xs) = xs

insert :: (Ord a ) => a -> Set a -> Set a
insert x (Set xs) = Set (helper x xs)
    where 
        helper :: (Ord a) => a -> [a] -> [a]
        helper x [] = [x]
        helper x l@(y:ys) = 
            case cmp x y of
                Equal -> l
                Lower -> x : y : ys
                Greater -> y : helper x ys

delete :: (Eq a) => a -> Set a -> Set a
delete a (Set xs) = Set (helper a xs)
    where 
        helper :: (Eq a ) => a -> [a] -> [a]
        helper x [] = []
        helper x (y:ys)
           | x == y = ys
           | otherwise = y : helper x ys

elemS ::(Eq a) => a -> Set a -> Bool
elemS x (Set xs) = x `elem` xs

union ::(Ord a) => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (mergeHelp xs ys)
    where mergeHelp :: (Ord a) => [a] -> [a] -> [a]
          mergeHelp [] ys = ys
          mergeHelp xs [] = xs
          mergeHelp (x:xs) (y:ys)
            | x < y = x : mergeHelp xs (y:ys)
            | otherwise = y : mergeHelp (x:xs) ys

intersect :: (Ord a) => Set a -> Set a -> Set a
intersect (Set xs) (Set ys) = Set (helper xs ys)
    where 
        helper :: (Ord a) => [a] -> [a] -> [a]
        helper [] _ = []
        helper _ [] = [] 
        helper l1@(x:xs) l2@(y:ys) =
            case cmp x y of
            Equal -> x : helper xs ys
            Lower -> helper xs l2
            Greater -> helper l1 ys

equal :: (Eq a) => Set a -> Set a -> Bool
equal (Set xs) (Set ys) = xs == ys

newtype Dict k v = Dict [(k,v)]
    deriving (Show)

emptyDict :: Dict k v
emptyDict = Dict []

singletonDict :: k -> v -> Dict k v
singletonDict key value = Dict [(key, value)]

quickSortKvp :: (Ord k) => [(k,v)] -> [(k,v)]
quickSortKvp [] = []
quickSortKvp (x:xs) = quickSortKvp small ++ [x] ++ quickSortKvp large
    where
        small = filterSmall xs x
        large = filterLarge xs x

        filterSmall ::  (Ord k) => [(k, v)] -> (k, v) -> [(k, v)]
        filterSmall [] _ =[]
        filterSmall (kvp@(key',_) : kvps) x@(key, _) 
            | key' <= key = kvp : filterSmall kvps x
            | otherwise = filterSmall kvps x
    
        filterLarge :: (Ord k) => [(k, v)] -> (k, v) -> [(k, v)]
        filterLarge [] _ =[]
        filterLarge (kvp@(key',_) : kvps) x@(key, _) 
            | key' > key = kvp : filterSmall kvps x
            | otherwise = filterSmall kvps x

elemKvp :: (Eq k) => (k,v) -> [(k,v)] -> Bool
elemKvp _ [] = False
elemKvp (k,v) ((key,_) : kvps)
    | k == key = True
    | otherwise = elemKvp (k,v) kvps

fromListDict :: (Ord k) => [(k,v)] -> Dict k v
fromListDict kvs = Dict (quickSortKvp (filterKvp kvs))
    where 
        filterKvp :: (Eq k) => [(k,v)] -> [(k,v)]
        filterKvp [] = [] 
        filterKvp (kvp : kvps)
            | kvp `elemKvp` kvps = filterKvp kvps
            | otherwise = kvp : filterKvp kvps

toListD :: Dict k v -> [(k,v)]
toListD (Dict kvs) = kvs

insertD :: (Ord k) => k -> v -> Dict k v -> Dict k v
insertD k v (Dict kvs) = Dict (helper k v kvs)
    where 
        helper :: (Ord k) => k -> v -> [(k,v)] -> [(k,v)]
        helper k v [] = [(k,v)]
        helper k v (dv@(key, _):xs) =
            case cmp k key of 
                Equal -> (k,v) : xs
                Lower -> (k,v) : dv : xs
                Greater -> dv : helper k v xs

deleteD :: (Eq k) => k -> Dict k v -> Dict k v
deleteD k (Dict kvs) = Dict (helper k kvs)
    where
        helper :: (Eq k) => k -> [(k,v)] -> [(k,v)]
        helper _ [] = []
        helper key ((k,_):xs)
            | key == k = xs
            | otherwise = helper key xs 

lookupD :: (Eq k) => k -> Dict k v -> Maybe v
lookupD key (Dict kvs) = helper key kvs
    where 
        helper :: (Eq k) => k -> [(k,v)] -> Maybe v
        helper _ [] = Nothing
        helper key ((k,v):kvs)
            | key == k = Just v
            | otherwise = helper key kvs 

mergeD :: (Ord k) => Dict k v -> Dict k v -> Dict k v
mergeD (Dict kvs1) (Dict kvs2) = Dict (helper kvs1 kvs2)
    where 
        helper :: (Ord k) => [(k,v)] -> [(k,v)] -> [(k,v)]
        helper kvs [] = kvs
        helper [] kvs= kvs
        helper l1@(kvp1@(k1,_) : kvs1) l2@(kvp2@(k2,_):kvs2) = 
            case cmp k1 k2 of
                Equal -> kvp2 : helper kvs1 kvs2
                Lower -> kvp1 : helper kvs1 l2
                Greater -> kvp2 : helper l1 kvs2 

dict1 :: Dict Int String
dict1 =
  fromListDict
    [ (3, "aa"),
      (6, "xyz"),
      (1, "st"),
      (7, "rrs"),
      (0, "fg"),
      (0, "ff")
    ]

dict2 :: Dict Int String
dict2 =
  fromListDict
    [ (4, "aba"),
      (2, "xqz"),
      (1, "sj"),
      (9, "rrs"),
      (0, "fw"),
      (3, "rr")
    ]

