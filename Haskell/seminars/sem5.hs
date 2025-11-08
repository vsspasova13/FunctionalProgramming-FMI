module Solutions where

import Prelude hiding(compose, flip, map, filter, foldr, foldl, all, 
                    any, concatMap, zipWith, curry, uncurry, iterate, nub, unfoldr)
import Data.List hiding (nub, compose, flip, map, filter, foldr, foldl, all, 
                    any, concatMap, zipWith, curry, uncurry, iterate, nub, unfoldr)
import Data.Maybe (isNothing)

compose :: (a -> b) -> (c -> a) -> c -> b
compose f g x = f $ g x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

map ::  (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs)
    | f x = x : filter f xs
    | otherwise = filter f xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ nv [] = nv
foldr op nv (x:xs) =  op x (foldr op nv xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ nv [] = nv
foldl op nv (x:xs) = foldl op (op nv x) xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then (:) x xs else xs) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs)
    | not (p x) = False
    | otherwise = all p xs

any :: (a -> Bool) -> [a] -> Bool
any p [] = False
any p (x:xs) 
    | p x = True
    | otherwise = any p xs

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f [] = []
concatMap f (x:xs) = (f x)++ (concatMap f xs)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] [] = []
zipWith op (x:xs) (y:ys) = op x y : zipWith op xs ys

curry ::  ((a,b) -> c) -> a -> b -> c
curry f a b = f (a,b)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) = f a b

iterate :: (a -> a) -> a -> [a]
iterate f x = f x : iterate f (f x)

nub :: Eq a => [a] -> [a]
nub [] = []
nub xs = reverse (helper [] xs)
    where 
        helper :: Eq a => [a] -> [a] -> [a] 
        helper seen []  = seen
        helper seen (x:xs) 
            | x `elem` seen = helper seen xs 
            | otherwise     = helper (x : seen) xs

nub' :: Eq a => [a] -> [a]
nub' xs = foldr f [] xs
    where 
        f x acc = if x `elem` acc then acc else (x:acc)

unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
unfoldr f b 
    | isNothing (f b) = []
    | otherwise = x : unfoldr f rest 
    where 
        Just(x, rest) = f b

(&&&) :: (a -> b) -> (a -> c) -> a -> (b,c)
(&&&) f g x = (f x, g x)

fixedPoints :: (Eq a) => (a -> a) -> [a] -> [a]
fixedPoints f [] = []
fixedPoints f xs = filter (\x -> f x == x) xs

compose1' :: [a->a] -> (a->a)
compose1' [] = id
compose1' (f:fs) = f . compose1' fs

compose2' :: [a->a] -> (a->a)
compose2' = foldr (.) id

minEl :: Ord a => [a] -> a
minEl (x:xs) = helper x (x:xs) x
    where 
        helper :: Ord a => a -> [a] -> a -> a
        helper curr [] min = if curr < min then curr else min
        helper curr (r:rest) min 
            | curr < min = helper r rest curr
            | otherwise  = helper r rest min

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort (x:xs) = 
    let 
        minElem = minEl (x:xs)
        rest = filter (/= minElem) (x:xs)
    in minElem : selectionSort rest

quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort p [] = []
quickSort p [x] = [x]
quickSort p xs = small ++ [pivot] ++ large
    where 
        rest = tail xs
        pivot = head xs
        small = quickSort p (filter (\x -> p x pivot) rest)
        large = quickSort p (filter (\x -> not (p x pivot)) rest)
