module Sem2 where

import Prelude hiding(reverse, take, drop)

reverse :: Int -> Int
reverse x = helper x 0
    where
        helper :: Int -> Int -> Int
        helper x acc
            | x < 10 = acc*10 + x
            | otherwise = helper (x `div` 10) (acc * 10 + x `mod` 10)

palindrome :: Int -> Bool
palindrome x = x == reverse x
    
sumOfNthPowers :: Int -> Int -> Int    
sumOfNthPowers  = helper 0
    where 
        helper :: Int -> Int -> Int ->  Int
        helper acc x n
            | acc > x = 0
            | x == 0 = 1
            | x < 0 = 0
            | otherwise = helper (acc + 1) (x - acc ^ n) n + helper (acc + 1) x n

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

append :: [Int] -> [Int] -> [Int]
append [] ys = ys
append xs [] = xs
append (x:xs) ys = x : append xs ys 

take :: Int -> [Int] -> [Int]
take 0 _ = []
take n (x:xs) = x : take (n - 1) xs

drop :: Int -> [Int] -> [Int]
drop 0 xs = xs
drop n (x:xs) = drop (n-1) xs

reverseStr :: String -> String
reverseStr [] = []
reverseStr ys = helper ys [] 
    where
        helper :: [Char] -> [Char] -> [Char]
        helper [] acc = acc
        helper (x:xs) acc = helper xs (x : acc)

split :: Char -> [Char] -> [String]
split _ [] = []
split ch (x:xs) = helper (x:xs) [] []
    where 
        helper :: [Char] -> [Char] -> [String] -> [String]
        helper [] acc res = res
        helper (x:xs) acc res
            | x == ch   = 
                if null acc then helper xs [] res else helper xs [] (res ++ [reverseStr acc])
            | otherwise = helper xs (x:acc) res


splitEvenOdd :: [Int] -> ([Int], [Int])
splitEvenOdd [] = ([],[])
splitEvenOdd l = helper 0 [] [] l
    where 
        helper :: Int -> [Int] -> [Int] -> [Int] -> ([Int], [Int])
        helper i res1 res2 [] = (res1,res2)
        helper i res1 res2 (x:xs)
            | i `mod` 2 == 0 =  helper (succ i) (res1 ++ [x]) res2 xs
            | otherwise =  helper (succ i) res1 (res2 ++ [x]) xs


suffixes :: [Int] -> [[Int]]
suffixes [] = []
suffixes l = helper (length l) [] 
    where 
        helper :: Int -> [[Int]]  -> [[Int]]
        helper i res 
            |i < 0 = res 
            |otherwise = helper (pred i) [drop i l] ++ res

prefixes :: [Int] -> [[Int]]
prefixes [] = []
prefixes l = helper 0 [] 
    where 
        helper :: Int -> [[Int]]  -> [[Int]]
        helper i res 
            |i > length l = res 
            |otherwise = helper (succ i) [take i l] ++ res

removeConsecutive :: [Int] -> [Int]
removeConsecutive [] = []
removeConsecutive [x] = [x]
removeConsecutive (x:(xx:xs)) = helper x xx xs []
    where 
        helper :: Int -> Int -> [Int] -> [Int] -> [Int]
        helper curr next [] res = if curr == next then res++[curr] else res++[curr]++[next]
        helper curr next (x:xs) res
            | curr == next = helper next x xs res
            | otherwise = helper next x xs  (res ++ [curr])

 -- to do 10,11