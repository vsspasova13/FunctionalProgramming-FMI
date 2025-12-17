module Solutions where

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = 
    map (x :) (subsets xs) ++ subsets xs

natPairs :: [(Int, Int)]
natPairs = [(x,n-x) | n <- [0..], x <- [0..n]]

intPairs :: [(Int, Int)]
intPairs = [(x,y) | n <-[0..], x <- [-n..n], y <- [-n..n], abs x + abs y == n]

powersOf2 :: [Int]
powersOf2 = [2 ^ x | x <- [0..]]

powersOf2' :: [Int]
powersOf2' = map (2^) [0..]

powersOf2'' :: [Int]
powersOf2'' = 1 : map (*2) powersOf2''

fibs :: [Int]
fibs = 0:1: zipWith (+) fibs (tail fibs)

fact :: Int -> Int
fact n = product [1..n]

facts :: [Int]
facts = [fact n | n <- [0..]]

facts' :: [Int]
facts' = map fact [0..]

facts'' :: [Int]
facts'' = 1 : zipWith (*) facts'' [2..]

pitagorTriple :: [(Int, Int, Int)]
pitagorTriple = [(a,b,c) | c <- [5..], a <- [1..c], b <- [a..c],  a^2 + b^2 == c^2]

isPrime :: Int -> Bool
isPrime n = length (filter (\x -> n `mod` x == 0) [2..n-1]) == 0

isPrime' :: Int -> Bool
isPrime' n = all (((/=) 0) . (n `mod`)) [2..n-1]

primes :: [Int]
primes  = filter isPrime [2..]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\n -> n `mod` x /= 0) xs)

primes' :: [Int]
primes' = sieve [2..]

triangleNums :: [Int]
triangleNums = scanl (+) 0 [1..]