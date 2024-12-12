import Prelude

isPrimeWrapp :: Integral t => t -> t -> Bool
isPrimeWrapp n i
    |i * i > n     = True
    |n `mod` i == 0 = False
    |otherwise      = isPrimeWrapp n (i+1)

isPrime :: Integral t => t -> Bool
isPrime n 
    |n < 2     = False
    |otherwise = isPrimeWrapp n 2

length2 :: Integer -> Integer
length2 0 = 0
length2 n = 1 + length2 (n `div` 10)

getLastDigit :: Integral a => a -> a
getLastDigit n = n `mod` 10

areAllDigitsOnPrimeIndPrime :: (Integral t1) =>  t1 -> t1 -> Bool
areAllDigitsOnPrimeIndPrime n ind 
    |n == 0                                         = True
    |isPrime ind && isPrime (getLastDigit n)        = areAllDigitsOnPrimeIndPrime (n `div` 10) (ind + 1) 
    |isPrime ind && not (isPrime (getLastDigit n))  = False
    |otherwise                                      = areAllDigitsOnPrimeIndPrime (n `div` 10) (ind + 1) 


--1
megaPrime :: Integer -> Bool
megaPrime n
    |isPrime n  = areAllDigitsOnPrimeIndPrime n 1 
    |otherwise  = False

allMegaPrimeNumbers :: Integer -> Integer -> [Integer]
allMegaPrimeNumbers from to = filter megaPrime [from..to]



--2

type Node = Int
type Graph = [(Node, [Node])]

{-
pathsFrom :: Graph -> Int -> Node -> [[Node]]
pathsFrom (v) 0 node = [[]]
pathsFrom (v:ch) k node
    |v == node  = filter (\m -> length m == k) (pathsFromWrap (v ch) node [])
    |otherwise = undefined

pathsFromWrap :: Graph ->  Node -> [[Node]]  -> [[Node]]
pathsFromWrap (v:ch) node acc = dfs [] v ac
    where dfs seen node ac 
           |node `elem` seen  = ac
           |otherwise         = foldl (\n -> acc : n) (node : seen) ([t | t <- ch, pathsFromWrap v ch])
        
-}

