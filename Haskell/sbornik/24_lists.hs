module Solutions24 where

--24.1

firstNeven :: Int -> [Int]
firstNeven n = helper 2 n
    where 
        helper :: Int -> Int -> [Int]
        helper i 0 = []
        helper i n = i : helper (i + 2) (n - 1)

aritmeticPrN :: Int -> Int -> Int -> [Int]
aritmeticPrN 0 _ _ = []
aritmeticPrN n a d = a : aritmeticPrN (n - 1) (a + d) d

fact :: Int -> Int
fact 1 = 1
fact n = n * fact(n - 1)

firstNfact :: Int -> [Int]
firstNfact n = helper 1 n
    where 
        helper :: Int -> Int -> [Int]
        helper i n 
            |i > n = []
            | otherwise = fact i : helper (i+1) n

even :: [Int]
even = helper 2
    where 
        helper :: Int -> [Int]
        helper i = i : helper(i+2)

aritmeticPr :: Int -> Int -> [Int]
aritmeticPr a d = a : aritmeticPr (a + d) d

allFact :: [Int]
allFact =  helper 1 
    where 
        helper :: Int -> [Int]
        helper i = fact i : helper (i+1)

--24.2

lengthN :: Int -> Int
lengthN n 
    | n < 10 = 1
    | otherwise = 1 + lengthN (n `div` 10)

digitList :: Int -> [Int]
digitList n = helper 1 n
    where 
        leng = lengthN n
        helper :: Int -> Int -> [Int]
        helper i n 
            | i > leng = []
            | otherwise = 
                let pow = 10 ^ (leng - i)
                in ((n `div` pow) `mod` 10) : helper (i+1) n

--24.3

digitsUniqueList :: Int -> [Int]
digitsUniqueList  n = helper 1 n []
    where 
        leng = lengthN n
        helper :: Int -> Int -> [Int] -> [Int]
        helper i n res
            | i > leng = res
            | otherwise = 
                let pow = 10 ^ (leng - i)
                    digit = (n `div` pow) `mod` 10
                in if digit `elem` res then helper (i+1) n res 
                                       else helper (i+1) n (res ++ [digit])
