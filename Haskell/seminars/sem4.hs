module Solutions4 where

import Prelude hiding (zip, length)
import Distribution.Simple.Command (OptDescr(BoolOpt))

zip :: [a] -> [b] -> [(a,b)]
zip [] [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + (length xs)

longestList :: [[a]] -> [a]
longestList [] = []
longestList (x:xs) = helper (x:xs) 0 []
    where 
        helper :: [[a]] -> Int -> [a] -> [a]
        helper [] _ res = res
        helper (x:xs) max res
            |length x > max = helper xs (length x) x
            |otherwise = helper xs max res

removeFirst :: Eq a => [a] -> a -> [a]
removeFirst [] _ = []
removeFirst (x:xs) el 
    | x == el = xs
    | otherwise = x : removeFirst xs el


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
selectionSort xs = minElem : selectionSort (removeFirst xs minElem)
    where
        minElem = minEl xs

pairWith :: a -> [b] -> [(a,b)]
pairWith _ [] = []
pairWith x (y:ys) = (x,y) : pairWith x ys

decartPr :: [a] -> [b] -> [(a,b)]
decartPr [] _ = []
decartPr xs ys = helper xs ys 
    where 
        helper :: [a1] -> [b1] -> [(a1,b1)]
        helper [] _ = []
        helper xs ys = pairWith (head xs) ys ++ helper (tail xs) ys
       
howMany :: Eq a => a -> [a] -> Int
howMany _ [] = 0
howMany x (y:ys)
    | x == y = 1 + howMany x ys
    | otherwise = howMany x ys

histogram :: Eq a => [a] -> [(Int, a)]
histogram [] = []
histogram xs = helper xs []
    where 
        helper :: Eq a => [a] -> [a] -> [(Int, a)]
        helper [] res = []
        helper xs res
            | head xs `elem` res = helper (tail xs) res
            | otherwise = (howMany (head xs) xs, (head xs)) : helper (tail xs) ((head xs) : res)

toBinary :: Int -> [Int]
toBinary n 
    | n < 2 = [n]
    | otherwise = toBinary (n `div` 2) ++ [n `mod` 2]

intListToString :: [Int] -> String
intListToString [] = ""
intListToString (x:xs) = show x ++ intListToString xs

generate :: Int -> [String]
generate n = intListToString (toBinary n) : generate (n + 1)

binary :: [String]
binary = generate 1

newtype Matrix = Matrix [[Int]]
    deriving (Show)

emptyMatrix :: Matrix
emptyMatrix = Matrix []

isMatrix :: Matrix -> Bool
isMatrix (Matrix (m:ms)) = helper ms
    where 
        lenM = length m
        helper :: [[Int]] -> Bool
        helper [] = True
        helper (x:xs) 
            |length x /= lenM = False
            |otherwise = helper xs

addRow :: [Int] -> [Int] -> [Int]
addRow [] _ = []
addRow _ [] = []
addRow (x:xs) (y:ys) = (x+y) : addRow xs ys

addMatrices :: Matrix -> Matrix -> Matrix
addMatrices (Matrix ms1) (Matrix ms2) = Matrix (helper ms1 ms2)
    where
        helper ::  [[Int]] -> [[Int]] -> [[Int]]
        helper (m:ms) (n:ns) = addRow m n : helper ms ns

transposeMatrix' :: [[Int]] -> [[Int]]
transposeMatrix' ([]:_) = []
transposeMatrix' xss = firstColumn xss : transposeMatrix' (restColumns xss)
    where
        firstColumn :: [[a]] -> [a]
        firstColumn [] = []
        firstColumn (x:xs) = head x : firstColumn xs

        restColumns :: [[a]] -> [[a]]
        restColumns [] = []
        restColumns (x:xs) = tail x : restColumns xs

dotPr :: [Int] -> [Int] -> Int
dorPr [] _ = 0
dotPr _ [] = 0
dotPr (x:xs) (y:ys) = (x * y) + dotPr xs ys

multMatrices :: Matrix -> Matrix -> Matrix
multMatrices (Matrix ms1) (Matrix ms2) = Matrix (helper ms1 (transposeMatrix' ms2))
     where
        helper ::  [[Int]] -> [[Int]] -> [[Int]]
        helper [] _ = []
        helper (x:xs) ms2 = multiplyByRow x ms2 : helper xs ms2

        multiplyByRow :: [Int] -> [[Int]] -> [Int]
        multiplyByRow _ [] = []
        multiplyByRow row (x:xs) = dotPr row x : multiplyByRow row xs

transpose :: Matrix -> Matrix
transpose (Matrix m) = Matrix ( transposeMatrix' m)

m1 = Matrix [[1,2,3],
             [4,5,6]]

m2 = Matrix [[7,8],
             [9,10],
             [11,12]]