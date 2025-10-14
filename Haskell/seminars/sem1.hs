module Solutions where

import Prelude hiding (exp, pred, succ)

trianglePerimeter ::  Double -> Double -> Double -> Double
trianglePerimeter a b c = a + b + c

triangleArea :: Double -> Double -> Double -> Double
triangleArea a b c = sqrt(p*(p - a) * (p - b) * (p - c))
    where p = (a + b + c)/2

type Point = (Double, Double)

sideLength :: Point -> Point -> Double
sideLength (x1, y1) (x2, y2) = sqrt(xd ^ 2 + yd ^ 2)
    where xd = x1 - x2
          yd = y1 - y2

trianglePerimeter' :: Point ->  Point -> Point -> Double
trianglePerimeter' p1 p2 p3 = trianglePerimeter side1 side2 side3
    where 
        side1 = sideLength p1 p2
        side2 = sideLength p1 p3
        side3 = sideLength p2 p3

triangleArea' :: Point -> Point -> Point -> Double
triangleArea' p1 p2 p3 = triangleArea side1 side2 side3
    where
        side1 = sideLength p1 p2
        side2 = sideLength p1 p3
        side3 = sideLength p2 p3

printStudent :: (String, String, String, Int) -> String
printStudent (name, fn, spec, kurs) = "This is " ++ name ++ "  with a faculty number of " ++ fn ++ " who is in year " ++ show kurs ++ " of " ++ spec

succ :: Int -> Int
succ x = x + 1

pred :: Int -> Int
pred 0 = 0
pred x = helper 0 x
    where 
        helper :: Int -> Int -> Int
        helper acc x
            | x <= 0 = 0
            | succ acc == x = acc 
            | otherwise = helper (succ acc) x

add :: Int -> Int -> Int
add 0 b = b
add a b = succ (add (pred a) b)

mult :: Int -> Int -> Int
mult 0 b = 0
mult a b = add b (mult (pred a) b) 

exp :: Int -> Int -> Int
exp a 0 = 1
exp a b = mult a (exp a (pred b))

digitSum :: Int -> Int
digitSum 0 = 0
digitSum a = helper a 0
    where 
        helper :: Int -> Int -> Int
        helper x acc
            | x < 10 = acc + x
            | otherwise = helper (x `quot` 10) (acc + x `mod` 10)

intervalSum :: Int-> Int -> Int
intervalSum m n 
    | m > n = 0
    | otherwise = m + intervalSum (m + 1) n

fact :: Int -> Int
fact 0 = 1
fact x = x * fact(x - 1)

doubleFatc :: Int -> Int
doubleFatc 0 = 1
doubleFatc 1 = 1
doubleFatc x = x * doubleFatc(x - 2)

gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b) 

lcm' :: Int -> Int -> Int
lcm' a b = a * b `div` (gcd' a b)

coprime :: Int -> Int -> Bool
coprime a b = gcd' a b == 1

totient :: Int -> Int
totient a = helper 1 a
    where 
        helper :: Int -> Int -> Int
        helper st n
            | st == n = 0
            | otherwise = if coprime st a then 1 + helper (st + 1) n else helper (st + 1) n 

prime :: Int -> Bool
prime x 
    | x <=2 = True
    | otherwise = helper x 2
        where 
            helper :: Int -> Int -> Bool
            helper x acc
                | x == acc = True
                | x `mod` acc == 0 = False
                | otherwise = helper x (succ acc)

goldbach :: Int -> (Int, Int)
goldbach = helper 2
    where 
        helper :: Int -> Int -> (Int, Int)
        helper acc n
            | acc >= n = error "oops"
            | prime acc && prime (n - acc) = (acc, n - acc)
            | otherwise = helper (succ acc) n