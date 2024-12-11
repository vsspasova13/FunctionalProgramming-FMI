
myAbs :: Int -> Int
myAbs x = if x < 0 then (x - 2*x) else x

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = (a + b > c) && (a + c > b) && (b + c > a)

isLeapY :: Int -> Bool
isLeapY y = ((( y `mod` 4) == 0) && (not ((y `mod` 100) == 0))) || ((y `mod` 400) == 0) 

countDays :: Int -> Int -> Int -> Int
countDays d m y = sum (take (m-1) daysInMonth) + d
    where
    daysInMonth = [31, if isLeapY y then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

isDividable :: Int -> Int -> Bool
isDividable x p
    |p * p > x = False
    |x `mod` p == 0 = True 
    |otherwise = isDividable x (p + 1)

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    |otherwise = not (isDividable n 2)

dividesN :: Int -> Int -> Int
dividesN n x
    |x >= n = 0
    |n `mod` x == 0 = x + (dividesN n (x+1))
    |otherwise = (dividesN n (x+1))

sumDivisors :: Int -> Int
sumDivisors n
    |isPrime n = 1
    |otherwise = dividesN n 1

isPerfect :: Int -> Bool
isPerfect n = (sumDivisors n == n)

countBinaryDigits :: Int -> Int
countBinaryDigits 1 = 1
countBinaryDigits x = 1 + (countBinaryDigits (div x 2))

countOneBits :: Int -> Int
countOneBits 0 = 0
countOneBits x = (x `mod` 2) + countOneBits (x `div` 2)

isEvil :: Int -> Bool
isEvil x = (countOneBits x) `mod` 2 == 0

sumEvil :: Int -> Int -> Int
sumEvil a b 
    |(a > b)        = 0
    |(isEvil a)     = a + (sumEvil (a+1) b)
    |not(isEvil a)  = (sumEvil (a+1) b)

compose :: (c -> a) -> (b -> c) -> b -> a
compose f g x = f (g x)





