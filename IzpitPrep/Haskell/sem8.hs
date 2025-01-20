myAbs :: (Ord a, Num a) => a -> a
myAbs n = if n < 0 then (-n) else n

isTriangle :: (Ord a, Num a) => a -> a -> a -> Bool
isTriangle a b c = (a + b > c) && (b + c > a) && (a + c > b)

isPrime :: Integral t => t -> Bool
isPrime n 
    |n < 2     = False
    |otherwise = isPrimeHelper 2 n

isPrimeHelper :: Integral t => t -> t -> Bool
isPrimeHelper i n 
    |i == n          = True
    |n `mod` i == 0  = False
    |otherwise       = isPrimeHelper (i + 1) n

sumDivisors :: Integral t => t -> t
sumDivisors n = sumHelper 0 2 n

sumHelper :: Integral t => t -> t -> t -> t
sumHelper sum i n 
    |i == n                       = sum
    |isPrime i && n `mod` i == 0  = sumHelper (sum + i) (i + 1) n
    |otherwise                    = sumHelper sum (i + 1) n

isPerfect :: Integral a => a -> Bool
isPerfect n = n == sumDivisors n

countBinaryDigits :: Int -> Int
countBinaryDigits n 
    |n < 0              = countBinaryDigits (-n)
    |n == 0             = 1
    |otherwise          = countBinaryDigits (n `div` 2)

countOnes :: (Num t, Integral a) => a -> t
countOnes n
    |n < 0             = countOnes (-n)
    |n == 0            = 0
    |n `mod` 2 == 0    = 1 + countOnes(n `div` 2)
    |otherwise         = countOnes(n `div` 2)

isEvil :: Int -> Bool
isEvil n = countOnes n `mod` 2 == 0

sumEvil :: Int -> Int -> Int
sumEvil a b 
    |a > b     = 0
    |isEvil a  = a + sumEvil (a + 1) b
    |otherwise = sumEvil (a + 1) b
    
compose :: (a -> b) -> (c -> a) -> c -> b
compose f g x = f (g x)