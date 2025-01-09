import Prelude hiding (length, foldr, foldl, reverse, init, product, zip, zipWith, forall, isPrime)

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

exists :: (a->Bool) -> [a] -> Bool
exists p [] = False
exists p (x:xs)
    |p x = True
    |otherwise = exists p xs

forAll :: (a->Bool) -> [a] -> Bool
forAll p [] = True
forAll p (x:xs)
    |not(p x) = False
    |otherwise = forAll p xs


member :: Eq t => t -> [t] -> Bool
member _ [] = False
member a (x:xs)
    |x == a = True
    |otherwise = member a xs

push :: a -> [a] -> [a]
push a [] = [a]
push a (x:xs) = x : push a xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

init :: [a] -> [a]
init [x] = []
init (x:xs) = x : init xs

insert :: a -> Int -> [a] -> [a]
insert a 0 l = a:l
insert a _ [] = [a]
insert a n (x:xs) = x : insert a (n-1) xs

foldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr op nv [] = nv
foldr op nv (x:xs) = x `op` foldr op nv xs

foldl :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldl op nv [] = nv
foldl op nv (x:xs) = foldl op (x `op` nv) xs

product :: Num a => [a] -> a
product = foldr (*) 1 

zip :: [a] -> [b] -> [(a,b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

zipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith op _ [] = []
zipWith op [] _ = []
zipWith op (x:xs) (y:ys) = (x `op` y) : zipWith op xs ys

interleave :: [a] -> [a] -> [a]
interleave l1 [] = l1
interleave [] l2 = l2
interleave (x:xs) (y:ys) = x : y : interleave xs ys

nats :: [Integer]
nats = 1:map (+1) nats

pytagorTriples :: [(Integer, Integer, Integer)]
pytagorTriples = [(x, y, z)|   z <- [1..],
                               y <- [1..z],
                               x <- [1..y],
                               x^2 + y^2 == z^2,
                               gcd x y == 1]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isPrimeWrapper :: Integral a => a -> a -> Bool
isPrimeWrapper x a
    |a * a > x       = True 
    |x `mod` a == 0 = False
    |otherwise      = isPrimeWrapper x (a + 1)

isPrime :: Integral a => a -> Bool
isPrime x
    | x < 2     = False 
    | otherwise = isPrimeWrapper x 2

primes = filter isPrime [1..]

sieve :: Integral a => [a] -> [a]
sieve [] = []
sieve (y:xs) = y : sieve [x| x <- xs, x `mod` y /= 0]  

primes2 :: [Integer]
primes2 = sieve [2..]



