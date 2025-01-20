import Prelude hiding (length, foldr, foldl, reverse, init, product, zip, zipWith)

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

exists :: (a -> Bool) -> [a] -> Bool
exists _ [] = False
exists p (x:xs) 
    |p x       = True
    |otherwise = exists p xs

forall :: (a -> Bool) -> [a] -> Bool
forall _ [] = True
forall p (x:xs)
    |not (p x)       = False
    |otherwise       = forall p xs


member :: Eq t => t -> [t] -> Bool
member _ [] = False
member k (x:xs)
    |k == x          = True
    |otherwise       = member k xs

push :: a -> [a] -> [a]
push x [] = [x]
push k (x:xs) = x : push k xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

init :: [a] -> [a]
init [x] = []
init (x:xs) = x : init xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' op init [] = init
foldr' op init (x:xs) = op x (foldr' op init xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' op init [] = init
foldl' op init (x:xs) = foldl' op (op init x) xs

product :: [Integer] -> Integer
product = foldr' (*) 1

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

zipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith op (x:xs) (y:ys) = (x `op` y) : zipWith op xs ys

interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave (x:xs) (y:ys) = [x, y] ++ interleave xs ys

nats :: [Integer]
nats = [1..]

pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples = [(x, y, z)| z <- [1..],
                                 x <- [1..z-1],
                                 y <- [x+1..z-1],
                                 x^2 + y^2 == z^2]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes2 :: [Integer]
primes2 = sieve [2..]
    where 
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]