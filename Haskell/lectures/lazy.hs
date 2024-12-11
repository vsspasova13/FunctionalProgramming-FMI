import Prelude hiding (enumFrom, repeat, cycle, iterate)

sumFirst :: Num a => [a] -> [a] -> a
sumFirst (x:_) (y:_) = x + y

enumFrom :: Enum t => t -> [t]
enumFrom from = from : enumFrom (succ from)

repeat :: t -> [t]
repeat x = x : repeat x

ones :: [Integer]
ones = repeat 1

twos :: [Integer]
twos = [2,2..]

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

cycle :: [a] -> [a]
cycle l = l ++ cycle l

iterate :: (t -> t) -> t -> [t]
iterate f n = n : iterate f (f n)

oddSquares :: [Integer]
oddSquares = [x^2 | x <- [1,3..]]

pairs :: [(Integer, Integer)]
pairs = [(x,y) | n <- [0..], x <- [0..n], y <- [0..n], x + y == n]

pairs2 :: [(Integer, Integer)]
pairs2 = [(x, n - x) | n <- [0..], x <- [0..n]]

powers2 :: [Integer]
powers2 = 1: map (*2) powers2

notdivk :: Integral a => a -> [a]
notdivk k = filter (\x -> x `mod` k /= 0) [1..]

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

triplets :: [[Integer]]
triplets = iterate (map (+3)) [3,2,1]


