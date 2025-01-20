import Prelude hiding (length, foldr, foldl, reverse, init, 
                       product, zip, zipWith, last, take, drop, map, 
                       filter, foldr1, foldl1, scanr, scanl, (++), elem, (!!), unzip,
                       takeWhile, dropWhile, span, break, any, all)

--lists comprehension

evens :: [Integer]
evens = [2 * x| x <- [1..]]

nums1 :: [Integer]
nums1 = [x^2 | x <- [1..10], odd x]

nums2 :: [(Integer, Integer)]
nums2 = [(x, y)| x <- [1..5], y <- [6..10]]

strings :: [[Char]]
strings = [ x ++ (' ':y) | x <- ["green", "blue"], y <- ["grass", "sky"]]

pytagorTriples :: Integral c => c -> c -> [(c, c, c)]
pytagorTriples a b = [(x, y, z) | z <- [a..b], x <- [a..z], y <- [x+1..z], z^2 == x^2 + y^2, gcd x y == 1]

--other functions

init2 :: [a] -> [a]
init2 [x] = []
init2 (x:xs) = x : init2 xs

last :: [a] -> a
last [x] = x
last (x:xs) = last xs

take :: (Eq t, Num t) => t -> [a] -> [a]
take 0 xs = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

drop :: (Eq t, Num t) => t -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) = drop (n - 1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

map :: (t -> a) -> [t] -> [a]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

foldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr _ nv [] = nv
foldr op nv (x:xs) = x `op` foldr op nv xs

foldl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldl _ nv [] = nv
foldl op nv (x:xs) = foldl op (nv `op` x) xs

foldr1 :: (t -> t -> t) -> [t] -> t
foldr1 op [x] = x
foldr1 op (x:xs) = x `op` foldr1 op xs

foldl1 :: (t2 -> t2 -> t2) -> [t2] -> t2
foldl1 op (x:xs) = foldl op x xs

map2 :: (t -> a) -> [t] -> [a]
map2 f = foldr (\x r -> f x : r) [] 

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p = foldr (\x -> if p x then (x:) else id) []

length :: [a] -> Integer
length = foldr (const (1+)) 0 

(++) :: [t1] -> [t1] -> [t1]
(++) xs ys = foldr (:) ys xs

reverse :: [t2] -> [t2]
reverse = foldl (flip (:)) [] 

elem :: Eq a => a -> [a] -> Bool
elem y = foldr (\x -> (||) (y == x)) False

last2 :: [t] -> t
last2 = foldr1 (const id)

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ nv [] = [nv]
scanr op nv (x:xs) =  x `op` r : rest
 where rest@(r:_) = scanr op nv xs

scanl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> [t1]
scanl _ nv [] = [nv]
scanl op nv (x:xs) = nv : scanl op (nv `op` x) xs

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys 

(!!) :: [a] -> Integer -> a
l !! i = head [x | (x,j) <- zip l [0..length l - 1], i == j]

unzip :: [(a,b)] -> ([a], [b])
unzip [] = ([],[])
unzip ((x,y):rest) = (x:xs, y:ys)
    where (xs, ys) = unzip rest

unzip2 :: [(a, b)] -> ([a], [b])
unzip2 = foldr (\(x,y) (xs,ys) -> (x:xs, y:ys)) ([],[])

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x: xs) (y: ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

takeWhile :: (a -> Bool) -> [a] ->[a]
takeWhile p  = foldr (\x r -> if p x then x : r else []) [] 

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 _ [] = []
dropWhile2 p (x:xs) = if p x then dropWhile2 p xs else xs

span :: (a -> Bool) -> [a] -> ([a], [a])
span p l = (takeWhile p l, dropWhile2 p l)

break :: (a -> Bool) -> [a] -> ([a], [a])
break p l = (takeWhile q l, dropWhile2 q l)
    where q x = not (p x)

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (\x -> (p x ||)) False

any2 :: (t -> Bool) -> [t] -> Bool
any2 p l = or (map p l)

all :: (t -> Bool) -> [t] -> Bool
all p l = and (map p l)

sorted :: Ord a => [a] -> Bool
sorted l = all (\(x,y) -> x <= y) (zip l (tail l))


