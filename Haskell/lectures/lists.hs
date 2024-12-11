import Prelude hiding (head, tail, null, length, enumFromTo, (++), reverse, (!!), elem,
                        init, last, take, drop, map, filter, foldr, foldl, foldr1, foldl1,
                        scanr,scanl, zip, zipWith, unzip, takeWhile, dropWhile,
                        any,all)
head :: [a] -> a
head (x:xs) = x

tail :: [a] -> [a]
tail (x:xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: [a] -> Integer
length [] = 0
length (x:xs) = 1 + (length xs)

length2 :: [a] -> Int
length2 l  
    |null l    = 0
    |otherwise = 1 + length2 (tail l)

length3 :: [a] -> Integer
length3 l = case l of
                []     -> 0
                (_:xs) -> 1 + length3 xs 


enumFromTo :: (Eq t, Enum t) => t -> t -> [t]
enumFromTo from to
    |from == to = [from]
    |otherwise  = from : enumFromTo (succ from) to

(++) :: [a] -> [a] -> [a]
(++) [] l2 = l2
(++) l1 [] = l1
(++) (x:xs) l2 = x:xs ++ l2

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(!!) :: [a] -> Int -> a
[] !! _ = error "Index too large"
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n - 1)

elem :: Eq t => t -> [t] -> Bool
elem _ [] = False
elem n (x:xs)
    |n == x    = True
    |otherwise = elem n xs

elem2 x l = not (null l) && (head l == x || elem x (tail l)) 

pythagorTriples :: Integral c => c -> c -> [(c, c, c)]
pythagorTriples from to= 
    [ (x , y, z) | x <- [from..to], 
                   y <- [x..to],
                   z <- [from..to],
                   x^2 + y^2 == z^2,
                   gcd x y == 1]

init :: [a] -> [a]
init [] = error "empty list"
init [_] = []
init (x:xs) = x : init xs 

last :: [a] -> a
last [] = error "empty list"
last [x] = x
last (x:xs) = last xs

take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take p (x:xs) = x : take (p-1) xs

drop :: (Eq t, Num t) => t -> [a] -> [a]
drop _ [] = []
drop 0 l = l
drop p (x:xs) = drop (p-1) xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs) 
        |f x       = x : filter f xs
        |otherwise = filter f xs

filter2 f l = [x | x <- l, f x]

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ nv [] = nv
foldr op nv (x: xs) = x `op` foldr op nv xs

map2 f  = foldr (\x -> (f x:)) [] 

filter3 f = foldr (\x -> if f x then (x:) else id) []

foldl _ nv [] = nv
foldl op nv (x:xs) = foldl op (nv `op` x) xs

length4 :: [a] -> Integer
length4  = foldr (const (+1)) 0 

--(++) :: [a] -> [a] -> [a]
--l1 ++ l2 = foldr (:) l2 l1

reverse2 :: [a] -> [a]
reverse2  = foldl (flip (:)) [] 

snoc :: a -> [a] -> [a]
snoc x r = r ++ [x]

rcons :: [a] -> a -> [a]
rcons xs x = x:xs

foldr1 :: (t -> t -> t) -> [t] -> t
foldr1 _ []      = error "Empty list"
foldr1 _ [x]     = x
foldr1 op (x:xs) = x `op` foldr1 op xs 

foldl1 :: (t -> t -> t) -> [t] -> t
foldl1 op (x:xs) = foldl op x xs 

scanr :: (t -> a -> a) -> a -> [t] -> [a]
scanr _ nv []      = [nv]
scanr op nv (x:xs) =  (x `op` r) : rest
    where rest@(r:_) = scanr op nv xs

scanr2 :: (t1 -> t2 -> t2) -> t2 -> [t1] -> [t2]
scanr2 op nv = foldr (\ x rest@(r:_) -> (x `op` r) : rest) [nv]

scanl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> [t1]
scanl op nv []     = [nv]
scanl op nv (x:xs) = nv : scanl op (nv `op` x) xs 

zipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith op [] _ = []
zipWith op _ [] = []
zipWith op (x:xs) (y:ys) = x `op` y : zipWith op xs ys

zip :: [a] -> [b] -> [(a,b)]
zip = zipWith (,)

unzip :: [(a1, a2)] -> ([a1], [a2])
unzip [] = ([],[])
unzip ((x,y):xys) = (x:xs, y:ys)
    where (xs,ys) = unzip xys

unzip2 :: [(a1, b)] -> ([a2], [a3])
unzip2 = foldr (\(x,y) (xs,ys) -> (xs,ys)) ([],[])

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p  = foldr (\x -> if p x then (x:) else const []) [] 

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p l@(x:xs) 
    |p x       = dropWhile p xs
    |otherwise = l
    
any :: (a -> Bool) -> [a] -> Bool
any p l = or (map p l)

all :: (a -> Bool) -> [a] -> Bool
all p l = and (map p l)

sorted :: Ord a => [a] -> Bool
sorted l = all (\(x,y) -> x <= y) (zip l (tail l))

