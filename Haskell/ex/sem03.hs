import Data.Char
import Data.List
import Prelude hiding (comparing)

whisper :: [Char] -> [Char]
whisper = map toLower

removeSpaces :: [Char] -> [Char]
removeSpaces = filter (not . isSpace)

switchCaps :: [Char] -> [Char]
switchCaps = map (\x -> if isLower x then toUpper x else toLower x) 

encrypt :: Int -> [Char] -> [Char]
encrypt n = map (shift n)

decrypt :: Int -> [Char] -> [Char]
decrypt n = map (shift (-n))

shift :: Int -> Char -> Char
shift n c
    |isAlpha c = chr $ ord base + (ord c - ord base + n) `mod` 26
    |otherwise = c
    where base = if c >= 'a' then 'a' else 'A'

joinWords :: Char -> [String] -> String
joinWords _ [] = ""
joinWords _ [x] = x
joinWords c (x:xs) = x ++ [c] ++ joinWords c xs

rotations :: String -> [String]
rotations str = take (length str) (iterate rotate str)
    where rotate s = tail s ++ [head s]

bwt :: String -> String
bwt str = map last $ sort $ rotations str

indicies :: Int -> [Int] -> [Int]
indicies n l = indiciesW n l 0

indiciesW :: Int -> [Int] -> Int -> [Int]
indiciesW _ [] _ = []
indiciesW n (x:xs) p
    |x == n    = p : indiciesW n xs (p+1)
    |otherwise =     indiciesW n xs (p+1)

lastIndex :: Int -> [Int] -> Int
lastIndex n l =  maxEl (indicies n l) 

maxEl :: Ord a => [a] -> a
maxEl [x] = x
maxEl (x:xs) = max x (maxEl xs)

minEl :: Ord a => [a] -> a
minEl [x] = x
minEl (x:xs) = min x (minEl xs)

countOcc _ [] = 0
countOcc n (x:xs)
    |x == n    = 1 + countOcc n xs
    |otherwise = countOcc n xs

countMin :: (Num a, Ord t) => [t] -> a
countMin l = countOcc (minEl l) l

isPrimeWrapper :: Integral a => a -> a -> Bool
isPrimeWrapper x a
    |a * a > x       = True 
    |x `mod` a == 0 = False
    |otherwise      = isPrimeWrapper x (a + 1)

isPrime :: Integral a => a -> Bool
isPrime x
    | x < 2     = False 
    | otherwise = isPrimeWrapper x 2

addEnd a [] = [a]
addEnd a (x:xs) = x : addEnd a xs


primeReorderW :: Integral t => [a] -> t -> [a] -> [a] -> [a]
primeReorderW [] _ primes others = primes ++ others
primeReorderW (x:xs) p primes others
    |isPrime p      = primeReorderW xs (p+1) (primes ++ [x]) others
    |otherwise      = primeReorderW xs (p+1) primes (others ++ [x])

primeReorder :: [a] -> [a]
primeReorder l = primeReorderW l 2 [] []

dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup l = dedupW [] l

dedupW :: Eq a => [a] -> [a] -> [a]
dedupW seen [] = seen
dedupW seen (x:xs)
    |x `elem` seen = dedupW seen xs
    |otherwise     = dedupW (seen ++ [x]) xs

merge :: Ord a => [a] -> [a] -> [a]
merge l1 [] = l1
merge [] l2 = l2
merge (x:xs) (y:ys)
    |x < y     = x : merge xs (y:ys)
    |otherwise = y : merge (x:xs) ys

splitat :: (Eq p, Num p) => p -> [a] -> ([a], [a])
splitat n [] = ([],[])
splitat 0 l  = ([],l)
splitat n (x:xs) =  
    let (left,right) = splitat (n-1) xs
    in (x:left, right)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where (left, right) = splitat (length xs `div` 2) xs

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs

pick k [] = []
pick k l = filter (\xs -> length xs == k) (subsets l)

comparing :: Ord a => (t -> a) -> t -> t -> Ordering
comparing f x y = compare (f x) (f y)

maximize :: (Ord a, Num a) => [a -> a] -> (a -> a)
maximize [] n = abs n
maximize functs a = maximumBy (comparing abs) (map ($ a) functs)

composing [] n = n
composing funct n = (head funct) $ composing (tail funct) n

fact :: (Eq t, Num t) => t -> t
fact 0 = 1
fact n = n * fact (n-1)

facts :: [Integer]
facts = map fact [1..]

points :: [(Integer, Integer)]
points = [(x,y) | x <- [0..] , y <- [0..x]]