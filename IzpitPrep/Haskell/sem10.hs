import Data.Char
import Data.List

whisper :: [Char] -> [Char]
whisper  = map toLower

removeSpaces :: [Char] -> [Char]
removeSpaces = filter (\x -> not (isSpace x))

switchCaps :: [Char] -> [Char]
switchCaps = map (\x -> if isLower x then toUpper x else toLower x)

shift :: Int -> Char -> Char
shift n c
  | isUpper c = chr $ (ord c - ord 'A' + n) `mod` 26 + ord 'A'
  | isLower c = chr $ (ord c - ord 'a' + n) `mod` 26 + ord 'a'
  | otherwise = c

encrypt :: Int -> [Char] -> [Char]
encrypt n = map (shift n) 

decrypt :: Int -> [Char] -> [Char]
decrypt n = map (shift (-n)) 

joinWords :: Char -> [String] -> String
joinWords _ [] = ""
joinWords c strs = tail (concat [c:s | s <-strs])

rotate :: Int -> [a] -> [a]
rotate i str = b ++ a 
    where (a,b) = splitAt i str

bwt :: String -> String
bwt str = map last (sort (rotations str))
    where rotations str = [rotate i str | i <- [0..length str - 1]]

indicies :: Eq a => a -> [a] -> [Int]
indicies _ [] = []
indicies n xs = foldr (\(x,ind) r -> if x == n then ind : r else r) [] (zip xs [0..])
  
maxEl :: Ord a1 => [a1] -> a1
maxEl [] = error "empty"
maxEl (x:xs) = foldr max x xs

minEl :: Ord a1 => [a1] -> a1
minEl [] = error "empty"
minEl (x:xs) = foldr min x xs

lastIndex :: Int-> [Int] -> Int
lastIndex n  xs = if null (indicies n xs) then error "not in list" else maxEl (indicies n xs)

countMin :: Ord a => [a] -> Int
countMin [] = 0
countMin xs = length (indicies (minEl xs) xs)

isPrime :: Integral t => t -> Bool
isPrime n 
    |n < 2     = False
    |otherwise = isPrimeHelper 2 n

isPrimeHelper :: Integral t => t -> t -> Bool
isPrimeHelper i n 
    |i == n          = True
    |n `mod` i == 0  = False
    |otherwise       = isPrimeHelper (i + 1) n

primeReorder :: [a] -> [a]
primeReorder [] = []
primeReorder (x:xs) = x : helper 2 xs []
    where helper _ [] acc = acc
          helper i (y:ys) acc
            |null (y:ys)   = acc
            |isPrime i     = helper (i + 1) ys (acc ++ [y])
            |otherwise     = helper (i + 1) ys (y : acc)

member :: Eq t => t -> [t] -> Bool
member _ [] = False
member k (x:xs)
    |k == x          = True
    |otherwise       = member k xs
    
dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs) = reverse (helper [x] xs)
    where helper acc [] = acc 
          helper acc (y:ys)
            |y `member` acc   = helper acc ys
            |otherwise        = helper (y : acc) ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    |x < y     = x : merge xs (y:ys)
    |otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = [] 
mergeSort [x] = [x]
mergeSort xs = 
    let (left, right) = splitAt (length xs `div` 2) xs
    in merge (mergeSort left) (mergeSort right)

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x: ) subs ++ subs
    where subs = subsets xs

pick :: Int -> [a] -> [[a]]
pick k [] = [] 
pick k xs = filter (\x -> length x == k) (subsets xs)

maximize :: Ord a => [p -> a] -> p -> a
maximize [] _ = error "empty"
maximize (f:fs) x = maxEl (results (f:fs))
    where results [] = []
          results (f:fs) = f x : results fs

compose :: [t -> t] -> t -> t
compose [] x = x
compose [f] x = f x
compose (f:fs) x = f (compose fs x)

fact :: (Eq t, Num t) => t -> t
fact 0 = 1
fact n = n * fact (n-1)

facts :: [Integer]
facts = [fact x| x <- [0..]]

points :: [(Integer, Integer)]
points = [(x, n - x) | n <- [0..],
                       x <- [0..]]