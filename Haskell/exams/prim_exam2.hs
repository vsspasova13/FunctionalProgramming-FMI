import Prelude

mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (x:xs) = x : mynub (filter (/=x) xs)

isNperm :: Int -> (Int-> Int) -> Bool
isNperm n f = all (\x -> f x >= 0 && f x < n) [0..n-1] && length (mynub results) == n
    where results = map f [1..(n-1)]

maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ [] = error "maximumBy: empty list"
maximumBy cmp [x] = x
maximumBy cmp (x:xs) = go cmp x xs
  where
    go cmp currentMax [] = currentMax
    go cmp currentMax (y:ys)
      | cmp currentMax y == GT = go cmp currentMax ys
      | otherwise              = go cmp y ys

comparing :: (Ord b) => (a -> b) -> a -> a -> Ordering
comparing f x y = compare (f x) (f y)

{-
maxCycle :: (Eq a, Num a, Enum a) => a -> (a -> a) -> [a]
maxCycle n f = maximumBy (comparing length) cycles
    where 
        cycles = getCycles [1..n-1] []
        getCycles [] _ = []
        getCycles (x:xs) seen 
           |x `elem` seen = getCycles xs seen 
           |otherwise     = let cycle = getCycle x seen in cycle : getCycles xs (seen ++ cycle)
        getCycle start seen = takeWhile (`notElem` seen) (iterate f start)
        -}

        
maxCycle :: Int -> (Int -> Int) -> [Int]
maxCycle n f = maximumBy (comparing length) (cycles n f)


cycles :: Int -> (Int -> Int) -> [[Int]]
cycles n f = [cycleFrom start f | start <- [0..n-1], not (visited start)]
  where
    visited x = any (elem x) (cycles n f)
    cycleFrom start f = takeWhile (/= start) (iterate f start) ++ [start]
