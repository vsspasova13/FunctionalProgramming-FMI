
findSum :: [Int] -> Int -> Int -> Int
findSum [] _ _  = 0
findSum xs m n = helper xs m n 1 0
    where 
        helper xs m n i acc
            | m + i > n - i    = acc
            | otherwise        = helper xs m n (i + 1) (acc + (!!) xs (m + i) * (!!) xs (n - i))

next :: [Int] -> Int -> Int -> Int
next xs k i
    |i < k = 1
    |otherwise = findSum xs m i `div` (xs !! m)
       where m = i - k

somos k = xs
    where xs = [next xs k i | i <- [0..]]
