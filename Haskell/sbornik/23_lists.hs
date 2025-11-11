module Solutions23 where

--23.4
sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist _ [] = False
sublist (x:xs) ys
    | x `elem` ys = sublist xs ys
    | otherwise = False

--23.5
common :: Eq a => [a] -> [a] -> Int
common [] _ = 0
common _ [] = 0
common (x:xs) ys
    | x `elem` ys = 1 + common xs ys
    | otherwise = common xs ys

--23.6
dublicates :: Eq a => [a] -> Bool
dublicates [] = False
dublicates (x:xs) = (x `elem` xs) ||  dublicates xs


