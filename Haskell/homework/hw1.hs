module Solutions where
import Distribution.Compiler (AbiTag(NoAbiTag))

--1
extendedGCD :: Int -> Int -> (Int, Int, Int)
extendedGCD a 0 = (1,0,a)
extendedGCD a b = 
    let (x1, y1, d) = extendedGCD b (a `mod` b)
        x = y1
        y = x1 - (a `div` b) * y1
    in (x, y, d)

data LDE2 = LDE2 Int Int Int 
    deriving (Show)

--1.1
concreteSolution :: LDE2 -> Maybe (Int, Int)
concreteSolution (LDE2 a b c) = helper a b c
    where 
        (x1, y1, d) = extendedGCD a b
        helper :: Int -> Int -> Int -> Maybe (Int, Int)
        helper a b c 
            | c `mod` d /= 0 = Nothing
            | otherwise = Just (x1 * (c `div` d), y1 * (c `div` d))

checkSolution :: (Int, Int) -> LDE2 -> Bool
checkSolution (x, y) (LDE2 a b c) = a * x + b * y == c

--1.2
diophantine :: LDE2 -> [(Int, Int)]
diophantine (LDE2 a b c) = 
    let
        (x1, y1, d) = extendedGCD a b
        x0 = x1 * (c `div` d)
        y0 = y1 * (c `div` d)
        u = b `div` d
        v = a `div` d
        ks = [0..] ++ [(-1), (-2)..]
    in [(x0 + k*u, y0 - k*v) | k <- ks]

--1.3
prettyPrint :: LDE2-> String
prettyPrint (LDE2 a b c) = show a ++ ".x " ++ (if b > 0 then "+ " else "- ") ++ show (abs b) ++ ".y = " ++ show c

--1.4
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just a) f = f a

parseA :: String -> Maybe (Int, String)
parseA s =
    case span (/='.') s of
        ("", _) -> Nothing
        (num, '.':'x':rest) -> Just (readMaybe num, dropWhile (==' ') rest)
            where readMaybe n = if all (`elem` "-0123456789") n then read n else 0
        _ -> Nothing

parseB :: String -> Maybe (Int, String)
parseB s =
    let trimmed = dropWhile (==' ') s
        sign 
            | take 1 trimmed == "-" = (-1)
            | take 1 trimmed == "+" = 1
            | otherwise = 1
        rest = dropWhile (`elem` " +-") trimmed
        (num, rest2) = span (/='.') rest
    in case rest2 of
         ('.':'y':r) ->
             if null num then Nothing
             else Just (sign * read num, dropWhile (==' ') r)
         _ -> Nothing

parseC :: String -> Maybe Int
parseC s =
    case dropWhile (==' ') s of
        ('=':rest) ->
            let trimmed = dropWhile (==' ') rest
            in if all (`elem` "-0123456789") trimmed
               then Just (read trimmed)
               else Nothing
        _ -> Nothing

toLDE2 :: String -> Maybe LDE2
toLDE2 str =
    parseA str `bindMaybe` \(a, rest1) ->
    parseB rest1 `bindMaybe` \(b, rest2) ->
    parseC rest2 `bindMaybe` \c ->
    Just (LDE2 a b c)


--2
data LDEn = LDEn { coef :: [Int], 
                   rhs :: Int
                   }
    deriving (Show)

listWithoutIJ :: [Int] -> Int -> Int ->[Int]
listWithoutIJ xs i j = helper xs i j 0
    where
        helper :: [Int] -> Int -> Int -> Int ->[Int]
        helper [] i j num = []
        helper (x:xs) i j num 
            | num == i || num == j = helper xs i j (num+1)
            | otherwise = x : helper xs i j (num+1)

allCombinations :: [[Int]] -> [[Int]]
allCombinations [] = [[]]
allCombinations (x:xs) = concatMap (\y -> map (y:) (allCombinations xs)) x


generateLDE2 :: [Int] -> LDEn -> [LDE2]
generateLDE2 ys (LDEn coef rhs) = concatMap (\(i,j) ->
                                                 let a_k = listWithoutIJ coef i j 
                                                     nrest = length a_k  
                                                 in map (\ys_k -> 
                                                                    let  rhs' = rhs - sum (zipWith (*) a_k ys_k)
                                                                    in LDE2 (coef !! i) (coef !! j) rhs'
                                                        ) (allCombinations (replicate nrest ys))
                                            ) pairs
    where 
        n = length coef
        pairs = [(i,j)| i <- [0..n-1], j <- [i+1..n-1]]
        
