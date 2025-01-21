
type Q = [Int]
type Start = Int
type T = [(Int, Char, Int)]
type F = [Int]

data NFA = NFA (Q,Start,T,F)
    deriving (Eq, Ord, Show, Read)

allWords :: Int -> [Char] -> [String]
allWords n letters = concatMap (\k -> generateCombinations k letters) [1..n]
    where
        generateCombinations 0 _ = [""]
        generateCombinations k letters = [x : rest | x <- letters, rest <- generateCombinations (k-1) letters]

isWordAccepted :: NFA -> String -> Bool
isWordAccepted (NFA (q, start, t, e)) word = isAccepted start word
    where 
        isAccepted curr [] = curr `elem` e
        isAccepted curr (x:xs) = any (\(_, symbol, nextState) -> symbol == x && isAccepted nextState xs) transitions
            where 
                transitions = filter (\(s,_,_) -> s == curr) t


rejectedWord :: NFA -> Either Bool [String]
rejectedWord (NFA (q,s,t,e))
    |and (map (\w -> isWordAccepted (NFA (q,s,t,e)) w) words)  = Left True
    |otherwise                                                 = Right $ filter (\w -> not (isWordAccepted (NFA (q,s,t,e)) w)) words
        where 
            words = allWords (length q) ['a', 'b']

nfa :: NFA
nfa = NFA ([0,1,2], 0, [(0,'a',1), (1, 'b', 1), (1,'a',2), (2, 'a', 2), (2,'b', 1)], [0, 1, 2])
