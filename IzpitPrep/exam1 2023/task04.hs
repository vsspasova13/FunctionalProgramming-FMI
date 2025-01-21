
type Name = String
type Prediction = Int
type Reall = Int
type Score = Int

data Player = Player (Name, Prediction, Reall)
     deriving (Eq, Ord, Show, Read)

predictions :: [Player] -> [(Name, Score)]
predictions = map (\(Player (n,p,r)) -> (n, abs(p - r))) 

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
    where 
        insert y [] = [y]
        insert y (z:zs)
            |y <= z     = y : z :zs
            |otherwise  = z : insert y zs

insertionSortBy :: (a -> a -> Ordering) -> [a] -> [a]
insertionSortBy _ [] = []
insertionSortBy cmp (x:xs) = insertBy cmp x (insertionSortBy cmp xs)
    where
        insertBy cmp y [] = [y]
        insertBy cmp y (z:zs)
            | cmp y z == LT = y : z : zs
            | otherwise     = z : insertBy cmp y zs

compareByDifference :: Player -> Player -> Ordering
compareByDifference (Player (_, p1, r1)) (Player (_, p2, r2)) = compare (abs (p1 - r1)) (abs (p2 - r2))

sortedByBest :: [Player] -> [Player]
sortedByBest = insertionSortBy compareByDifference 


sortedByWorst:: [Player] -> [Player]
sortedByWorst pl = reverse (sortedByBest pl)

getName :: Player -> Name
getName (Player (n,_,_)) = n

getReal :: Player -> Reall
getReal (Player (_,_,r)) = r

getPred:: Player -> Prediction
getPred (Player (_,p,_)) = p

compareByPoints :: (Name, Reall) -> (Name, Reall) -> Ordering
compareByPoints (_,r1) (_, r2) = compare r2 r1

finalScores :: [Player] -> [(Name, Reall)]
finalScores [] = []
finalScores players = insertionSortBy compareByPoints (helper 0  [])
    where 
        helper i acc
            |i == length players   = acc
            |otherwise             = helper (i + 1)  ((getName (bests !! i) , newPoints) : acc)
                where
                    bests  = sortedByBest players
                    worsts = sortedByWorst players
                    newPoints = getReal (bests !! i) + abs (getReal (worsts !! i) - getPred (worsts !! i))

test :: [Player]
test = 
    [ Player ("Angel", 14, 15)
    , Player ("Andrei", 8, 10)
    , Player ("Atanas", 10, 3)
    , Player ("Georgi", 6, 4)
    ]

bests  = sortedByBest test
worsts = sortedByWorst test
