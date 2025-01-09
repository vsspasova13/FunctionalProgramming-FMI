import Prelude hiding (span)
import Data.List(group,sort)


span :: (a->Bool) -> [a] -> ([a],[a])
span _ [] = ([],[])
span p (x:xs)
    |p x      = let (ys, zs) = span p xs
                 in (x:xs, zs)
    |otherwise = ([],x:xs)

mostFrequent :: (Num a, Ord a) => [[a]] -> a
mostFrequent [[]] = 0
mostFrequent l =
    let flatLists = concat l
        grouped = group (sort flatLists)
        frequences = map length grouped
        maxFreq = maximum frequences
        mostCommon = map head (filter (\g -> length g == maxFreq) grouped)
    in if length mostCommon == 1
        then head mostCommon
        else 0

--2
type Node = Int
data Tree = Empty | Node {root :: Int, left :: Tree, right :: Tree}
    deriving (Show)

isEmpty :: Tree -> Bool
isEmpty Empty = True
isEmpty _ = False

addLeaf :: Tree -> Int -> Tree
addLeaf Empty x = Node x Empty Empty
addLeaf (Node r left right) x 
    |isEmpty left  = Node r (addLeaf left x) right
    |isEmpty right = Node r left (addLeaf right x)
    |otherwise     = Node r (addLeaf left x) right

grow :: Tree -> Int -> Tree
grow Empty _ = Empty
grow (Node r Empty Empty) x = Node r (Node x Empty Empty) (Node x Empty Empty)
grow (Node r left right)  x = Node r (grow left x) (grow right x)

isComplete :: Tree -> Bool
isComplete Empty = False
isComplete (Node _ Empty Empty) = True
isComplete (Node _ left right) = isComplete left && isComplete right

generateFullTree :: Int -> Tree
generateFullTree 0 = Empty
generateFullTree 1 = Node 1 Empty Empty
generateFullTree n = Node n (generateFullTree (n-1)) (generateFullTree (n-1))

growingTrees :: [Tree]
growingTrees = map generateFullTree [1..]


--3

type Name = String
type Start = (Int, Int)
type Duration = Int

data TvShow = TvShow Name Start Duration

type Program = [TvShow]

areSorted :: Program -> Bool
areSorted [] = True
areSorted [x] = True
areSorted (x:y:xs) = isBefore x y && areSorted (y:xs)
    where
        isBefore (TvShow _ (h1, m1) _) (TvShow _ (h2, m2) _) =
            (h1 < h2) || (h1 == h2 && m1 < m2)

areNonOverLapping :: Program -> Bool 
areNonOverLapping [] = True
areNonOverLapping [x] = True
areNonOverLapping (x:y:xs) = not (isOverLeaping x y) && areNonOverLapping (y:xs)
    where 
        isOverLeaping (TvShow _ (h1, m1) dur1) (TvShow _ (h2, m2) dur2) =
            let end1 = (h1 * 60 + m1) + dur1
                start2 = h2 * 60 + m2
            in start2 >= end1

isValidProgram :: Program -> Bool
isValidProgram p = areSorted p && areNonOverLapping p

endTime :: TvShow -> Int
endTime (TvShow _ (h, m) d) = h*60 + m + d

lastShow :: Program -> String
lastShow [] = ""
lastShow shows  = nameOfShow (head shows) (tail shows)
    where
        nameOfShow lastShow [] = let TvShow name _ _ = lastShow in name
        nameOfShow lastShow (s:sx)
            |endTime lastShow >= endTime s = nameOfShow lastShow sx
            |otherwise                     = nameOfShow s sx

--to do b)

    

