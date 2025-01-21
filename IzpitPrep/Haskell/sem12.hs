type Student = String  
type Subject = String  
type Note = Double     

data Record = Record {student :: Student, subject :: Subject, note :: Note}
             deriving (Read, Show)

avgNote :: [Record] -> Student -> Note
avgNote records s = sum (map note stRec) / fromIntegral ( length stRec)
    where
        stRec = filter (\r -> student r == s) records

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs)
    |x `elem` xs  = nub xs
    |otherwise    = x : nub xs

goodStudentAverage :: [Record] -> Note
goodStudentAverage [] = error "empty list"
goodStudentAverage records = if count == 0 then 0 else totalNote / count
   where 
       notes = filter (\n -> note n == 6.0) records
       st = map student notes
       unique = nub st
       totalNote = sum [avgNote records s | s <-unique]
       count = fromIntegral (length unique)
        
testRecords :: [Record]
testRecords = 
  [ Record "Иван" "Математика" 5.5
  , Record "Мария" "Математика" 6.0
  , Record "Иван" "Български" 6.0
  , Record "Георги" "Математика" 4.0
  , Record "Георги" "Български" 5.0
  , Record "Мария" "Български" 5.5
  , Record "Иван" "Физика" 6.0
  ]


type Name = String      
type Goals = Int       
type Assists = Int      
type Hometown = Name    

data Player = Player {playerName :: Name, goals :: Goals, assists :: Assists}
  deriving (Read, Show)

data Team = Team {teamName :: Name, hometown :: Hometown, players :: [Player]}
  deriving (Read, Show)

player1 = Player {playerName = "John Doe", goals = 5, assists = 3}
player2 = Player {playerName = "Jane Smith", goals = 2, assists = 7}
player3 = Player {playerName = "James Brown", goals = 6, assists = 4}
player4 = Player {playerName = "Emily Davis", goals = 3, assists = 5}
player5 = Player {playerName = "Michael Wilson", goals = 8, assists = 2}
player6 = Player {playerName = "Sophia Johnson", goals = 4, assists = 6}

team1 = Team {teamName = "Lions", hometown = "Chicago", players = [player1, player2]}
team2 = Team {teamName = "Tigers", hometown = "Los Angeles", players = [player3, player4]}
team3 = Team {teamName = "Bears", hometown = "Chicago", players = [player5, player6]}
team4 = Team {teamName = "Eagles", hometown = "Miami", players = [player1, player4]}

teams = [team1, team2, team3, team4]

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy f (x:xs) = (x : takeWhile (f x) xs) : groupBy f (dropWhile (f x) xs)


maximumBy _ [] = error "empty list"
maximumBy f (x:xs) = foldl (\x y -> if f x > f y then x else y) x xs

mostGoals :: [Player] -> Player
mostGoals [] = error "empty list"
mostGoals (p:pl) = helper p pl
    where 
        helper max [] = max
        helper max (r:rs)
            |goals r > goals max    = helper r rs
            |otherwise              = helper max rs

topScorrer :: [Team] -> Name
topScorrer [] = ""
topScorrer records = playerName bestPlayer 
    where
        pls = concatMap players records
        bestPlayer = mostGoals pls

topAssists :: [Team] -> Name
topAssists [] = ""
topAssists teams = playerName (foldl compareAssists (head allPlayers) (tail allPlayers))
    where
       allPlayers = concatMap players teams
       compareAssists p1 p2
        |assists p1 > assists p2   = p1
        |assists p1 < assists p2   = p2
        |otherwise = p1

topTeam :: [Team] -> Name
topTeam (t:ts) = teamName (foldl compareGoals t ts)
    where
        goalsInOneTeam t = sum $ map goals (players t)
        compareGoals t1 t2
            |goalsInOneTeam t1 > goalsInOneTeam t2  = t1
            |goalsInOneTeam t1 < goalsInOneTeam t2  = t2
            |otherwise                              = t1

topCity :: [Team] -> Name
topCity [] = ""
topCity teams =  fst (maximumCount cityCounts)
    where
        cities = map hometown teams
        countCities [] = []
        countCities (x:xs) = (x, 1 + length (filter (==x) xs)) : countCities (filter (/=x) xs)
    
        cityCounts = countCities cities
        maximumCount [x] = x
        maximumCount (x:xs) = if snd x > snd maxRest then x else maxRest
            where maxRest = maximumCount xs

--binTrees

data BinTree a = Empty | Node {root :: a, left, right :: BinTree a }
    deriving (Eq, Show, Ord, Read)

leaf x = Node x Empty Empty

tree :: BinTree Int
tree = Node 10 
                (Node 5 (leaf 3) (leaf 7)) 
                (Node 15 (leaf 12) (Node 20 (leaf 18) (leaf 25)))

depth :: BinTree a -> Integer
depth Empty = 0
depth (Node x l r) = 1 + max (depth l) (depth r)

countLeaves :: BinTree a -> Integer
countLeaves Empty = 0
countLeaves (Node x Empty Empty) = 1
countLeaves (Node x l r) = countLeaves l + countLeaves r

collectPreOrder :: BinTree a -> [a]
collectPreOrder Empty = []
collectPreOrder (Node x l r) = [x] ++ collectPreOrder l ++ collectPreOrder r

collectInOrder :: BinTree a -> [a]
collectInOrder Empty = []
collectInOrder (Node x l r) = collectInOrder l ++ [x] ++ collectInOrder r


level :: BinTree a -> Integer -> [a]
level Empty _ = []
level (Node x _ _) 0 = [x]
level (Node x r l) n = level r (n-1) ++ level l (n-1)

prune :: BinTree a -> BinTree a
prune Empty = Empty
prune (Node x Empty Empty) = Empty
prune (Node x l r) = Node x (prune l) (prune r)

invert :: BinTree a -> BinTree a
invert Empty = Empty
invert (Node x l r) = Node x (invert r) (invert l)

isLeaf (Node x Empty Empty) = True
isLeaf _ = False

path :: Eq t => t -> BinTree t -> [t]
path _ Empty = []
path el (Node x l r) 
        |x == el     =  [x]
        |otherwise   = case path el l of
            [] -> case path el r of
                [] -> []
                path -> x : path
            path -> x : path

contains ::Eq a => BinTree a -> [a] -> Bool
contains Empty [] = True
contains Empty xs = False
contains t [] = False
contains (Node x l r) (p:ps) 
    |x /= p     = False
    |otherwise  = (contains l ps) || (contains r ps)


isBST :: Ord a => BinTree a -> Bool
isBST Empty = True
isBST (Node x Empty Empty) = True
isBST (Node x l r)
    |(root l > x) || root r < x    = False
    |otherwise         = isBST l && isBST r

bstInsert :: Ord t => t -> BinTree t -> BinTree t
bstInsert el Empty = Node el Empty Empty
bstInsert el (Node x l r)
    |el < x       = bstInsert el l
    |otherwise    = bstInsert el r

treeSort :: Ord a => [a] -> [a]
treeSort [] = []
treeSort xs = collectInOrder (foldr bstInsert Empty xs) 

treePaths :: BinTree a -> [[a]]
treePaths Empty = []
treePaths t = helper [] t
    where
        helper _ Empty = []
        helper paths (Node x l r)
            |isLeaf (Node x l r)    = reverse (x : paths) : []
            |otherwise              = helper (x: paths) r ++ helper (x : paths) l

average [] = 0
average xs = sum xs `div` length xs

kids Empty = []
kids (Node x l r) = [root l, root r]

findMeanNodes Empty = []
findMeanNodes t = helper Nothing t
    where helper _ Empty = []
          helper parent (Node x l r) 
            |isMean parent l r x      = x : (helper (Just x) l ++ helper (Just x) r) 
            |otherwise                  = helper (Just x) l ++ helper (Just x) r

          isMean Nothing _ _ _ = False
          isMean (Just p) Empty Empty x = p == x
          isMean (Just p) (Node lval _ _) (Node rval _ _) x = x == (p + lval + rval) `div` 3
          isMean _ _ _ _ = False


children :: Eq t => t -> BinTree t -> [t]
children _ Empty = []
children n (Node x Empty Empty) = []
children n (Node x l r) = if n == x then [root l, root r] else (children n l ++ children n r)

grandChildren :: Eq t => t -> BinTree t -> [t]
grandChildren t tree = concatMap (`children` tree) (children t tree)

findGrandpas :: (Eq a, Num a) => BinTree a -> [a]
findGrandpas tree = [x | x <- (collectInOrder tree), x == sum (grandChildren x tree)]

heavyNodes :: (Ord a, Num a) => BinTree a -> [a]
heavyNodes tree = [x | x <- (collectInOrder tree), x > sum (path x tree)]





