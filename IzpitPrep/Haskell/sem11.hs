
find :: (Eq a) => [(a, b)] -> a -> b
find [] _ = error "not found" 
find ((k,v) : xs) key 
    |k == key    = v
    |otherwise   = find xs key

groupBy _ [] = []
groupBy f (x:xs)  = (key, same) : groupBy f rest
    where
        key = f x
        same = x : filter (\y -> f y == key) xs
        rest = filter (\y -> f y /= key) xs

type Subject = String
type Student = String
type Exam = Int
type Note = Double

type Record = (Subject, Student, Exam, Note)

getNote :: Record -> Note
getNote (_, _, _, n) = n

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

averageNote :: [Record] -> Double
averageNote recs = average [note | (_, _, _, note) <- recs]

maximumOn :: (Ord b) => (a -> b) -> [a] -> a
maximumOn f = foldl1 (\x y -> if f x > f y then x else y)

topOfClass :: [Record] -> Student
topOfClass [] = ""
topOfClass records = fst $ maximumOn snd averages
    where
       students = groupBy (\ (_, s, _, _) -> s) records
       averages = [(s,averageNote recs) | (s, recs) <- students]

minimumOn :: (Ord b) => (a -> b) -> [a] -> a
minimumOn f = foldl1 (\x y -> if f x < f y then x else y)

hardestExam :: [Record] -> Exam
hardestExam [] = error "empty list"
hardestExam records = fst $ minimumOn snd exams
    where
        exams = groupBy (\(_,_,e,_) -> e) records
        averages = [(e, averageNote ex) | (e, ex) <- exams]

perfectScorers :: [Record] -> Subject -> [Student]
perfectScorers [] _ = []
perfectScorers records subj = [st | (st,note) <- averages, note == 6.00]
    where 
        subjectRecords = filter (\(s, _, _, _) -> s == subj) records
        students = groupBy (\(_, s, _, _) -> s) subjectRecords
        averages = [(st, averageNote recs) | (st,recs) <- students]

failingStudents :: [Record] -> [Student]
failingStudents [] = []
failingStudents records = [st | (st,grades) <- averages, length( filter (< 3.0) grades) > 1]
    where 
         students = groupBy (\(_, s, _, _) -> s) records
         stSubjectRecords = [(st, groupBy (\(s,_,_,_) -> s) recs) | (st, recs) <- students]
         averages = [(st, [averageNote recs | (_, recs) <- subjRecs]) | (st, subjRecs) <- stSubjectRecords]


type Node = Int
type Edge = (Node, Node)
type AdjNode = (Node, [Node])
type Graph = [AdjNode]

nodes :: Graph -> [Node]
nodes = map fst

neighbors :: Graph -> Node -> [Node]
neighbors graph node = head [ns | (n,ns) <- graph, n == node]

dfs :: Graph -> Node -> [Node] -> [Node]
dfs graph node visited
    |node `elem` visited  = visited
    |otherwise            = foldr (dfs graph) (node : visited) (neighbors graph node)

buildGraph :: [Node] -> [Edge] -> Graph
buildGraph nodes edges = [(n,neighbors n) | n <- nodes]
    where 
        neighbors node = [y | (x, y) <- edges, x == node] ++ [x | (x, y) <- edges, y == node]

reachableNodes :: Graph -> Node -> [Node]
reachableNodes graph node = dfs graph node []

connectedComponents :: Graph -> [[Node]]
connectedComponents graph = helper (nodes graph) [] []
    where 
        helper [] _ css = css
        helper (n : ns) visited css
            |n `elem` visited = helper ns visited css
            |otherwise        = helper ns (visited ++ cc) (cc : css)
            where 
                cc = reachableNodes graph n

isFullyConnected :: Graph -> Bool
isFullyConnected g = length (connectedComponents g) == 1

longestPathFrom :: Graph -> Node -> [Node] -> Int
longestPathFrom graph curr visited
    |curr `elem` visited = 0
    |otherwise = 1 + maximum (0 : [longestPathFrom graph neighbor (curr: visited) | neighbor <- neighbors graph curr])


diameter :: Graph -> Int
diameter graph = maximum [longestPathFrom graph n [] | n <- nodes graph]

{-
dfs2 :: Graph -> [Node] -> Int -> [Node] ->[Node]
dfs2 _ []_ _ = []
dfs2 g (curr:rest) 0 visited = curr : dfs2 g rest 0 visited
dfs2 g (curr:rest) steps visited
    |curr `elem` visited = dfs2 g rest steps visited
    |otherwise           = dfs2 g (rest ++ neighbors curr) (steps - 1) (curr : visited)
    where
        neighbors n = case lookup n g of
            Just ns -> filter (`notElem` visited) ns
            Nothing -> []

nodesAtDistance graph k = concatMap (\u -> [(u, v) | v <- reachable u k, u < v]) (nodes graph)  
    where 
        reachable start k = dfs2 graph [start] k []    
-}

graph :: Graph
graph = 
  [ (1, [2]),   -- Връх 1 е свързан с 2, 3, 4
    (2, [1, 5, 6]),   -- Връх 2 е свързан с 1, 5, 6
    (3, [7, 8]),   -- Връх 3 е свързан с 1, 7, 8
    (4, [9]),      -- Връх 4 е свързан с 1, 9
    (5, [2, 10]),     -- Връх 5 е свързан с 2, 10
    (6, [2]),         -- Връх 6 е свързан само с 2
    (7, [3]),         -- Връх 7 е свързан само с 3
    (8, [3]),         -- Връх 8 е свързан само с 3
    (9, [4]),         -- Връх 9 е свързан само с 4
    (10, [5])         -- Връх 10 е свързан само с 5
  ]