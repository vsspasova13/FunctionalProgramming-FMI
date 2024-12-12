import Data.List (nub, maximumBy, minimumBy)
import Data.Ord  (comparing)

find :: Eq a => [(a,b)] -> a -> b
find [] key = error "xs does not contains key"
find ((x,y):xs) key 
    |x == key  = y
    |otherwise = find xs key

groupBy :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupBy f xs = map (\b -> (b, filter (\k -> f k == b) xs)) uniqueValues
    where 
    uniqueValues = removeDublicates (map f xs)
    removeDublicates [] = []
    removeDublicates (y:ys) = y : removeDublicates (filter (/= y) ys)
 

type Subject = String   
type Student = String   
type Exam = Int         
type Note = Double      

type Record = (Subject, Student, Exam, Note)
  

notes :: Record -> Note
notes (_,_,_,note) = note

students :: Record -> Student
students (_,s,_,_) = s

exams :: Record -> Exam
exams (_,_,e,_) = e

avgGrade :: [Record] -> Student -> Double
avgGrade records st = sum allNotes / fromIntegral (length allNotes)
    where allNotes = map notes stRec
          stRec = filter (\s -> students s == st ) records

avgGradeEx :: [Record] -> Exam -> Double
avgGradeEx records ex = sum allNotes / fromIntegral (length allNotes)
    where allNotes = map notes exRec
          exRec = filter (\e -> exams e == ex ) records
 
topOfClass :: [Record] -> Student
topOfClass [] = ""
topOfClass records = fst $ maximumBy (comparing snd) grades
    where uniqueStudents = nub (map students records)
          grades = map (\st -> (st, avgGrade records st)) uniqueStudents
    
hardestExam :: [Record] -> Exam
hardestExam records = fst $ minimumBy (comparing snd) grades
    where uniqueExams = nub (map exams records)
          grades = map (\ex -> (ex, avgGradeEx records ex)) uniqueExams

perfectScores :: [Record] -> Subject -> [Student]
perfectScores [] _ = []
perfectScores records s = map students perfect
    where sts = filter (\(subj,_,_,_) -> subj == s) records 
          perfect = filter (\(_,_,_,g) -> g == 6.00) sts

failingStudents :: [Record] -> [Student]
failingStudents [] = []
failingStudents records = multipleFails
    where failing = filter (\(_,_,_,g) -> g < 3.00) records
          stFailed = map students failing
          multipleFails = filter (\st -> length (filter (==st) stFailed) > 1) (nub stFailed)

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
    |node `elem` visited = visited
    |otherwise = foldr (dfs graph) (node: visited) (neighbors graph node)

buildGraph :: [Node] -> [Edge] -> Graph
buildGraph nodes edges = [(n, neighbors n) | n <- nodes]
    where 
        neighbors node = [y | (x,y) <- edges, x == node] ++ [x | (x,y) <- edges, y == node]

reachableNodes :: Graph -> Node -> [Node]
reachableNodes graph node = dfs graph node []

connectedComponents :: Graph -> [[Node]]
connectedComponents graph = helper (nodes graph) [] []
    where helper [] _ ccs = ccs
          helper (n:ns) visited ccs
            |n `elem` visited = helper ns visited ccs
            |otherwise = helper ns (visited ++ cc) (cc : ccs)
            where 
                cc = reachableNodes graph n

isFullyConnected :: Graph -> Bool
isFullyConnected graph = length (connectedComponents graph) == 1

bfs :: Graph -> Node -> Node -> Int
bfs graph start end = bfsHelper [(start, 0)] []
    where 
      neighbors n = case lookup n graph of 
        Just ns -> ns
        Nothing -> []
      bfsHelper [] _ = maxBound
      bfsHelper ((n,d):queue) visited
        |n == end = d
        |n `elem` visited = bfsHelper queue visited
        |otherwise = bfsHelper (queue ++ [(neighbor, d + 1) | neighbor <- neighbors n]) (n:visited)

diameter :: Graph -> Int
diameter graph = maximum [bfs graph a b | (a,_) <- graph, (b, _) <- graph, a /= b]

nodesAtDistance :: Graph -> Int -> [(Node, Node)]
nodesAtDistance graph k = [(a,b) | (a,_) <- graph, (b, _) <- graph, a<b, bfs graph a b == k]