
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

