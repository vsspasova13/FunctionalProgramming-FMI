data Tree = Empty | Node Int [Tree]
    deriving (Show)

prune :: Tree -> Tree
prune Empty = Empty
prune (Node v c) = 
    Node v [pruneChild t | t <- c, pred v t]
    where pred p Empty = False
          pred p (Node val _) = gcd p val == 1
          pruneChild Empty = Empty
          pruneChild (Node v c) = Node v [pruneChild t | t <- c, pred v t]

tree :: Tree
tree =
  Node
    10
    [ Node 3 [Empty, Node 3 [], Node 2 [Empty, Empty]],
      Node 7 [Node 14 [], Node 2 [], Node 1 [Empty], Node 15 [Empty]],
      Node 20 [Node 9 [Node 10 []]]
    ]

result :: Tree
result = prune tree


mergeFromMaybes :: [Maybe a] -> [Maybe a] -> [a]
mergeFromMaybes xs ys = concatMap f $ zipWith (<|>) xs ys
    where Just a <|> _ = Just a
          _ <|> Just b = Just b
          Nothing <|> Nothing = Nothing
          f (Just a) = [a]
          f Nothing = []

