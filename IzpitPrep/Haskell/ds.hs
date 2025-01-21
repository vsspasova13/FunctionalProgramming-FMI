import Distribution.PackageDescription.FieldGrammar (benchmarkStanzaBenchmarkModule)

class Measurable a where
    size :: a -> Int
    empty :: a -> Bool
    empty = (==0) . size

larger :: Measurable a => a -> a -> Bool
larger x y = size x > size y

instance Measurable Integer where
    size 0 = 0
    size n = 1 + size (n `div` 10) 

instance (Measurable a, Measurable b) => Measurable (a,b) where
    size (x,y) = size x + size y

instance Measurable a => Measurable [a] where
    size = sum . map size

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq, Ord, Enum, Show, Read)

today = Wed

{-
instance Show Weekday where
    show Mon = "понеделник"
    show Tue = "вторник"
    show Wed = "сряда"
    show _   = "вече нз"
-}


type Name = String 
type Score = Int 
--data Player = Player Name Score
data Player = Player {name :: Name, score :: Score}

getName :: Player -> Name
getName (Player name _ ) = name

better :: Player -> Player -> String
better (Player name1 score1) (Player name2 score2)
    |score1 > score2    = name1
    |otherwise          = name2

data Shape =
    Circle { radius :: Double} |
    Rectangle {width, height :: Double}
    deriving (Eq, Ord, Show, Read)

circle :: Shape
circle = Circle 2.3

area :: Shape -> Double
area (Circle r) = pi * r **2

getAt :: Int -> [a] -> Maybe a
getAt _ [] = Nothing
getAt 0 (x:_) = Just x
getAt n (x:xs) = getAt (n-1) xs

searchBest :: [Player] -> Either Score [Name]
searchBest players
    | length bestPlayers == 1 = Left best
    | otherwise = Right $ map name bestPlayers
      where best = maximum $ map score players
            bestPlayers = filter ((==best) . score) players

data Nat = Zero | Succ Nat
    deriving (Eq, Ord, Show, Read)

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Succ (toNat (n - 1))

data Bin = One | BitZero Bin | BitOne Bin
    deriving (Eq, Ord, Show, Read)

fromBin :: Bin -> Integer
fromBin One = 1
fromBin (BitOne b) = 2 * fromBin b + 1
fromBin (BitZero b) = 2 * fromBin b

succBin :: Bin -> Bin
succBin One = BitZero One
succBin (BitZero b) = BitOne b
succBin (BitOne b)  = BitZero $ succBin b

data List a = Nil | Cons a (List a)
    deriving (Eq, Ord, Show, Read)

l = Cons 1 $ Cons 2 $ Cons 3 $ Nil

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

(+++) :: List a -> List a -> List a
Nil +++ ys         = ys
(Cons x xs) +++ ys = Cons x (xs +++ ys)

data BinTree a = Empty | Node {root :: a, left, right :: BinTree a}
    deriving (Eq, Show, Read, Ord)

leaf x = Node x Empty Empty
t = Node 3 (leaf 1) (leaf 5)

depth :: BinTree a -> Integer
depth Empty = 0
depth (Node x r l) = 1 + max (depth r) (depth l)

leaves :: BinTree a -> [a]
leaves Empty = [] 
leaves (Node x Empty Empty) = [x]
leaves (Node x r l) = leaves l ++ leaves r

mapBinTree :: (t -> a) -> BinTree t -> BinTree a
mapBinTree _ Empty = Empty
mapBinTree f (Node x r l) = Node (f x) (mapBinTree f l) (mapBinTree f r)

foldrBinTree _ nv Empty = nv
foldrBinTree op nv (Node x r l) = foldrBinTree  op 
                                 (x `op` foldrBinTree op nv r) l


data Tree a = Tree { rootTree :: a, subtrees :: TreeList a}
                    deriving (Eq, Show, Read, Ord)
data TreeList a = None | SubTree { firstTree :: Tree a,
                                   restTrees :: TreeList a}
                    deriving (Eq, Show, Read, Ord)

leafTree x = Tree x None
tree = Tree 1 $ SubTree (leafTree 2)
              $ SubTree (Tree 3 $ SubTree (leafTree 4) $ None)
              $ SubTree (leafTree 5) $ None

level :: Integer -> Tree a -> [a]
level 0 (Tree x _)  = [x]
level n (Tree _ ts) = levelTrees (n - 1) ts

levelTrees :: Integer -> TreeList a -> [a]
levelTrees _ None          = []
levelTrees n (SubTree t ts) = level n t ++ levelTrees n ts


data SExpr = SBool Bool | SChar Char | SInt Int | SDouble Double | SList { list :: [SExpr] }
        deriving (Eq, Ord, Show, Read)

sexpr = SList [SInt 2, SChar 'a', SList [SBool True, SDouble 1.2, SList []]]

countAtoms :: SExpr -> Integer
{-
countAtoms (SList []) = 0
countAtoms (SList (se:ses)) = countAtoms se + countAtoms (SList ses)
countAtoms _ = 1
-}
countAtoms (SList ses) = sum $ map countAtoms ses
countAtoms _           = 1

flatten :: SExpr -> SExpr
{-
flatten (SList [])       = SList []
flatten (SList (se:ses)) = SList (list (flatten se) ++ list (flatten (SList ses)))
flatten se               = SList [se]
-}
flatten (SList ses) = SList $ map flatten ses
flatten se          = SList [se]

