import Prelude hiding (Maybe, Just, Nothing, Either, Left, Right, Nat, Succ, Zero, Bin)


class Measurable a where
    size :: a -> Int
    empty :: a -> Bool
    empty = (==0) . size 

larger :: Measurable a => a -> a-> Bool
larger x y = size x > size y

instance Measurable Integer where
    size 0 = 0
    size n = 1 + size (n `div` 10)
    empty = (== 0)

instance (Measurable a, Measurable b) => Measurable (a,b) where
    size (x,y) = size x + size y

instance Measurable a => Measurable [a] where
    size = sum . map size

type Name = String
type Score = Int
data Player = Player {name :: Name, score :: Score}

katnis :: Player
katnis = Player "Katnis Everdeen" 45

getName :: Player -> Name
getName (Player name _) = name

better :: Player -> Player -> String
better (Player name1 score1) (Player name2 score2)
    |score2 > score1 = name2
    |otherwise       = name1

data Shape = Circle {radius :: Double} | Rectangle {width, height :: Double}
    deriving (Eq, Ord, Show, Read)

data Maybe a = Nothing | Just a
    deriving (Eq, Ord, Show, Read)

getAt :: Int -> [a] -> Maybe a
getAt _ []     = Nothing
getAt 0 (x:_)  = Just x
getAt n (_:xs) = getAt (n-1) xs

data Either a b = Left a | Right b
    deriving (Eq, Ord, Show, Read)

searchBest :: [Player] -> Either Int [String]
searchBest players
    |length bestPlayers == 1 = Left best
    |otherwise               = Right $ map name bestPlayers
    where best = maximum $ map score players
          bestPlayers = filter ((==best).score) players


data Nat = Zero | Succ Nat
    deriving (Eq, Ord, Show, Read)

fromNat :: Nat -> Integer
fromNat Zero     = 0
fromNat (Succ n) = 1 + fromNat n

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Succ (toNat (n-1))

data Bin = One | BitZero Bin | BitOne Bin
    deriving (Eq, Ord, Show, Read)

six = BitZero $ BitOne $ One

fromBin :: Bin -> Integer
fromBin One         = 1
fromBin (BitZero b) = 2 * fromBin b
fromBin (BitOne b)  = 2 * fromBin b + 1

succBin :: Bin -> Bin
succBin One         = BitZero One
succBin (BitZero b) = BitOne b
succBin (BitOne b)  = BitZero $ succBin b 

data List a = Nil | Cons a (List a)
    deriving (Show, Eq, Read, Ord)

l = Cons 1 $ Cons 2 $ Cons 3 $ Nil

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

(+++) :: List a -> List a -> List a
Nil +++ ys         = ys
(Cons x xs) +++ ys = Cons x (xs +++ ys)


data BinTree a = Empty | Node {root :: a,
                                left, right :: BinTree a }
        deriving (Show, Eq, Ord, Read)

leaf x = Node x Empty Empty
t = Node 3 (leaf 1) (leaf 5)

depth :: BinTree a -> Integer
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

leaves ::BinTree a -> [a]
leaves Empty                = []
leaves (Node x Empty Empty) = [x]
leaves (Node x l     r)     = leaves l ++ leaves r

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree _ Empty        = Empty
mapBinTree f (Node x l r) = Node (f x) (mapBinTree f l) (mapBinTree f r)

foldrBinTree :: (a -> b -> b) -> b -> BinTree a -> b
foldrBinTree _ nv Empty = nv
foldrBinTree op nv (Node x l r) = foldrBinTree op (x `op` foldrBinTree op nv r) l



