module Solutions where

import Prelude hiding (Functor (..), Monoid (..), Semigroup (..))
--1

class Sizable a where
    size :: a -> Int
    size _ = 1

instance Sizable Int where
    size _ = 4

instance (Sizable a) => Sizable (Maybe a) where
    size Nothing = 0
    size (Just x) = 1 + size x

instance Sizable a => Sizable [a] where
    size xs = sum $ map size xs

--2

newtype Down a = Down a
  deriving (Show, Eq)

instance (Ord a) => Ord (Down a) where
    Down x <= Down y = x >= y

--3

data List a = Nil | Cons a (List a)

data NonEmpty a = a :| [a]
    deriving (Show, Eq)

class Semigroup a where
    (<>) :: a -> a -> a

    sconcat:: NonEmpty a -> a
    sconcat (x :| xs) = foldl (<>) x xs 

instance Semigroup (List a) where
    Nil <> l = l
    (Cons x xs) <> l = Cons x ( xs <> l)

class (Semigroup a) => Monoid a where
    mempty :: a

    mappend :: a -> a -> a
    mappend = (<>)

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

instance Monoid (List a) where
    mempty = Nil

instance (Show a) => Show (List a) where
        show xs = "(" ++ show' xs ++ ")"
            where
            show' :: (Show a) => List a -> String
            show' Nil = ""
            show' (Cons a Nil) = show a
            show' (Cons a xs) = show a ++ "," ++ (show' xs)

instance (Eq a) => Eq (List a) where
    Nil == Nil = True
    Cons a xs == Cons b ys = a == b && xs == ys
    _ == _ = False

instance (Ord a) => Ord (List a) where
    Nil <= _ = True
    _ <= Nil = False
    Cons x xs <= Cons y ys = x <= y && xs <= ys

class Foldable' c where
    foldr' :: (a -> b -> b) -> b -> c a -> b

instance Foldable' List where
    foldr' _ nv Nil = nv
    foldr' op nv (Cons x xs) = x `op` foldr' op nv xs

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs) 

--4

class Stream s where
    empty :: s a

    cons :: a -> s a -> s a

    uncons :: s a -> Maybe (a, s a)


instance Stream [] where
    empty = []
    cons = (:)
    uncons xs = 
        case xs of 
            [] -> Nothing
            h:t -> Just (h, t)

instance Stream List where
    empty = Nil
    cons = Cons
    uncons xs = 
        case xs of
            Nil -> Nothing
            (Cons h t) -> Just (h,t)

--5

data BinTree a = Empty | Node a (BinTree a) (BinTree a)
  deriving (Show, Eq)

instance Foldable' BinTree where
    foldr' _ nv Empty = nv
    foldr' op nv (Node x l r) = foldr' op (x `op` foldr' op nv r) l

instance Functor BinTree where
    fmap _ Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)


--6

