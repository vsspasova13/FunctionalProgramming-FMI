module Solutions where

import Prelude hiding (Applicative (..), Either (..), Functor (..), Monad (..), (<$>))

class Functor f where
    fmap :: (a -> b) -> f a -> f b

    (<$>) :: (Functor f) => (a -> b) -> f a -> f b
    (<$>) = fmap

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f rhs lhs = pure f <*> rhs <*> lhs 

class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    (>>) lhs rhs = lhs >>= const rhs


--1

instance Functor Maybe where
    fmap  _ Nothing = Nothing
    fmap f (Just a) = Just (f a)

instance Applicative Maybe where
    pure = Just

    (<*>) (Just f) (Just x) = Just (f x)

    liftA2 op (Just a) (Just b) = Just $ a `op` b

instance Monad Maybe where 
    (>>=) Nothing _ = Nothing
    (>>=) (Just x) f = f x

    (>>) Nothing _ = Nothing
    (>>) _ (Just x) = Just x

--2

data Either a b = Left a | Right b
  deriving (Show)

instance Functor (Either c) where
    fmap _ (Left x) = Left x
    fmap f (Right x) = Right (f x)

instance Applicative (Either c) where
    pure = Right

    (<*>) (Right f) (Right x) = Right $ f x
    (<*>) (Left f) _ = Left f
    (<*>) (Right f) (Left x) = Left x

instance Monad (Either c) where
    (>>=) (Left x) _ = Left x
    (>>=) (Right x) f = f x

--3

newtype Sum a = Sum a
newtype Product a = Product a

instance Num a => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

instance Num a => Monoid (Sum a) where 
    mempty = Sum 0

instance Functor Sum where
    fmap f (Sum a) = Sum (f a)

instance Applicative Sum where
    pure = Sum

    Sum f <*> Sum a = Sum (f a) 

instance Monad Sum where
    Sum a >>= f = f a

--

instance Num a => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)

instance Num a => Monoid (Product a) where 
    mempty = Product 1

instance Functor Product where
    fmap f (Product a) = Product (f a)

instance Applicative Product where
    pure = Product

    Product f <*> Product a = Product (f a) 

instance Monad Product where
    Product a >>= f = f a

--4

data List a = Nil | Cons a (List a)

instance Semigroup (List a) where
    Nil <> xs = xs
    Cons x xs <> ys = Cons x (xs <> ys)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil

    Nil <*> _ = Nil
    (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
    Nil >>= _ = Nil
    (Cons x xs) >>= f = f x <> (xs >>= f)

--5

class BiFunctor bf where
    fmapbi :: (a -> b) -> (c -> d) -> bf a c -> bf b d
    fmapbi f g = second g . first f

    first :: (a -> b) -> bf a c -> bf b c
    first f = fmapbi f id

    second :: (c -> d) -> bf a c -> bf a d
    second g = fmapbi id g

instance BiFunctor (,) where
  fmapbi f g (x, y) = (f x, g y)