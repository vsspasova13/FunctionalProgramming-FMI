{-# LANGUAGE LambdaCase #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use foldr" #-}

module ListMaybe where

import Prelude hiding (all, and, concat, drop, filter, length, map, null, product, replicate, reverse, subtract, sum, take, zip, zipWith, (++))

-- TODO:
-- ask about problems with homework (see README, remind about formatter, etc)
-- remind about doing PRs as early as possible to get feedback (demonstrate feedback)
-- remind about new homework (maybe will be released before end of week)

-- f :: Integer -> (Integer -> (Integer -> Integer))
-- f a b c = a + b + c

-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (.) = \f g x -> f (g x)

-- f ∘ g
-- o
-- .

-- >>> print $ f $ g $ h x
-- >>> f' = print . f . g . h
-- >>> f' x = print $ f $ g $ h x
-- >>> f' = \x -> print $ f $ g $ h x
-- >>> print (f (g (h x)))
-- >>> (f 1) $ (2 3)
-- 6

-- remind about currying
-- remind about ($), (.), types

-- Lists
-- ask about linked list, explain what they are
-- show definition, contrast with builtin (hoogle workflow)
-- data List
-- sumList, replicate
-- explain naming convention (akin to scheme/lisp in general)
-- list comprehension (!!!)
-- cartesianProd with list comprehension?
--
-- brief mention of Strings (C mentioned??????)

-- data List a = Nil | Cons a (List a)
-- data [] a = [] | a : (List a)

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x : xs) = x + sumList xs

-- >>> sumList [1, 2, 3]
-- 6

replicate :: Integer -> a -> [a]
replicate 0 x = []
replicate n x = x : replicate (n - 1) x

-- >>> :t replicate 10 ('a', [(0 :: Integer)])
-- replicate 10 ('a', [(0 :: Integer)]) :: [(Char, [Integer])]

-- Maybe
-- explicit permission for a value to be missing
-- usually called nullability (Kotlin, C#) (why not Java, js?)
-- show definition
-- kind of like pointers, except the compiler makes you always check for nullability
-- mention how Rust Option works
-- used for error handling
-- safeDiv, headMaybe

-- data Maybe a = Just a | Nothing

safeDiv2 :: Integer -> Maybe Integer
safeDiv2 n =
  if even n
  then Just $ n `div` 2
  else Nothing

-- >>> safeDiv2 4
-- Just 2

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead [] = Nothing

safeHead' :: [a] -> [a]
safeHead' (x : _) = [x]
safeHead' [] = []


-- NOTE: try doing some exercises with recursion and some with list comprehension (or both)

-- EXERCISE
-- Generate all the numbers in the ("mathematical range") [n, m] in a list (inclusive).
-- EXAMPLES
-- >>> listFromRange 3 12
-- [3,4,5,6,7,8,9,10,11,12]
-- >>> listFromRange 8 6
-- []
listFromRange :: Integer -> Integer -> [Integer]
listFromRange a b = if a > b then []
                    else a : (listFromRange (a + 1) b)

-- EXERCISE
-- Multiply all the elements of a list
-- EXAMPLES
-- >>> product [2,4,8]
-- 64
-- >>> product []
-- 1
product :: [Integer] -> Integer
product [] = 1
product (a : xs) = a * (product xs)

-- EXERCISE
-- Implement factorial with prod and listFromRange
fact :: Integer -> Integer
fact 1 = 1
fact x = product (listFromRange 1 x);

-- EXERCISE
-- Return a list of the numbers that divide the given number.
-- EXAMPLES
-- >>> divisors 5
-- [1,5]
-- >>> divisors 64
-- [1,2,4,8,16,32,64]
-- >>> divisors 24
-- [1,2,3,4,6,8,12,24]

f :: Integer -> Integer -> [Integer]
f x i = if i == x then [x]
        else  if (x `rem` i == 0) then i : (f x (i + 1)) 
        else (f x (i + 1)) 

divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors x = f x 1

-- EXERCISE
-- Implement prime number checking using listFromRange and divisors
-- EXAMPLES
-- >>> isPrime 7
-- True
-- >>> isPrime 8
-- False
isPrime :: Integer -> Bool
isPrime x = sumList (divisors x) == x + 1

-- EXERCISE
-- Get the last element in a list.
-- EXAMPLES
-- >>> lastMaybe []
-- Nothing
-- >>> lastMaybe [1,2,3]
-- Just 3
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [a] = Just a
lastMaybe (a : xs) = lastMaybe xs

-- EXERCISE
-- Calculate the length of a list.
-- EXAMPLES
-- >>> length [1,2,8]
-- 3
-- >>> length []
-- 0
length :: [a] -> Integer
length [] = 0
length (a : xs) = 1 + (length xs)

-- EXERCISE
-- Return the nth element from a list (we count from 0).
-- If n >= length xs, return a Nothing
-- EXAMPLES
-- >>> ix 2 [1,42,69]
-- Just 69
-- >>> ix 3 [1,42,69]
-- Nothing
ix :: Integer -> [a] -> Maybe a
ix n [] = Nothing
ix 0 (a : xs) = Just a
ix n (a : xs) = if n >= length (a : xs) then Nothing
                else (ix (n - 1) xs)


-- EXERCISE
-- "Drop" the first n elements of a list.
-- If n > length xs, then you should drop them all.
-- EXAMPLES
-- >>> drop 5 $ listFromRange 1 10
-- [6,7,8,9,10]
-- >>> drop 20 $ listFromRange 1 10
-- []

drop :: Integer -> [a] -> [a]
drop n (a : xs) = if n > length (a : xs) then []
                  else if (n == 1) then xs
                  else (drop (n-1) xs)

-- EXERCISE
-- "Take" the first n elements of a list.
-- If n > length xs, then you should take as many as you can.
-- EXAMPLES
-- >>> take 5 $ listFromRange 1 10
-- [1,2,3,4,5]
-- >>> take 20 $ listFromRange 1 10
-- [1,2,3,4,5,6,7,8,9,10]
take :: Integer -> [a] -> [a]
take n (a : xs) = if n == 0 then []
                  else if n > (length (a : xs)) then (a : xs)
                  else a : (take (n - 1) xs)

-- EXERCISE
-- Append one list to another. append [1,2,3] [4,5,6] == [1,2,3,4,5,6]
-- This is called (++) in the base library.
-- HINT: Of course, you can think in the classic "inductive" way - I've got the result - what do I need to do at this step.
-- Or alternatively, you can think about "placing the second list at the end of the first":
-- 0. how do you get to the end of the first one?
-- 1. what do you need to do at each step to "remember" the elements of the first one?
-- EXAMPLES
-- >>> append [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]
-- >>> append [] [4,5,6]
-- [4,5,6]
append :: [a] -> [a] -> [a]
append [] [] = []
append [] (b : ys) = b : (append [] ys)
append (a : xs) (b : ys) = a : (append xs (b : ys))


-- EXERCISE
-- Concatenate all the lists together.
-- EXAMPLES
-- >>> concat [[1,2,3], [42,69], [5,7,8,9]]
-- [1,2,3,42,69,5,7,8,9]
-- >>> concat [[1,2,3], [], [5,7,8,9]]
-- [1,2,3,5,7,8,9]
-- >>> concat []
-- []
concat :: [[a]] -> [a]
concat [] = []
concat (a : xs) =  append a (concat xs)

-- EXERCISE
-- Reverse a list. It's fine to do this however you like.
-- EXAMPLES
-- >>> reverse [1,2,3]
-- [3,2,1]
-- >>> reverse []
-- []
reverse :: [a] -> [a]
reverse = undefined

-- EXERCISE
-- Square all the numbers in a list
-- EXAMPLES
-- >>> squareList [1,2,3,5]
-- [1,4,9,25]
sqarr :: Integer -> Integer
sqarr x = x * x

squareList :: [Integer] -> [Integer]
squareList [] = []
squareList (a : xs) = (sqarr a) : (squareList xs)

-- EXERCISE
-- Pair up the given element with each of the elements a list.
-- EXAMPLES
-- >>> megaPair 42 [69,7,42]
-- [(42,69),(42,7),(42,42)]
megaPair :: a -> [b] -> [(a, b)]
megaPair x [] = []
megaPair x (a : xs) = (x,a) : (megaPair x xs)

-- EXERCISE
-- Both of those functions above have the same structure - apply a function to each element of a list.
-- We can abstract this and get one of the most useful functions over lists (and containers in general).
-- EXAMPLES
-- >>> map succ [1,2,3]
-- [2,3,4]
-- >>> map (\x -> x * x) [1,2,3] -- same as squareList
-- [1,4,9]
-- >>> map (\x -> (3,x)) [1,2,3] -- same as megaPair 3
-- [(3,1),(3,2),(3,3)]
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a : xs) = (f a) : (map f xs)

-- EXERCISE
-- Check if all the elements in a list are True.
-- EXAMPLES
-- >>> and []
-- True
-- >>> and [False]
-- False
-- >>> and [True, True]
-- True
and :: [Bool] -> Bool
and [] = True 
and (a: xs) = undefined

-- EXERCISE
-- Check if all the elements of a list satisfy a predicate
-- Implement this using map and and.
-- EXAMPLES
-- >>> all isPrime [2,3,7]
-- True
-- >>> all isPrime [1,2,3,7]
-- False
all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p (a : xs) = undefined

-- EXERCISE
-- Implement the cartesian product of two lists.
-- Don't use a list comprehension!
-- >>> cartesian [1,2,3] [4,5,6]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- >>> cartesian [] [4,5,6]
-- []
-- >>> cartesian [1,2,3] []
-- []
cartesian :: [a] -> [b] -> [(a, b)]
cartesian = undefined

-- EXERCISE
-- We can generalise cartesian to work with arbitrary functions instead of just (,),
-- taking elements "each with each"
-- This is also the generalisation of cartesian, as seen in the examples.
-- EXAMPLES
-- >>> lift2List (+) [1] [2]
-- [3]
-- >>> lift2List (+) [1,2] [2]
-- [3,4]
-- >>> lift2List (*) [1,2,3] [1,2]
-- [1,2,2,4,3,6]
-- >>> lift2List (,) [1,2,3] [4,5,6] -- same as cartesian [1,2,3] [4,5,6]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
lift2List :: (a -> b -> c) -> [a] -> [b] -> [c]
lift2List = undefined

-- EXERCISE
-- The "filtering" part of a list comprehension - leave only those elements, that satisfy the given predicate.
-- EXAMPLES
-- >>> even 2
-- True
-- >>> even 3
-- False
-- >>> filter even [1..10]
-- [2,4,6,8,10]
-- >>> filter isPrime [1..20]
-- [2,3,5,7,11,13,17,19]
filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

data Digit
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Show, Enum)

-- EXERCISE
-- Parse a character into a digit.
-- The easiest way is to pattern match on all the cases.
--
-- OPTIONAL: You can try to use the LambdaCase extension here, in order to avoid writing parseDigit 10 times:
-- First, you need to add {-# LANGUAGE LambdaCase #-} to the top of the file (it's already enabled in this file)
-- Second, lambda case is used as follows:
-- Every time you write @\case ...@, that is desugared into @\x -> case x of ...@
-- For example:
-- safeDiv n = \case
--   0 -> Nothing
--   m -> ...
--
-- EXAMPLES
-- >>> parseDigit '6'
-- Just Six
-- >>> parseDigit '9'
-- Just Nine
-- >>> parseDigit 'c'
-- Nothing
parseDigit :: Char -> Maybe Digit
parseDigit = undefined

-- EXERCISE
-- See if all the values in a list xs are Just, returning Just xs only if they are.
-- We can think of this as all the computations in a list "succeeding",
-- and therefore the entire "computation list" has "succeeded.
-- Note that it is vacuously that all the elements in the empty list are Just.
-- EXAMPLES
-- >>> validateList []
-- Just []
-- >>> validateList [Just 42, Just 6, Just 9]
-- Just [42,6,9]
-- >>> validateList [Nothing, Just 6, Just 9]
-- Nothing
-- >>> validateList [Just 42, Nothing, Just 9]
-- Nothing
-- >>> validateList [Just 42, Just 6, Nothing]
-- Nothing
validateList :: [Maybe a] -> Maybe [a]
validateList = undefined

-- EXERCISE
-- You often have a collection (list) of things, for each of which you want to
-- perform some computation, that might fail (returning Maybe).
-- Let's implement a function to do exactly this -
-- execute a "failing computation" for all the items in a list,
-- immediately "aborting" upon a failure.
-- Think about how to reuse validateList.
-- This is called traverseListMaybe, because it's a special case of a generic function called traverse
-- that performs "actions" for each element of a "collection", specialised to List and Maybe
-- EXAMPLES
-- >>> traverseListMaybe (\x -> if even x then Just x else Nothing) [2,4,6]
-- Just [2,4,6]
-- >>> traverseListMaybe (\x -> if even x then Just x else Nothing) [1,2,3]
-- Nothing
-- >>> traverseListMaybe (5 `safeDiv`) [0,2]
-- Nothing
-- >>> traverseListMaybe (8 `safeDiv`) [3,2]
-- Just [2,4]
traverseListMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe = undefined

-- EXERCISE
-- Convert a list of digits to a number.
--
-- You can use @toEnum :: Digit -> Int@ to convert digit into an @Int@
-- and @fromIntegral :: Int -> Integer@ to convert an @Int@ into an @Integer@
--
-- Assume that the empty list converts to 0.
-- HINT: It might be easier to first reverse the list and then operate on it with a helper.
-- EXAMPLES
-- >>> digitsToNumber [Six,Nine]
-- 69
-- >>> digitsToNumber [One,Two,Zero]
-- 120
-- >>> digitsToNumber [Zero,One,Two,Zero]
-- 120
digitsToNumber :: [Digit] -> Integer
digitsToNumber = undefined
  where
    -- for some reason, we often call helpers in haskell "go", as in "go do the thing"
    go = undefined

-- EXERCISE
-- Combine the previous functions to parse a number.
-- EXAMPLES
-- >>> parseNumber "0"
-- Just 0
-- >>> parseNumber "3"
-- Just 3
-- >>> parseNumber "69"
-- Just 69
-- >>> parseNumber "0123"
-- Just 123
-- >>> parseNumber "blabla"
-- Nothing
-- >>> parseNumber "133t"
-- Nothing
parseNumber :: String -> Maybe Integer
parseNumber = undefined

-- EXERCISE
-- Notice how in parseNumber, in the Nothing case we returned Nothing,
-- and in the Just case, we returned Just again, with a "non-maybe" function inside.
-- This turns out to be very useful, and if you compare it to the map for lists, it's almost the same.
-- Let's write it now, so we don't have to do that pattern match again in the future.
-- Afterwards, you can reuse this function in parseNumber.
-- EXAMPLES
-- >>> maybeMap succ $ Just 5
-- Just 6
-- >>> maybeMap succ Nothing
-- Nothing
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap = undefined

-- EXERCISE
-- Another way to combine lists
-- Instead of "taking all possible combinations" we group the lists "pointwise"
-- If one list is shorter than the other, you can stop there.
-- EXAMPLES
-- >>> zip [1,2,3] [4,5,6]
-- [(1,4),(2,5),(3,6)]
-- >>> zip [1,2] []
-- []
-- >>> zip [1] [4,5,6]
-- [(1,4)]
zip :: [a] -> [b] -> [(a, b)]
zip = undefined

-- EXERCISE
-- And the generalised version of zip.
-- EXAMPLES
-- >>> zipWith (,) [1,2,3] [4,5]
-- [(1,4),(2,5)]
-- >>> zipWith (+) [1,2,3] [4,5,6]
-- [5,7,9]
-- >>> zipWith (:) [1,2,3] [[4],[5,7],[]]
-- [[1,4],[2,5,7],[3]]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = undefined

-- EXERCISE
-- Transpose a matrix. Assume all the inner lists have the same length.
-- HINT: zipWith and map might be useful here.
-- EXAMPLES
-- >>> transpose [[1]]
-- [[1]]
-- >>> transpose [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]
-- >>> transpose [[1],[2]]
-- [[1,2]]
-- >>> transpose [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
transpose :: [[a]] -> [[a]]
transpose = undefined

-- EXERCISE
-- Reverse a list, but in linear time (so if the input list has n elements, you should only be doing at most ~n operations, not n^2)
-- You will need a helper local definition.
-- EXAMPLES
-- >>> reverse [1,2,3]
-- [3,2,1]
-- >>> reverse []
-- []
reverseLinear :: [a] -> [a]
reverseLinear = undefined
