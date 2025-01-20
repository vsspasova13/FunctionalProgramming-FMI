 import Prelude hiding (repeat, cycle, iterate)
 
 repeat :: a -> [a]
 repeat x = x : repeat x

 cycle :: [a] -> [a]
 cycle l = l ++ cycle l

 iterate :: (a -> a) -> a -> [a]
 iterate f z = z : iterate f (f z)

 fibs :: [Integer]
 fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

 notdiv :: Integral a => a -> [a]
 notdiv k = filter (\x -> x `mod` k > 0) [1..]

 