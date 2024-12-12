import Prelude hiding (span)
import Data.List(group,sort)


span :: (a->Bool) -> [a] -> ([a],[a])
span _ [] = ([],[])
span p (x:xs)
    |p x      = let (ys, zs) = span p xs
                 in (x:xs, zs)
    |otherwise = ([],x:xs)

mostFrequent [[]] = 0
mostFrequent l =
    let flatLists = concat l
        grouped = group (sort flatLists)
        frequences = map length grouped
        maxFreq = maximum frequences
        mostCommon = map head (filter (\g -> length g == maxFreq) grouped)
    in if length mostCommon == 1
        then head mostCommon
        else 0