----- Problems 21 - 28: Lists again -----

module Lists_2 where
import Data.List

----- 21: Insert an element at a given position in a list
-- λ> insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt y xs 1 = y:xs
insertAt y (x:xs) n = x : insertAt y xs (n - 1)

----- 22: Create a list containing all integers within a given range
-- λ> range 4 9
-- [4,5,6,7,8,9]
range :: Int -> Int -> [Int]
-- range a b = [a..b]
range a b
    | a > b = []
    | otherwise = a : range (a + 1) b

