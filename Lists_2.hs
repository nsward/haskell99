----- Problems 21 - 28: Lists again -----

module Lists_2 where
import Data.List

----- 21: Insert an element at a given position in a list
-- λ> insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt y xs 1 = y:xs
insertAt y (x:xs) n = x : insertAt y xs (n - 1)
