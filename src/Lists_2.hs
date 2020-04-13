----- Problems 21 - 28: Lists again -----

module Lists_2 where

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
range a b = [a..b]

----- 23: Extract a given number of randomly selected elements from a list
-- λ> rnd_select "abcdefgh" 3 >>= putStrLn
-- eda

----- 24: Draw N different random numbers from the set 1..M.
-- λ> diff_select 6 49
-- [23,1,17,33,21,37]

----- 25: Generate a random permutation of the elements of a list.
-- λ> rnd_permu "abcdef"
-- "badcef"

----- 26: Generate the combinations of K distinct objects chosen from the N elements of a list
-- λ> combinations 3 "abcdef"
-- ["abc","abd","abe",...]
-- combinations :: Int -> [Char] -> [[Char]]
-- combinations = replicate
