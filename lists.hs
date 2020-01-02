---- Problems 1 - 10: Lists ----

---- 1: Find the last element of a list
-- λ> myLast [1,2,3,4]
-- 4
-- λ> myLast ['x','y','z']
-- 'z'
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

---- 2: Find the last but one element of a list.
-- λ> myButLast [1,2,3,4]
-- 3
-- λ> myButLast ['a'..'z']
-- 'y'
myButLast :: [a] -> a
myButLast xs
    | length xs < 2 = error "not enough elements"
    | otherwise = xs !! (length xs - 2)

---- 3: Find the K'th element of a list. The first element in the list is number 1.
-- λ> elementAt [1,2,3] 2
-- 2
-- λ> elementAt "haskell" 5
-- 'e'
elementAt :: [a] -> Int -> a
elementAt xs k
    | length xs < k = error "not enough elements"
    | otherwise = xs !! (k - 1)
