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
myButLast x
    | length x < 2 = error "not enough elements"
    | otherwise = x !! (length x - 2)
