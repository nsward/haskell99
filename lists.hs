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

---- 4: Find the number of elements of a list.
-- λ> myLength [123, 456, 789]
-- 3
-- λ> myLength "Hello, world!"
-- 13
myLength :: [a] -> Int
myLength = foldr (\_ n -> n + 1) 0

---- 5: Reverse a list
myRev :: [a] -> [a]
myRev [] = []
myRev (x:xs) = myRev xs ++ [x]

myRev' :: [a] -> [a]
myRev' = foldr (\a b -> b ++ [a]) []

---- 6: Find out whether a list is a palindrome.
-- λ> isPalindrome [1,2,3]
-- False
-- λ> isPalindrome "madamimadam"
-- True
-- λ> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)
