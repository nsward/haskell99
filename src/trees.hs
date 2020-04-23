import Data.List (sort)

----- Problems 54A - 60: Binary Trees -----

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

----- 54A: Check whether a given term represents a binary tree
-- The type system does this!

----- 55: Construct completely balanced binary trees. Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. Put the letter 'x' as information into all nodes of the tree.
-- cbalTree :: Char a => Int -> Tree a
cbalTree :: Int -> Tree Char
cbalTree 0 = Empty
cbalTree n = Branch 'x' (cbalTree $ ceiling half) (cbalTree $ floor half)
  where half = fromIntegral (n-1) / 2

----- 56: Symmetric Binary Trees
-- Write a predicate symmetric/1 to check whether a given binary tree is symmetric.
-- λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
-- False
-- λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
-- True
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r
  where
    mirror Empty Empty = True
    mirror (Branch _ l r) (Branch _ l' r') = mirror l l' && mirror r r'
    mirror _ _ = False

----- 57: Binary search trees
-- write a predicate to construct a binary search tree from a list of integer numbers.
-- λ> construct [3, 2, 5, 7, 1]
-- Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
construct :: Ord a => [a] -> Tree a
construct = build . halve . sort
  where
    halve xs = splitAt (length xs `div` 2) xs
    build (_,[]) = Empty
    build (l,r)  = Branch (head r) (build $ halve l) (build . halve $ tail r)

