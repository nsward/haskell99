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
