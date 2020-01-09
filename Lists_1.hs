----- Problems 11 - 20: Lists, continued -----

module Lists_1 where
import Lists_0
import Data.List

----- 11: Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
-- λ> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']
data EncodedListItem a = Single a | Multiple Int a deriving (Show)

encodeMod :: Eq a => [a] -> [EncodedListItem a]
encodeMod = map tupleToEli . encode
    where
     tupleToEli (1, li) = Single li
     tupleToEli (n, li) = Multiple n li


----- 12: Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
-- λ> decodeModified
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeMod :: Eq a => [EncodedListItem a] -> [a]
decodeMod = concatMap decoder
    where
     decoder (Single a) = [a]
     decoder (Multiple n a) = replicate n a
