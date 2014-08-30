module Sample_06_02 where

import Data.List
import Data.Char
import qualified Data.Map as Map

wordsNums :: String -> [(String, Int)]
wordsNums = map (\ws -> (head ws, length ws)) . group . sort . words

wordsNums' :: String -> [(String, Int)]
wordsNums' xs = map (\ws -> (head ws, length ws)) (group (sort (words xs)))

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

---

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

