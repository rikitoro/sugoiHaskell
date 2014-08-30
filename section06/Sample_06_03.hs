module Sample_06_03 where

import qualified Data.Map as Map
import Data.Char

findKey' :: (Eq k) => k -> [(k,v)] -> v
findKey' key xs = snd . head . filter (\(k, v) -> key == k) $ xs

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v):xs)
    | key == k = Just v
    | otherwise = findKey key xs

phoneBook :: [(String, String)]
phoneBook = [("betty", "555-2938")
            ,("bonnie", "452-2928")
            ,("patsy", "493-2928")
            ,("lucille", "205-2928")
            ,("wendy", "939-8282")
            ,("penny", "853-2492")
            ]

findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key xs = foldr
                   (\(k, v) acc -> if key == k then Just v else acc)
                   Nothing xs

-------------

phoneBookMap :: Map.Map String String
phoneBookMap = Map.fromList $ phoneBook

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

phoneBook'' :: [(String, String)]
phoneBook'' = [("betty", "555-2938")
              ,("bonnie", "452-2928")
              ,("patsy", "493-2928")
              ,("patsy", "111-1111")
              ,("lucille", "205-2928")
              ,("wendy", "939-8282")
              ,("penny", "853-2492")
              ,("penny", "888-8888")
              ,("bonnie", "000-0000")
             ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
  where add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs
