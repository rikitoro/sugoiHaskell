module Sample_14_05 where

import Control.Monad.State
import Control.Monad.Writer

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell ["Keeping " ++ show x]
    return True
  | otherwise = do
    tell [show x ++ " is too large"]
    return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x 
  | x > 9     = Nothing
  | otherwise = Just (acc + x)