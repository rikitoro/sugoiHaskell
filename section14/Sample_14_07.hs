module Sample_14_07 where

--import Control.Monad
import Data.List

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f)

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = filter onBoard
  [ (c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1), 
  (c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2) ] 
    where onBoard (c, r) = c `elem` [1..8] && r `elem` [1..8]

inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start