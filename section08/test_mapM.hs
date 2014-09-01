import Control.Monad

main :: IO [()]
main = do
  mapM print [1..3]
  --mapM_ print [4..6]
