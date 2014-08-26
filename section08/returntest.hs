main = do
  a <- return "hello"
  b <- return "yeah"
  putStrLn $ a ++ " " ++ b
