tellFortune :: String -> String
tellFortune "rikitoro" = "Great!!!"
tellFortune _ = "Bad"


main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Zis is your future: " ++ tellFortune name


