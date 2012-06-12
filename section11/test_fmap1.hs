main = do line <- getLine
          let line' = reverse line
          putStrLn $ "you said " ++ line' ++ " backwards!"
