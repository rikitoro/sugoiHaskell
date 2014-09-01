
main = do
  myPutStr "Hey, "
  myPutStr "I'm, "
  myPutStr "rikitoro!\n"

myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do
  putChar x
  myPutStr xs