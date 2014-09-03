import System.Environment
import System.Directory
import System.IO 
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "view"   = view
dispatch "remove" = remove
dispatch "bump"   = bump
dispatch command  = doesntExist command

doesntExist :: String -> [String] -> IO ()
doesntExist command _ =
  putStrLn $ "The " ++ command ++ " command does't exist"
  
main :: IO ()
main = do 
  (command:argList) <- getArgs
  dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
        [0..] todoTasks
  mapM_ putStrLn numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newTodoItems
      hClose tempHandle
      removeFile fileName
      renameFile tempName fileName)

bump :: [String] -> IO ()
bump [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      number = read numberString
      priorityTask = todoTasks !! number
      newTasks = priorityTask : (delete priorityTask todoTasks)
      newTodoItems = unlines newTasks
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newTodoItems
      hClose tempHandle
      removeFile fileName
      renameFile tempName fileName)