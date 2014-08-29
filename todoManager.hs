-- Part of 09_input_and_output.hs (but independent on its own)
-- Learn You a Haskell For Great Good
-- Chapter 9: Input and Output
-- http://learnyouahaskell.com/input-and-output

-- create an executable file out of this .hs file
-- Compile into an "exe" 
    -- > ghc --make todoManager.hs  

-- Execution:
-- > todoManager.exe add "todo.txt" 
-- or
-- > runhaskell todoManager.hs 

import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
  
-- association list with command line arguments as keys and 
-- function names as corresponding values
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [("add", add), ("view", view), ("remove", remove)]  
   
-- By convention, type declaration is not usually written for "main"   
main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  

-- to add a todo item in the given file
-- runhaskell todoManager.hs add todo.txt "This is the task to be added!"
add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")  

-- to view the existing todo list in the given file
-- runhaskell todoManager.hs view todo.txt
view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  
  
-- to remove a specific item in the todo list in the given file
-- runhaskell todoManager.hs remove todo.txt 1
remove :: [String] -> IO ()
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName
    