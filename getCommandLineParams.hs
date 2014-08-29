-- Part of 09_input_and_output.hs (but independent on its own)
-- Learn You a Haskell For Great Good
-- Chapter 9: Input and Output
-- http://learnyouahaskell.com/input-and-output

-- create an executable file out of this .hs file
-- Compile into an "exe" 
    -- > ghc --make getCommandLineParams.hs  

-- Execution:
-- > getCommandLineParams.exe
-- or
-- > runhaskell getCommandLineParams.hs 

--(needs to imported at the beginning of the .hs file)
import System.Environment 

-- getArgs gets the command line arguments to a .hs program 
-- getArgs :: IO [String]
-- getProgName gets the name of the executable .hs file
-- getProgName :: IO String
-- Does not get anything in interactive mode

main = getCommandLineParams

getCommandLineParams = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName  
