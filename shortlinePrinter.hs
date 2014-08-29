-- Part of 09_input_and_output.hs (but independent on its own)
-- Learn You a Haskell For Great Good
-- Chapter 9: Input and Output
-- http://learnyouahaskell.com/input-and-output

-- getContents takes from standard input; or file contents can be piped into it	
-- create an executable file out of this .hs file
-- Compile into an "exe" 
    -- > ghc --make shortlinePrinter.hs  

-- Execution:
-- 1. (to give input in command line)
-- > shortlinePrinter.exe
-- or
-- > runhaskell shortlinePrinter.hs 

-- 2. (to take input from a text file)
-- > type someFile.txt | shortlinePrinter.exe
-- > or
-- > type someFile.txt | runhaskell shortlinePrinter.hs
        
main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result 