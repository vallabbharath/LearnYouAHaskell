-- Part of 09_input_and_output.hs (but independent on its own)
-- Learn You a Haskell For Great Good
-- Chapter 9: Input and Output
-- http://learnyouahaskell.com/input-and-output

-- getContents takes from standard input; or file contents can be piped into it	
-- create an executable file out of this .hs file
-- Compile into an "exe" and run
    -- > ghc --make capslocker.hs  

-- create a sample file haiku.txt    
-- cat haiku.txt | ./capslocker     (on linux machines)
-- type haiku.txt | capslocker.exe  (on windows machines)
    
import Data.Char    
main = do
    contents <- getContents  
    putStr (map toUpper contents)

-- capslocker.exe works without requiring external file piping too
-- > capslocker.exe
-- this is a 
-- THIS IS A
-- sample output
-- SAMPLE OUTPUT