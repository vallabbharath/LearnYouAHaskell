-- Part of 09_input_and_output.hs (but independent on its own)
-- Learn You a Haskell For Great Good
-- Chapter 9: Input and Output
-- http://learnyouahaskell.com/input-and-output

-- getContents takes from standard input; or file contents can be piped into it	
-- create an executable file out of this .hs file
-- Compile into an "exe" 
    -- > ghc --make interactPrinterCompact.hs  

-- Execution:
-- 1. (to give input in command line)
-- > interactPrinterCompact.exe
-- or
-- > runhaskell interactPrinterCompact.hs 

-- 2. (to take input from a text file)
-- > type someFile.txt | interactPrinterCompact.exe
-- > or
-- > type someFile.txt | runhaskell interactPrinterCompact.hs
    
main = interactPrinterCompact
    
interactPrinterCompact = interact $ unlines . filter ((<10) . length) . lines