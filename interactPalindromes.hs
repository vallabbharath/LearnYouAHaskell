-- Part of 09_input_and_output.hs (but independent on its own)
-- Learn You a Haskell For Great Good
-- Chapter 9: Input and Output
-- http://learnyouahaskell.com/input-and-output

-- getContents takes from standard input; or file contents can be piped into it	
-- create an executable file out of this .hs file
-- Compile into an "exe" 
    -- > ghc --make interactPalindromes.hs  

-- Execution:
-- 1. (to give input in command line)
-- > interactPalindromes.exe
-- or
-- > runhaskell interactPalindromes.hs 

-- 2. (to take input from a text file)
-- > type someFile.txt | interactPalindromes.exe
-- > or
-- > type someFile.txt | runhaskell interactPalindromes.hs
    
respondPalindromes :: String -> String
respondPalindromes = unlines . map (\xs -> if isPalindrome xs 
                                           then "palindrome"
                                           else "not a palindrome") . lines  
                    where isPalindrome xs = xs == reverse xs  

-- Now, a function that takes input from Std IO and returns IO action                    
-- in similar fashion as interactPrinter
interactPalindromes = interact respondPalindromes

main = interactPalindromes