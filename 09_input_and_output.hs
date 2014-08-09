-- Learn You a Haskell For Great Good
-- Chapter 9: Input and Output
-- http://learnyouahaskell.com/input-and-output

-- Two ways of executing this haskell file
-- From Command line:

-- Method 1: Compile into an "exe" and run
    -- > ghc --make 09_input_and_output.hs  
    -- > ./09_input_and_output.exe

-- Method 2: Run from commandline without creating "exe"
    -- > runhaskell 09_input_and_output.hs

    
-- importing this to use functions like "when", "forever"
import Control.Monad    
import Data.Char
    
-- implementation of putStr using putChar
-- note the usage of "return" -- this is not a function return
-- return gives an empty IO() object.
putStr' :: String -> IO ()  
putStr' [] = return ()  
putStr' (x:xs) = do  
    putChar x  
    putStr' xs 
    
main = do   
-- putStr: Takes in a string parameter and returns an I/O action (to console)
-- putStrLn: Same as putStr, just adds a new line after the string
-- Indentation is very important in Haskell!
-- There cannot be multiple "main"
-- "main" is the keyword which makes I/O action performed; only when the I/O..
-- ..actions are given the name "main", they get performed
    putStr "First, "
    putStr "Second"  
    putStrLn " And Last!"   
-- putChar takes a character and returns an I/O action to print on terminal
-- Make a note of single quote for char; double quote implies [char] or string
    putChar 'A'
    putChar 'B'
    putChar 'C'
-- Checking the implementation of putStr'
    putStr' "Testing putStr'..."
    putStrLn "Testing Done! Works!"
-- print: Nothing but putStrLn . show
-- applies only on those which are instances of "Show"
    print [3, 4, 5]
-- same as
    print "The above print is same as the one below"
    putStrLn . show $ [3, 4, 5]


-- sequence
-- takes in a list of IO actions to be performed one after another
-- read in 3 lines from console and output.
    putStrLn "Type 3 sentences one after another!"
    lines <- sequence [getLine, getLine, getLine]
    print lines     -- lines will be a list of 3 strings

-- Try this!
    putStrLn "Here it is.. printed using sequence and map"
    sequence $ map print [1, 2, 3]
    
-- mapM is same as performing map and applying sequence on the same    
    putStrLn "Here it is.. printed using mapM"
    mapM print [1, 2, 3]    -- is same as the above command
    putStrLn "Here it is.. printed using mapM_"
    mapM_ print [1, 2, 3]   -- same, just the final empty prints removed.
    
-- Invocation of getch.  See implementation below    
    putStrLn "getch is invoked next! Type a few words and press enter"
    getch
    
-- Invotation of getchWhen. See implementation below
    putStrLn "\ngetchWhen is invoked next! Type a few words and press enter"
    getchWhen

-- Invocation of colorMapper - Explains forM
    colorMapper
    
-- Invocation of convertUpperInfinite. See implementation below    
    putStrLn "convertUpperInfinite is invoked next! Goes on infinitely.."
    putStrLn "Type few words and press enter"
    convertUpperInfinite
    
-- getChar
-- reads in a character input. Use '<-' to bind it to a name
getch = do
    c <- getChar
    if c /= ' '
        then do 
            putChar c
            getch
        else return()

-- "when" takes in a boolean and IO action
-- returns the same IO action when boolean is true
-- returns empty IO when boolean is false
-- Rewriting getch using when
getchWhen = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        getchWhen
-- So, "when" provides a simple way of encapsulating if ... then ... else        

-- convertUpperInfinite
convertUpperInfinite = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l
    
-- colorMapper
colorMapper = do   
    -- forM takes in a list and a function and then maps that function to each
    -- element in the list and then applies "sequence" on it.
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors   -- same as forM colors putStrLn
    
-- Usefullness of forM command
-- Make an I/O action for every element in this list. What each I/O action will 
-- do can depend on the element that was used to make the action. Finally, 
-- perform those actions and bind their results to something. 
-- We don't have to bind it, we can also just throw it away.    
