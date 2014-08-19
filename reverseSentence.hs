-- Two ways of executing this haskell file
-- From Command line:

-- Method 1: Compile into an "exe" and run
    -- > ghc --make 09_input_and_output.hs  
    -- > ./09_input_and_output.exe

-- Method 2: Run from commandline without creating "exe"
    -- > runhaskell 09_input_and_output.hs
    
main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  


-- The following two are the same!
-- return takes in a value and bind it to IO
-- "<-" operator takes in a IO String and gets the value

main' = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b 
    
main'' = do  
    let a = "hell"  
        b = "yeah"  
    putStrLn $ a ++ " " ++ b    
    
    