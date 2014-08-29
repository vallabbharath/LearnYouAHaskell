-- Learn You a Haskell For Great Good
-- Chapter 9: Input and Output
-- http://learnyouahaskell.com/input-and-output

-- Unlike the other chapters, the order does not follow the contents of the 
-- chapter; But, the notes for entire contents are captured.

-- Two ways of executing this haskell file
-- From Command line:

-- Method 1: Compile into an "exe" and run
    -- > ghc --make 09_input_and_output.hs  
    -- > ./09_input_and_output.exe

-- Method 2: Run from commandline without creating "exe"
    -- > runhaskell 09_input_and_output.hs

-- Method 3: From GHCI console
    -- > ghci> :l 09_input_and_output.hs
    -- > ghci> main         (run main function) - There can be only one!!
    
-- importing this to use functions like "when", "forever"
import Control.Monad    


import Data.Char
import System.IO
import System.IO.Error      -- needed for "catch"
import System.Directory
import Data.List

import System.Environment
import System.Random

-- ByteString related imports
import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S

-- In Haskell Code which are purely functional is separted from code which
-- has side effects (code which deals with IO action, sequenced execution etc)

-- IO action is performed when it is given a name of "main" and there can be 
-- a maximum of only one main
    
-- implementation of putStr using putChar
-- note the usage of "return" -- this is not a function return
-- return gives an empty IO() object(IO action of empty tuple).
putStr' :: String -> IO ()  
putStr' [] = return ()  
putStr' (x:xs) = do  
    putChar x  
    putStr' xs 
    
-- Make line-by-line execution possible, using the keyword "do"
-- The last IO action has a type IO() and that's what will be the return type.
-- main has a type of IO something. Try :t main
-- General Convention: No type declaration for main!
main = do   
    greetMe
    reverseSentence
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
-- print: Nothing but (putStrLn . show)
-- applies only on those which are instances of "Show"
    print [3, 4, 5]
-- same as
    print "The above print is same as the one below"
    putStrLn . show $ [3, 4, 5]

-- More details about "return"
    putStrLn "Exploring more on return keyword..."
    returnInFocus

-- sequence
-- takes in a list of IO actions to be performed one after another
-- returns an IO action 
-- read in 3 lines from console and output.
    putStrLn "Type 3 sentences one after another!"
    lines <- sequence [getLine, getLine, getLine]
    print lines     -- lines will be a list of 3 strings
-- The above is equivalent of 
{-
    a <- getLine  
    b <- getLine  
    c <- getLine  
    print [a,b,c]  
-}    

-- Try this!
    -- To transform a list of IO actions to a single IO action, use "sequence"
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
    
-- Invocation of convertUpperForever. See implementation below    
    putStrLn "convertUpperForever is invoked next! Goes on infinitely.."
    putStrLn "Type few words and press enter"
    convertUpperForever

--------------------------------------------------------------------------------
--          END OF MAIN
--------------------------------------------------------------------------------

-- Greeting!
greetMe = do
    putStr "Enter your name: "  -- Need not assign it to a name
    _ <- putStr "::"            -- Or, can be assigned to a dummy name
    
    -- getLine returns an IO string, that is bound to a name "firstName"
    -- note that, it is not "firstName = getLine"
    -- "firstName = getLine" is effectively giving another name for getLine!
    -- so, instead of "inName <- getLine", inName <- firstName can be used!!
    firstName <- getLine
    -- this last command gets automatically bound to greetMe
    -- And, you cannot bind the last IO action to a name!
    putStr "...Last name: "
    lastName <- getLine
    -- Use of "let" inside "do" block
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName     
    putStrLn $ "Hi " ++ bigFirstName ++ bigLastName   
    -- Note that we cannot do something like ((putStrLn "Hi " ++ getLine))
    -- "++" needs both of same type, not one String and another IO String
    
-- return is just a container! Takes something and returns it as IO action    
returnInFocus = do
    return ()       -- does not cause to return the do block; it keeps going!
    return "some string"    -- returns an IO action, not captured; thrown away;
    -- return can be considered as inverse function of "<-"
    -- return takes a value and wraps it in a box
    -- "<-" takes a box and takes the value out of it
    a <- return "another string"
    -- do block needs an IO action as a last statement. 
    return()
    
-- reverseSentence
reverseSentence = do
    putStrLn "Enter a sentence to reverse each word: "
    putStrLn "(You can stop this by entering a blank line!)"
    line <- getLine
    -- "else" is must for "if" in Haskell!
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            reverseSentence  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  

-- getChar
-- reads in a character input. Use '<-' to bind it to a name
getch = do
    c <- getChar
    if c /= ' '
        then do 
            putChar c
            getch
        else return()

-- "when" takes in a boolean and IO action (defined in Control.Monad)
-- just a normal function as anything else
-- inputs: a predicate and an IO action
-- returns the same IO action when the predicate is true
-- returns empty IO when the predicateis false
-- Rewriting getch using when
getchWhen = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        getchWhen
-- So, "when" provides a simple way of encapsulating if ... then ... else        

-- convertUpperForever
-- "forever" takes in an IO action
-- and keeps returning the same IO action, forever.
-- defined in Control.Monad
convertUpperForever = forever $ do  
    putStr "Give me some input: "
    -- getLine has a type IO String
    -- that is given a name "l" which has a type String!
    -- Note the use of "<-" here, the name "l" gets bound to getLine
    l <- getLine  
    putStrLn $ map toUpper l
    
-- colorMapper
-- to explain the "forM" function (defined in Control.Monad)
-- forM is very similar to mapM (but first parameter is list, 2nd is fn)
-- forM is mapM with parameter order reversed!
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

-- PART 2: FILES AND STREAMS
-- getContents
-- takes in everything from the standard input till End of File char Ctrl-D)
-- type:  getContents :: IO String
-- Lazy evaluation; does not get all contents and store; takes only when needed
-- Rewriting convertUpperForever using getContents
convertUpperGetcontents = do
    contents <- getContents  
    putStr (map toUpper contents)

-- getContents takes from standard input; or file contents can be piped into it	
-- see capslocker.hs (same functionality of convertUpperGetcontents given as a
-- separate file with main)

-- see shortlinePrinter.hs for another sample program using getContents
shortlinePrinter = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result 
    
-- So, it is observed that the following pattern is very common
-- Get some input as IO action ---> Process ---> Get the result out as IO action
-- There exists a special function for this
-- interact
-- :t interact      interact :: (String -> String) -> IO ()
-- Takes in a function of type (String -> String) and an IO action
-- The IO action takes input, applies the function and print out the result

-- shortlintPrinter is thus rewritten
interactPrinter = interact shortLinesOnly  

-- Or rewriting it in a single line
-- Have a look at interactPrinterCompact.hs, as well.
interactPrinterCompact = interact $ unlines . filter ((<10) . length) . lines

-- function that tells whether a string is palindrome or not
checkPalindromes :: String -> String
checkPalindromes contents = 
    unlines (map (\xs -> if isPalindrome xs 
                         then "palindrome" 
                         else "not a palindrome") (lines contents))  
    where   isPalindrome xs = xs == reverse xs  

-- Same function rewritten using function composition    
respondPalindromes :: String -> String
respondPalindromes = unlines . map (\xs -> if isPalindrome xs 
                                           then "palindrome"
                                           else "not a palindrome") . lines  
                    where isPalindrome xs = xs == reverse xs  

-- Now, a function that takes input from Std IO and returns IO action                    
-- in similar fashion as interactPrinter
interactPalindromes = interact respondPalindromes

-- To specifically read from a given file
readMe = do
    putStr "File to read and display: "
    fileName <- getLine
    handle <- openFile fileName ReadMode    -- Note "openfile" & "ReadMode"
    contents <- hGetContents handle         -- Note "hGetContents"
    putStr contents  
    hClose handle                           -- Note "hClose"

-- openfile     openFile :: FilePath -> IOMode -> IO Handle
-- FilePath     type synonym for String
    -- type FilePath = String
-- IOMode  (not IO Mode)
    -- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode  
-- hGetContents
    -- hGetContents :: Handle -> IO String
    -- like getContents, hGetContents too is lazy (reads only when needed)
    
-- Same readMe function rewritten using withFile function
-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- (Handle -> IO a)  function that takes a Handle and returns an IO action
-- usually implemented as a lambda function

readMeWithFile fileName = do
    withFile fileName ReadMode (\handle -> do  
        contents <- hGetContents handle
        putStr contents)
    
-- Implementation of withFile
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result
    
-- Other equivalent functions on file handle
-- Standard Input   vs  File Handle
-- getContents          hGetContents
-- getLine              hGetLine
-- putStr               hPutStr
-- putStrLn             hPutStrLn
-- getChar              hGetChar

-- readFile
-- readFile :: FilePath -> IO String    
readMeReadFile fileName = do  
    contents <- readFile fileName
    putStr contents  
    
-- Implementation of readFile using withFile
readFile' :: FilePath -> IO String
readFile' path = withFile path ReadMode (\handle -> do hGetContents handle)

readMeReadFile' fileName = do  
    contents <- readFile' fileName
    putStr contents  

-- writeFile
-- writeFile :: FilePath -> String -> IO ()
-- input: Path to a file and String to write to
-- output: IO action that does the writing
-- NOTE:  Overwrites an existing file. 
-- Takes input from say "haiku.txt", makes caps and writes into "haiku.txt.caps"
writeFileCaps fileName = do
    contents <- readFile fileName
    writeFile (fileName ++ ".caps") (map toUpper contents)
    
-- appendFile
-- appendFile :: FilePath -> String -> IO ()
-- same as writeFile; but does not truncate the file to zero size before writing
-- if a file exists, appends to the same

-- making a todo list, using appendFile
todoList = do     
    todoItem <- getLine  
    -- keeps adding to "todo.txt"
    appendFile "todo.txt" (todoItem ++ "\n") 

-- hSetBuffering
-- hSetBuffering :: Handle -> BufferMode -> IO ()
-- BufferMode can be NoBuffering, LineBuffering or BlockBuffering (Maybe Int)
-- usually the buffer size is a line - reads line by line
-- NoBuffering: reads a single character at a time
-- LineBuffering: reads a single line at a time
readCustomBuffer = do   
    withFile "haiku.txt" ReadMode (\handle -> do  
        hSetBuffering handle $ BlockBuffering (Just 2048)  
        contents <- hGetContents handle  
        putStr contents) 
-- reading bigger chunks helps minimizing the disk accesses

-- hFlush
-- hFlush :: Handle -> IO ()
-- generally, the flush happens after the end of a block read (or line read)
-- hFlush can be considered as a manual flush

-- todoDelete
-- presents the contents of the file "todo.txt"
-- asks for the line to delete; and deletes the line
todoDelete = do        
    handle <- openFile "todo.txt" ReadMode  
    -- openTempFile
    -- openTempFile :: FilePath -> String -> IO (FilePath, Handle)
    -- "getCurrentDirectory" can be used ".", but "." works on any os
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    -- removeFile :: FilePath -> IO ()
    removeFile "todo.txt"  
    -- renameFile :: FilePath -> FilePath -> IO ()
    renameFile tempName "todo.txt"
    
-- PART 3
-- COMMAND LINE ARGUMENTS
-- import System.Environment (imported at the beginning of the .hs file)

-- getArgs gets the command line arguments to a .hs program 
-- getArgs :: IO [String]
-- getProgName gets the name of the executable .hs file
-- getProgName :: IO String
-- Does not get anything in interactive mode
-- see getCommandLineParams.hs for more details
getCommandLineParams = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName  

-- Okay, now rewriting the todoManager as a separate program
-- with the use of command line parameters
-- with the options of add, view and remove
-- see todoManager.hs for more details

-- PART 4
-- RANDOMNESS
-- Most important feature of Haskell is a function has a definitive output
-- So, how to get a random number?!
-- Feature available with System.Random module
-- But, why talking about Random in Input / Output chapter? Will Explore!

-- random
-- random :: (RandomGen g, Random a) => g -> (a, g)
-- Simply put, takes in a g which is of RandomGen data type
-- And gives out a pair (a, g) where 'a' is of Random data type
-- "Random" is a class which has many instances like Random Int, Random Bool ...
-- try :info Random

-- "random" function needs a RandomGen value, which can be obtained using
-- "mkStdGen"
-- mkStdGen :: Int -> StdGen
-- StdGen is an instance of the class RandomGen
-- try :info RandomGen and :info StdGen

-- randNum = random (mkStdGen 100) -- This cannot be given as it needs to be
-- explicitly told what is the data type of the random output
randInt = random (mkStdGen 100) :: (Int, StdGen)
-- (-1352021624,651872571 1655838864)
-- first of the tuple is random number, the second part is the representation
-- of the random generator

-- try the following
-- > mkStdGen 100
-- > random(read("101 1") :: StdGen) :: (Int, StdGen)

randBool = random (mkStdGen 100) :: (Bool, StdGen)
-- (True,4041414 40692)

randFloat = random (mkStdGen 100) :: (Float, StdGen)
-- (0.41323137,651872571 1655838864)

-- Function to toss a coin 3 times (Or tossing 3 coins together)
-- A StdGen is provided as input; that produces new StdGen value which
-- is given as the input for the next tossing
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    -- random gen :: (Bool, StdGen) not needed, already inferred from 
    -- the function type signature
    let (firstCoin, newGen) = random gen        
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)  

-- using mkStdGen function    
tossOut1 = threeCoins (mkStdGen 100)

-- using read and explicit StdGen typing
tossOut2 = threeCoins (read ("101 1") :: StdGen)
-- both tossOut1 and tossOut2 will give the same output

-- randoms
-- randoms :: (RandomGen g, Random a) => g -> [a]
-- input: a random generator 'g'
-- output: array of infinite random numbers (of any type, as specified)
rand10Int = take 10 $ randoms (mkStdGen 11) :: [Int]
rand8Bool = take 8  $ randoms (mkStdGen 111) :: [Bool]

-- implementation of randoms function
-- that generates an infinite list of random numbers
randoms' :: (RandomGen g, Random a) => g -> [a]  
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen 

-- finiteRandoms (defined below)
-- custom function to create finite random numbers
finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)  
finiteRandoms 0 gen = ([], gen)  
finiteRandoms n gen =   
    let (value, newGen) = random gen  
        (restOfList, finalGen) = finiteRandoms (n - 1) newGen  
    in  (value:restOfList, finalGen)  
    
-- usage: 
-- need to tell explicitly the random numbers to be returned are of which type    
rand10Num = finiteRandoms 10 (mkStdGen 11) :: ([Int], StdGen)

-- randomR (built-in)
-- to specify random number within a given range
rand1to6 = randomR (1, 6) (mkStdGen 359353) :: (Int, StdGen)
randTtoF = randomR (True, False) (mkStdGen 12234134) :: (Bool, StdGen)

-- randomRs (built-in)
-- gives out infinite random numbers within a given range
randWord = take 10 $ randomRs ('a', 'z') (mkStdGen 200) :: [Char]


-- getStdGen (built-in)
-- getStdGen :: IO StdGen
-- takes in a random generator whenever the program starts (not from user)
-- And, this is why, Random functions are discussed in IO section
tellPassword = do  
    gen <- getStdGen  
    putStrLn $ take 10 (randomRs ('a', 'z') gen)  

-- keep executing tellPassword, it keeps spitting out new random words.    
-- okay, it does not. because getStdGen returns same value every time

-- newStdGen generates new random generator whenever called
-- so, this can be used to keep creating new passwords!
createPassword = do 
    gen <- newStdGen  
    putStrLn $ take 10 (randomRs ('a', 'z') gen)  

-- Function to make user guess the number which computer thinks!
-- guessNumber
-- guessNumber :: IO()
guessNumber = do  
    gen <- getStdGen  
    askForNumber gen  
  
askForNumber :: StdGen -> IO ()  
askForNumber gen = do  
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)  
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number   
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        askForNumber newGen 
        
-- PART 5
-- BYTESTRINGS
-- The data is read in terms of byte-strings; Each element is one byte (8 bits)
-- Two Flavours: Strict and Lazy
-- Strict: 
-- No laziness; No promise to read later kind of stories; 
-- Thunk is a technical term for "the promise to read later"
-- Read into memory then and there!

-- Lazy:
-- Read in terms of chunks (not thunks) - Each chunk is 64K bytes in size
-- Till that 64K, it is just a promise to read that many bytes (until it is 
-- absolutely necessary). Read when necessary or wait till 64K
-- Once the first 64K read, wait till next 64K

-- import the ByteString related modules
-- done at the start of this file

-- ByteString functions

-- pack  (remember B and S are qualified module imports)
-- pack :: [Word8] -> B.ByteString or
-- pack :: [Word8] -> S.ByteString

canPack = B.pack [99,97,110]
atozPack = B.pack [97..122]
atozPack1 = B.pack [353..378] -- same, as Word8 rounds up 256 to 0

unpackedWord8 = B.unpack (read "Haskell" :: B.ByteString)

-- fromChunks
-- takes a list of strict bytestrings and converts them into a lazy bytestring
-- toChunks
-- takes a lazy bytestring and converts into a list of strict bytestrings

lazyOne = B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]
strictOnes = B.toChunks lazyOne

-- cons
-- Lazy implementation of ":"
-- B.cons :: GHC.Word.Word8 -> B.ByteString -> B.ByteString
-- S.cons :: GHC.Word.Word8 -> S.ByteString -> S.ByteString
-- takes a byte and a bytestring and puts the byte in the beginning
scanCons = B.cons 83 $ B.pack [99, 97, 110]         -- "Scan"
-- Chunk "S" (Chunk "can" Empty)  

-- cons is lazy
-- uses a new chunk even if the old chunk is not full
-- better to use the stricter version

-- cons'
-- Strict implementation of ":"
-- B.cons' :: GHC.Word.Word8 -> B.ByteString -> B.ByteString
-- S.cons' :: GHC.Word.Word8 -> S.ByteString -> S.ByteString
scanConsStrict = B.cons' 83 $ B.pack [99, 97, 110]         -- "Scan"
-- Chunk "Scan" Empty  

zeroToNineMultiChunk = foldr B.cons B.empty [48..57]
zeroToNineSingleChunk = foldr B.cons' B.empty [48..57]  

-- B.empty
-- S.empty  -- creates empty bytestrings

-- The bytestring modules have functions that are analogous to fns in Data.List
-- head, tail, init, null, length, map, reverse, foldl, foldr, 
-- concat, takeWhile, filter, etc.

-- readFile of System.IO
-- readFile :: FilePath -> IO String

-- readFile of Data.ByteString
-- readFile :: FilePath -> IO ByteString

-- Example
-- Function to copy a file
-- see byteStringCopy.hs

-- Important note:
-- Implementation using ByteStrings gives possible performance boost!

-- PART 6
-- EXCEPTIONS

-- C language returns (by convention) null pointer or -1 in case of error
-- Java use exceptions to handle failure cases
-- When exception occurs in Java, it is caught using "catch" block where the 
-- control flow sits to handle those exceptions or, throws it to get handled
-- somewhere else

-- Haskell supports "exceptions" - helps a lot in IO handling
-- For example, a simple act like opening a file on the system can fail because
-- of various possible reasons

-- impure code (like IO) throws exceptions
-- even, pure code (purely functional) too throws exceptions
-- examples: 
-- head []  (head function on an empty list)
-- 2 `div` 0 (divide by zero)

-- NOTE:
-- Even, if the pure code throws exceptions, it can be caught only in the IO
-- part of the code - because the functions are lazy and unless it is invoked
-- (usually by IO) - it remains as it is without getting invoked and causing
-- exceptions

lengthOfFile = do 
    (fileName:_) <- getArgs  
    contents <- readFile fileName  
    putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  

-- this throws following error if operated on file that does not exist
-- " openFile: does not exist (No such file or directory)" 


-- Rewriting the same to check whether the file exists, before opening
-- "doesFileExist" is defined in System.Directory
-- doesFileExist :: FilePath -> IO Bool
lengthFileAfterChecking = do 
    (fileName:_) <- getArgs  
    fileExists <- doesFileExist fileName  
    if fileExists  
        then do contents <- readFile fileName  
                putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
        else do putStrLn "The file doesn't exist!"  
        
        
-- Implementing the same, using "catchIOError " function in Haskell
-- catchIOError   :: IO a -> (IOError -> IO a) -> IO a
-- catchIOError  takes two parameters
-- an IO action (IO a) and a function that takes IO error and returns IO action
-- the function that takes IO error and returns IO action is called "handler"
-- catchIOError  returns an IO action
lengthFileWithCatch = toTry `catchIOError` handler  
toTry :: IO ()  
toTry = do 
    (fileName:_) <- getArgs  
    contents <- readFile fileName  
    putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
handler :: IOError -> IO ()  
handler e = putStrLn "Whoops, had some trouble!"  

-- In the above code, the handler does not categorize the error
-- the error message is not meaningfull, though there is power in Haskell to 
-- make it meaningful
-- So, rewriting the same, to check for particular error message "e" in handler
lengthFileSensibleHandler = toTry1 `catchIOError` handler1  
toTry1 :: IO ()  
toTry1 = do 
    (fileName:_) <- getArgs  
    contents <- readFile fileName  
    putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
-- isDoesNotExistError takes error and returns True or False
-- isDoesNotExistError :: IOError -> Bool. 
handler1 :: IOError -> IO ()  
handler1 e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise = ioError e         -- rethrowing the error back!
                                    -- rethrow if it does not match any of the
                                    -- possible deterministic criteria
-- ioError     
-- ioError :: IOException -> IO a
-- takes an IOException and produces and IO action that throws the exception

-- Other predicates that acts on IOError
{-
isAlreadyExistsError
isDoesNotExistError
isAlreadyInUseError
isFullError
isEOFError
isIllegalOperation
isPermissionError
isUserError
-}

-- Attributes of IO Errors
-- to ask more info from IO errors
-- See in detail, @ 
-- http://www.haskell.org/ghc/docs/6.10.1/html/libraries/base/System-IO-Error.html#3
-- Example:
-- ioeGetFileName 
-- ioeGetFileName :: IOError -> Maybe FilePath
ioeIllustration = toTry2 `catchIOError` handler2     
toTry2 :: IO ()     
toTry2 = do 
    (fileName:_) <- getArgs     
    contents <- readFile fileName     
    putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"     
handler2 :: IOError -> IO ()     
handler2 e     
    | isDoesNotExistError e =   
        case ioeGetFileName e of 
            Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
            Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e   
    