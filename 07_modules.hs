-- Chapter 7: Modules
-- http://learnyouahaskell.com/modules#loading-modules
-- Module: Collection of related types, typeclasses and functions.

-- The core module in Haskell is "Prelude" which is imported by default.

-- To specifically import a module, use "import <module name>"

-- Module to be imported to work with lists.
import Data.List
-- Now all exposed functions in Data.List become available in global namespace.

-- Importing specific functions from a module
import Data.List (nub, sort)

-- Importing all functions in a list leaving a few
import Data.List hiding (nub)

-- IMPORTANT: All import statements to be written at the start of the file.

-- Qualified Import
import qualified Data.List
-- Now, functions like "nub" cannot be called directly.  Use Data.List.nub
import qualified Data.List as DL
-- Now use DL.nub to call "nub" function

-- Complete index of Modules
-- http://www.haskell.org/ghc/docs/latest/html/libraries/ 

-- Search for Modules or APIs here
-- Hoogle: http://haskell.org/hoogle
-- Hayoo: http://holumbus.fh-wedel.de/hayoo/hayoo.html 


-- nub is a function exposed by Data.List to remove duplicates
unique_arunram = nub "arunram" -- returns "arunm"

-- Writing a function to count number of unique elements
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub 
num_unique_arunram = numUniques "arunram" -- returns 5

-- Loading from ghci console - Use ":m +"
-- Prelude> :m + Data.List
-- Prelude Data.List>

-- Loading multiple modules in ghci console.
-- Prelude? :m + Data.List Data.Set
-- Prelude Data.List Data.Set>

-- Some functions of Data.List module are already made available in "Prelude"
-- "map" "filter" "init" "tail" "head" etc

-- intersperse
-- input: an element, a list
-- output: a list
-- functionality: intersperses the element between list items.
varIntBaba = intersperse '.' "BABA"
-- varIntBaba gets "B.A.B.A" - B to the A to the B to the A - BABA! :)
varIntNum = intersperse 0 [1, 2, 3, 4, 5]
-- varIntNum gets [1, 0, 2, 0, 3, 0, 4, 0, 5]
-- Means, effectively the length of the new list becomes 2 * length - 1

-- intercalate
-- input: 2 lists (the second one is list of list)
-- output: single list
-- functionality: puts the first list in between the lists in second, flattens
spacedSentence = intercalate " " ["Learn", "For", "Great", "Good"]
-- spacedSentence gets "Learn For Great Good"
numsWith12 = intercalate [1,2] [[98,99],[-5, -8, 6], [200,250]]
-- numsWith12 gets [98, 99, 1, 2, -5, -8, 6, 1, 2, 200, 250]

-- transpose
-- transposes a list of lists
transposedNum = transpose [[1..3], [4..6], [7..9]] -- gets [[1,4,7], [2,5,8], [3,6,9]]
transposedString = transpose ["Learn", "For", "Great", "Good"]
-- transposedString gets ["LFGG","eoro","areo","rad","nt"]

-- Example for transpose
-- A has 5 apples, 6 oranges and 7 pears and 8 bananas [5, 6, 7, 8]
-- B has 12 pears and 2 bananas [0, 0, 12, 2]
-- C has 23 oranges and 9 pears [0, 23, 9, 0]
-- Get total number of fruits in each category

inFruits = [[5, 6, 7, 8], [0, 0, 12, 2], [0, 23, 9, 0]]
sumTypes = map sum $ transpose inFruits
-- sumTypes get [5, 29, 28, 10]

-- foldl', foldl1' : Stricter version of foldl and foldl1
-- Strict Evaluation, rather lazy evaluation
-- Lazy fold may cause stack overflow if the list is relatively big
-- Lazy fold does not keep changing the accumulator until necessary
-- Strict fold keeps updating the accumulator; avoids stack overflow
reverseFold :: [a] -> [a]  
reverseFold = foldl' (\acc x -> x : acc) []  

revNum = reverseFold [1..10]

-- concat
-- flattens a list of lists into a list of elements
-- Think of this as intercalate without the option of giving the list separator
joinedWords = concat ["Learn", "For", "Great", "Good"]

-- concatMap
-- takes a function and a list
-- applies the function on to the list and concatenates
concatMapped = concatMap (replicate 2) [1..4] -- [1,1,2,2,3,3,4,4]

-- and: takes in a list of boolean values and return True if all are true
-- or : takes in a list of boolean values and return True if any one is true
andExample1 = and [True, False, True, True]   -- returns False
andExample2 = and [True, True, True]          -- returns True

andExample3 = and (map (>3) [1,2,3,4])      -- returns False
andExample4 = and (map (<5) [1,2,3,4])      -- returns True

-- using function application operator $
andExample5 = and $ map (==3) [3,3,3]       -- returns True

orExample1 = or $ map (>3) [1,2,3,4,5]      -- returns True
orExample2 = or $ map (==2) [1,2,3,4,5]     -- returns True
orExample3 = or $ map (<7) [8, 9]           -- returns False

-- any / all: takes in a predicate and checks against each of the list member.
-- any returns True if anyone is true; all returns True only if all are true.
-- and $ map (fn) is equivalent of "all (fn)"
-- or $ map (fn) is equivalent of "any (fn)"

anyExample = any (==4) [1, 2, 3, 4]         -- returns True
allExample = all (==4) [1, 2, 3, 4]         -- returns False

-- user defined function to check whether all/any are upper / lower case
anyUppercase :: [Char] -> Bool
anyUppercase = any (`elem` ['A'..'Z'])
anySmallcase :: [Char] -> Bool
anySmallcase = any (`elem` ['a'..'z'])
allUppercase :: [Char] -> Bool
allUppercase = all (`elem` ['A'..'Z'])
allSmallcase :: [Char] -> Bool
allSmallcase = all (`elem` ['a'..'z'])

-- iterate
-- takes in a function (fn) and a starting value (sv)
-- result is infinite list [sv, fn(sv), fn(fn(sv)), fn(fn(fn(sv)))...]
first5Nat = take 5 $ iterate (+1) 1         -- returns [1,2,3,4,5]
first10Power3 = take 10 $ iterate (*3) 1    -- returns [1,3,9,27..19683]

-- splitAt
-- splits a list into a pair of lists, at a specified position.
splitted = splitAt 12 "LearnHaskellForGreatGood"
splittedLists = splitAt 5 [1..10]

-- takeWhile
-- takeWhile takes a predicate and a list
-- returns the list from the start, until before the first encounter of false
firstWord = takeWhile (/=' ') "Whatever be the sentence!"
gtThan3   = takeWhile (>3) [10,9,8,1,2,8,9,10]      -- returns [10, 9, 8]

-- sum of log10 of all factors of n
sumLogFactors :: Int -> Float
sumLogFactors n = sum $ map (logBase 10) $ map (fromIntegral) factors
    where factors = [x | x <- [1..n], n `mod` x == 0]
    
-- dropWhile
-- dropWhile takes a predicate and a list
-- returns the list from the first instance of false till the end
-- The complement of takeWhile
leaveFirstWord = dropWhile (/=' ') "Whatever be the sentence!"
dropGtThan3   = dropWhile (>3) [10,9,8,1,2,8,9,10]      -- returns [1,2,8,9,10]

-- Finding when the stock price exceeds 1000
stock = [(994.4,2008,9,1), (995.2,2008,9,2), 
    (999.2,2008,9,3), (1001.4,2008,9,4), (998.3,2008,9,5)]
dtExd1000 = head $ dropWhile 
    (\(val,y,m,d) -> val < 1000) stock -- returns (1001.4,2008,9,4)



