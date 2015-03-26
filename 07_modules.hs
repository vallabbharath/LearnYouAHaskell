-- Chapter 7: Modules
-- http://learnyouahaskell.com/modules
-- Module: Collection of related types, typeclasses and functions.

-- The core module in Haskell is "Prelude" which is imported by default.

-- To specifically import a module, use "import <module name>"

-- Module to be imported to work with lists.
import Data.List
-- Now all exposed functions in Data.List become available in global namespace.

import Data.Char
import Data.Function

-- qualified import for Data.Map: functions clash with Data.List and Prelude
import qualified Data.Map as Map
-- qualified import for Data.Set: functions clash with Data.List and Prelude
import qualified Data.Set as Set

-- Importing in the ghci interpreter
-- Prelude> :m + Data.List
-- Prelude Data.List> :m + Data.Map
-- Prelude Data.List Data.Map>

-- Importing specific functions from a module
import Data.List (nub, sort)

-- Importing all functions in a list leaving a few
import Data.List hiding (nub)
-- (nub is a function defined in Data.List to remove duplicates from a list)

-- IMPORTANT: All import statements to be written at the start of the file.

-- Qualified Import
import qualified Data.List
-- Now, functions like "nub" cannot be called directly.  Use Data.List.nub
import qualified Data.List as DL
-- Now use DL.nub to call "nub" function

-- The next two lines are user defined modules.
-- See the last section "Creating Modules" for further explanation
import Geometry1 as Geometry1
-- (import Geometry2 as Geometry2) will be wrong! Geometry2 is not a .hs file!

import Geometry2.Sphere as Sphere
import Geometry2.Cuboid as Cuboid
import Geometry2.Cube as Cube

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
-- sumTypes gets the value [5, 29, 28, 10]

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
splittedWords = splitAt 12 "LearnHaskellForGreatGood"
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
dropGtThan3   = dropWhile (>3) [10,9,8,1,2,8,9,10]    -- returns [1,2,8,9,10]

-- Finding when the stock price exceeds 1000
stock = [(994.4,2008,9,1), (995.2,2008,9,2),
    (999.2,2008,9,3), (1001.4,2008,9,4), (998.3,2008,9,5)]
dtExd1000 = head $ dropWhile
    (\(val,y,m,d) -> val < 1000) stock -- returns (1001.4,2008,9,4)

-- span
-- input: a predicate and a list (same as takewhile)
-- output: a tuple containing two lists
-- first list is same as "takeWhile" output; second is remaining of the input
sepAtFirstSpace     = span (/=' ') "Whatever be the sentence!"
sepAtFirstGtThan3   = span (>3) [10,9,8,1,2,8,9,10]
tupleOut            = let (fw, rest) = span (/=' ') "This is a sentence"
                      in "First word:" ++ fw ++ ", the rest:" ++ rest

-- break
-- input: a predicate and a list
-- output: a tuple containing two lists
-- the first list contains elements until predicate gets matched
-- the second list will be remaining until end of list (ignore predicate)
breakGtThan3 = break (>3) [1,2,8,9,10,1,2] -- returns ([1,2], [8,9,10,1,2])
-- "break" is equivalent of "span (not . p)"

-- sort
-- sorts a list
-- Requirement: the list elements to be part of 'Ord' typeclasses
sortedWord = sort "In Alphabetical Order"
sortedList = sort [5, 8, 4, 9, 0, 2, 3, 7]

-- group
-- groups equal adjacent elements together in a sublist
-- input: a single list
-- output: list containing grouped sublists
groupedLists = group [1, 2, 2, 1, 3, 4, 2, 4, 3]
-- groupedLists will have [[1],[2, 2],[1],[3],[4],[2],[4],[3]]

-- to group identical elements use group after sort
groupedOrdLists = group $ sort [1, 2, 2, 1, 3, 4, 2, 4, 3]
-- groupedOrdLists will have [[1,1],[2,2,2],[3,3],[4,4]]

-- to count the occurrence of each distinct element
countedElements = map (\l@(x:xs) -> (x, length l)) . group .
    sort $ [1, 2, 2, 1, 3, 4, 2, 4, 3]

-- inits : recursively apply init
initsOut = inits "loop" -- gives ["", "l", "lo", "loo", "loop"]
-- Remember "init" and "last" are complements

-- tails : recursively apply tail
tailsOut = tails "loop" -- gives ["loop", "oop", "op", "p", ""]
-- Remember "head" and "tail" are complements
-- But, "inits" and "tails" can be considered complements

loop5 = zipWith (++) (inits "loop")  (tails "loop")
-- returns ["loop", "loop", "loop", "loop", "loop"]

-- isInfixOf: built in function to search for a sublist within a list
-- takes two lists as argument
-- the following "search" is a kind of implementation of "isInfixOf"
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False
        (tails haystack)

searchOut = search "vib" "vibgyor"          -- returns True
infixOfOut = "bgy" `isInfixOf` "vibgyor"    -- returns True

-- isPrefixOf : built-in function to check whether given list is prefix
-- isSuffixOf : built-in function to check whether given list is suffix
prefixOfOut = "Haskell" `isPrefixOf` "LearnYouAHaskell"     -- False
suffixOfOut = "Haskell" `isSuffixOf` "LearnYouAHaskell"     -- True

-- Similarly,
-- elem: built-in function to check whether given element is present in list
-- notElem: built-in function to check whether given element is not present
elemOut1 = 'a' `elem` "Haskell"         -- True
elemOut2 = 'b' `elem` "Haskell"         -- False
notElemOut1 = 'a' `notElem` "Haskell"   -- False
notElemOut2 = 'b' `notElem` "Haskell"   -- True

-- partition : built-in function
-- input: a predicate and a single list
-- output: a tuple containing 2 lists
-- the first one satisfying the predicate, the second one not satisfying
partitionOut1 = partition (`elem` ['A'..'Z']) "LearnYouAHaskell"
-- partitionOut1 will get ("LYAH","earnouaskell")
-- remember, it is tuple containing two lists, strings are lists
partitionOut2 = partition (\x -> x `mod` 3 == 0) [1, 2, 3, 4, 5, 6]
-- partitionOut2 will get ([3, 6], [1, 2, 4, 5])

-- find
-- input: a predicate and a list
-- output: an element (of May be type)
-- returns the first element where the predicate becomes true
findOut1 = find (`elem` ['a'..'z']) "arun" -- gives "Just 'a'"
findOut2 = find  (`elem` ['a'..'z']) "ARUN" -- gives "Nothing"
findOut3 = find (>3) [1, 2, 3, 4, 5, 6]   -- gives "Just 4"

-- elemIndex: same as 'elem' but, instead returning boolean, returns the index
index1 = elemIndex 'a' "Tendulkar"                  -- gives "Just 7"
-- findIndex: same as 'find' but, instead of the element, returns the index
index2 = findIndex (>13) [11, 12, 13, 14, 15, 16]   -- gives "Just 3"
-- Return type of elemIndex and findIndex - both are "May be"

-- elemIndices: same as elemIndex, but returns a list
-- instead of first occurrence, returns all satisfying elements
indices1 = elemIndices 'a' "SachinTendulkar" -- gives [1, 13]
-- findIndices: same as findIndex, but returns a list
-- instead of first occurrence, returns all satisfying elements
indices2 = findIndices (>13) [11, 12, 13, 14, 15, 16] -- gives [3, 4, 5]

-- zip and zipWith : operates on two lists
-- zip3, zip4 ... zip7:  operates on 3, 4.. 7 lists respectively
-- same case with zipWith3, zipWith4 .. zipWith7
zipped3Sum = zipWith3 (\x y z -> x + y + z) [1,2,3] [9,10,11] [-5,-3,-9]

-- lines: splits based on '\n'
-- input: single string
-- output: list of separate strings
genLines = lines "first line\nsecond line\nthird line"

-- unlines: reverse of lines
-- input: list of strings
-- output: a single string joined by introduction of "\n" between them
joinedLine = unlines ["This", "will", "be", "joined"]

-- words: splits on seeing "\n", "\t", " " (whitespace)
-- input: single string
-- output: list of separate strings
genWords = words "first line\nsecond line\nthird line"

-- unwords: reverse of words; but just introduces a " " not \n or \t
-- input : list of strings
-- output: a single string joined by introduction of a space
joinedWord = unwords ["Learn","you","a", "Haskell"]

-- nub: weeds out the duplicate elements
-- input: a single list
-- output: a single list with all unique elements
nubOutNum = nub [1, 2, 1, 3, 2, 4, 3, 5, 2, 1, 2, 7]
nubOutString = nub "Sad! All my look-alikes will be removed!"

-- delete: deletes the first occurrence of given element in a list
-- input: the element and the list
-- output: a list
deleteOutNum    = delete 17 [2, 4, 10, 17, 1, 3, 9] -- returns [2,4,10,1,3,9]
deleteOutString = delete 'a' "asymmetric becomes symmetric"

-- (\\) : (Double back slash) List Difference Function (Eq. of Math. Set Diff.)
-- input: two lists (say A and B)
-- output: single list (A - B)
-- Removes the elements of B from A if present, and outputs the remaining in A
diffListNum = (\\) [1..10] [2, 3]  -- returns [1, 4, 5, 6, 7, 8, 9, 10]
diffListString = (\\) "Do not eat apples" "not"    -- returns "D o eat apples"'
-- Note that 'o' after 'D' gets removed; not the 'o' after 'n'
-- This operator can be used like +, - * or /
diffList2ndWay = "Do not eat apples" \\ "not"

-- union: Similar to the union function on the set; returns combined elements
-- input: two lists
-- output: Single list with the combined elements of the two lists
unionOutput = "Hi, there!" `union` "There is a dog" --returns "Hi, thereTsadog"
-- note that it does not remove the two 'e's present in the first list
-- basically, union does not do concatenation followed by nub.

-- intersect: Returns elements common to both the lists
intersectOutput = "Hi, there!" `intersect` "There is a dog" --returns "i here"
-- Output is the elements that got removed from union (but, not in that order)
-- The order is the duplicate elements present in the first list

-- insert: takes an element and inserts to a list (which can be ordered)
-- position of insert: the last position where it is still less than the next
insertOutput = insert 'm' "hello, there!"   -- returns "hellmo, there!"
-- if insert is operated on a sorted list, the result remain sorted
insertNumOut = insert 5 [1, 2, 4, 7, 10]    -- returns  [1,2,4,5,7,10]

-- Generic Equivalents:
-- length, take, drop, splitAt, !! and replicate have "Int" as i/p or ret. type
-- let xs = [1..6] in sum xs / length xs gives error ( / does not take "Int")
-- Solution:  We need a function that returns "Num" instead of "Int"
-- :t length            length :: [a] -> Int
-- :t genericLength     genericLength :: Num i => [b] -> i (defined in Data.List)
sumXS = let xs = [1..6] in sum xs / genericLength xs
-- genericTake, genericDrop, genericSplitAt, genericIndex, genericReplicate

-- nub, delete, union, intersect, group  (== is the default test here)
-- nubBy, deleteBy, unionBy, intersectBy, groupBy (we can specify the fn here)
inVals = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, -2.4, -14.5, 2.9, 5.3]
plminVals = groupBy (\x y -> (x > 0) == (y > 0)) inVals
-- returns [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1],[-2.4,-14.5],[2.9,5.3]]
-- Explicit definition: Take two elements; see whether both > 0 or both < 0
plminVals2 = groupBy (\x y -> (x > 0) && (y > 0) || (x <= 0) && (y <= 0)) inVals
-- "group" is nothing but "groupBy (==)"

-- "By" functions usually uses "on" function from Data.Function
-- "on" definition:
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- f `on` g = \x y -> f (g x) (g y)
-- (==) `on` (> 0) is with f as (==) and g as (> 0)
-- \x y -> f (g x) (g y) is expanded as \x y -> (==) ((> 0) x) ((>0) y)
-- \x y -> (==) ((> 0) x) ((>0) y) expressed as \x y -> (==) (x > 0) (y > 0)
plminVals3 = groupBy ((==) `on` (> 0)) inVals

-- sort, insert, maximum, minimum
-- genericEquivalents: sortBy, insertBy, maximumBy, mininmumBy
-- :t sortBy
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- "sort" is nothing but "sortBy compare"
sortByList = let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]] in
            sortBy (compare `on` length) xs
-- outputs list ordered by sub-list's lengths.
-- compare `on` length is \x y -> (length x) `compare` (length y)

-- Data.Char Module (IMPORTANT: To have imported in the beginning)
-- Deals with characters and character lists (strings)

-- Functions to check for a specific condition over given character
-- Type signature: Char -> Bool
inChar = 'g'
x1 = isControl inChar   -- checks whether the character is a control character
x2 = isSpace inChar     -- checks whether the character is one of whitespace
                        -- space, tab, newlines etc [\s]
x3 = isLower inChar     -- checks whether the character is lower case [a-z]
x4 = isUpper inChar     -- checks whether the character is upper case [A-Z]
x5 = isAlpha inChar     -- checks whether the character is an alphabet [a-zA-Z]
x6 = isAlphaNum inChar  -- checks whether fits in RegEx [0-9a-zA-Z]
x7 = isPrint inChar     -- checks whether the character is printable
x8 = isDigit inChar     -- checks whether the character is a digit [0-9]
x9 = isOctDigit inChar  -- checks whether the character is an octal digit [0-7]
x10 = isHexDigit inChar -- checks whether the character is a hex [0-9A-Fa-f]
x11 = isLetter inChar   -- same as isAlpha
x12 = isMark inChar     -- checks whether the character is a Unicode mark
x13 = isNumber inChar   -- same as isDigit
x14 = isPunctuation inChar  -- checks whether the character is a punctuation
x15 = isSymbol inChar   -- checks whether the character is a Math symbol
x16 = isSeparator inChar    -- checks whether the character is Unicode space
x17 = isAscii inChar    -- checks whether the character is one of 128 ASCII set
x18 = isLatin1 inChar   -- checks whether it is one of first 256 Unicode char
x19 = isAsciiUpper inChar   -- checks whether ASCII and also upper case
x20 = isAsciiLower inChar   -- checks whether ASCII and also lower case

-- Practical Example
-- Using to check password combination using "all" function
-- Requirement: Should use only alphabets or numbers
isPasswordValid :: [Char] -> Bool
isPasswordValid = all $ isAlphaNum
-- Use isPasswordValid "Abc123" from console to test this function

-- Simulating "words" function of Data.List
-- "words" is roughly equivalent to "groupBy ((==) `on` isSpace)"
-- "groupBy ((==) `on` isSpace)" catches the single spaces between the words too!
-- filter out the empty spaces
-- filter (not . any isSpace) ["hi", " ", "hello", " ", "greet"]
-- The above function removes space sub-lists (returns ["hi","hello","greet"])
-- So, "words" is equivalent to "(filter (not . any isSpace) . groupBy ((==) `on` isSpace))"
customWords :: [Char] -> [[Char]]
customWords = filter (not . any isSpace) . groupBy ((==) `on` isSpace)

-- GeneralCategory (a kind of enum - different from generalCategory (a fn))
-- :t generalCategory (function that takes in a character, returns its gen.cat)
-- generalCategory :: Char -> GeneralCategory
genCatOut = map generalCategory " \nA9?|"
-- [Space,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]

inLetter = 'a'
outLetter1 = toUpper inLetter       -- converts to upper case
outLetter2 = toLower inLetter       -- converts to lower case
outLetter3 = toTitle inLetter       -- converts to title case (usually upper)

-- digitToInt :: Char -> Int
outNum1 = digitToInt inLetter       -- gets decimal equivalent
-- '0' to '9' gives 0 to 9 (Int type); 'a'(or 'A') to 'f'(or 'F') gets 10 to 15

-- intToDigit: reverse of digitToInt
-- intToDigit :: Int -> Char
outLetter4 = intToDigit 15          -- gets 'f'
-- Try converting 16! Output will be "Exception: Char.intToDigit: not a digit 16"

-- The following code converts a number from hexadecimal to decimal
convToDec :: [Char] -> Int
convToDec inHex = sum $ zipWith (*)
                    (zipWith (^)
                        (take lenH [16, 16..]) $
                        [lenH - 1, lenH - 2..0]) $
                    map digitToInt inHex
                    where lenH = length inHex

-- ord: converts a letter to its ascii value
-- chr: given an ascii value, converts to the corresponding letter
chrOut = chr 97         -- chrOut gets 'a'
ordOut = ord 'a'        -- ordOut gets 97

-- Ceasar cipher: (shift each letter by a fixed position) using ord and chr
encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted

decode :: Int -> String -> String
decode shift msg =
    let ords = map ord msg
        shifted = map ((-) shift) ords
    in  map chr shifted

-- Same, using function composition
encodeComp shift msg = map (chr . ((+) shift) . ord) msg
decodeComp shift msg = map (chr . ((-) shift) . ord) msg

-- Simpler decoding!
decodeSimple :: Int -> String -> String
decodeSimple shift msg = encode (negate shift) msg

-- Data.Map module (make sure you have imported at the beginning of .hs file)
-- (do qualified import, it conflicts with Data.List and Prelude)
-- Map also termed as "Association Lists"
-- or, "Dictionaries"
-- or, "Key-Value pairs"

-- Representation in Haskell: Using list of pairs
cricketRunsList = [("Tendulkar", 15921), ("Ponting", 13378), ("Kallis", 13289),
    ("Dravid", 13288), ("Lara", 11000), ("Jayawardene", 11493), ("Lara", 11953)]

-- Function to look up a value, given a key
findKey1 :: (Eq k) => k -> [(k,v)] -> v
-- Filter (k, v) pattern where k matches given 'key'.
-- Take the first item in the filtered list and get its value (2nd in the pair)
findKey1 key xs = snd . head . filter (\(k,v) -> key == k) $ xs
-- If 'key' not found, head fn on empty list throws an error

-- So, we need the same implementation using "Maybe" type class
findKey2 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey2 key [] = Nothing
findKey2 key ((k, v) : xs) = if key == k
                            then Just v
                            else findKey2 key xs
-- findKey on empty list returns 'Nothing'
-- Takes each element and recursively performs findKey on first element

-- Using Fold Right function (above is a typical case for a foldr operation)
findKey3 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey3 key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

-- findKey functions above are nothing but "lookup" of Data.List module

-- fromList
-- input: an association list
-- output: a Map object
mapRuns = Map.fromList cricketRunsList
-- ignores duplicates, retains only the latest (recent) value for key 'k'
-- type definition
-- Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v

-- empty
-- represents empty map
emptyMap = Map.empty

-- insert
-- input: a key, a value and a map
-- output: a map with the given (key, value) pair inserted
mapRunsNew = Map.insert "Sangakkara" 11493 mapRuns

-- fromList function implementation of Map module
-- using foldr and Map.empty
-- input: association list
-- output: Map
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

-- null: checks whether map is empty
isNull1 = Map.null emptyMap       -- returns True
isNull2 = Map.null mapRuns        -- returns False

-- size: returns the size of the map
sizeRuns = Map.size mapRuns

-- singleton: creates a map with single entry
-- input: the key and the value
-- output: a map with just one element of given key, value pair
mapGoals = Map.singleton "Messi" 5

-- singleton implementation
singleton' :: (Ord k) => k -> v -> Map.Map k v
singleton' k v = Map.insert k v Map.empty

-- lookup
-- same as Data.List's lookup; here the input is a Map
goalsLara = Map.lookup "Lara" mapGoals      -- Nothing
goalsMessi = Map.lookup "Messi" mapGoals    -- Just 5

-- member
-- checks whether the key is present in the map or not
isLaraAvbl = Map.member "Lara" mapRuns      -- True
isMessiAvbl = Map.member "Messi" mapRuns    -- False

-- map function
-- input: a function and a Map
-- output: processed Map
mapFiveMoreRuns = Map.map (+5) mapRuns

-- filter function
-- input: a predicate and a Map
-- output: processed Map
mapGt12000 = Map.filter (> 12000) mapRuns

-- toList (inverse of fromList)
-- input: Map
-- output: association list
listRuns = Map.toList mapRuns

-- keys
keysRuns = Map.keys mapRuns

-- elems (Values)
elemsRuns = Map.elems mapRuns

-- fromListWith
-- special implementation of fromList, to take care of duplicate keys
-- fromListWith takes 2 arguments
-- A function and an association list
-- The function tells what to do when exist more than two values for the same key
runsListToMap :: (Ord k) => [(k, Integer)] -> Map.Map k Integer
runsListToMap xs = Map.fromListWith (\runs1 runs2 -> if runs1 > runs2
                                                        then runs1
                                                        else runs2) xs
mapRunsMax = runsListToMap cricketRunsList

-- Or simply
-- If the function takes just two parameters, we can as well give only that!
mapRunsMax1 = Map.fromListWith max cricketRunsList
-- To add
mapRunsAdd = Map.fromListWith (+) cricketRunsList

-- insertWith: to take care of duplicate keys
-- specifies a function which tells what to do if exists multiple key entries
-- Here, retains the value which is maximum of the two
mapRunsDup = Map.insertWith max "Tendulkar" 20000 mapRuns


-- Data.Set Module
-- remember to do a qualified import at the beginning of .hs file
-- Data.Set functions conflicts with Prelude and Data.List
fibList = [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
prmList = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]

-- fromList function
fibSet = Set.fromList fibList
prmSet = Set.fromList prmList

fibPrmCom = Set.intersection fibSet prmSet
fibButPrm = Set.difference fibSet prmSet
prmButFib = Set.difference prmSet fibSet
fibAndPrm = Set.union fibSet prmSet

-- empty
emptySet = Set.empty

-- null
isNullSet1 = Set.null emptySet      -- returns True
isNullSet2 = Set.null fibAndPrm     -- returns False

-- size
fibSize = Set.size fibSet           -- returns 10

-- member
is10inFib = Set.member 10 fibSet    -- returns False

-- singleton
singSet = Set.singleton 4           -- returns a set with single value

-- insert
fibSet2 = Set.insert 144 fibSet     -- fibSet2 will have 144 added to fibSet

-- delete
fibSet3 = Set.delete 144 fibSet2    -- fibSet3 will be same as fibSet

-- isSubsetOf and isProperSubsetOf
-- takes two sets (first is the smaller one) which is searched in second
-- returns True or False
smallSet = Set.fromList [1, 2, 3]
mediumSet = Set.fromList [1..10]
bigSet = Set.fromList [1..10]

smallBigSub = smallSet `Set.isSubsetOf` bigSet                  -- True
mediumBigSub = mediumSet `Set.isSubsetOf` bigSet                -- True
mediumBigProperSub = mediumSet `Set.isProperSubsetOf` bigSet    -- False

-- toList: takes in a set and converts to a list
-- 'nub' implementation using toList and fromList
nub' xs = Set.toList $ Set.fromList xs
-- this is faster than 'nub' from Data.List (which preserves order)

-- map and filter function in Sets
-- Set intersection implementation using filter
intersection' x y = Set.filter (`elem` Set.toList x) $ y
fibIntPrm = intersection' fibSet prmSet

-- Using map function
mult2 :: Set.Set Integer -> Set.Set Integer
mult2 = Set.map (*2)
fibSetX2 = mult2 fibSet

-- Creating Own Modules
-- Method 1: Write all "Module" functions in a single file "Module.hs"
-- Method 2: Create a folder called "Module" and use multiple .hs files inside

-- Illustration of Method 1:
-- See example module file: Geometry1.hs
-- Name the module with the same file name, in this case "Geometry1"

-- Illustration of Method 2:
-- See the folder Geometry2 (refers to the super-module name)
-- And, related / associated functions are defined in separate .hs files.

-- Usage of the modules
-- First we need to import the written modules (which is done at the beginning)
-- import Geometry1 as Geometry1

-- import Geometry2 as Geometry2
-- (the above statement is wrong! We can import a module based file, not folder)

-- Usage:
volumeCube = Cube.volume(4)
areaSphere = Geometry1.sphereArea(3)

-- To import a module, use
-- :m +ModuleName in the ghci shell
-- import ModuleName in a Haskell file (.hs file)
-- Read more here on ghci:
-- http://www.haskell.org/ghc/docs/5.04/html/users_guide/ghci.html
