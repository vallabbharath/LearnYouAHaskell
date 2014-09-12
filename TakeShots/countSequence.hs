-- Given the current number, finds the next number in the following sequence
-- 1
-- 11
-- 21
-- 1211
-- 111221
-- 312211
-- 13112221
-- The next number in the sequence is found by counting the current digits!

import Data.List

-- Converts a character list (i.e String) to an Int list
strToIntList :: String -> [Int]
strToIntList = map (read . (:""))

-- Converts an Int list to a character list
intListToStr :: [Int] -> String
intListToStr xs = intercalate "" (map show xs)

-- counts the number of digits as it occurs in the given number string
countDig :: String -> [Int]
countDig [] = []
countDig [x] = [1]
countDig (x:xs) = 
    length (takeWhile (==x) xs) + 1 : countDig (dropWhile (==x) xs)

-- separates out the digits, ignoring the number of occurrence 
uniq :: [Int] -> [Int]
uniq [] = []
uniq [x] = [x]
uniq (x:xs) = head (takeWhile (==x) (x:xs)) : uniq (dropWhile (==x) (x:xs))

-- same as uniq; uses uniq; instead of taking in Int List, takes a String input
uniqDig :: String -> [Int]
uniqDig = uniq . strToIntList

-- takes in two lists (of any type) and interleaves them and gives a single list
interleave :: [a] -> [a] -> [a]
interleave [x] [] = [x]
interleave [] [y] = [y]
interleave [x] [y] = [x, y]
interleave (x:xs) (y:ys) = x : y : (interleave xs ys)

-- uses the basic functions defined, and finds the next number in sequence
nextNum :: Integer -> Integer
nextNum cur = 
    read $ intListToStr $ interleave (countDig $show cur) (uniqDig $show cur)
