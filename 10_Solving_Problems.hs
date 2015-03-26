-- Learn You a Haskell For Great Good
-- Chapter 10: Functionally Solving Problems
-- http://learnyouahaskell.com/functionally-solving-problems

import Data.List  
  
solveRPN ::  String -> Double  
solveRPN = head . foldl foldFn [] . words 

foldFn :: (Floating a, Read a) => [a] -> String -> [a]
foldFn (x:y:xs) "*" = (x * y) : xs
foldFn (x:y:xs) "+" = (x + y) : xs
foldFn (x:y:xs) "-" = (y - x) : xs
foldFn (x:y:ys) "/" = (y / x) : ys  
foldFn (x:y:ys) "^" = (y ** x): ys  
foldFn (x:xs) "ln"  = log x   : xs  
foldFn xs     "sum" = [sum xs]  
foldFn xs numberString = read numberString:xs
