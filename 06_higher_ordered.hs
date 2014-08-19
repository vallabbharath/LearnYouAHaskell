-- Chapter 6
-- Higher Order Functions: http://learnyouahaskell.com/higher-order-functions

-- Every function in Haskell takes only one parameter
-- curried functions: functions with more than one parameter
-- fn (x, y, z) : fn x y z :  fn -> x -> y -> z : fn -> x -> (y -> z)

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z 

-- in the statements below, a and b evaluates to be same
a = multThree 2 3 4
b = ((multThree 2) 3) 4 

-- ((multThree 2) 3) 4, here two functions are built up implicitly
-- each taking one argument

multTwoBy2 = multThree 2    -- multThree 2 returns a function (not value)
multBy6    = multTwoBy2 3   -- multTwoBy 3 returns another function

-- Taking another example
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x  

-- Simply re-written as
compWith100 :: (Num a, Ord a) => a -> Ordering  
compWith100 = compare 100

-- Function to divide by 10
divby10 :: (Floating a) => a -> a
divby10 x = x / 10

-- Instead, it can simply be written as
-- Most Important, variable and function names SHOULD start with small letters.
div10 :: (Floating a) => a -> a
div10 = (/10)
-- So, 200/10 is equivalent of saying (/10) 200
-- Never say, (-100) 300 to do 300 - 100 = 200.  (-100) is (-100)
-- Valid form is (subtract 100) 300

-- is upper case : Takes in a letter and checks whether it is in A..Z
isUpper :: Char -> Bool  
isUpper = (`elem` ['A'..'Z']) 

-- Rewriting: Take in a string and check whether 'a' is present
check'a'present :: [Char] -> Bool
check'a'present = (elem 'a')

-- Double application of the same function
-- Takes two arguments and gives out one value - of same type as input
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

twice = applyTwice (/10) 345       -- Returns 3.45

-- Implementing zipWith function
-- zipWith takes two lists and a function (taking two parameters)
-- Takes ith element from each list and performs the function, creates new list
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

-- Interpreting the type declaration
-- zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
-- The above statement means f -> [a] -> [b] -> [c] 
-- (a -> b -> c) is f, function which takes in two argument resulting in third
-- The parameters to zipWith' are function followed by two lists [a] [b]
-- the result [c] is also a list.


-- Let us use currying and add a new function which increments list items by 1
-- zipPlus takes in two lists and produces a new list as result
zipPlus :: (Integral a) => [a] -> [a] -> [a]
zipPlus = zipWith' (+)

-- plusOneList takes in a list and results in a new list.
plusOneList :: (Integral a) => [a] -> [a]
plusOneList = zipPlus infiniteOne where infiniteOne = [1,1..]

-- Even numbers less than 20
even_lt_20 = [x * 2 | x <- [1..10]]
-- Same, using the zipWith function
even_20 = zipWith' (*) [1..] (replicate 10 2)

-- zipProd - multiplies each elements using zipWith'
zipProd :: (Floating a) => [a] -> [a] -> [a]
zipProd = zipWith' (*)

xyz = zipWith' zipProd [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]  
-- xyz gives [[3.0,4.0,6.0],[9.0,20.0,30.0],[10.0,12.0,12.0]]

---------------------------------------------------------------------------
-- NEED MORE WORK  (Written to simulate matrix multiplication)
-- splitter
splitter _ _ [] = []
splitter r c xs = take r xs : splitter r c (drop r xs) 

splitter2 = flip splitter

-- This splitter3 is wrong / in accurate, need to work on the same
splitter3 [[],[]] = []  
splitter3 ([x:xs, y:ys]) = (x,y) : splitter3 ([xs, ys])

vSplitter _ _ [] = []
vSplitter r c xs = splitter3 (splitter2 r c xs)
---------------------------------------------------------------------------

-- flip: built-in funcion
-- takes in a function and returns another function 
-- the first 2 parameters of the returned function are swapped.
flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x  
-- takes in a function (a->b->c) and returns a function (b->a->c)
-- the parenthesis in the second set is not necessary,
-- because "->" is right associative by default
-- (a -> b -> c) -> b -> a -> c

-- Also, rewritten as follows
flips' :: (a -> b -> c) -> b -> a -> c  
flips' f x y = f y x

-- Usage of flip
xx = mod 2 3             -- returns 2 : eq. of 2 `mod` 3
yy = flip mod 2 3        -- returns 1

aa = zip [1, 2, 3] [4, 5, 6]         -- returns [(1,4), (2,5), (3, 6)]
bb = flip zip [1, 2, 3] [4, 5, 6]    -- returns [(4,1), (5,2), (6, 3)]

-- map: built-in function - Same map of map-reduce paradigm
-- Takes in a function and list, applies function to each element
-- Returns a list
map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map' f xs

-- Examples
pw2 = map (^2) [1, 2, 3]        -- returns [1, 4, 9]
add1 = map (+1) [2, 3, 4, 5]    -- returns [3, 4, 5, 6]

-- If needed to apply list-inside-list, apply map twice
inmap = map (map (subtract 3)) [[10,8], [5, 3]] -- returns [[7, 5], [2, 0]]
-- Note that it is (map (subtract 3)) and not (map (-3))

aaa = map (+1) [2, 3, 4, 5]    -- returns [3, 4, 5, 6]
bbb = [x + 1 | x <- [2, 3, 4, 5] ] -- list comprehension equivalent 

-- filter: built-in function
-- takes in a predicate (a condition check) and a list
-- returns another list containing elements that satisfies the condition
filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []  
filter' p (x:xs)   
    | p x       = x : filter' p xs  
    | otherwise = filter' p xs  
    
-- Examples
gt_three  = filter (>3) [1,2,3,4,5]     -- returns [4,5]
even_nums = filter even [1..100]        -- returns [2,4..100]

-- Let us write a function to remove non-letters in the given string
retainAlpha :: [Char] -> [Char]
retainAlpha = filter (`elem` ['a'..'z'])

-- Quick Sort using filter function
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
    where smallerSorted = quicksort (filter (<=x) xs)  
          biggerSorted = quicksort (filter (>x) xs)   

-- Largest number divisible by 4123 less than 100000
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 4123 == 0           
    
-- takeWhile: built-in function
-- Very similar to "filter", takes in a list and a predicate 
-- Keeps checking for the predicate to be true, stop when it finds false
-- In "filter", search goes from start to end
-- Just as "filter" returns a list, takeWhile too returns a list

-- Example
getFirstWord :: [Char] -> [Char]
getFirstWord = takeWhile (/= ' ')

-- Sum of odd squares less than 10000
oddSqSum = sum (takeWhile (< 10000) (filter (odd) (map (^2) [1..])))

-- Same, using list comprehension
oddSqSum2 = sum (takeWhile (< 10000) [n^2 | n <- [1..], odd (n^2)])

-- Collatz Sequence http://en.wikipedia.org/wiki/Collatz_conjecture
collatzSeq :: (Integral a) => a -> [a]
collatzSeq n
    | (n == 1) = [1]
    | (n `mod` 2 == 0) = n : collatzSeq (n `quot` 2)
    | otherwise = n : collatzSeq (3 * n + 1)


-- Find the length of Collatz Sequences of 1 to 100 whose length > 15    
numLongChains :: Int  
numLongChains = length (filter isLong (map collatzSeq [1..100]))
    where isLong xs = length xs > 15      

-- map (2*) [0..10]  gives [0, 2, 4.. 20]
-- What does map (*) does?
-- map (*) [0..10] gives out a list of functions which takes one parameter 
-- [(*0), (*1), (*2) .. (*10)]

mulByWholeNums = map (*) [0..10]
-- Take out the second element (means multiply by 2) - index starts at 0
mul2 = (mulByWholeNums !! 2) 327        -- returns 654

-- Usage of Lambdas (represented as "\ -> ")
-- Lambdas are used for temporary functions
-- Rewriting numLongChains

-- Find the length of Collatz Sequences of 1 to 100 whose length > 15    
-- isLong defined using where is replaced below by lambda
numSeq :: Int  
numSeq = length (filter (\xs -> length xs > 15) (map collatzSeq [1..100]))

-- We know zipWith takes a function and applies that over two list.
zippedList = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]  
-- Here, the function is defined by lambda (\a b -> (a * 30 + 3) / b) 

-- pattern matching in lambda
-- pattern matching for tuple
sumTuple = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)] 

-- The three representations below are equivalent
addThree1 :: (Num a) => a -> a -> a -> a  
addThree1 x y z = x + y + z  

addThree2 :: (Num a) => a -> a -> a -> a  
addThree2 = \x -> (\y -> (\z -> x + y + z))

addThree3 :: (Num a) => a -> a -> a -> a  
addThree3 = \x -> \y -> \z -> x + y + z

-- Introducing "FOLD"
-- foldl : left fold, takes in a binary function (that takes two parameters)
-- Other parameters for foldl: an accumulator, and a list
-- return type is same as that of the accumulator
-- Processes the binary function on the list, folds it up into a single value

-- Example
-- Sum of a list
sumFold :: (Num a) => [a] -> a
sumFold xs = foldl binFn 0 xs
                where binFn acc x = acc + x

-- Rewriting the same using lambdas
sumFoldLambda :: (Num a) => [a] -> a
sumFoldLambda xs = foldl (\acc x -> acc + x) 0 xs

-- Or Simply, this!
sumSimple :: (Num a) => [a] -> a
sumSimple = foldl (+) 0 
-- The above is so because (\acc x -> acc + x) is equivalent to (+)
-- xs is common on both Left and Right of "=", it can be removed

-- Implementing "elem" function with Left Fold foldl
elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  

-- Left Fold :: foldl :: parameters : binary function, accumulator, list
-- (accumulator, current value) is the order for binary function in foldl

-- Right Fold:: foldr :: parameters : binary function, accumulator, list
-- (current value, accumulator) is the order for binary function in foldl

-- Implementing "map" function using foldr
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x acc -> f x : acc) [] 

-- Same, using foldl
mapFoldl :: (a -> b) -> [a] -> [b]
mapFoldl f = foldl (\acc x -> acc ++ [f x]) [] -- cannot do "acc : f x"

-- ++ is more expensive than ":", so, better use the foldr implementation

-- Right fold works on infinite lists. Left Fold does not!!
-- foldr : starts from a specific point in the infinite list and folds to first
-- foldl : starts from the beginning and may never end in the infinite list

-- foldl1 and foldr1 : Here no need to specify explicit start value
-- assumes the first or (last for foldr1) to be the starting value

-- We just saw, sumSimple = foldl (+) 0
sumUltraSimple :: (Num a) => [a] -> a
sumUltraSimple = foldl1 (+)

-- Implementing bult-in functions using foldl, foldr, foldl1, foldr1

maximumFold :: (Ord a) => [a] -> a  
maximumFold = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverseFold :: [a] -> [a]  
reverseFold = foldl (\acc x -> x : acc) []  

reverseFold2 :: [a] -> [a]  
reverseFold2 = foldl (flip (:)) []  

-- reverseFold2 [3,4,56] is
-- flip (:) (flip (:) (flip (:) (flip (:) [] 3) 4) 5) 6
-- [6,5,4,3]
-- The innermost bracket is (flip (:) [] 3) which is (:) 3 [] = [3]
  
productFold :: (Num a) => [a] -> a  
productFold = foldr1 (*)  
  
filterFold :: (a -> Bool) -> [a] -> [a]  
filterFold p = foldr (\x acc -> if p x then x : acc else acc) []  
  
headFold :: [a] -> a  
headFold = foldr1 (\x _ -> x)  
  
lastFold :: [a] -> a  
lastFold = foldl1 (\_ x -> x) 

-- scanl and scanr : similar to foldl and foldr
-- Fold returns a single value from a list(or a single list in case
-- of list of lists)
-- Scan returns all the intermediate values.
-- If "sum" can be implemented using fold, 
-- "cumulative sum" can be implemeted using scan.

cumSum :: (Integral a) => [a] -> [a]
cumSum = scanl (+) 0    -- places overall sum as the last element

cumSumR :: (Integral a) => [a] -> [a]
cumSumR = scanr (+) 0   -- places overall sum as the first element

-- Scan Example
-- How many elements does it take for the sum of the roots of all 
-- natural numbers to exceed 1000?

sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- map sqrt [1..] produces [1, 1.414, 1.732, 2 ....] (infinite list)
-- scanl1 (+) is not an equivalent of scanl (+) 0  
-- scanl1 (+) does not give 0 as the first element which scanl (+) 0 does!
-- scanl1 (+) (map sqrt [1..])) returns [1, (1+1.414), (1+1.414+1.732)..]
-- takeWhile takes all the elements which are less than 1000
-- calculate length of the list produced by takeWhile, add 1 to it.

-- NOTE: filter does not work on infinite lists: 
-- filter needs to check for each and every element in the list
-- takeWhile works on infinite lists; It stops the moment it encounters False.

-- Function Application (also a function) "$"
-- Definition
-- ($) :: (a -> b) -> a -> b  
-- f $ x = f x  

-- Default all functions are left associative
-- f x y z is nothing but ((f x) y) z
-- $ helps in making it right associative
-- sqrt 4 + 3 + 9 is 

sqEx1 = sqrt 4 + 3 + 9      -- (sqrt 4) + 3 + 9 = 14
sqEx2 = sqrt (4 + 3 + 9)    --  sqrt 16 = 4
-- This can be simply written as
sqEx3 = sqrt $ 4 + 3 + 9    -- no need of parenthesis. Evaluates right first.

-- Both the following expressions evaluates to same value (80)
dollarEx1 = sum (filter (> 10) (map (*2) [2..10]))
dollarEx2 = sum $ filter (> 10) $ map (*2) [2..10]

-- Without $ the following is not possible!
-- map (3) [(4+), (10*), (^2), sqrt]  
operateThree = map ($ 3) [(4+), (10*), (^2), sqrt]  

-- Function Composition
-- ( (f(g)) x)  = f (g (x) )
-- "." is used for function composition in Haskell
-- Definition
-- (.) :: (b -> c) -> (a -> b) -> a -> c  
-- f . g = \x -> f (g x)  

-- Example
mulByMin3 = negate . (* 3) 
minus6 = mulByMin3 2

-- Function composition used for creating functions on the fly
-- just very similar to the use of lambdas, only more elegant.

-- Creating negative numbers from the given list
negLambda = map (\x -> negate (abs x)) 
negList = negLambda [5,-3,-6,7,-3,2,-19,24] -- o/p: [-5,-3,-6,-7,-3,-2,-19,-24]

-- Same, using function composition
negFnComp = map (negate . abs) 
negList' = negFnComp [5,-3,-6,7,-3,2,-19,24]
-- So, function composition is right associative
-- unlike functions which are left associative.

-- Both below gives [-14, -15, -27]
negSumTail = map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]  
negSumTail' = map (negate . sum . tail) [[1..5],[3..6],[1..7]]  

-- Same, using Function Application operator $
negSumTail'' = map (\xs -> negate $ sum $ tail xs) [[1..5],[3..6],[1..7]]

-- Function Composition is used to remove x on both sides
fnOld x =  ceiling (negate (tan (cos (max 50 x))))  -- Need x on both sides!
fnAppl x = ceiling $ negate $ tan $ cos $ max 50 x  -- Need x on both sides!

fnComp = ceiling . negate . tan . cos . max 50      -- Removed!
-- The above is called "pointfree" or "pointless" style.


-- Revisiting Odd Square Sum
-- Sum all squares which are odd and less than 10000

-- As written before:
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))    

-- Now, with pointless style.
-- If a function ends with 3 braces, there could be 3 "." used.
oddSqSumPointFree :: Integer  
oddSqSumPointFree = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- Better Readability
oddSqSumLetIn :: Integer  
oddSqSumLetIn =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit
    
-- End of Chapter --