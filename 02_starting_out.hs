-- from http://learnyouahaskell.com/starting-out

doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100  then x  else x*2  

conanO'Brien = "It's a-me, Conan O'Brien!"   

lostNumbers = [4,8,15,16,23,42]

hw = "hello" ++ " " ++ "world"

-- to put at the beginning:
sm_cat = 'A':" SMALL CAT"

new_list = 5:[1,2,3,4,5]  -- [5, 1, 2, 3, 4, 5]

-- get element out of a list using !!
the_letter = "Arunram A" !! 6  -- indexing starts at 0; this gets m
the_number = [1,2,3,4,5] !! 3  -- gets 4

-- lists can be compared; compare first element; if equal, go next and so on
bool_value = [3,2,1] > [2,10,100]   -- True
bool_value2 = [3,4,2] > [3,4]       -- True; nothing to compare the 3rd against!

a = head [5,4,3,2,1]        -- a gets 5
b = tail [5,4,3,2,1]        -- b gets [4,3,2,1]  (everything except head)

c = last [5,4,3,2,1]        -- c gets 1
d = init [5,4,3,2,1]        -- d gets [5,4,3,2] (everything except last)

len = length [5,4,3,2,1]  -- len gets 5
bool_null = null [1,2,3]  -- bool_null gets False
bool_what = null []       -- now, bool_what gets True

revd = reverse ['a', 'b', 'c', 'd']  -- revd gets "dcba"

-- both the following expressions are same
cam1 = ['c', 'a', 'm', 'e', 'l']
cam2 = "camel"

-- but this is different. list of strings (with double quotes)
cam3 = ["c", "a", "m", "e", "l"]

-- 'take' extracts specified number of elements from a list
taken1 = take 3 [1, 2, 3, 4, 5]
taken2 = take 0 [1, 2, 3]                   -- returns empty list []
taken3 = take 4 ["c", "a", "m", "e", "l"]
taken4 = take 2 ['c', 'a', 'm', 'e', 'l']   -- returns "ca"

-- 'drop' is just the negation of 'take'
dropped1 = drop 3 [1, 2, 3, 4, 5]
dropped2 = drop 0 [1, 2, 3]                   
dropped3 = drop 4 ["c", "a", "m", "e", "l"]   -- returns ["l"]
dropped4 = drop 2 ['c', 'a', 'm', 'e', 'l']   -- returns "mel"

-- 'take' and 'drop' with same argument - gives the complete string back
inList = [1, 2, 3, 4, 5]
merged1 = take 3 inList ++ drop 3 inList

-- A new function mergedFn giving back the complete list below
mergedFn n list = take n list ++ drop n list

-- Circular Shift can be achieved with 'drop' and 'take'
leftShiftCircular n list = drop (n `mod` length(list)) list 
    ++ take (n `mod` length(list)) list
rightShiftCircular n list = drop (length(list) - (n `mod` length(list))) list 
    ++ take (length(list) - (n `mod` length(list))) list

shiftL1 = leftShiftCircular 7 [1, 2, 3, 4, 5]
shiftR1 = rightShiftCircular 7 [1, 2, 3, 4, 5]

min_val = minimum [1, 4, 89, 23, 17, 2, -3]     -- min_val gets -3
max_val = maximum [1, 4, 89, 23, 17, 2, -3]     -- max_val gets 89

sum_val = sum [1, 2, 3, 4, 5]                   -- sum_val gets 15
prod_val = product [1, 2, 3, 4, 5]              -- prod_val gets 120

is_present = elem 3 [1, 2, 3, 4, 5]         -- 'elem' checks for the presence
                                            -- Will Return True
-- The above expression can also be written as..
also_fine = 3 `elem` [1, 2, 3, 4, 5]        -- also_fine is True

-- To specify range, use following syntax
one_to_ten = [1..10]                        -- contains all nums from 1 to 10

-- If the increments are other than +1, (even for -1), the second number in the
-- sequence must be given.
-- As per above rule, never specify [10..1].. it needs [10, 9...1]

ten_to_one = [10, 9..1]                     -- ten to one
-- Other way of writing [10, 9..1] is reverse [1..10]
one_to_ten_reversed = reverse [1..10]       -- is equivalent to ten_to_one

-- Try making lists with increments other than "+1"
even_nums = [2, 4..20]                      -- Even numbers <= 20

-- This works with letter too
alphas = ['a'..'z']                         -- returns abcd...xyz (one string)

-- To reiterate, list of characters is string, different from list of strings

-- Infinite lists - Haskell will not evaluate this, "Lazy Evaluation"
all_evens = [2, 4..]

-- Now all_evens get evaluated till 100 elements
even_100 = take 100 all_evens

-- "cycle" creates an infinite list by repeating the input list
cycle_vowels = cycle ['a', 'e', 'i', 'o', 'u']
take_8_vowels = take 8 cycle_vowels         -- Gives "aeiouaei"

-- "repeat" creates an infinite list by taking a single element
repeat_5 = repeat 5
ten_fives = take 10 repeat_5

-- The above objective can also be achieved by:
ten_fives_using_cycle = take 10 (cycle [5])

-- Instead, simply use "replicate"
ten_fives_via_replicate = replicate 10 5

-- Mathematics Function::
-- fn(x) = {expression in x | x belongs to N; x is odd}

-- In Haskell, a list comprehension is similar to a Math function
natural_100 = [1..100]
even_lt_20 = [x * 2 | x <- natural_100, x <= 10]

-- Get all numbers less than 200 which when div by 11 gives reminder 6
by_11_rem_6 = [ x | x <- [1..200], x `mod` 11 == 6] 

-- LIST COMPREHENSION:  Take in from a list, filter and take required elements.

-- A function expression, takes out just the even numbers
even_fn xs = [ x | x <- xs, even x]

-- Using the function expression to create even numbers till 20
even_till_20 = even_fn [1..20]

-- even tells whether the given number is even or odd. (Used above)
is_5_even = even 5              -- Returns False
is_6_even = even 6              -- Returns True

-- Following filters and takes even numbers, divides by 5 and tells true or false
checkerlist xs = [if x `mod` 5 == 0 then True else False | x <- xs, even x]

mod5checker x = if x `mod` 5 == 0 then True else False

-- Returns all numbers which are even and also multiples of 5 (in input list)
even_5multiples xs = [x | x <- xs, mod5checker x, even x]

-- All numbers less than 20; exclude certain numbers
specific_20 = [ x | x <- [1..20], x /= 7, x /= 11, x /= 13]  

-- List comprehension in two variables. Takes each val. of x and operates on y.
xy_product = [x * y | x <-[1, 2, 3], y <-[10,20,30]]

-- Can add filters specific to x, or y, or to both
xy_product_filtered = [x * y | x <-[1, 2, 3], y <-[10,20,30], x >= 2, x * y > 40]

adverbs = ["slyly", "nicely", "thoughtfully"]  
verbs = ["made", "written", "done"]  
joined_words = [adverb ++ " " ++ verb | adverb <- adverbs, verb <- verbs]

-- Function for calculating length of a list; "_" is "do-not-care-variable"
len_fun xs = sum [1 | _ <- xs] 
-- could also have been written as len_fun xs = sum [1 | x <- xs] 

-- Function to filter vowels
filter_vowels xs = [x | x <- xs, x `elem` "aeiouAEIOU"]

-- Function to remove vowels
remove_vowels xs = [x | x <- xs, not(x `elem` "aeiouAEIOU")]

-- Nested List Comprehensions
string_list = ["learn", "you", "a", "haskell", "for", "great", "good"]
-- pruned_list below will contain all 7 strings above, with vowels removed!
pruned_list = [ [remove_vowels xs] | xs <- string_list]

-- Lists can contain any number of elements. A nested list can contain 
--     individual lists of varying dimensions.  [[1,2], [1,2,3]]
-- List will contain all elements of the same type. [1, 'a'] is not possible
-- Tuple can contain elements of varying type. (1, 'a') is possible!
-- But, a list of tuple contains same number of elements / configuration in 
--     each tuple [(1, 'a'), (2, 'b', False)] is not possible
-- Tuple of tuple can contain ((1, 'a'), (2, 'b', False)) 

sample_tuple = (1, 2)

-- fst takes the first element of a pair i.e 2-tuple (not of triple of 4-tuple)
first_elem = fst sample_tuple
-- snd takes the second element of a pair (not of triple of 4-tuple)
second_elem = snd sample_tuple

-- zip takes two lists and converts to a single list of pairs.
paired = zip [1, 2, 3] ['a', 'b', 'c'] -- produces [(1,'a'),(2,'b'),(3,'c')]


-- Let us write a make_tuple function which creates a tuple from a list
make_tuple :: [a] -> (a, a)         -- this is a type declaration!
make_tuple [a, b] = (a, b)

-- we can create our own zip function as below
-- this is not the same as zip.  The following will generate 9 tuples in list
own_zip xs1 xs2 = [ (x, y) | x <- xs1, y <- xs2]

-- here is the own zip: but it requires recursion - covered in Chapter 5
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- zip with infinite lists
paired_alpha = zip [1..]["One", "Two", "Three"]


-- Make triangles using list comprehension
right_triangles = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], 
    a^2 + b^2 == c^2]

-- Some Other Points::
-- On a GHC console
--  :l first        Loads the script "first.hs" (.hs can be omitted)
--  :l first.hs     This too is just fine
--  :r              Loads the active script, as of now "first.hs" 
--  :reload         Same as above
--  :t              Gets the type of the expression  
--                  Integer, [Integer], Bool, Char, [Char] etc

-- End of Chapter --