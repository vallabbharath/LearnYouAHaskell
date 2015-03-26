-- Chapter 4
-- http://learnyouahaskell.com/syntax-in-functions

-- The following is function type declaration
letterize :: (Integral a) => a -> String
letterize 1 = "One"
letterize 2 = "Two"
letterize 3 = "Three"
letterize x = "Not 1, 2, 3"     -- This is a catch all pattern!

-- A function without a catch all pattern will compile.
-- But, may throw runtime errors

-- Factorial using recursion
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial(x - 1)


-- Finding slope between two points given as tuple pairs
slope :: (Fractional a) => (a, a) -> (a, a) -> a
slope a b = (snd b - snd a) / (fst b - fst a)

-- Slope function using pattern matching
slope_pat :: (Fractional a) => (a, a) -> (a, a) -> a
slope_pat (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)

-- first, second, third functions for triples
first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z  

-- pattern matching in list comprehensions
given_list = [(a, b) | a <- [1, 2], b <- [3, 4]]
summed_val = [a + b | (a, b) <- given_list]

head' :: [a] -> a
head' [] = error "Empty List!"
head' (x:_) = x         -- Note the use of curvy brackets

tell :: (Show a) => [a] -> String
tell [] = "Empty!"
tell (x:[]) = "One Element! : " ++ show x
tell (x:y:[]) = "Two Elements! : " ++ show x ++ " and " ++ show y
tell (x:y:_) = "More than Two Elements!"

-- Function for calculating length: Using  Pattern Matching
len_fun :: (Num b) => [a] -> b
len_fun [] = 0
len_fun (_: xs) = 1 + len_fun xs

-- Function to calculate sum
sum_fun :: (Num a) => [a] -> a
sum_fun [] = 0
sum_fun (x:xs) = x + sum_fun xs

-- Usage of Patterns with "@"
first_letter :: String -> String  
first_letter "" = "Empty string, whoops!"  
first_letter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Usage of "if" or "switch" kind of statements via patterns
match_predictor :: (RealFloat a) => a -> String
match_predictor run_rate
    | run_rate <= 5.1 = "Easy to chase!"
    | run_rate <= 5.7 = "Tough fight for chasing!"
    | run_rate <= 6.3 = "Very difficult!"
    | otherwise = "Forget it!"  -- Note that "otherwise" is simply "True" in disguise!
    
-- What if "run_rate" is to be calculated based on score and overs
match_guess :: (RealFloat a) => a -> a -> String
match_guess score overs
    | run_rate <= 5.1 = "Easy to chase!"
    | run_rate <= 5.7 = "Tough fight for chasing!"
    | run_rate <= 6.3 = "Very difficult!"
    | otherwise = "Forget it!"  -- Note that "otherwise" is simply "True" in disguise!
    where run_rate = score / overs
    
-- Include the limits in terms of names (visible only inside this function).
match_chance :: (RealFloat a) => a -> a -> String
match_chance score overs
    | run_rate <= easy = "Easy to chase!"
    | run_rate <= tough = "Tough fight for chasing!"
    | run_rate <= vdiff = "Very difficult!"
    | otherwise = "Forget it!"  -- Note that "otherwise" is simply "True" in disguise!
    where   run_rate = score / overs    
            easy = 5.1
            tough = 5.7
            vdiff = 6.3
            
-- Other way of writing the names
match_win :: (RealFloat a) => a -> a -> String
match_win score overs
    | run_rate <= easy = "Easy to chase!"
    | run_rate <= tough = "Tough fight for chasing!"
    | run_rate <= vdiff = "Very difficult!"
    | otherwise = "Forget it!"  -- Note that "otherwise" is simply "True" in disguise!
    where   run_rate = score / overs    
            (easy, tough, vdiff) = (5.1, 5.7, 6.3)

-- Find maximum of two elements
max_fn :: (Ord a) => a -> a -> a
max_fn a b
    | a > b = a
    | otherwise = b

-- Compare function
compare_fn :: (Ord a) => a -> a -> Ordering
a `compare_fn` b        -- Equivalent of compare_fn a b
    | a > b = GT
    | a < b = LT
    | otherwise = EQ

    
make_initials :: String -> String -> String  
make_initials first last = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = first  
          (l:_) = last

-- Instead of defining constants in where, one can use functions
calc_interest :: (RealFloat a) => [(a, a, a)] -> [a]
calc_interest xs = [interest p n r | (p, n, r) <- xs]
    where interest p n r = p * n * r / 100
    
-- Using "let - in" keywords for similar purpose
cuboid_area :: (RealFloat a) => a -> a -> a -> a
cuboid_area l b h = 
    let side1 = 2 * l * b
        side2 = 2 * b * h
        side3 = 2 * h * l
    in side1 + side2 + side3

-- 'where' is syntactic construct; "let" defines local expressions and functions
-- let..in can be used like if..else expression, almose anywhere

pruned_string = [if 5 > 3 then "Five" else "Three", if 'a' > 'b' then "a" else "b"]

exprn_with_let = 4 * (let a = 9 in a + 1) + 2

-- Functions using let
fn_with_let = [let factorial x = product[1..x] in (factorial 5, factorial 3)]

-- To bind several variables in single line, use semicolon;
-- also see (a, b, c) are assigned using pattern matching!
num_and_foobar = (let (a, b, c) = (1, 2, 3) in sum[a, b, c], let foo="Foo "; bar = "Bar!" in foo ++ bar)

-- let in list comprehensions
-- calculate runrate and get only those which are above 5.
calc_runrate :: (RealFloat a) => [(a, a)] -> [(a, a, a)]
calc_runrate xs = [(score, overs, rr) | (score, overs) <- xs, 
    let rr = score / overs, rr > 5.0]
-- in the function above, at this part "(score, overs) <- xs" Haskell has 
-- no knowledge of "rr"

-- case expressions; pattern matching in function definitions!
-- General Syntax
-- case expression of pattern -> result  
--                   pattern -> result  
--                   pattern -> result  
--                   ...  

-- Representing the following series
-- 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000
-- 0  1  2  3   4    5   6    7    8   9
-- Get the value, given the index

value4 :: (Integral a) => a -> a
value4 n = case n of 0 -> 1 -- means value for index 0 is 1
                     1 -> 2 
                     2 -> 5
                     n -> value4 p * value4 q 
                            where   p = ceiling (fromIntegral n / 2 - 1) 
                                    q = floor (fromIntegral n / 2 + 1)                       
                                    