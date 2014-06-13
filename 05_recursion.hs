-- Chapter 5 - Recursion
-- http://learnyouahaskell.com/recursion


-- Takes a list and returns the maximum 
-- Using explicit comparison
max_fn :: (Ord a) => [a] -> a
max_fn [] = error "List is empty"
max_fn [x] = x
max_fn (x:xs)           -- Note the curved braces here
    | x > max_in_tail = x
    | otherwise = max_in_tail
    where max_in_tail = max_fn xs
    
    
-- Second implementation of max_fn
-- Takes in a list and returns the maximum
-- Using the built-in 'max' which compares two and returns max
max_fn2 :: (Ord a) => [a] -> a
max_fn2 [] = error "List is empty"
max_fn2 [x] = x
max_fn2 (x:xs) = max x (max_fn2 xs) -- Using recursion and 'max' 

-- Implementing "replicate" 
-- "replicate" is a built-in function which takes two arguments
-- replicate 3 5 returns [5,5,5]
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x 
    
-- Implementing "take"
-- "take is a built-in function takes two arguments: list and number
-- extracts specified number of elements from the list
take' :: (Num i, Ord i) => i -> [a] -> [a]      
take' _ [] = []
take' n (x:xs)
    | n <= 0    = []
    | otherwise = x : take' (n-1) xs
    
-- Implementing "reverse"    
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]      

-- Infinite list
-- Implementing "repeat"
repeat' :: a -> [a]  
repeat' x = x:repeat' x

-- repeat 1  => this generates a list of infinite '1' s
-- So, where is it useful?
three_2s = take' 3 (repeat' 2)        -- produces [2, 2, 2]
-- means, take 3 elements out of an infinite list containing 2s

-- Let us generate power of 2
-- input is [0, 1, 2 ..] or any list whose power of 2 is returned
pwr_of_2 :: (Integral a) => [a] -> [a]
pwr_of_2 [] = []
pwr_of_2 [x] = [2^x]
pwr_of_2 (x:xs) = 2^x : pwr_of_2 xs

-- implementing zip function
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- implementing 'elem' function
-- checks whether a particular element is present in the list
-- usage elem 'a' [list]  or 'a' `elem` [list]
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs  -- recursively check whether a present in rem.
-- If at any point 'a' is compared agains an empty list, return False!!

-- quicksort!!
-- Algorithm
-- A sorted list is a list that has all the values smaller than (or equal to) 
-- the head of the list in front (and those values are sorted), then comes the
-- head of the list in the middle and then come all the values that are bigger 
-- than the head (they're also sorted). 
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  
    
-- End of Chapter --