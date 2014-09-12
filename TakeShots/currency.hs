{-# OPTIONS_GHC -Wall #-}
-- Representing the following series
-- 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000
-- 0  1  2  3   4    5   6    7    8   9

-- 3   10  2 * 5   1, 2
-- 4   20  2 * 10  1, 3
-- 5   50  5 * 10  2, 3
-- 6   100 5 * 20  2, 4
-- 7   200 10* 20  3, 4
-- 8   500 10* 50  3, 5
-- 9  1000 20* 50  4, 5     

-- value(n) = {value((n+1)/2 - 1) * value((n+1)/2) for all n belongs to (3, 5, 7...)}
-- value(n) = {value(n/2 - 1) * value(n/2 + 1) for all n belongs to (4, 6, 8...)}
-- value(n) = {(1,2,5) for n = (0, 1, 2) respectively}

-- Now, representing the same in Haskell

-- Get the value, given the index
value :: (Integral a) => a -> a
value 0 = 1 -- means value for index 0 is 1
value 1 = 2 
value 2 = 5
value n 
    | odd n  = value ((quot (n+1) 2) - 1) * value (quot (n+1) 2)
    | even n = value ((quot n 2) - 1) * value ((quot n 2) + 1)


-- Get the value, given the index
-- Second implementation
value2 :: (Integral a) => a -> a
value2 n 
    | n == 0 = 1 -- means value for index 0 is 1
    | n == 1 = 2 
    | n == 2 = 5
    | odd n  = value2 ((quot (n+1) 2) - 1) * value2 (quot (n+1) 2)
    | even n = value2 ((quot n 2) - 1) * value2 ((quot n 2) + 1)

-- Get the value, given the index
-- Third implementation
value3 :: (Integral a) => a -> a
value3 n 
    | n == 0 = 1 -- means value for index 0 is 1
    | n == 1 = 2 
    | n == 2 = 5
    | otherwise  = value3 p * value3 q 
    where p = ceiling (fromIntegral n / 2 - 1) 
          q = floor (fromIntegral n / 2 + 1) 

-- Get the value, given the index
-- Fourth implementation          
value4 :: (Integral a) => a -> a
value4 n = case n of 0 -> 1 -- means value for index 0 is 1
                     1 -> 2 
                     2 -> 5
                     n -> value3 p * value3 q 
                            where   p = ceiling (fromIntegral n / 2 - 1) 
                                    q = floor (fromIntegral n / 2 + 1)                       

-- Get the value, given the index
-- Fifth implementation          
value5 :: (Integral a) => a -> a
value5 n = case n of 0 -> 1 -- means value for index 0 is 1
                     1 -> 2 
                     2 -> 5
                     n -> let p = ceiling (fromIntegral n / 2 - 1) 
                              q = floor (fromIntegral n / 2 + 1)                       
                          in value3 p * value3 q 
                                    