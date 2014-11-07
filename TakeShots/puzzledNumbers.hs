import Data.Char

-- "puzzler" function takes a string input and returns a number
-- puzzler "5+3+2" == 151022
-- puzzler "9+2+4" == 183652
-- puzzler "8+6+3" == 482466
puzzler :: String -> Int
puzzler xs = processNums (map (asInt_fold) (splitWith (=='+') xs))

-- Takes a list of 3 ints and generate the desired result as int.
-- This is the key part, how the final number is got, from the given input
processNums :: [Int] -> Int
processNums (x:y:z:[]) = asInt_fold (show (x*y) ++ show (x*z) ++ 
                                        show (x*y + x*z - y))
processNums _   = 0

-- Given a number as string, converts it into int type. "123" will be 123    
processInt :: String -> Int    
processInt xs = foldl (convertInt) 0 (map digitToInt (xs))

-- helper function for foldl operation in processInt 
convertInt :: Int -> Int -> Int
convertInt x y = y + 10 * x

-- effectively converting a string to a number, taking care of prefix "-"
asInt_fold :: String -> Int
asInt_fold [] = 0
asInt_fold (x:xs) 
    | x == '-' = 0 - processInt xs
    | otherwise = processInt (x:xs)

-- given a list and a separator element, process the contents to separate lists
splitWith :: Eq a => (a -> Bool) -> [a] -> [[a]]
splitWith f xs = filter (/=[]) (separateItems f xs)

-- helper function for splitWith.  This does the actual separation
separateItems :: (a -> Bool) -> [a] -> [[a]]
separateItems _ [] = []
separateItems f xs =  (fst broken) : separateItems f (sTail (snd broken))
                        where broken = break f xs

-- safe version of tail. sTail of empty list gives [] instead of error
sTail :: [a] -> [a]
sTail (_:xs) = xs
sTail _ = []
