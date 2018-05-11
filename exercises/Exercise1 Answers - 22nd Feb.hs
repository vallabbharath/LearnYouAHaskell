import Data.Char (ord)

--1. Write a function to get the volume of a cylinder
volumeOfCyl :: (Fractional a) => a -> a -> a
volumeOfCyl r h = pi * r *r * h where pi = 22/7


--2. Write your own functions for
testList = [1,2,3]
--    1. maximum of a list
max' :: (Ord a) => [a] -> a
max' [] = error "Operation not supported for empty list"
max' [x] = x
max' (x:xs)
	| x > maxOfRemainingList = x
	| otherwise = maxOfRemainingList
		where maxOfRemainingList = max' xs 
		
--    2. minimum of a list
min' :: (Ord a) => [a] -> a
min' [] = error "Operation not supported for empty list"
min' [x] = x
min' (x:xs)
	| x < minOfRemainingList = x
	| otherwise = minOfRemainingList
		where minOfRemainingList = min' xs 
		
--    3. length of a list
len' :: [a] -> Int
len' [] = 0
len' (x:xs) = 1 + len' xs
		
--    4. sum of a list
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs
		
--    5. head of a list
head' :: [a] -> a
head' [] = error "Operation not supported for empty list"
head' (x:_) = x

--    6. tail of a list
tail' :: [a] -> [a]
tail' [] = error "Operation not supported for empty list"
tail' (x:xs) = xs

--    7. init of a list
init' :: [a] -> [a]
init' [] = error "Operation not supported for empty list"
init' (x:xs) 
	| null xs = []
	| otherwise = x:init' xs 

--    8. last of a list
last' :: [a] -> [a]
last' [] = error "Operation not supported for empty list"
last' (x:xs) 
	| null xs = [x]
	| otherwise = last' xs 

--3. Write a function to generate this series [1, -2, 3, -4, 5 ..]

-- Note : I don't really like using '++' here in recursion, as it is a costly operation. Just used a logic.. will reframe better logic soon.
genSignChangingList :: Int -> [Int]
genSignChangingList a 
	| (a <= 0) = error "Input length should be greater than 0"
	| (a == 1) = [1]
	| even a = genSignChangingList (a-1) ++ [(-a)]
	| otherwise = genSignChangingList (a-1) ++ [a]


--4. Write a function to generate a tuple with +1 and -1 values of the given number
genNeighboursInTuple :: Int -> (Int, Int)
genNeighboursInTuple a = ((a-1), (a+1))

--5. Write a function to calculate the hypotenuse of a triangle
calcHypotunese :: (Floating t) => t -> t -> t
calcHypotunese a b = sqrt ((a*a) + (b*b))

--6. Write a function to calculate the left over area inside a circle circumscribing a square.

-- This function is for calculating the left over area inside a circle circumscribing a square.
-- Diameter of circle = Diagonal of square.
getDiagonalOfSquareFromSide :: Float -> Float
getDiagonalOfSquareFromSide a = (sqrt 2) * a

getSideOfSquareFromDiagonal :: Float -> Float
getSideOfSquareFromDiagonal a = a / (sqrt 2)

areaOfSquare :: Float -> Float
areaOfSquare a = a*a

areaOfCircle :: Float -> Float
areaOfCircle a = pi*a*a 
					where pi = 22/7

calculateLeftOverAreaGivenSide :: Float -> Float
calculateLeftOverAreaGivenSide a = (areaOfCircle radius) - (areaOfSquare a)
	where radius = (getDiagonalOfSquareFromSide a)/2
	
calculateLeftOverAreaGivenRadius :: Float -> Float
calculateLeftOverAreaGivenRadius radius = (areaOfCircle radius) - (areaOfSquare a)
	where a = getSideOfSquareFromDiagonal (radius*2)
	
--7. Write a function to take out the second element in the list
take2ndElement :: [a] -> a
take2ndElement (x:y:yz) = y
take2ndElement _ = error "less than two elements"

--8. Write a function to take out the penultimate element in the list
takePenultimateElement :: [a] -> a
takePenultimateElement (x:xs)
		| len' xs == 0 = error "less than two elements"
		| len' xs == 1 = x
		| otherwise = takePenultimateElement xs
		
--9. Write a function that takes an alphabet and creates a tuple 'a' gives ('a', 1); 'A' gives ('A', 1)
alphabets = ['a'..'z']
capsAlphabets = ['A'..'Z']
numbers = [1..26]

--Method 1 using fromEnum 
genTuple :: Char -> (Char, Int)
genTuple x 
	| x `elem` alphabets =  (x, posn x 'a') 
	| x `elem` capsAlphabets =  (x, posn x 'A')
	| otherwise = error "Invalid input"
		where posn a b = fromEnum a - fromEnum b + 1

--Method 2 using ord 		
genTuple' :: Char -> (Char, Int)
genTuple' x 
	| x `elem` alphabets =  (x, posn x 'a') 
	| x `elem` capsAlphabets =  (x, posn x 'A')
	| otherwise = error "Invalid input"
		where posn a b = ord a - ord b + 1
		
-- Method 3 using list comprehension
zippedAlphabets = alphabets `zip` numbers ++ capsAlphabets `zip` numbers
genTupleUsingList :: Char -> (Char, Int)
genTupleUsingList x 
	| not (null outputList) = head outputList 
	| otherwise = error "Invalid input"
		where outputList = [ tuple | tuple <- zippedAlphabets, fst(tuple) == x]
		
--10. Country and Capital are given as list of tuples [('India', 'Delhi')]. Write functions getCapital and getCountry that takes in country and capital respectively and gives proper result.
inputTuples = [("India", "Delhi"), ("Australia", "Canberra")]

getCapital :: [(String, String)] -> String -> String
getCapital xs country 
	| not (null outputList) = head outputList 
	| otherwise = "Not in list"
		where outputList = [ (snd tuple) | tuple <- xs, country == (fst tuple)]

getCountry :: [(String, String)] -> String -> String
getCountry xs capital 
	| not (null outputList) = head outputList 
	| otherwise = "Not in list"
		where outputList = [ (fst tuple) | tuple <- xs, capital == (snd tuple)]
		
--11. Write a function to convert celcius to farenheit
celciusToFarenheit :: (Fractional t) => t -> t
celciusToFarenheit celciusValue =  celciusValue * 1.8000 + 32.00

--12. Generalise the temperature conversion function.  From To Value
--    Example: 100 "C" "to K"  gives 373
--             373 "K" "to F"  gives 212

temperatureConversion :: (Fractional t) => t -> Char -> Char -> t
--toUnit and fromUnit can have 3 possible values 'C', 'F', 'K' denoting Celsius, Farenheit, Kelvin

temperatureConversion inputTemperature fromUnit toUnit 
		| (fromUnit == 'C' && toUnit == 'F') = celciusToFarenheit inputTemperature
		| (fromUnit == 'C' && toUnit == 'K') = inputTemperature + 273.15
		| (fromUnit == 'K' && toUnit == 'C') = inputTemperature - 273.15
		| (fromUnit == 'K' && toUnit == 'F') = celciusToFarenheit (inputTemperature - 273.15)
		| (fromUnit == 'F' && toUnit == 'C') = farenheitToCelcius inputTemperature
		| (fromUnit == 'F' && toUnit == 'K') = (farenheitToCelcius inputTemperature) + 273.15
		| otherwise = error "Not valid input"
		
farenheitToCelcius :: (Fractional t) => t -> t
farenheitToCelcius inputTemperature = (inputTemperature - 32.00) / 1.8000
