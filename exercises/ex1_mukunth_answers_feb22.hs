-- 1. Write a function to get the volume of a cylinder

pi :: Float
pi = 3.14
doubleRadius r = r * r
Cyl_Vol :: Float -> Float -> Float
Cyl_Vol cylinderVolume = pi * doubleRadius r * h 

------------------------------------------------------------------

--2. Write your own funtions for
  --  1. maximum of a list
  --  2. minimum of a list
  --  3. length of a list
  --  4. sum of a list
  --  5. head of a list
  --  6. tail of a list
 --  7. init of a list
 --   8. last of a list
	

	
	
--------------------------------------------------------------------	
	
-- 3. Write a function to generate this series [1, -2, 3, -4, 5 ..]

evenNegatives x = if (x mod 2) /= 0 then x else x = (-x). 

evenNegatives xs = [if (x mod 2) /= 0 then x else x = (-x) | x <- xs]

--------------------------------------------------------------------

-- 4. Write a function to generate a tuple with +1 and -1 values of the given number



--------------------------------------------------------------------

-- 5. Write a function to calculate the hypotenuse of a triangle

double a = a * a
hypotenuse c = sqrt(double a + double b)

--------------------------------------------------------------------

-- 6. Write a function to calculate the left over area inside a circle circumscribing a square.



--------------------------------------------------------------------

-- 7. Write a function to take out the second element in the list

xs = [1,2,3,4,5]
secondElement_list a = head (tail xs)

--------------------------------------------------------------------

-- 8. Write a function to take out the penultimate element in the list

xs = [1,2,3,4,5]
penultimateElement_list a = last(init xs)

--------------------------------------------------------------------

-- 9. Write a function that takes an alphabet and creates a tuple 'a' gives ('a', 1); 'A' gives ('A', 1)



--------------------------------------------------------------------

-- 10. Country and Capital are given as list of tuples [('India', 'Delhi')]. Write functions getCapital and getCountry that takes in country and capital respectively and gives proper result.


capitals xs = xs ++ ys

--------------------------------------------------------------------

-- 11. Write a function to convert celcius to farenheit
 
celciusToFarenheit a = ((a * 9) / 5 ) + 32


--------------------------------------------------------------------

-- 12. Generalise the temperature conversion function.  From To Value
    Example: 100 "C" "to K"  gives 373
             373 "K" "to F"  gives 212
			 
celciusToKelvin a = a + 273.15

kelvinToFahrenheit b = 1.8 * (b - 273) + 32

kelvinToCelcius c = c - 273
