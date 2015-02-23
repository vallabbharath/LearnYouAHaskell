-- from package arithmoi
import Math.NumberTheory.Primes.Factorisation
import Data.List

givenNum = 72
primeFactors = factorise givenNum

expand (c, x) = (c, [0..x])
factorPairs = map expand primeFactors

makeBase (x, ys) = map ((^) x) ys
factorBases = map makeBase factorPairs

makeFactors [xs, ys] = [x * y | x <- xs, y <- ys]
factors = makeFactors factorBases

factorSets xs = [(x, y, z) | x <- xs, 
                             y <- xs, 
                             z <- xs, 
                             x * y * z == givenNum, 
                             y >= x, 
                             z >= y]
possibleTriplets = factorSets factors

sumTriplet (a, b, c) = a + b + c
possibleSums = map sumTriplet possibleTriplets

mappedTriplets = zipWith (,) possibleSums possibleTriplets

uniqueSums = nub possibleSums
nonUniqueSums = nub $ possibleSums \\ uniqueSums

takeTriplet x (a, ts)
    | (x == a) = Just ts
    | otherwise = Nothing
    
dig x = map (takeTriplet x) mappedTriplets
pruner x = filter (/= Nothing) (dig x)

resultList = map pruner nonUniqueSums
