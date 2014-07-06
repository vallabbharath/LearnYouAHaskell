-- Chapter 8: Making Our Own Types and Type Classes
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses

data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  
