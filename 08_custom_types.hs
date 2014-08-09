-- Chapter 8: Making Our Own Types and Type Classes
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses


-- "data" keyword is used to define a new type

-- Possible definition of Bool in the standard library
-- data Bool = False | True
-- Custom definition of the same
data Bool' = FALSE | TRUE
-- TRUE and FALSE are value constructors

-- Let us define a new data type Shape which can hold Circle or Rectangle
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
-- Shape is a "type"
-- Circle is a "value constructor"
-- Rectangle is another "value constructor"
-- Circle value constructor takes 3 fields - all are floats
-- Rectangle value constructor takes 4 fields - all are floats

-- Try the following in ghci console
-- :t Circle    => Circle :: Float -> Float -> Float -> Shape
-- :t Rectangle => Rectangle :: Float -> Float -> Float -> Float -> Shape      

-- So value constructors are nothing but "functions" - looks very similar!

-- Function to calculate the surface area using Shape, Circle and Rectangle
-- Input: Shape
-- Output: Float (surface area)
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Type definition cannot be Circle -> Float (as Circle is not a type!)
-- Circle is a value constructor for "Shape" type
-- call the surface function using surface(Circle 10 10 10)

-- Make 'Shape' type an instance of 'Show' typeclass
-- This makes "Circle 10 10 10" to get printed on console instead of error
-- Now, Haskell knows how to "show" the type
-- The usage of single quote at the end is to differentiate from the type
-- that we already created.  So that this file can be compiled!
data Shape' = Circle' Float Float Float | Rectangle' Float Float Float Float
    deriving Show

-- The following command makes a new value constructor 'Shape' which is 
-- different from the type 'Shape' that we saw.  Both have no connection at all!    
data Shapes = Shape deriving Show    

-- map command can use a value constructor as a function
-- Below a partial value constructor is used which needs a third field, passed
-- in a list, so for each element a Circle' is created
circles = map (Circle' 10 20) [4,5,6,6] 

-- Improvisation on the data types
-- Creation of a new data type "Point"
data Point = Point Float Float deriving (Show)  
data Shape'' = Circle'' Point Float | Rectangle'' Point Point deriving (Show)

-- Rewriting surface based on the new definition of Shape''
surface'' :: Shape'' -> Float  
surface'' (Circle'' _ r) = pi * r ^ 2  
surface'' (Rectangle'' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1) 

-- Now, surface'' function can be invoked by commands such as
circleArea = surface''(Circle'' (Point 3 3) 10)    
rectangleArea = surface'' $ Rectangle'' (Point 3 3) (Point 5 5)

-- Function to nudge a shape! be it rectangle or circle, move by defined amount.
-- takes in a Shape'' and 2 floats (on x and y directions)
-- returns a Shape'' data type
nudge :: Shape'' -> Float -> Float -> Shape''
nudge (Circle'' (Point x y) r) a b = Circle'' (Point (x + a) (y + b)) r  
nudge (Rectangle'' (Point x1 y1) (Point x2 y2)) a b = 
    Rectangle'' (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))  
    
-- Invocation of nudge
newCircle = nudge (Circle'' (Point 34 34) 10) 5 10
newRectangle = nudge (Rectangle'' (Point 0 0) (Point 40 100)) 60 23

-- nudge, given a base circle or base rectangle (center / lowerbottom at origin)
-- First, definitions of base circle and base rectangle
baseCircle :: Float -> Shape''
baseCircle r = Circle'' (Point 0 0) r

baseRect :: Float -> Float -> Shape''
baseRect width height = Rectangle'' (Point 0 0) (Point width height)

newRectangle' = nudge (baseRect 40 100) 60 23 -- same as newRectangle
    
-- See sample of how Shape module is exported in "shapes.hs"    
-- See syntax for data type and functions export

-- To import a module, use 
-- :m +ModuleName in the ghci shell
-- import ModuleName in a Haskell file (.hs file)
    
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  
