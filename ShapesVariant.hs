-- (..) after data type inside module command signifies all "value constructors"
-- are also exported with the said data type.  Here, for Point, the value
-- constructors are exported, but for Shape, these are not exported.
-- To create an instance of Shape, one has to use baseCircle or baseRect fns.
module ShapesVariant( 
    Point(..),      -- exporting a data type with assoc. value constructors
    Shape,      -- exporting a data type
    surface,        -- exporting a function
    nudge,
    baseCircle,
    baseRect  
) where  

data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Function to calculate the surface area
surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1) 

-- Function to nudge a shape! be it rectangle or circle, move by defined amount.
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = 
    Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))  
    
-- Definitions of base circle and base rectangle
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
