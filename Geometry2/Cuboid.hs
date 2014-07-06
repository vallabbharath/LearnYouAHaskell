-- Method 2 of creating a module
-- Create a folder with the module name and write individual ".hs" files inside
-- Note the usage of "Module.subModule" for module name definition

module Geometry2.Cuboid(
    volume,
    area) where  
  
volume :: Float -> Float -> Float -> Float  
volume a b c = rectangleArea a b * c  
  
area :: Float -> Float -> Float -> Float  
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
  
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b  