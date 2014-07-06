-- Method 2 of creating a module
-- Create a folder with the module name and write individual ".hs" files inside
-- Note the usage of "Module.subModule" for module name definition

module Geometry2.Cube(
    volume,
    area) where  
  
import qualified Geometry2.Cuboid as Cuboid  
  
volume :: Float -> Float  
volume side = Cuboid.volume side side side  
  
area :: Float -> Float  
area side = Cuboid.area side side side  