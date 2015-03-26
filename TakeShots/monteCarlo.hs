import Data.List
import Control.Monad
import System.Random

{-# LANGUAGE BangPatterns #-}

-- Number of samples to take
count = 10000
 
-- Function to process our random sequence
process :: [(Double, Double)] -> (Int, Int)
process = foldl' sumInCircle (0, 0)
 
-- Function to process a running value and a random value, producing a new running value.
sumInCircle :: (Int, Int) -> (Double, Double) -> (Int, Int)
sumInCircle (!ins, !total) (x, y) = (ins + if x*x + y*y < 1.0 then 1 else 0,
                               total + 1)
 
-- Function to display the running value.
display:: (Int, Int) -> String
display (heads, coins) = "π = "  ++ (show $ 4.0 * fromIntegral heads / fromIntegral coins)
 
-- function to prepare the random sequence for processing
prep :: [Double] -> [(Double, Double)]
prep (a:b:r) = (a,b):prep r
 
main = do
  g <- newStdGen
  putStrLn . display . process . take count . prep $ randoms g
