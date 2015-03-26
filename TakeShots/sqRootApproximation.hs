-- Square Root Approximation method
-- Taken from http://www.quora.com/What-are-some-great-mathematics-tricks-you-know-of/answer/Lakshya-Jain?srid=5bi8&share=1

import Data.List

diffList :: Float -> [Float]
diffList x = map abs $ zipWith (-) (map (**2) [1..(x/2)]) [x, x ..]

takeOut :: Maybe Int -> Int
takeOut (Just a) = a
takeOut Nothing = 0

baseNum x = (takeOut $ elemIndex (minimum $ diffList x) (diffList x)) + 1

limit x = x / (fromIntegral $ baseNum x)

sqrtApprox x = (fromIntegral (baseNum x) + (limit x)) / 2

