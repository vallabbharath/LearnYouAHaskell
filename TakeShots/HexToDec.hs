import Data.Char
import Control.Applicative

-- Implementation 1
-- Converts a given hexa decimal number (string) into a decimal number
convToDec :: [Char] -> Integer
convToDec inHex =   sum $ zipWith (*) (zipWith (^) (take lenH [16, 16..]) $ 
                                                   [lenH - 1, lenH - 2..0]) $ 
                                       map (toInteger.digitToInt) inHex 
                    where lenH = length inHex
                    
-- Implementation 2
convToDec1 :: [Char] -> Integer
convToDec1 inHex = let
    power16     = 
        (^) <$> (ZipList $ repeat 16) <*> (ZipList [lenH - 1, lenH - 2..0])
        where lenH = length inHex
    digitVals   =   ZipList (map (toInteger . digitToInt) inHex)
        in sum . getZipList $ (*) <$> power16  <*> digitVals
    