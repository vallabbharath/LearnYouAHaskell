
-- Converts temperature from one unit to another

celcius :: Float -> Char -> Float
celcius x 'F' = fromFarenheit x
celcius x 'K' = fromKelvin x
celcius x 'C' = x
celcius x _ = 0

fromKelvin :: Float -> Float
fromKelvin x = x - 273.15

fromFarenheit :: Float -> Float
fromFarenheit x = (x - 32) / 1.8

toKelvin :: Float -> Float
toKelvin x = x + 273.15

toFarenheit :: Float -> Float
toFarenheit x = x * 1.8 + 32

temperature :: Float -> Char -> Char -> Float
temperature x f t 
    | t == 'C' = temprC
    | t == 'K' = toKelvin temprC
    | t == 'F' = toFarenheit temprC
    | otherwise = 0
    where temprC = celcius x f
    