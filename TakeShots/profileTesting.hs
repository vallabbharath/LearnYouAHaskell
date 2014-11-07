main = print (badSum [1..1000000])
    
badSum :: Num a => [a] -> a
badSum []     = 0
badSum (x:xs) = x + badSum xs

lazySum :: Num a => [a] -> a
lazySum = go 0
  where go acc []     = acc
        go acc (x:xs) = go (x + acc) xs
        
-- Execute this using following commands
-- ghc profileTesting.hs -rtsopts
-- profileTesting.exe +RTS -s -h -i0.001
-- hp2ps -c profileTesting.hp
