{- 
    Problem From:
    https://twitter.com/1HaskellADay/status/566127619020980224
    
    http://lpaste.net/120422 
-}

{--

    From @OnThisDayinMath

    2015 The first of three Friday the 13ths this year. When will the next year be 
    with three?  
-}


threeFriday13ths :: Integer -> Integer
threeFriday13ths year 
    | (mc == 5) || (mc == 2) = yr
    | otherwise = threeFriday13ths yr
    where yr = (year + 1)
          cc = floor ((fromIntegral yr) / 100)
          yy = mod yr 100
          mc = mod ((2 * (mod cc 4)) + 6 - yy - floor ((fromIntegral yy) / 4)) 7

          
{-
========
WORKOUT!
========
    F S M T T F Su W F M Th S T Th
mc  5 6 1 2 2 5 0  3 5 1 4  6 2 4

dd / mm / ccyy

x = dd + mc + yy + floor (yy / 4) - 2 * rem (cc/4)
d = rem (x / 7) 

d  --> 5 
dd --> 13

5, 12, 19 etc = 13 + mc + yy + floor(yy/4) - 2 * rem(cc/4)

given = 2011
next = 2012
ccyy = 2012

5, 12, 19 etc = 13 + mc + 12 + 3  - 0
5, 12, 19 etc = 28 + mc
mc = 5

(5 - 13 - yy - floor(yy/4) + 2 * rem (cc/4)) mod 7 = mc
if mc == 5 or 2, then return the year or else do for next

(-8 - yy - floor(yy/4) + 2 * rem (cc/4)) mod 7 = mc

(6 - yy - floor(yy/4) + 2 * rem (cc/4)) mod 7 = mc

(2 * rem (cc/4) + 6 - yy - floor(yy/4)) mod 7 = mc

In the table above, only 5 or 2 can correspond to 3 similar days.
-}
