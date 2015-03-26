{-
MF Comparator

Objective:
1. To see IRR of any SIP or CAGR any SWP (based on past performance)
   - for any mutual fund

SIP:
   
Inputs:
    1. SIP amount
    2. SIP start date; frequency
    3. MF (or index) invested in
    4. Redemption date
    
Outputs:
    1. Total invested amount
    2. Total Redemption amount
    3. IRR
    4. Comparable Maturity @8%; @10% and @12%
    
SWP:

Inputs:
    1. Lumpsum investment
    2. SWP start date, frequency
    3. SWP amount
    4. SWP reinvestment interest rate (0%, 4% and 8%)
    5. Redemption Date
    6. MF (or index) invested in
    
Output:
    1. Total invested amount
    2. Redemption + SWP reinvest proceeds
    3. CAGR for 3 cases (0, 4 and 8% reinv. proceeds)
    4. Comparable Maturity @8%; @10% and @12%
-}

------------------------------------------------------------

-- Get the MF units for the given amount at the given NAV
units amt nav = amt / nav

navs :: Fractional a => [a]
navs = [10, 10.8, 9.3, 9.2, 8.5, 9.9, 10.5, 11.2, 10.5, 11.5, 9.5, 8.5]
dts = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
sip :: Double
sip = 1000
redeemDay = 365

numSips = length navs
unitList = map (units sip) navs
sips = take numSips $ repeat sip

totalUnits = sum unitList

invAmt = sip * (fromIntegral . length) navs
finalAmt = 12171.07 --totalUnits * last navs

days = map (redeemDay-) dts
yrs = map (/365.25) days

xirr :: [Double] -> [Double] -> Double -> [Double]
xirr sips yrs amt = [r | r <- [0.01, 0.02..100], 
    abs(sum (zipWith (*) sips (map ((1 + r/100) **) yrs)) - amt) <= 2]

    
maturity :: [Double] -> [Double] -> Double -> Double
maturity sips yrs rate = sum (zipWith (*) sips (map ((1 + rate/100) **) yrs))
