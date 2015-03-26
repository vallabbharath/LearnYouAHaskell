-- 20 sparrows for Re 1
-- 1 crow for Re 1
-- 1 Parrot for Rs 5

sparrow = [1..5]
crow = [1..100]
parrot = [1..100]

nSparrow x = (x `div` 1) * 20
nCrow x = (x `div` 1) * 1
nParrot x = (x `div` 5)

finalVal = [(a, b, c) | a <- sparrow, b <- crow, c <- parrot, c `rem` 5 == 0,
    (nSparrow a + nCrow b + nParrot c == 100), (a + b + c == 100)]