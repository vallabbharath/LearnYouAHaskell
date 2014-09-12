import Control.Monad.Writer  
import Control.Monad.State
  
gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b) 
        
  
gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result         
        
type Stack = [Int]  

{-  
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  
  
push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)
-}
pop :: State Stack Int
-- The following line was wrong in the book:
-- pop = State $ \(x:xs) -> (x,xs)  
pop = do
 x:xs <- get
 put xs
 return x

push :: Int -> State Stack ()  
-- The following line was wrong in the book:
-- push a = State $ \xs -> ((),a:xs)
push a = do
 xs <- get
 put (a:xs)
 return ()

pop1 = runState pop [1..5]
push1 = runState (push 1) [2..5]

stackManip :: State Stack Int  
stackManip = do  
 push 3  
 a <- pop  
 pop 
 