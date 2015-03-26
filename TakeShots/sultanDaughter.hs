-- Taken from http://lpaste.net/5006337360527360000

{-

A certain prince Al-Khizr was in love with the sultan's daughter and asked for
her hand in marriage.

"My daughter is very choosy," said the sultan, "and wants to marry only someone
who shows extraordinary intelligence. So if you want to marry her, you must
first pass eight tests."

"What are the tests?" asked the suitor.

"Well, for the first test, you have to write down a number that will be sent to
the princess. She will then send back a number to you. If she sends back the 
very same number that you have sent her, then she will allow you to take the
second test. But if her number is different from yours, then you are out."

"Now, how can I possibly know what number to write?" asked the suitor. "How can
I guess what number the princess has in mind?"

"Oh, she doesn't have a number in mind," said the sultan. "The number she sends
back is DEPENDENT on the number you send. The number you send /completely
determines/ what number she will send back. And if you send the right number,
then she will send back the same number."

"Then how can I guess the right number?" asked the suitor.

"It's not a matter of /guessing/," said the sultan. "You must /deduce/ the
correct number from the rules I am about to give you. For any number x and y,
by xy I mean not x times y but x followed by y, both numbers, of course, 
written in base ten Arabic notation. For example, if x is 5079 and y is 863,
then by xy I mean 5079863. Now here are the rules:

Rule 1: For any number x, if you write her 1x2, then she will send you back
        the number x. For example, if you write 13542, she will write back 354.

Rule 2: For any number x, the repeat of x means xx. For example, the repeat
        of 692 is 692692.  And now, the second rule is that if x brings back y,
        then 3x brings back the repeat of y.  For example, since 15432 brings
        back 543, then 315432 brings back 543543. From which it further follows
        that if you send her 3315432, you will get back 543543543543 (since
        315432 brings back 543543).

Rule 3: The reverse of a number means the number written backwards. For example,
        the reverse of 62985 is 58926. The third rule is that if x brings back
        y, then 4x brings back the reverse of y. For example, since 172962
        brings back 7296, then 4172962 brings back 6927. Thus, if you send her
        the number 4172962, you will get back 6927. Or, combining rules 1, 2
        and 3, since 316982 brings back 698698 (by rules 1 and 2), then
        4316982 brings back 896896

Rule 4 (The Erasure Rule): If x brings back y, and if y contains at least two
        digits, then 5x brings back the result of erasing the first digit of y.
        For example, since 13472 brings back 347, then 513472 brings back 47.

Rule 5 (The Addition Rule): If x brings back y, then 6x brings back 1y and 7x
        brings back 2y. For example, since 15832 brings back 583, then 615832
        brings back 1583, and 715832 brings back 2583.

"Those are the rules," said the sultan, "and from them can be deduced a number
x that will bring back the very number . There are actually an infinite number
of solutionss, but any single one will suffice for passing the first test."

"Are there any /meanings/ to these numbers?" asked the suitor.

"Ah, that is the princess' secret, but fortunately you don't have to know the
meaning in order to pass the first test."

WHAT IS THE NUMBER X SUCH THAT THE PRINCESS SENDS BACK THAT SELF-SAME NUMBER?

-}

rule1 :: String -> String
rule1 [] = []
rule1 (x:xs)
    | x == '2' = reverse xs
    | otherwise = []
    
rule2 :: String -> String
rule2 xs = xs ++ xs

rule3 :: String -> String
rule3 = reverse

rule4 :: String -> String
rule4 [] = []
rule4 (x:xs) = xs

rule5 :: Char -> String -> String
rule5 a xs = a : xs
    
princessReply :: String -> String
princessReply [] = []
princessReply (x:xs)
    | x == '1' = rule1 $ reverse xs
    | x == '3' = rule2 $ princessReply xs
    | x == '4' = rule3 $ princessReply xs
    | x == '5' = rule4 $ princessReply xs
    | x == '6' = rule5 '1' $ princessReply xs
    | x == '7' = rule5 '2' $ princessReply xs
    | otherwise = ""
    
firstValidReply :: [Integer] -> Integer
firstValidReply xs
	| null xs                  = 0
	| (princessReply y) == y   = x
	| otherwise = firstValidReply (tail xs)
    where x = head xs
          y = (show x) ++ "1" ++ (show x) ++ "2"
        
        