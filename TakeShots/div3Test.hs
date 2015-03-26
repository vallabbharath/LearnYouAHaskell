{-
Three guys go out fishing. They decide in advance that whatever they catch, they're going to divvy it up equally.
So, they finish fishing for the day. They pull back into port, and they're going to sleep on the boat overnight. They're going to get up in the morning, divvy up the fish, and go home. In the middle of the night, however, one of the guys has a severe hemorrhoidal flare-up, and he's got to get to the drugstore right away to buy some stuff.

So, he goes to take his third of the fish, and he notices that the number that they caught is not divisible by three, unless he throws one of the fish overboard. So, he throws one of the fish overboard, takes his third and leaves. A few hours later, in the middle of the night, another guy wakes up with horrible stomach pains. He’s gotta have the Kaopectate. So he goes to take his third of the fish, and he notices, interestingly, the same thing -- he can't take a third unless he throws one fish overboard. He throws one fish overboard, takes his third, and goes home.

Third guy gets up in the morning and figures the other guys are still sleeping. So he figures, “I'll just take my third, and I'll go. When they wake up, they can take their third.” However, he realizes that he can't take a third. It's not divisible by three.

He throws one fish overboard, takes his third, and leaves. Question: What is the smallest number of fish by which this little scenario could have taken place?
-}

{-
original = x
personC = (x - 1) / 3
remaining = (x - 1 - (x - 1) / 3) = 2/3 (x - 1) --> B
personB = ((2/3 (x - 1)) - 1) / 3
remaining = (2/3 (x - 1) - 1) - ((2/3 (x - 1)) - 1) / 3
          = 2/3 (2/3 (x - 1) - 1) --> A
personA   =   (A - 1) / 3


A - 1 = 3
A = 4
2/3 ( B - 1) = 4
B - 1 = 6
B = 7
2/3 (x - 1) = 7
x - 1 = 21/2
x = 11.5
-}

outX  = [x | x <- [1..200],  div3Tester x]

div3Tester :: Int -> Bool
div3Tester x = 
    and [(aNet `mod` 3 == 0), (bNet `mod` 3 == 0), (cNet `mod` 3 == 0)]
    where cSees = x
          cThrows = 1
          cNet = cSees - cThrows
          cTakes = cNet `quot` 3
          bSees = cNet - cTakes
          bThrows = 1
          bNet = bSees - bThrows
          bTakes = bNet `quot` 3
          aSees = bNet - bTakes
          aThrows = 1
          aNet = aSees - aThrows
          aTakes = aNet `quot` 3

