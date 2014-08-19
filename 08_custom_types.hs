-- Chapter 8: Making Our Own Types and Type Classes
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses

import qualified Data.Map as Map

-- "data" keyword is used to define a new type
-- Note:
-- 1. The data type should start with a upper case letter
-- 2. The value constructor (will see what it is) should start with upper case.

-- Possible definition of Bool in the standard library
-- data Bool = False | True
-- Custom definition of the same
data Bool' = FALSE | TRUE
-- TRUE and FALSE are value constructors

-- Let us define a new data type Shape which can hold Circle or Rectangle
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
-- Shape is a "type"
-- Circle is a "value constructor"
-- Rectangle is another "value constructor"
-- Circle value constructor takes 3 fields - all are floats
-- Rectangle value constructor takes 4 fields - all are floats

-- Try the following in ghci console
-- :t Circle    => Circle :: Float -> Float -> Float -> Shape
-- :t Rectangle => Rectangle :: Float -> Float -> Float -> Float -> Shape      

-- So value constructors are nothing but "functions" - looks very similar!

-- Function to calculate the surface area using Shape, Circle and Rectangle
-- Input: Shape
-- Output: Float (surface area)
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Type definition cannot be Circle -> Float (as Circle is not a type!)
-- Circle is a value constructor for "Shape" type
-- call the surface function using surface(Circle 10 10 10)

-- Make 'Shape' type an instance of 'Show' typeclass
-- This makes "Circle 10 10 10" to get printed on console instead of error
-- Now, Haskell knows how to "show" the type
-- The usage of single quote at the end is to differentiate from the type
-- that we already created.  So that this file can be compiled!
data Shape' = Circle' Float Float Float | Rectangle' Float Float Float Float
    deriving Show

-- The following command makes a new value constructor 'Shape' which is 
-- different from the type 'Shape' that we saw.  Both have no connection at all!    
data Shapes = Shape deriving Show    

-- map command can use a value constructor as a function
-- Below a partial value constructor is used which needs a third field, passed
-- in a list, so for each element a Circle' is created
circles = map (Circle' 10 20) [4,5,6,6] 

-- Improvisation on the data types
-- Creation of a new data type "Point"
data Point = Point Float Float deriving (Show)  
data Shape'' = Circle'' Point Float | Rectangle'' Point Point deriving (Show)

-- Rewriting surface based on the new definition of Shape''
surface'' :: Shape'' -> Float  
surface'' (Circle'' _ r) = pi * r ^ 2  
surface'' (Rectangle'' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1) 

-- Now, surface'' function can be invoked by commands such as
circleArea = surface''(Circle'' (Point 3 3) 10)    
rectangleArea = surface'' $ Rectangle'' (Point 3 3) (Point 5 5)

-- Function to nudge a shape! be it rectangle or circle, move by defined amount.
-- takes in a Shape'' and 2 floats (on x and y directions)
-- returns a Shape'' data type
nudge :: Shape'' -> Float -> Float -> Shape''
nudge (Circle'' (Point x y) r) a b = Circle'' (Point (x + a) (y + b)) r  
nudge (Rectangle'' (Point x1 y1) (Point x2 y2)) a b = 
    Rectangle'' (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))  
    
-- Invocation of nudge
newCircle = nudge (Circle'' (Point 34 34) 10) 5 10
newRectangle = nudge (Rectangle'' (Point 0 0) (Point 40 100)) 60 23

-- nudge, given a base circle or base rectangle (center / lowerbottom at origin)
-- First, definitions of base circle and base rectangle
baseCircle :: Float -> Shape''
baseCircle r = Circle'' (Point 0 0) r

baseRect :: Float -> Float -> Shape''
baseRect width height = Rectangle'' (Point 0 0) (Point width height)

newRectangle' = nudge (baseRect 40 100) 60 23 -- same as newRectangle
    
-- See sample of how Shape module is exported in "Shapes.hs"    
-- See syntax for data type and functions export
-- A data type can be exported with or without some or all value constructors!
-- See both Shapes.hs and ShapesVariant.hs

-- To import a module, use 
-- 1. From GHCI     -- :m +ModuleName 
-- 2. In .hs file   -- import ModuleName     
    
-- Record Syntax
data Person = Person { firstName :: String, 
                       lastName :: String,
                       age :: Int,
                       height :: Float,
                       phoneNumber :: String,
                       flavor :: String  
                     } deriving (Show)   
                     
-- for each of firstName, lastName ... flavor, a function is created.
-- :t firstName => firstName :: Person -> String

-- Another Example, creating a new data type CarPlain
data CarPlain = CarPlain String String Int deriving (Show) 

-- Need to maintain the order, if written without record syntax
toyotaCar = CarPlain "Toyota" "Corolla" 2000

-- Rewriting the same in record syntax
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- Creating instances of Car data type
fordCar = Car {company="Ford", model="Mustang", year=1967} 

-- No need to give the input fields in a specific order
hondaCar = Car {year=2013, model="Amaze", company="Honda"} 


-- Review:
-- Value Constructors: Takes in value parameters and produce new value
-- Remember, Circle and Rectangle are value constructor for data type 'Shape'
-- 'Car' is a value constructor for the data type 'Car'

-- Introducing 'Type Constructors'
-- Type Constructors: Takes in types as parameters and produces a new type!!
-- Example:  Implementation of "Maybe" type
data Maybe' a = Nothing' | Just' a  
-- Here 'a' is a type parameter (it could be anything! String, Int, or any)
-- Another Example:
-- [a] is a type parameter!! [] alone cannot be a type, but [Int] or [Char] is!
-- [a] is the type of empty list. Try giving :t []

-- A description function for the Car data type!
tellCar :: Car -> String  
tellCar (Car {company = c, model = m, year = y}) = 
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

hondaCarDesc = tellCar hondaCar
fordCarDesc = tellCar fordCar

-- Map k v from Data.Map is a parametrized type! k and v are type parameters.
-- k and v can be anything; only constraint is that k is part of Ord typeclass
-- data (Ord k) => Map k v = ....     " (Ord k) => " is typeclass constraint
-- so, that's how goes the datatype declaration of Map

-- Strong convention in Haskell: 
-- Never add typeclass constraints in data declarations. 
-- Why? Results in writing unnecessary class constraints for functions!

-- Irrespective of the (Ord k) typeclass constraint in the data declaration for 
-- Map k v, the constraint is necessary in function definition that assume 
-- the keys in a map can be ordered. 
-- If no typeclass constraint in data declaration, no need to write "(Ord k) =>"
-- in the type declarations of functions that don't need keys to be ordered. 

-- Example:
-- toList of Map: takes a mapping and converts it to an associative list. 
-- Type Signature is toList :: Map k a -> [(k, a)]. 
-- If Map k v had a type constraint in its data declaration, the type for toList 
-- would have to be toList :: (Ord k) => Map k a -> [(k, a)], 
-- when toList doesn't do any comparing of keys by order.

-- Final Word:  Never put typeclass constraint in data type declaration!


-- Implementation of a customized data type for Vector
-- and, related functions like vector addition (vplus), vector multiplication
-- (vectMult) and scalar multiplication (scalarMult)

-- Note that the type class constraint is added only in the function definition.

data Vector a = Vector a a a deriving (Show)  
-- Here "Vector" before the "=" sign is a type constructor
-- and, "Vector" after the "=" sign is a value constructor (which is a fn!)
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

-- "TypeClass" is kind of an interface which defines behaviour
-- "Type" is an instance of a particular "TypeClass"
-- Example:
-- "Int" type is an instance of "Eq" typeclass

-- Usually, we define a data type and make it associated with (an instance of)
-- a particular type class - using the keyword "deriving"


data Student = Student {fname :: String, lname :: String, curAge :: Int}
                    deriving (Eq, Show, Read)
-- Caution:                    
-- Student {firstName :: String, lastName :: String, age :: Int} is wrong!
-- as, the firstName, lastName and age functions are already defined for 
-- "Person" type and cannot be used for some other data type in the same file!
                    
sTendulkar = Student {fname = "Sachin", lname = "Tendulkar", curAge = 40}
rDravid = Student {fname = "Rahul", lname = "Dravid", curAge = 38}
msDhoni = Student {fname = "MS", lname = "Dhoni", curAge = 35}

-- since Student is an instance of Eq, we can compare using "==" and "/="
-- function that checks whether the given student is rDravid
isRahul :: Student -> Bool
isRahul = (==) rDravid
-- isRahul rDravid gives True; isRahul msDhoni gives False

isNotMSD :: Student -> Bool
isNotMSD = (/=) msDhoni

-- `elem` is also part of "Eq" typeclass
newStudents = [rDravid, msDhoni]

isNewStudent :: Student -> Bool
isNewStudent x = x `elem` newStudents

-- Since, Student is an instance of "Show", a student can be printed!
-- "Show" helps Haskell in how to convert the given data type to string and 
-- print out to terminal

-- read: an inverse function of "show"
-- we need to explicitly tell the data type that we are reading
ySingh = 
    read "Student {fname =\"Yuvraj\", lname =\"Singh\", curAge = 38}" :: Student

-- but if there is a context, we do not need to explicitly mention it
isLaraRahul = 
    isRahul (read "Student {fname =\"Brian\", lname =\"Lara\", curAge = 43}")

-- To read parametrized types:
paramRead = read "Just 't'" :: Maybe Char
-- paramRead = read "Just 't'" :: Maybe a 
-- above statement will not work. Type has to be given explicitly.

-- If a datatype derives "Ord" type class, the value which was made with a
-- constructor that defined first is considered smaller
data CustBool = False1 | True1 deriving (Eq, Ord)
-- so, here False1 is considered lesser to True1

-- In "May be" data type "Nothing" is defined ahead of "Just something"
-- so, Nothing is lesser than "Just -100" or even "Just -99999"
-- Just (*3) > Just (*2) is an illegal comparison, because (*3), (*2) are fns!

-- Illustrating another example:
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  
           
-- "Day" data type is an instance of Show and Read type classes
strMonday = show Monday
dayFriday = read "Friday" :: Day

-- "Day" is part of Eq and Ord type classes
boolSunEqMon = Sunday == Monday     -- returns False
boolSunGtMon = Sunday > Monday      -- returns True (Sunday is defined later)
compSunMon   = Sunday `compare` Monday -- returns GT

-- "Day" is part of Bounded type class
minDay = minBound :: Day            -- returns Monday
maxDay = maxBound :: Day            -- returns Sunday

-- "Day" is part of Enum type class
dayAfterTuesday = succ Tuesday
dayBeforeFriday = pred Friday

allDays = [Monday .. Sunday]        -- the space before ".." is mandatory!
minToMaxDays = [minBound .. maxBound] :: [Day]

-- Introducing Type Synonyms!
-- [Char] and String are same and interchangeable! achieved by type synonym
-- type String = [Char]  (this is how it is done!)

-- custom type synonym for character list (or string)
type String' = [Char]       -- notice the "type" keyword

-- Note:
-- "data" keyword creates a new data type
-- "type" keyword does not create a new type; but just a synonym

-- Illustration
-- Definition of cricket match squad, without type synonyms

matchSquad :: [(String, String)]  
matchSquad = [("VenkLaxman","Batsman"), 
    ("RahulDravid","Batsman"),
    ("AnilKumble","Bowler"),
    ("RaviAshwin","Bowler"),
    ("RaviJadeja","AllRounder"),
    ("MahiDhoni","WicketKeeper"),
    ("SouravGanguly", "Captain")]

-- Modifying the same, using type synonyms
type PlayerName = String
type Profile = String
type MatchSquad = [(PlayerName, Profile)]

inMatchSquad :: PlayerName -> Profile -> MatchSquad -> Bool
inMatchSquad name profile squad = (name, profile) `elem` squad
    
-- type synonyms can also be parametrized
type AssocList k v = [(k,v)] 

-- A function is partially applied to get a new function
-- Similarly, partially applied type parameters create new type constructors
type IntMap v = Map.Map Int v

-- here AssocList or IntMap is a type constructor, not a value constructor
-- so we cannot do:  AssocList [(1,2),(4,5),(7,9)]
-- But, the following can be done!
assocList = [(1,2),(3,5),(8,9)] :: AssocList Int Int
-- a type can come only in the type portion of Haskell
-- 1. after "::" operator
-- 2. following "data" keyword (to define a new type)
-- 3. following "type" keyword (to define a type synonym)

-- Introducing "Either a b" data type
-- Possible Definition in Haskell built-in library

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

-- Type Constructor: Either
-- Value Constructors
-- 1. Left (takes one particular type a field)
-- 2. Right (can take a completely different type b field)

-- General Convention:
-- Errors use Left value constructor
-- Results use Right value constructor

-- Illustration
-- Students and Lockers
-- A student tell a locker number and asks for the code
-- If no such locker number, student is informed and he may re-select
-- If the locker number exists:
--    if not already taken: code is given and locker marked as taken
--    if already taken: student is informed and he may re-select

-- defining LockerState data type to mark free or taken
data LockerState = Taken | Free deriving (Show, Eq)  
-- defining type synonym for locker code
type Code = String  
-- defining LockerMap data type with Int keys and value is (LockerState, Code)
type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap  
lockers = Map.fromList [(100, (Taken,"ZD39I")), 
                        (101, (Free,"JAH3I")),
                        (103, (Free,"IQSA9")),
                        (105, (Free,"QOTSA")),
                        (109, (Taken,"893JJ")),
                        (110, (Taken,"99292"))]  

-- Lookup function:
lockerLookup :: Int -> LockerMap -> Either String Code 
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ 
            " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ 
                                    " is already taken!"
                                    
-- New Concept, yet again!
-- Recursive DataStructures
-- [] a list can be empty or joined together with an element using ":"
-- [5] = 5:[]
-- [4,5] = 4:(5:[])
-- [3,4,5] = 3:(4:(5:[]))  or 3:4:5:[]  (as ":" is right associative)

-- Custom made list
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)  

-- the same, can also be written as following (using record syntax)
data List' a = Empty' | Cons' { listHead :: a, listTail :: List' a} 
    deriving (Show, Read, Eq, Ord)  

-- try the following in ghci console
-- Empty
-- 5 `Cons` Empty
-- 6 `Cons` (5 `Cons` Empty)
-- 7 `Cons'` (6 `Cons'` (5 `Cons'` Empty'))

-- Introducing Fixity Declarations
-- Right associative: infixr
-- Left  associative: infixl

infixr 5 :-:  -- (this means    :-:  is the new operator defined here)
data Listing a = EmpT | a :-: (Listing a) deriving (Show, Read, Eq, Ord)  

-- Note:
-- the default mult "*" is defined as infixl 7 *
-- the default addn "+" is defined as infixl 6 + 
-- 7 is higher priority than 6.  So, 7 * 8 + 2 = 58 and not 70!!

-- Making use of the "Listing" data type, that got created just now
listNum1 = 3 :-: 4 :-: 5 :-: EmpT
listNum2 = 2 :-: listNum1

-- "++" is the built-in list concatenator! [3, 4] ++ [5] is [3,4,5]
-- This is how it is defined.  Using custom ".++" instead of "++"
infixr 5  +++        -- defines 1. fixity 2. priority and 3. right-associative
(+++) :: [a] -> [a] -> [a]  
[]     +++ ys = ys  
(x:xs) +++ ys = x : (xs +++ ys)

normalList = [3, 4] +++ [5]

-- Defining for our "Listing" data type
-- Note the use of 
-- 1. Listing a     instead of      [a]
-- 2. :-:           instead of      :
-- 3. EmpT          instead of      []
infixr 5  .++        -- defines 1. fixity 2. priority and 3. right-associative
(.++) :: Listing a -> Listing a -> Listing a
EmpT     .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys)

listNum3 = listNum1 .++ listNum2

-- Implementation of normal Binary Search Tree
-- To add more spice: Data.Set and Data.Map in Haskell are implemented using
-- balanced search trees (a little advanced compared to normal BST)

-- Defining custom data type for the tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
-- (a tree is either EmptyTree or a single Node with two sub trees)

-- Binary search tree:  Algorithm Explanation
-- Get a number and put in as a node
-- Get successive number and compare with top most node, if current is lesser,
-- go left, else go right and add as a new node.  Rinse, repeat!

-- singleton:
-- takes in an element and returns it as a tree with two empty sub trees
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
-- treeInsert: 
-- takes in an element and a tree and returns a tree with element in it.
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)  
    
-- treeElem:
-- takes in an element and a tree and returns True if the element is in tree
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right 

-- Building a tree using foldr function
nums = [1..10]
numsTree = foldr treeInsert EmptyTree nums  -- completely left leaning tree!
    
numsArbitrary = [3,4,2,5,1,7,6]    
numsTreeA = foldr treeInsert EmptyTree numsArbitrary

-- TYPECLASSES 102
-- All this while, custom "types" are made
-- Now, start treading the jungle of making "typeclasses"!

-- Recap:
-- If a type is an instance of a particular "typeclass", the functions that the
-- typeclass defines can be used by the type

-- Eq:  Typeclass for things that can be equated
-- Defines two functions
-- 1. (==)
-- 2. (/=)

-- This is how it is defined in the standard "Prelude" of Haskell
-- Making it as a custom Eq' with (===) for (==) and (/==) for (/=)
class Eq' a where  
    (===) :: a -> a -> Bool  
    (/==) :: a -> a -> Bool  
    x === y = not (x /== y)  
    x /== y = not (x === y)
    
data TrafficLight = Red | Yellow | Green 

-- Use "instance Eq TrafficLight where..." or "data TrafficLigt ... deriving Eq"
instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False 
    
-- See above that only (==) is defined and (/=) is not defined in the instance
-- But if the class definition of Eq is defined as
class Eq'' a where  
    (====) :: a -> a -> Bool  
    (/===) :: a -> a -> Bool      
-- both (==) and (/=) had to be defined in the instance, because Haskell does 
-- not know how (==) and (/=) are related to each other


-- to specifically tell how it needs to be printed!    
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  
    
-- Sub Classes
-- Num class (sub class of Eq) is defined like following

-- class (Eq a) => Num a where  
--      ...        
   
-- Effectively, adding a class constraint "(Eq a) => " is all that required!
-- Means, before 'a' is considered as "Num" it should be an instance of "Eq"

-- The curious case of "Maybe" - to make it an instance of Eq'
-- ??? instance Eq' (Maybe m) where  
-- ???     Just x === Just y = x === y  
-- ???     Nothing === Nothing = True  
-- ???     _ === _ = False 

-- Still, not sure whether 'm' itself an instance of Eq.  So,
-- need to add class constraint for 'm'
instance (Eq' m) => Eq' (Maybe m) where  
    Just x === Just y = x === y  
    Nothing === Nothing = True  
    _ === _ = False      

-- class constraints in "class" declaration:
--      to make a class sub-class of another one (Num is sub class of Eq)

-- class constraints in "instance" declaration:
--      to express requirements about contents (above, 'm' to be of Eq' class)

{- 
This is a block comment (or multi-line comment) in Haskell!

Use ":info TypeClass" from GHCI console to see the class definition

Example:

*Main> :info Num
class Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
        -- Defined in `GHC.Num'
instance Num Integer -- Defined in `GHC.Num'
instance Num Int -- Defined in `GHC.Num'
instance Num Float -- Defined in `GHC.Float'
instance Num Double -- Defined in `GHC.Float'

-}


-- Fun Illustration
-- Implementaion of JavaScript kind of a boolean check
-- evaluate "", 0 or false to be False and any nonempty, non-zero to be True
-- Note: for Haskell "False" is false and nothing else.

class YesNo a where  
    yesno :: a -> Bool  
    
-- Define instances

-- For Int
instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True     

-- For List
instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True 
    
-- For Bool
instance YesNo Bool where  
    yesno = id       
    -- Note the use of "id"!!!
    -- standard parameter that takes a parameter and returns the same
    -- try :t id and id(5), id([4,5,6]) id(False)
    
-- For Maybe type
instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False  
    
-- For Tree (custom type, defined in this file)
instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True  

-- For TrafficLight (custom type, defined in this file)
instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True      
    
-- Function using YesNo typeclass    
yesnoIf :: (YesNo y) => y -> a -> a -> a  
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult      

-- Try the following
{-

yesnoIf [] "YEAH!" "NO!"
yesnoIf [2,3,4] "YEAH!" "NO!"
yesnoIf (Just 500) "YEAH!" "NO!" 

-}


-- Introducing a new concept, yet again!
-- FUNCTOR TYPECLASSES!

-- Functor class declares just one function: fmap!
-- the 'f' below is a type constructor (and not a concret type)

{-
class Functor f where  
    fmap :: (a -> b) -> f a -> f b    
-}
   
-- This looks eerily close to map function type signature
-- map :: (a -> b) -> [a] -> [b]    
-- where f is nothing but []
   
-- [] list type is an instance of Functor type class
-- The following is how the [] instance of Functor defined!
{-
instance Functor [] where  
    fmap = map  
-}
-- the instance of Functor [] implements fmap    


-- Functor Instance for Maybe data type
-- Defined by default
{-
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing
-}    

-- Let us create the Functor instance for Tree data type
-- Tree data type is customized. So, we shall define the instance below
instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub) 
    
baseTree = foldr treeInsert EmptyTree [5,7,3,2,1,7]    
functorMadeTree = fmap (+4) baseTree

-- This is already defined in Data.Either
-- Note the use of (Either a) and not (Either)
-- because it has to be a type constructor that takes just one type
{-
instance Functor (Either a) where  
    fmap f (Right x) = Right (f x)  
    fmap f (Left x) = Left x 
-}    

-- Kinds: The type of a type
-- Tells what level a particular type is
-- Is it a type constructor or a concrete type?

-- try giving   :k Int      in ghci console (returns '*' which means concrete)
-- *            Concrete
-- * -> *       Type constructor that takes one concrete i/p (Ex: Maybe)
-- * -> * -> *  Type constructor that takes two concrete i/p (Ex: Either)

-- Now try :k Functor
-- Functor :: (* -> *) -> Constraint

-- Trying kind on some arbitrary class
class Tofu t where  
    tofu :: j a -> t a j
    
-- Try :k Tofu
-- a is *
-- j is * -> *
-- t takes a and j and produces *, so: * -> (* -> *) -> *
-- And class Tofu takes in t and produces a Constraint

data Frank a b  = Frank {frankField :: b a} deriving (Show)
-- Very Interesting!
-- frankField value constructor takes two types 'b' and 'a'
-- 'a' is a concrete type while 'b' is a type constructor taking a concrete type

maybeStringFrank = Frank {frankField = Just "HAHA"} 
treeFrank = Frank {frankField = Node 'a' EmptyTree EmptyTree}  
stringFrank = Frank {frankField = "YES"}  
numListFrank = Frank {frankField = [1,2,3]} 

-- Frank is of the kind * -> (* -> *) -> *    Think it as (a) -> (b) -> Frank
-- try  :k Frank
-- also try 
-- 1.   :t maybeStringFrank
-- 2.   :t treeFrank            and others


-- Making Frank an instance of Tofu!!
instance Tofu Frank where  
    tofu x = Frank x  
    
-- Now try the following in ghci console
-- :info Frank
-- :info Tofu
-- :k Frank
-- :k Tofu

-- Note the need of explicitly telling the data type!
toFraMaybeChar = tofu (Just 'a') :: Frank Char Maybe
toFraCharList = tofu ["HELLO"] :: Frank [Char] []     

-- Creating another custom type to play more!
data Barry t k p = Barry { yabba :: p, dabba :: t k }  

-- Barry data type takes in 3 types t, k, p
-- t is a type constructor taking k
-- k and p are concrete types
-- So, Barry's kind is (* -> *) -> * -> * -> *   Think as t -> k -> p -> Barry

-- Making Barry an instance of Functor
-- Partially apply type constructors on Barry 
-- just to be left with just one concrete to apply
instance Functor (Barry a b) where  
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
    
    