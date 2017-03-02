module Chapter_5_my_note where

import           Chapter_4_my_note (maxThree, triAreaMy)
import           Data.Char
import           Prelude           hiding (id)
import           Test.QuickCheck

type ShopItem = (String, Integer)
type Basket = [ShopItem]

basket1 :: Basket
basket1 = []

basket2 :: Basket
basket2 = [ ("Sugar", 12)
          , ("Oil", 10)
          , ("Crisp", 5) ]

minAndMax :: Integer -> Integer -> (Integer, Integer)
minAndMax x y
    | x > y        = (y, x)
    | otherwise    = (x, y)

addPair :: (Integer, Integer) -> Integer
addPair (x, y) = x + y

shift :: ((Integer, Integer), Integer) -> (Integer, (Integer, Integer))
shift ((x, y), z) = (x, (y, z))

name :: ShopItem -> String
name (_name, _int) = _name

price :: ShopItem -> Integer
price (_name, _price) = _price

addPair' :: (Integer, Integer) -> Integer
addPair' pair_ = fst pair_ + snd pair_

fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (u, v) = (v, u + v)

fibPair :: Integer -> (Integer, Integer)
fibPair num
    | num == 0     = (0, 1)
    | otherwise    = (fibStep . fibPair) (num - 1)

fastFib :: Integer -> Integer
fastFib = fst . fibPair

maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs n m
    | n == m       = (m, 2)
    | otherwise    = (max n m, 1)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs n m p
    | maxFirstTwo > p     = (maxFirstTwo, occFirstTwo)
    | maxFirstTwo == p    = (maxFirstTwo, occFirstTwo + 1)
    | otherwise           = (p, 1)
  where
    maxFirstTwo = fst $ maxOccurs n m
    occFirstTwo = snd $ maxOccurs n m

orderedTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderedTriple (x, y, z) = (minThree x y z, middleNumber x y z, maxThree x y z)
  where
    minThree :: Integer -> Integer -> Integer -> Integer
    minThree x' y' = min (min x' y')
    middleNumber :: Integer -> Integer -> Integer -> Integer
    middleNumber x' y' z'
        | between y' x' z'    = x'
        | between x' y' z'    = y'
        | otherwise           = z'
    between :: Integer -> Integer -> Integer -> Bool
    between x' y' z'
        | (x' - y') * (y' - z') >= 0    = True
        | otherwise                     = False

pointGet :: (Float, Float) -> (Float, Bool)
pointGet (incline, cut)
    | incline == 0    = (-1, False)
    | otherwise       = (cut / incline * (-1), True)

data People = Person Name Age
              deriving (Eq, Show)
type Name   = String
type Age    = Integer

showPerson :: People -> String
showPerson (Person st n) = st ++ "--" ++ show n

data Shape = Circle Float |
             Rectangle Float Float |
             Triangle Float Float Float
             deriving (Eq, Ord, Show)

isRound :: Shape -> Bool
isRound (Circle _)          = True
isRound (Rectangle _ _)     = False
isRound (Triangle _ _ _)    = False

area :: Shape -> Float
area (Circle r)          = pi * r * r
area (Rectangle h w)     = h * w
area (Triangle a b c)    = triAreaMy a b c

circulation :: Shape -> Float
circulation (Circle r)          = 2.0 * pi * r
circulation (Rectangle h w)     = 2 * (h + w)
circulation (Triangle a b c)    = a + b + c

data Item = ItemStruct ItemName ItemPrice
            deriving (Eq, Show)
type ItemName     = String
type ItemPrice    = Integer

regexShape :: Shape -> Bool
regexShape (Circle _)    = True
regexShape (Rectangle a b)
    | a == b       = True
    | otherwise    = False
regexShape (Triangle a b c)
    | a == b && b == c    = True
    | otherwise           = False

type Position = (Float, Float)

data NewShape = NewCircle Float Position |
                NewRectangle Float Float Position |
                NewTriangle Float Float Float Position
                deriving (Eq, Show, Ord, Read)

move :: Float -> Float -> NewShape -> NewShape
move deltaX deltaY (NewCircle radius (x, y))     = NewCircle radius (x + deltaX, y + deltaY)
move deltaX deltaY (NewRectangle a b (x, y))     = NewRectangle a b (x + deltaX, y + deltaY)
move deltaX deltaY (NewTriangle a b c (x, y))    = NewTriangle a b c (x + deltaX, y + deltaY)

data Flat = FlatPlaceStruct Integer |
            FlatNameStruct String
            deriving (Eq, Show, Ord)

showFlat :: Flat -> String
showFlat (FlatNameStruct str)     = str
showFlat (FlatPlaceStruct plc)    = show plc

-- amazing method
addPairs :: [(Integer, Integer)] -> [Integer]
addPairs pairlist = [m + n | (m, n) <- pairlist]

digits :: String -> String
digits str = [satis | satis <- str, isDigit satis]

allEven :: [Integer] -> Bool
allEven datalist = datalist == [num | num <- datalist, mod num 2 == 0]

allOdd :: [Integer] -> Bool
allOdd datalist = [] == [num | num <- datalist, mod num 2 == 0]

totalRadius :: [Shape] -> [Float]
totalRadius shapes = [rad | Circle rad <- shapes]

totalRadiusSum :: [Shape] -> Float
totalRadiusSum shapes = sum [rad | Circle rad <- shapes]

doubleAll :: [Integer] -> [Integer]
doubleAll datalist = [2 * x | x <- datalist]

singleList :: [[Integer]] -> [Integer]
singleList datalist = [x | [x] <- datalist]

capitalize :: String -> String
capitalize str = [toUpper a | a <- str]

capitalizeLetters :: String -> String
capitalizeLetters str = [toUpper x | x <- str, isLower x]

divisors :: Integer -> [Integer]
divisors edge = [x | x <- [1 .. edge], mod edge x == 0]

isPrime :: Integer -> Bool
isPrime num
    | divisors num == [1, num]    = True
    | otherwise                   = False

match :: Integer -> [Integer] -> [Integer]
match target datalist = [x | x <- datalist, target == x]

elem' :: Integer -> [Integer] -> Bool
elem' target datalist
    | match target datalist == []    = False
    | otherwise                      = True

oneSeparateLine :: [String] -> String
oneSeparateLine strbase = concat [s ++ "\n" | s <- strbase]

duplicate :: String -> Integer -> String
duplicate str n
    | n <= 0       = ""
    | otherwise    = concat [str | _ <- [1 .. n]]

pushRight :: String -> Integer -> String
pushRight str num = [' ' | _ <- [1 .. (num - toInteger (length str))]] ++ str

fibTable :: Integer -> String
fibTable number = header ++ "\n" ++ rows number
  where
    header :: String
    header = "n\t\tfib n"
    rows :: Integer -> String
    rows num = concat [show n ++ "\t\t" ++ show (fastFib n) ++ "\n" | n <- [1 .. num]]

type Person      = String
type Book        = String
type Database    = [(Person, Book)]

exampleBase :: Database
exampleBase = [ ("Alice", "Tintin")
              , ("Anna", "Little Women")
              , ("Alice", "Asterix")
              , ("Rory", "Tintin")]

books :: Database -> Person -> [Book]
books _database _person = [book | (person, book) <- _database, person == _person]

borrowers :: Database -> Book -> [Person]
borrowers _database _book = [person | (person, book) <- _database, _book == book]

borrowed :: Database -> Book -> Bool
borrowed _database _book = borrowers _database _book == []

numBorrowed :: Database -> Person -> Integer
numBorrowed _database _book = toInteger $ length $ borrowers _database _book

makeLoan :: Database -> Person -> Book -> Database
makeLoan _database _person _book = [(_person, _book)] ++ _database

returnLoan :: Database -> Person -> Book -> Database
returnLoan _database _person _book = [(p, b) | (p, b) <- _database, (p, b) /= (_person, _book)]

prop_db1 :: Database -> Person -> Book -> Bool
prop_db1 dBase pers bk =
    elem bk loanedAfterLoan == True
  where
    afterLoan = makeLoan dBase pers bk
    loanedAfterLoan = books afterLoan pers
