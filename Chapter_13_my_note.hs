{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module Chapter_13_my_note where

import           Chapter_5_my_note ( Shape ( Circle
                                           , Rectangle
                                           , Triangle )
                                   , area
                                   , circulation
                                   , isRound )

import           Chapter_7_my_note (isSorted)

listMatch :: Eq a => [a] -> a -> Bool
listMatch ls val    = all (== val) ls

numEqual :: Eq a => [a] -> a -> Int
numEqual xs x    = length (filter (== x) xs)

oneLookupFirst :: Eq a => [(a, b)] -> a -> b
oneLookupFirst xs vala    = (head . map snd . filter (\(x, _) -> x == vala)) xs

oneLookupSecond :: Eq b => [(a, b)] -> b -> a
oneLookupSecond xs valb    = (head . map fst . filter (\(_, y) -> y == valb)) xs

class Info a where
    example :: [a]
    example         = []
    size :: a -> Int
    size _          = 1
    provideExp :: a -> [a]
    provideExp _    = example

instance Info Bool where
    example    = [True, False]

instance Info Char where
    example    = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

instance Info Int where
    example    = [-100 .. 100]

instance Info Float where
    example    = [3.0, 45.9, 87.6]

instance Info Shape where
    example    = [ Circle (head (example :: [Float]))
                 , Rectangle ((example :: [Float]) !! 1) ((example :: [Float]) !! 2) ]
    size       = round . area

instance Info a => Info [a] where
    example    = [[]] ++ [[x] | x <- example] ++ [[x, y] | x <- example, y <- example]
    size       = foldr ((+) . size) 0

instance (Info a, Info b) => Info (a, b) where
    example          = [(_a, _b) | _a <- example, _b <- example]
    size (va, vb)    = size va + size vb

class Equ a where
    (===), (/==) :: a -> a -> Bool
    x /== y    = not (x === y)
    x === y    = not (x /== y)

instance Equ Integer where
    x === y    = x == y
--    x /== y    = x == y
--    I know how it is defined here.
--    define one side and accomplish two sides

class (Ord a, Show a) => OrdShow a

instance OrdShow Char
instance OrdShow Integer
instance OrdShow Float
instance OrdShow Int

insert :: Ord a => [a] -> a -> [a]
insert ls targ    = [x | x <- ls, x < targ] ++ [targ] ++ [x | x <- ls, x >= targ]

iSort :: Ord a => [a] -> [a]
iSort    = foldr (flip insert) []

vSort :: OrdShow a => [a] -> String
vSort    = show . iSort

class Checkable b where
    infoCheck :: (Info a) => (a -> b) -> Bool

instance Checkable Bool where
    infoCheck property    = all property example

instance (Info a, Checkable b) => Checkable (a -> b) where
    infoCheck property    = all (infoCheck . property) example

data Move = Paper | Stone | Scissors
            deriving Eq

instance Show Move where
    show Paper    = "paper"
    show Stone    = "stone"
    show Scissors = "scissors"

data Suit = Spade | Heart | Diamond | Club
            deriving Eq

instance Show Suit where
    show Spade   = "spade"
    show Heart   = "heart"
    show Diamond = "diamond"
    show Club    = "club"

data Value = One | Two | Three | Four | Five | Six |
             Seven | Eight | Nine | Ten | Jack | Queen |
             King | Ace
             deriving (Eq, Ord)

instance Show Value where
    show One   = "one"
    show Two   = "two"
    show Three = "three"
    show Four  = "four"
    show Five  = "five"
    show Six   = "six"
    show Seven = "seven"
    show Eight = "eight"
    show Nine  = "nine"
    show Ten   = "ten"
    show Jack  = "jack"
    show Queen = "queen"
    show King  = "king"
    show Ace   = "ace"

data Card = CardConstruct Suit Value
            deriving Eq

instance Show Card where
    show (CardConstruct suit val)    = show suit ++ " " ++ show val

data Trip = MakeTrip { a :: Int
                     , b :: Int
                     , c :: Int }
            deriving Eq

instance Show Trip where
    show (MakeTrip _a _b _c)    = "(" ++ show _a ++ ", "
                                      ++ show _b ++ ", "
                                      ++ show _c ++ ")"

class (Show a) => Visible a where
    visualize :: a -> String
    visualize    = show

instance Visible Int
instance Visible Float
instance Visible Bool

instance Visible Char where
    visualize v    = [v]

instance (Visible a) => Visible [a] where
    visualize []       = ""
    visualize (x : xs) = visualize x ++ concatMap ((" " ++) . visualize) xs

instance (Info a, Info b, Info c, Visible a, Visible b, Visible c) => Visible (a, b, c) where
    visualize (_a, _b, _c)    = "(" ++ visualize _a
                                    ++ ", " ++ visualize _b
                                    ++ ", " ++ visualize _c ++ ")"

instance Info (Int -> Bool) where
    example    = [(== 1)]
    size _     = 1

instance Info (Int -> Int) where
    example    = [id]
    size _     = 1

infoCompare :: (Info a, Info b) => a -> b -> Bool
infoCompare _a _b    = size _a <= size _b

class Eq a => Order a where
    (-<), (>-), (-<=), (=>-) :: a -> a -> Bool
    a1 -< a2           = a2 >- a1
    a1 >- a2           = a2 -< a1
    a1 -<= a2          = a2 =>- a1
    a1 =>- a2          = a2 -<= a1
    _max, _min :: a -> a -> a
    _max a1 a2         = if a1 >- a2 then a1 else a2
    _min a1 a2         = if a1 >- a2 then a2 else a1
    _compare :: a -> a -> Ordering
    _compare a1 a2
        | a1 >- a2     = GT
        | a1 == a2     = EQ
        | otherwise    = LT

instance (Order a, Order b) => Order (a, b) where
    (a1, b1) -< (a2, b2)
        | a1 -< a2                = True
        | a1 == a2 && b1 -< b2    = True
        | otherwise               = False
    _max (a1, b1) (a2, b2)
        | (a1, b1) -< (a2, b2)    = (a1, b1)
        | otherwise               = (a2, b2)
    _min (a1, b1) (a2, b2)
        | (a1, b1) >- (a2, b2)    = (a1, b1)
        | otherwise               = (a2, b2)

instance Order a => Order [a] where
    _  -< []            = False
    [] -< (_ : _)       = True
    (_a : lsa) -< (_b : lsb)
        | _a -< _b      = True
        | _a == _b      = lsa -< lsb
        | otherwise     = False
    _max lsa lsb
        | lsa -< lsb    = lsb
        | otherwise     = lsa
    _min lsa lsb
        | lsa -< lsb    = lsa
        | otherwise     = lsb

instance Order Char where (-<) = (<)
instance Order Int where (-<) = (<)
instance Order Float where (-<) = (<)
instance Order Bool where (-<) = (<)

class Ord a => Enumerate a where
    esucc, epred        :: a -> a
    toEnumerate         :: Int -> a
    fromEnumerate       :: a -> Int
    enumerateFrom       :: a -> [a]
    enumerateFromThen   :: a -> a -> [a]
    enumerateFromTo     :: a -> a -> [a]
    enumerateFromThenTo :: a -> a -> a -> [a]

instance Enumerate Int where
    esucc                              = succ
    epred                              = pred
    fromEnumerate                      = fromEnum
    toEnumerate                        = toEnumerate
    enumerateFrom val                  = [val ..]
    enumerateFromThen val nxt          = [val, nxt ..]
    enumerateFromTo val fin            = [val .. fin]
    enumerateFromThenTo val nxt fin    = [val, nxt .. fin]

instance Enumerate Integer where
    esucc                              = succ
    epred                              = pred
    fromEnumerate                      = fromEnum
    toEnumerate                        = toEnumerate
    enumerateFrom val                  = [val ..]
    enumerateFromThen val nxt          = [val, nxt ..]
    enumerateFromTo val fin            = [val .. fin]
    enumerateFromThenTo val nxt fin    = [val, nxt .. fin]

instance Enumerate Char where
    esucc                              = succ
    epred                              = pred
    fromEnumerate                      = fromEnum
    toEnumerate                        = toEnumerate
    enumerateFrom val                  = [val ..]
    enumerateFromThen val nxt          = [val, nxt ..]
    enumerateFromTo val fin            = [val .. fin]
    enumerateFromThenTo val nxt fin    = [val, nxt .. fin]

data Boolean = True_ | False_
                deriving (Eq, Show, Read)

-- https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Ord.html
-- fuck you asshole haskell
-- I used to have the idea about (a1 .. an) ord is automatic
-- then here I see it is just ... fuck...

-- instance Show (Bool -> Bool) where
--     show    = showBoolFun

showBoolFun :: (Bool -> Bool) -> String
showBoolFun func    = "Truth table\n------+------\n"
                      ++ show True ++ "  |" ++ (show . func) True ++ "\n"
                      ++ show False ++ " |" ++ (show . func) False ++ "\n"

showBoolFunGen :: (a -> String) -> (Bool -> a) -> String
showBoolFunGen expr func    = "Truth table\n------+------\n"
                      ++ show True ++ "  |" ++ (expr . func) True ++ "\n"
                      ++ show False ++ " |" ++ (expr . func) False ++ "\n"

instance (Info a, Show a, Show b) => Show (a -> b) where
    show func    = concatMap (\x -> show x ++ " " ++ ((++ "\n") . show . func) x) example

class StrangeOrder a  where
    (<~) :: a -> a -> Bool
    (~>) :: a -> a -> Bool
    (<~) _ _ = False
    (~>) _ _ = False

instance StrangeOrder Move where

data Roman = Roman Integer

instance Num Roman where
    (Roman va) + (Roman vb)    = Roman (va + vb)
    (Roman va) - (Roman vb)    = Roman (va - vb)
    (Roman va) * (Roman vb)    = Roman (va * vb)

instance Show Roman where
    show (Roman val)    = (if val < 0 then "(NEGATIVE) " else "")
                          ++ valRoman convMap (abs val) []

filtRoman :: String -> String
filtRoman    = filter (`elem` "MCDXLVIO")

splitRoman :: String -> [Integer]
splitRoman []     = []
splitRoman str    = (fst . matchFir) str :
                    splitRoman (drop (length ((snd . matchFir) str)) str)

matchRomFir :: String -> [(Integer, String)]
matchRomFir str    = map (\x -> if headsub (snd x) str then x else (-1, "")) convMap

matchFir :: String -> (Integer, String)
matchFir str    = (head . filter (\x -> fst x == maxv) . matchRomFir) str
  where maxv    = (maximum . map fst . matchRomFir) str :: Integer

headsub :: String -> String -> Bool
headsub str1 str2    = if length str2 >= 2
                       then and (zipWith (==) str1 str2)
                       else str1 == str2

readroman :: String -> Roman
readroman str       = if (isSorted . reverse) digarray
                      then Roman (sum digarray)
                      else error "wrong type roman value"
  where digarray    = (splitRoman . filtRoman) str :: [Integer]

convMap :: [(Integer, String)]
convMap = [ (1000, "M"), (900, "CM"), (500, "D"), (400, "CD")
          , (100, "C"), (90, "XC"), (50, "L"), (40, "XL")
          , (10, "X"), (9, "IX"), (5, "V"), (4, "IV")
          , (1, "I"), (0, "O") ]

mapFindNext :: [(Integer, String)] -> Integer -> (Integer, String)
mapFindNext [] _          = error "empty list, error"
mapFindNext (s : ls) curr = if fst s <= curr then s else mapFindNext ls curr

valRoman :: [(Integer, String)] -> Integer -> String -> String
valRoman _ 0 []         = "O"
valRoman _ 0 wtf        = wtf
valRoman vsp val str    = valRoman convMap (val - eliminVal) (str ++ addingStr)
  where addingPair      = mapFindNext vsp val
        addingStr       = snd addingPair
        eliminVal       = fst addingPair
