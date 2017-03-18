{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module Chapter_13_my_note where

import           Chapter_5_my_note ( Shape ( Circle
                                           , Rectangle
                                           , Triangle )
                                   , isRound
                                   , area
                                   , circulation )

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
    show Paper       = "paper"
    show Stone       = "stone"
    show Scissors    = "scissors"

data Suit = Spade | Heart | Diamond | Club
            deriving Eq

instance Show Suit where
    show Spade      = "spade"
    show Heart      = "heart"
    show Diamond    = "diamond"
    show Club       = "club"

data Value = One | Two | Three | Four | Five | Six |
             Seven | Eight | Nine | Ten | Jack | Queen |
             King | Ace
             deriving (Eq, Ord)

instance Show Value where
    show One      = "one"
    show Two      = "two"
    show Three    = "three"
    show Four     = "four"
    show Five     = "five"
    show Six      = "six"
    show Seven    = "seven"
    show Eight    = "eight"
    show Nine     = "nine"
    show Ten      = "ten"
    show Jack     = "jack"
    show Queen    = "queen"
    show King     = "king"
    show Ace      = "ace"

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
    visualize []          = ""
    visualize (x : xs)    = visualize x ++ concatMap ((" " ++) . visualize) xs

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
    _max, _min :: a -> a -> a
    _compare :: a -> a -> Ordering
    a1 -< a2           = a2 >- a1
    a1 >- a2           = a2 -< a1
    a1 -<= a2          = a2 =>- a1
    a1 =>- a2          = a2 -<= a1
    _max a1 a2         = if a1 >- a2 then a1 else a2
    _min a1 a2         = if a1 >- a2 then a2 else a1
    _compare a1 a2
        | a1 >- a2     = GT
        | a1 == a2     = EQ
        | otherwise    = LT
