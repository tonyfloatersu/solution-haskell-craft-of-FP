{-# OPTIONS_GHC -XFlexibleContexts #-}

module Chapter_13_my_note where

import           Chapter_5_my_note ( Shape ( Circle
                                           , Rectangle
                                           , Triangle )
                                   , isRound
                                   , area
                                   , circulation )

listMatch :: Eq a => [a] -> a -> Bool
listMatch ls val    = (and . map (== val)) ls

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
    example    = [1.1, 2.2]

instance Info Shape where
    example    = [Circle 3.0, Rectangle 45.9 87.6]
    size       = round . area

instance Info a => Info [a] where
    example    = [[]] ++ [[x] | x <- example] ++ [[x, y] | x <- example, y <- example]
    size       = foldr ((+) . size) 0

instance (Info a, Info b) => Info (a, b) where
    example          = [(a, b) | a <- example, b <- example]
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
vSort ls    = show (iSort ls)
