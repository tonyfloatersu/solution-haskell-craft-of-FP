module Chapter_13_my_note where

import           Chapter_5_my_note ( Shape ( Circle
                                           , Rectangle
                                           , Triangle)
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
    size _          = 0
    provideExp :: a -> [a]
    provideExp _    = example

instance Info Bool where
    example    = [True, False]
    size _     = 1

instance Info Char where
    example    = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
    size _     = 1

instance Info Int where
    example    = [-100 .. 100]
    size _     = 1

instance Info Shape where
    example    = [Circle 3.0, Rectangle 45.9 87.6]
    size       = round . area

instance Info a => Info [a] where
    example    = [[]] ++ [[x] | x <- example] ++ [[x, y] | x <- example, y <- example]
    size       = foldr ((+) . size) 0
