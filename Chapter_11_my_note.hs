module Chapter_11_my_note where

infixl 9 >.>

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)

g >.> f = f . g

composeList :: [a -> a] -> (a -> a)
composeList    = foldr (.) id

mapFuns :: [a -> b] -> a -> [b]
mapFuns fs val    = map (\f -> f val) fs

comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2 f g    = \x y -> g (f x) (f y)

isBlank :: Char -> Bool
isBlank    = \ch -> not (ch `elem` "\t\n")

total :: (Integer -> Integer) -> (Integer -> Integer)
total f    = \n -> foldr ((+) . f) 0 [0 .. n]

invseq :: (a -> b -> c) -> (b -> a -> c)
invseq f    = \v2 v1 -> f v1 v2
