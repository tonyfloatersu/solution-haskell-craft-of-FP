module Chapter_17_my_note where

import           Data.List ( (\\) )

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys    = [(x, y) | x <- xs, y <- ys]

triangle :: Int -> [(Int, Int)]
triangle n    = [(x, y) | x <- [1 .. n], y <- [1 .. x]]

pyTriple :: Int -> [(Int, Int, Int)]
pyTriple n    = [(x, y, z) | x <- [1 .. n], y <- [x + 1 .. n], z <- [y + 1 .. n]
                           , x * x + y * y == z * z]

perms :: Eq a => [a] -> [[a]]
perms []    = [[]]
perms xs    = [x : ps | x <- xs, ps <- perms (xs \\ [x])]

perm :: Eq a => [a] -> [[a]]
perm []          = [[]]
perm (x : xs)    = [ps ++ [x] ++ qs | rs <- perm xs, (ps, qs) <- splits rs]

splits :: [a] -> [([a], [a])]
splits []          = [([], [])]
splits (y : ys)    = ([], y : ys) : [(y : ps, rs) | (ps, rs) <- splits ys]

-- [x] -> ([], x : []) : [(y : ps, rs) | (ps, rs) | splits []] = ([], x : []) : [([x], [])]
-- then = [([], [x]), ([x], [])] ...

type Vector = [Float]
