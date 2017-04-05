module Chapter_17_my_note where

import           Data.List ( (\\) )
import           SetADT
import           Relation

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
-- = [([], [x]), ([x], [])] ...

type Vector = [Float]

scalarProduct :: Vector -> Vector -> Float
scalarProduct v1 v2    = sum $ zipWith (*) v1 v2

type Matrix = [Vector]

matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct horz vert    = [map (scalarProduct line) (columns vert) | line <- horz]

columns :: Matrix -> Matrix
columns vert    = [[(vert !! i) !! j | i <- [0 .. length vert - 1]]
                    | j <- [0 .. (length . head) vert - 1]]

subList :: [a] -> [[a]]
subList []          = [[]]
subList (x : xs)    = map (x : ) (subList xs) ++ subList xs

subsequence :: [a] -> [[a]]
subsequence    = filter (not . null) . concatMap (map snd . splits . fst) . splits

lenpairs :: Int -> [(Int, Int)]
lenpairs len    = [(x, y) | x <- [0 .. len - 1], y <- [x .. len - 1]]

cutlen :: (Int, Int) -> [a] -> [a]
cutlen (x, y) len    = concatMap ((: []) . (len !!)) [x .. y]

subseqlist :: [a] -> [[a]]
subseqlist xs    = map (`cutlen` xs) lenxs
  where lenxs    = (lenpairs . length) xs :: [(Int, Int)]

graphEx :: Relation Int
graphEx    = makeSet [(1, 2), (1, 3), (2, 4), (3, 5), (3, 6), (5, 6)]

graphEx2 :: Relation Int
graphEx2    = makeSet [(1, 2), (1, 3), (2, 1), (2, 4), (3, 5), (3, 6), (5, 6)]

routes :: Ord a => Relation a -> a -> a -> [[a]]
routes rel x y | x == y       = [[x]]
               | otherwise    = [x : r | z <- nbhrs rel x, r <- routes rel z y]

nbhrs :: Ord a => Relation a -> a -> [a]
nbhrs rel x    = flatten (image rel x)

routesC :: Ord a => Relation a -> a -> a -> [a] -> [[a]]
routesC rel x y avoid | x == y       = [[x]]
                      | otherwise    = [x : r | z <- nbhrs rel x \\ avoid
                                              , r <- routesC rel z y (x : avoid)]
