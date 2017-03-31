{-# OPTIONS_GHC -XFlexibleInstances #-}

module SetADT ( Set
              , empty
              , sing
              , memSet
              , union, inter, diff
              , eqSet
              , subSet
              , makeSet
              , mapSet
              , filterSet
              , foldSet
              , showSet
              , card
              , symmDiff
              , powerSet
              , setUnion
              , setInter ) where

import           Data.List hiding (union)

newtype Set a = Set [a] deriving (Show)

instance Eq a => Eq (Set a) where
  (==)    = eqSet

instance Ord a => Ord (Set a) where
  (<=)    = leqSet

empty :: Set a
empty    = Set []

sing :: a -> Set a
sing v    = Set [v]

memSet :: Ord a => a -> Set a -> Bool
memSet _ (Set [])    = False
memSet v (Set (x : xs))
    | v < x          = memSet v (Set xs)
    | v == x         = True
    | otherwise      = False

union :: Ord a => Set a -> Set a -> Set a
union (Set lsa) (Set lsb)    = Set (merge lsa lsb)

merge :: Ord a => [a] -> [a] -> [a]
merge (x : xs) (y : ys)
    | x < y        = x : merge xs (y : ys)
    | x == y       = x : merge xs ys
    | otherwise    = y : merge (x : xs) ys
merge [] ys        = ys
merge xs []        = xs

inter :: Ord a => Set a -> Set a -> Set a
inter (Set lsa) (Set lsb)    = Set (lsit lsa lsb)

lsit :: Ord a => [a] -> [a] -> [a]
lsit (x : xs) (y : ys)
    | x < y        = lsit xs (y : ys)
    | x == y       = x : lsit xs ys
    | otherwise    = lsit (x : xs) ys
lsit _ _           = []

diff :: Ord a => Set a -> Set a -> Set a
diff (Set lsa) (Set lsb)    = Set (diffls lsa lsb)

diffls :: Ord a => [a] -> [a] -> [a]
diffls (x : xs) (y : ys)
    | x < y        = x : diffls xs (y : ys)
    | x == y       = diffls xs ys
    | otherwise    = diffls (x : xs) ys
diffls [] _     = []
diffls xs []    = xs

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (Set xs) (Set ys)    = xs == ys

leqSet :: Ord a => Set a -> Set a -> Bool
leqSet (Set xs) (Set ys)    = xs <= ys

subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set xs) (Set ys)    = subLs xs ys

subLs :: Ord a => [a] -> [a] -> Bool
subLs [] _         = True
subLs _ []         = False
subLs (x : xs) (y : ys)
    | x == y       = subLs xs ys
    | x < y        = False
    | otherwise    = subLs (x : xs) ys

makeSet :: Ord a => [a] -> Set a
makeSet    = Set . removeEqu . sort
  where removeEqu :: Ord a => [a] -> [a]
        removeEqu (x : y : rs)
            | x == y       = removeEqu (x : rs)
            | otherwise    = x : removeEqu (y : rs)
        removeEqu other    = other

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f (Set as)    = (makeSet . map f) as

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet f (Set as)    = (Set . filter f) as

foldSet :: (a -> b -> b) -> b -> Set a -> b
foldSet f x (Set as)    = foldr f x as

showSet :: (a -> String) -> Set a -> String
showSet f (Set xs)    = (unlines . map f) xs

card :: Set a -> Int
card (Set xs)    = length xs

symmDiff :: Ord a => Set a -> Set a -> Set a
symmDiff sa sb    = diff sa sb `union` diff sb sa

powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set [])          = Set [Set []]
powerSet (Set (x : xs))    = powerSet (Set xs) `union`
                             mapSet (union (Set [x])) (powerSet (Set xs))

setUnion :: Ord a => Set (Set a) -> Set a
setUnion    = foldSet union empty

setInter :: Ord a => Set (Set a) -> Set a
setInter    = foldSet inter empty
