module Relation ( Relation
                , image
                , setImage
                , addImage
                , addChildren
                , compose
                , setProduct
                , adjoin
                , tClosure
                , limit
                , connect ) where

import           SetADT

type Relation a = Set (a, a)

image :: Ord a => Relation a -> a -> Set a
image rel val    = mapSet snd (filterSet ((== val) . fst) rel)

setImage :: Ord a => Relation a -> Set a -> Set a
setImage rel    = setUnion . mapSet (image rel)

addImage :: Ord a => Relation a -> Set a -> Set a
addImage ret st    = st `union` setImage ret st

compose :: Ord a => Relation a -> Relation a -> Relation a
compose rel1 rel2    = mapSet (\((a, _), (_, d)) -> (a, d))
                       (filterSet (\((_, b), (c, _)) -> b == c) (setProduct rel1 rel2))

setProduct :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
setProduct st1    = setUnion . mapSet (adjoin st1)

adjoin :: (Ord a, Ord b) => Set a -> b -> Set (a, b)
adjoin sett targ    = mapSet (\x -> (x, targ)) sett

tClosure :: Ord a => Relation a -> Relation a
tClosure rela    = limit (\x -> x `union` (x `compose` rela)) rela

limit :: Eq a => (a -> a) -> a -> a
limit f x
    | x == f x     = x
    | otherwise    = limit f (f x)

type People = String

isParents :: Relation People
isParents    = makeSet [("Ben", "Sue"), ("Ben", "Leo"), ("Sue", "Joe")]

isGrandParents :: Relation People
isGrandParents    = isParents `compose` isParents

inverser :: Ord a => Relation a -> Relation a
inverser    = mapSet (\(a, b) -> (b, a))

addChildren :: Set People -> Set People
addChildren    = addImage isParents

connect :: Ord a => Relation a -> Relation a
connect rel    = tClosure rel `union` (inverser . tClosure) rel

elems :: Ord a => Relation a -> Set a
elems rel    = mapSet fst rel `union` mapSet snd rel
