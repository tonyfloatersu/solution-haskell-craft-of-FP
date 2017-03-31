module Relation ( Relation
                , image
                , setImage
                , addImage
--                , addChildren
                , compose
                , setProduct
                , adjoin
                , tClosure
                , limit ) where

import           SetADT

type Relation a = Set (a, a)

image :: Ord a => Relation a -> a -> Set a
image rel val    = mapSet snd (filterSet ((== val) . fst) rel)

setImage :: Ord a => Relation a -> Set a -> Set a
setImage rel    = setUnion . mapSet (image rel)

addImage :: Ord a => Relation a -> Set a -> Set a
addImage ret st    = st `union` setImage ret st

compose :: Ord a => Relation a -> Relation a -> Relation a
compose rel1 rel2    = mapSet (\((a, _), (_, d)) -> (a, d)) (setProduct rel1 rel2)

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
