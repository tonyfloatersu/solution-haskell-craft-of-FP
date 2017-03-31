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
                , connect
                , classes
                , breadthFirst
                , depthFirst ) where

import           SetADT

import           Data.List ( nub )

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

addImages :: Ord a => Relation a -> Set (Set a) -> Set (Set a)
addImages rel    = mapSet (addImage rel)

classes :: Ord a => Relation a -> Set (Set a)
classes rel    = limit (addImages rel) (mapSet sing (elems rel))

newDescs :: Ord a => Relation a -> Set a -> a -> Set a
newDescs rel st v    = image rel v `diff` st

findDescs :: Ord a => Relation a -> [a] -> a -> [a]
findDescs rel xs v    = flatten (newDescs rel (makeSet xs) v)

flatten :: Set a -> [a]
flatten    = undefined

breadthFirst :: Ord a => Relation a -> a -> [a]
breadthFirst rel val    = limit (\x -> x ++ (nub . concatMap (findDescs rel x)) x) [val]

depthFirst :: Ord a => Relation a -> a -> [a]
depthFirst rel v    = depthSearch rel v []

depthSearch :: Ord a => Relation a -> a -> [a] -> [a]
depthSearch rel v used    = v : depthList rel (findDescs rel (v : used) v) (v : used)

depthList :: Ord a => Relation a -> [a] -> [a] -> [a]
depthList _ [] _                   = []
depthList rel (val : rest) used    = next ++ depthList rel rest (used ++ next)
  where next    = if val `elem` used then [] else depthSearch rel val used
