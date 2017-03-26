module PropertyQue ( PropertyQue
                   , emptyPQ
                   , isEmptyPQ
                   , addPQ
                   , remPQ ) where

newtype PropertyQue a = PropertyQue { unPropertyQue :: [a] }

emptyPQ :: PropertyQue a
emptyPQ    = PropertyQue []

isEmptyPQ :: PropertyQue a -> Bool
isEmptyPQ (PropertyQue [])    = True
isEmptyPQ _                   = False

addPQ :: (Eq a, Ord a) => a -> PropertyQue a -> PropertyQue a
addPQ val (PropertyQue ls)    = PropertyQue (insert val ls)

insert :: (Eq a, Ord a) => a -> [a] -> [a]
insert val []          = [val]
insert val (x : xs)    = if x >= val
                         then val : x : xs
                         else x : insert val xs

remPQ :: (Eq a, Ord a) => PropertyQue a -> PropertyQue a
remPQ (PropertyQue [])    = error "propertyque empty"
remPQ (PropertyQue xs)    = PropertyQue (init xs)
