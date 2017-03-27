module TreeADT ( Tree
               , nil
               , isNil
               , isNode
               , leftSub
               , rightSub
               , treeVal
               , insTree
               , delete
               , minTree
               , elemT ) where

data Tree a = Nil | Node a (Tree a) (Tree a)

nil :: Tree a
nil    = Nil

isNil :: Tree a -> Bool
isNil Nil    = True
isNil _      = False

isNode :: Tree a -> Bool
isNode Node {}    = True
isNode _          = False

leftSub :: Tree a -> Tree a
leftSub Nil               = Nil
leftSub (Node _ lft _)    = lft

rightSub :: Tree a -> Tree a
rightSub Nil               = Nil
rightSub (Node _ _ rht)    = rht

treeVal :: Tree a -> a
treeVal Nil             = error "tree is nil"
treeVal (Node v _ _)    = v

insTree :: Ord a => a -> Tree a -> Tree a
insTree val Nil    = Node val Nil Nil
insTree val (Node v t1 t2)
    | val == v     = Node v t1 t2
    | val < v      = Node v (insTree val t1) t2
    | otherwise    = Node v t1 (insTree val t2)

delete :: Ord a => a -> Tree a -> Tree a
delete _ Nil       = Nil
delete val (Node v tl tr)
    | val < v           = Node v (delete val tl) tr
    | val > v           = Node v tl (delete val tr)
    | isNil tl          = tr
    | isNil tr          = tl
    | otherwise         = Node modif tl (delete modif tr)
  where (Just modif)    = minTree tr

minTree :: Ord a => Tree a -> Maybe a
minTree tree
    | isNil tree              = Nothing
    | isNil (leftSub tree)    = Just (treeVal tree)
    | otherwise               = minTree (leftSub tree)

elemT :: Ord a => a -> Tree a -> Bool
elemT _ Nil        = False
elemT val (Node wh tl tr)
    | val < wh     = elemT val tl
    | val > wh     = elemT val tr
    | otherwise    = True
