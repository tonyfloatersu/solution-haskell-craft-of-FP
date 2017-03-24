module MakeTree (makeTree) where

import           Types ( Bit (L, R)
                       , HCode
                       , Table
                       , Tree (Leaf, Node) )

makeTree :: [(Char, Int)] -> Tree
makeTree    = makeCodes . toTreeList

toTreeList :: [(Char, Int)] -> [Tree]
toTreeList    = map (uncurry Leaf)

makeCodes :: [Tree] -> Tree
makeCodes [t]    = t
makeCodes ts     = makeCodes (amalgmates ts)

amalgmates :: [Tree] -> [Tree]
amalgmates []                = error "***MakeTree :: amalgmate -> empty list"
amalgmates [_]               = error "***MakeTree :: amalgmate -> one element list"
amalgmates (t1 : t2 : ts)    = insTree (pair t1 t2) ts

insTree :: Tree -> [Tree] -> [Tree]
insTree tr []          = [tr]
insTree tr (t : ts)    = if value t >= value tr
                         then tr : t : ts
                         else t : insTree tr ts

pair :: Tree -> Tree -> Tree
pair t1 t2    = Node (value t1 + value t2) t1 t2

value :: Tree -> Int
value (Node n _ _)    = n
value (Leaf _ n)      = n
