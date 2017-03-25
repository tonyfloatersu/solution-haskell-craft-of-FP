module CodeTable (codeTable) where

import           Types ( Bit (L, R)
                       , HCode
                       , Table
                       , Tree (Leaf, Node) )

codeTable :: Tree -> Table
codeTable    = convert []

convert :: HCode -> Tree -> Table
convert hc (Leaf c _)        = [(c, hc)]
convert hc (Node _ t1 t2)    = convert (hc ++ [L]) t1 ++ convert (hc ++ [R]) t2
