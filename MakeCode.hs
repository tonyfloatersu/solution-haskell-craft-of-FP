module MakeCode (codeTable, codes) where

import           MakeTree (makeTree)
import           CodeTable (codeTable)
import           Frequency (frequency)

import           Types ( Bit (L, R)
                       , HCode
                       , Table
                       , Tree (Leaf, Node) )

codes :: String -> Tree
codes    = makeTree . frequency
