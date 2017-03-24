module Types ( Bit (L, R)
             , HCode
             , Table
             , Tree (Leaf, Node) ) where

-- you can also use Tree (..) and Bit (..) here

data Bit = L | R
         deriving (Eq, Show)

type HCode = [Bit]

type Table = [(Char, HCode)]

data Tree = Leaf Char Int
          | Node Int Tree Tree
          deriving (Show)
