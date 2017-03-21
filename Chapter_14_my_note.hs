{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module Chapter_14_my_note where

data Ntree = NilT
           | Node Integer Ntree Ntree

data Expr = Lit Integer
          | Add Expr Expr
          | Sub Expr Expr

eval :: Expr -> Integer
eval (Lit val)          = val
eval (Add val1 val2)    = eval val1 + eval val2
eval (Sub val1 val2)    = eval val1 - eval val2

instance Show Expr where
    show (Lit n)        = show n
    show (Add n1 n2)    = "(" ++ show n1 ++ " + " ++ show n2 ++ ")"
    show (Sub n1 n2)    = "(" ++ show n1 ++ " - " ++ show n2 ++ ")"

sumTree, depth :: Ntree -> Integer
sumTree NilT                      = 0
sumTree (Node val tree1 tree2)    = val + sumTree tree1 + sumTree tree2
depth NilT                        = 0
depth (Node _ tree1 tree2)      = 1 + max (depth tree1) (depth tree2)

occurs :: Ntree -> Integer -> Integer
occurs NilT _    = 0
occurs (Node val t1 t2) p    = (if val == p then 1 else 0) + occurs t1 p + occurs t2 p
