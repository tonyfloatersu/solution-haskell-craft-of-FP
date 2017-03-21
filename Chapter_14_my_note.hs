{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module Chapter_14_my_note where

data Ntree = NilT
           | Node Integer Ntree Ntree

data Expr = Lit Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Prd Expr Expr
          | Div Expr Expr

eval :: Expr -> Integer
eval (Lit val)          = val
eval (Add val1 val2)    = eval val1 + eval val2
eval (Sub val1 val2)    = eval val1 - eval val2
eval (Prd val1 val2)    = eval val1 * eval val2
eval (Div val1 val2)    = if eval val2 == 0
                          then error "divide 0?"
                          else div (eval val1) (eval val2)

instance Show Expr where
    show (Lit n)        = show n
    show (Add n1 n2)    = "(" ++ show n1 ++ " + " ++ show n2 ++ ")"
    show (Sub n1 n2)    = "(" ++ show n1 ++ " - " ++ show n2 ++ ")"
    show (Prd n1 n2)    = "(" ++ show n1 ++ " * " ++ show n2 ++ ")"
    show (Div n1 n2)    = "(" ++ show n1 ++ " / " ++ show n2 ++ ")"

sumTree, depth :: Ntree -> Integer
sumTree NilT                      = 0
sumTree (Node val tree1 tree2)    = val + sumTree tree1 + sumTree tree2
depth NilT                        = 0
depth (Node _ tree1 tree2)        = 1 + max (depth tree1) (depth tree2)

occurs :: Ntree -> Integer -> Integer
occurs NilT _    = 0
occurs (Node val t1 t2) p    = (if val == p then 1 else 0) + occurs t1 p + occurs t2 p

assoc :: Expr -> Expr
assoc (Add (Add e1 e2) e3)    = assoc (Add e1 (Add e2 e3))
assoc (Add e1 e2)             = Add (assoc e1) (assoc e2)
assoc (Lit fa)                = Lit fa

sizeepr :: Expr -> Integer
sizeepr (Lit _)            = 0
sizeepr (Sub exp1 exp2)    = 1 + sizeepr exp1 + sizeepr exp2
sizeepr (Add exp1 exp2)    = 1 + sizeepr exp1 + sizeepr exp2
sizeepr (Prd exp1 exp2)    = 1 + sizeepr exp1 + sizeepr exp2
sizeepr (Div exp1 exp2)    = 1 + sizeepr exp1 + sizeepr exp2

data Express = Liter Integer
             | Express :+: Express
             | Express :-: Express
             | Express :*: Express
             | Express :/: Express

evaluation :: Express -> Integer
evaluation (Liter valu)       = valu
evaluation (exp1 :+: exp2)    = evaluation exp1 + evaluation exp2
evaluation (exp1 :-: exp2)    = evaluation exp1 - evaluation exp2
evaluation (exp1 :*: exp2)    = evaluation exp1 * evaluation exp2
evaluation (exp1 :/: exp2)    = if evaluation exp2 == 0
                                then error "wrong division, snd val is 0"
                                else div (evaluation exp1) (evaluation exp2)

instance Show Express where
    show (Liter val)        = show val
    show (exp1 :+: exp2)    = "(" ++ show exp1 ++ " + " ++ show exp2 ++ ")"
    show (exp1 :-: exp2)    = "(" ++ show exp1 ++ " - " ++ show exp2 ++ ")"
    show (exp1 :*: exp2)    = "(" ++ show exp1 ++ " * " ++ show exp2 ++ ")"
    show (exp1 :/: exp2)    = "(" ++ show exp1 ++ " / " ++ show exp2 ++ ")"

assocr :: Express -> Express
assocr ((e1 :+: e2) :+: e3)    = assocr (e1 :+: (e2 :+: e3))
assocr (e1 :+: e2)             = assocr e1 :+: assocr e2
assocr (Liter val)             = Liter val

data Expression = Literal Integer
                | Op Ops Expression Expression
data Ops = Addi | Subs | Mult | Divi | Modu

evaluate :: Expression -> Integer
evaluate (Literal valu)         = valu
evaluate (Op Addi exp1 exp2)    = evaluate exp1 + evaluate exp2
evaluate (Op Subs exp1 exp2)    = evaluate exp1 - evaluate exp2
evaluate (Op Mult exp1 exp2)    = evaluate exp1 * evaluate exp2
evaluate (Op Divi exp1 exp2)    = if evaluate exp2 == 0
                                  then error "wrong division, snd val is 0"
                                  else div (evaluate exp1) (evaluate exp2)
evaluate (Op Modu exp1 exp2)    = if evaluate exp2 == 0
                                  then error "wrong mod, snd val is 0"
                                  else mod (evaluate exp1) (evaluate exp2)

instance Show Expression where
    show (Literal val)          = show val
    show (Op Addi exp1 exp2)    = "(" ++ show exp1 ++ " + " ++ show exp2 ++ ")"
    show (Op Subs exp1 exp2)    = "(" ++ show exp1 ++ " - " ++ show exp2 ++ ")"
    show (Op Mult exp1 exp2)    = "(" ++ show exp1 ++ " * " ++ show exp2 ++ ")"
    show (Op Divi exp1 exp2)    = "(" ++ show exp1 ++ " / " ++ show exp2 ++ ")"
    show (Op Modu exp1 exp2)    = "(" ++ show exp1 ++ " mod " ++ show exp2 ++ ")"

sizeexpr :: Expression -> Integer
sizeexpr (Literal _)     = 0
sizeexpr (Op _ e1 e2)    = 1 + sizeexpr e1 + sizeexpr e2

associater :: Expression -> Expression
associater (Op Addi (Op Addi e1 e2) e3)    = associater (Op Addi e1 (Op Addi e2 e3))
associater (Op Addi e1 e2)                 = Op Addi (associater e1) (associater e2)
associater (Literal valu)                  = Literal valu