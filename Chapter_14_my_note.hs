{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module Chapter_14_my_note where

import           Chapter_7_my_note (qSort)

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
                | If BExp Expression Expression

data Ops = Addi | Subs | Mult | Divi | Modu

data BExp = BoolLit Bool
          | And BExp BExp
          | Not BExp
          | Equal Expression Expression
          | Greater Expression Expression

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
evaluate (If what exp1 exp2)    = if bevaluate what
                                  then evaluate exp1
                                  else evaluate exp2

bevaluate :: BExp -> Bool
bevaluate (BoolLit boo)      = boo
bevaluate (And b1 b2)        = bevaluate b1 && bevaluate b2
bevaluate (Not b1)           = (not . bevaluate) b1
bevaluate (Equal e1 e2)      = evaluate e1 == evaluate e2
bevaluate (Greater e1 e2)    = evaluate e1 > evaluate e2

instance Show Expression where
    show (Literal val)          = show val
    show (Op Addi exp1 exp2)    = "(" ++ show exp1 ++ " + " ++ show exp2 ++ ")"
    show (Op Subs exp1 exp2)    = "(" ++ show exp1 ++ " - " ++ show exp2 ++ ")"
    show (Op Mult exp1 exp2)    = "(" ++ show exp1 ++ " * " ++ show exp2 ++ ")"
    show (Op Divi exp1 exp2)    = "(" ++ show exp1 ++ " / " ++ show exp2 ++ ")"
    show (Op Modu exp1 exp2)    = "(" ++ show exp1 ++ " mod " ++ show exp2 ++ ")"
    show (If what exp1 exp2)    = "(" ++ show exp1 ++ show what ++ show exp2 ++ ")"

instance Show BExp where
    show (BoolLit boo)          = show boo
    show (And bexp1 bexp2)      = "(" ++ show bexp1 ++ " and " ++ show bexp2 ++ ")"
    show (Not bexp)             = "(" ++ "not " ++ show bexp ++ ")"
    show (Equal exp1 exp2)      = "(" ++ show exp1 ++ " == " ++ show exp2 ++ ")"
    show (Greater exp1 exp2)    = "(" ++ show exp1 ++ " > " ++ show exp2 ++ ")"

sizeexpr :: Expression -> Integer
sizeexpr (Literal _)     = 0
sizeexpr (Op _ e1 e2)    = 1 + sizeexpr e1 + sizeexpr e2

associater :: Expression -> Expression
associater (Op Addi (Op Addi e1 e2) e3)    = associater (Op Addi e1 (Op Addi e2 e3))
associater (Op Addi e1 e2)                 = Op Addi (associater e1) (associater e2)
associater (Literal valu)                  = Literal valu

lrtrees :: Ntree -> (Ntree, Ntree)
lrtrees NilT                = error "empty list now"
-- or we can return (NilT, NilT)
lrtrees (Node _ lft rht)    = (lft, rht)

intList :: Ntree -> [Integer]
intList NilT                  = []
intList (Node val lft rht)    = [val] ++ intList lft ++ intList rht

inTree :: Ntree -> Integer -> Bool
inTree tree val    = val `elem` intList tree

treeMaxMin :: Ntree -> (Integer, Integer)
treeMaxMin tree    = ((maximum . intList) tree, (minimum . intList) tree)

reflect :: Ntree -> Ntree
reflect NilT                  = NilT
reflect (Node val lft rht)    = Node val (reflect rht) (reflect lft)

collapse :: Ntree -> [Integer]
collapse NilT                  = []
collapse (Node val lft rht)    = intList lft ++ [val] ++ intList rht

sort :: Ntree -> [Integer]
sort    = qSort . collapse

data Bio = Parent String [Person]
         | NonParent String

data Person = Adult { name :: String
                    , address :: String
                    , bio :: Bio }
            | Child { name :: String }

showBio :: Bio -> String
showBio (Parent names humans)    = names ++ concatMap showPerson humans
showBio (NonParent names)        = names

showPerson :: Person -> String
showPerson (Adult _name _address _bio)    = _name ++ _address ++ showBio _bio
showPerson (Child _name)                  = _name

data Pairs a = Pr a a

equalPair :: Eq a => Pairs a -> Bool
equalPair (Pr va vb)    = va == vb

infixr 5 :::
data List a = NilL
            | a ::: (List a)
            deriving (Eq, Ord, Show, Read)

data Tree a = Nil
            | NodeT a (Tree a) (Tree a)
            deriving (Eq, Ord, Show, Read)

depthT :: Tree a -> Integer
depthT Nil                  = 0
depthT (NodeT _ lft rht)    = 1 + max (depthT lft) (depthT rht)

occursT :: Eq a => Tree a -> a -> Integer
occursT Nil _                    = 0
occursT (NodeT vnd lft rht) t    = (if vnd == t then 1 else 0) + occursT lft t + occursT rht t

collapseT :: Tree a -> [a]
collapseT Nil                  = []
collapseT (NodeT v lft rht)    = collapseT lft ++ [v] ++ collapseT rht

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Nil                  = Nil
mapTree f (NodeT vnd lt rt)    = NodeT (f vnd) (mapTree f lt) (mapTree f rt)

data EitherTp a b = ELeft a
                  | ERight b
                  deriving (Eq, Ord, Read, Show)
-- copy the either

isLeftTp :: EitherTp a b -> Bool
isLeftTp (ELeft _)     = True
isLeftTp (ERight _)    = False

eitherTp :: (a -> c) -> (b -> c) -> EitherTp a b -> c
eitherTp f _ (ELeft vl)     = f vl
eitherTp _ f (ERight vl)    = f vl

applyLft :: (a -> c) -> EitherTp a b -> c
applyLft f (ELeft v)     = f v
applyLft _ (ERight _)    = error "apply to the right"

twist :: EitherTp a b -> EitherTp b a
twist (ELeft vl)     = ERight vl
twist (ERight vr)    = ELeft vr

applyLftEth :: (a -> c) -> EitherTp a b -> c
applyLftEth f    = eitherTp f (\_ -> error "apply to the right")

changeToLeft :: (a -> b) -> (a -> EitherTp b c)
changeToLeft f    = ELeft . f

changeToRight :: (a -> b) -> (a -> EitherTp c b)
changeToRight f    = ERight . f

joinTp :: (a -> c) -> (b -> d) -> EitherTp a b -> EitherTp c d
joinTp f1 f2    = eitherTp (ELeft . f1) (ERight . f2)
