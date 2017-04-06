module ExpressionAnalysis where

import           Chapter_17_my_note

data Ops = Add | Sub | Mul | Div | Mod

type Var = String

data Expr = Lit Int
          | Var Var
          | Op Ops Expr Expr

parser :: Parse Char Expr
parser    = litParse `alt` varParse

varParse :: Parse Char Expr
varParse    = spotVar `build` Var

spotVar :: Parse Char String
spotVar content    = if null content || (not . isVarFrag . head) content
                     then succeed [] content
                     else ((spotVarFrag >*> spotVar) `build` uncurry (:)) content

spotVarFrag :: Parse Char Char
spotVarFrag    = spot isVarFrag

isVarFrag :: Char -> Bool
isVarFrag frag    = frag <= 'z' && frag >= 'a'

isOp :: Char -> Bool
isOp ch    = ch `elem` "+-*/%"

charToOp :: Char -> Ops
charToOp ch | ch == '+'    = Add
            | ch == '-'    = Sub
            | ch == '*'    = Mul
            | ch == '/'    = Div
            | ch == '%'    = Mod
            | otherwise    = error "wrong operation"

litParse :: Parse Char Expr
litParse    = (optional (token '~') >*> neList dig) `build` (charListToExp . uncurry (++))

charListToExp :: String -> Expr
charListToExp chl    = if head chl == '~'
                       then Lit ((-1) * read (init chl) :: Int)
                       else Lit (read chl :: Int)

opExprParse :: Parse Char Expr
opExprParse    = (token '(' >*> (parser >*> (spot isOp >*> (parser >*> token ')'))))
                 `build` makeExpr

makeExpr :: (t1, (Expr, (Char, (Expr, t)))) -> Expr
makeExpr (_, (e1, (bop, (e2, _))))    = Op (charToOp bop) e1 e2
