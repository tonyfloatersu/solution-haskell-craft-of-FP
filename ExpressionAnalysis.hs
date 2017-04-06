module ExpressionAnalysis where

import           Chapter_17_my_note

data Ops = Add | Sub | Mul | Div | Mod

type Var = String

data Expr = Lit Int
          | Var Var
          | Op Ops Expr Expr

parser :: Parse Char Expr
parser    = undefined

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
