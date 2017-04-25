module CalcInptStoreModule ( Store, initial, valueS, update ) where

import           CalcInptType ( Expr (..), Ops (..), Parse )

type Var = String

newtype Store = Store [(Expr, Var)]

initial :: Store
initial    = Store []

valueS :: Store -> Var -> Expr
valueS (Store []) _                           = Var "Fail"
valueS (Store((e, v) : xs)) vt | vt == v      = e
                               | otherwise    = valueS (Store xs) vt

update :: Store -> Var -> Expr -> Store
update (Store st) v e    = Store ((e, v) : st)
