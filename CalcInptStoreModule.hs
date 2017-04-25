module CalcInptStoreModule () where

import           CalcInptType ( Expr (..), Ops (..), Parse )

type Var = String

newtype Store = Store [(Expr, Var)]
