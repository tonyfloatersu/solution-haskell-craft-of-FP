module CalcInptType ( Expr (..), Ops (..), Parse ) where

data Expr = Lit { value    :: Int }
          | Var { variable :: String }
          | Op { operate   :: Ops
               , express1  :: Expr
               , express2  :: Expr }
          deriving (Eq, Show)

data Ops = Add | Sub | Mul | Div | Mod | Def | Frc
         deriving (Eq, Show)

type Parse a b = [a] -> [(b, [a])]
