module CalcInpt where

import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Char

data Expr = Lit { value    :: Int }
          | Var { variable :: Char }
          | Op { operate   :: Ops
               , express1  :: Expr
               , express2  :: Expr }
          deriving Show

data Ops = Add | Sub | Mul | Div | Mod
         deriving Show

type Parse a b = [a] -> [(b, [a])]

none :: Parse a b
none _    = []

spot :: (a -> Bool) -> Parse a a
spot f (x : xs) | f x          = [(x, xs)]
                | otherwise    = []
spot _ []                      = []

token :: Eq a => a -> Parse a a
token t    = spot (== t)

-- tc is testcase and tp is testpass
succeed :: b -> Parse a b
succeed tp tc    = [(tp, tc)]

alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 tc    = p1 tc ++ p2 tc

-- actually a combinator with originally: [a] to b, c, [a], then to [((b, c), [a])]
infixr 5 >*>
(>*>) :: Parse a b -> Parse a c -> Parse a (b, c)
(parse1 >*> parse2) testcase    = [ ((get1, get2), rem2) | (get1, rem1) <- parse1 testcase
                                                         , (get2, rem2) <- parse2 rem1 ]

-- change the form of parsed val, [(val, rem)] to [(f val, rem)]
build :: Parse a b -> (b -> c) -> Parse a c
build p f t    = map (\(v, r) -> (f v, r)) (p t)

-- list all the parsed until not valid: [a] to [([b], [a])]
list :: Parse a b -> Parse a [b]
list p    = succeed [] `alt` ((p >*> list p) `build` uncurry (:))

bracketL :: Parse Char Char
bracketL    = token '('

digit :: Parse Char Char
digit    = spot isDigit

digList :: Parse Char String
digList    = list digit

-- optional is something that get one parse at the top
-- like optional (token '~') "~123123" is [("~", "123123")]
-- then optional (token '~') "123123" is [("", "123123")]
optional :: Parse a b -> Parse a [b]
optional p val | null subres    = [([], val)]
               | otherwise      = (p `build` (: [])) val
  where subres                  = p val

-- neList digit "" is [("", "")]
-- neList digit "123" is [("123", "")]
-- neList digit "123a213" is [("123", "a213")]
-- neList digit "a123" is [("", "a123")]
neList :: Parse a b -> Parse a [b]
neList _ []                 = [([], [])]
neList p val | null res     = succeed [] val
             | otherwise    = ((p >*> neList p) `build` uncurry (:)) val
  where res                 = p val

-- since the number restrict has been applied
-- we consider only the problem about parser's recognization
-- since nTimes for nTimes 5 digit "a1234", then digit "p" is []
-- thus it is []
nTimes :: Int -> Parse a b -> Parse a [b]
nTimes 0 _ c                            = succeed [] c
nTimes t p c | length c < t || t < 0    = []
             | otherwise                = ((p >*> nTimes (t - 1) p) `build` uncurry (:)) c

parser :: Parse Char Expr
parser    = undefined

varParse :: Parse Char Expr
varParse    = spot isAlpha `build` Var

litParse :: Parse Char Expr
litParse    = (optional (token '~') >*> neList digit) `build` (charlistToLit . uncurry (++))

-- considered about how to change the charlist into a Int then to Lit
-- first the form might be "~123123" or "123123"
-- thus no null charlist
-- then if charlist is one, then no "~"
charlistToLit :: String -> Expr
charlistToLit []                                = error "wrong input of charlist"
charlistToLit (x : xs) | null xs && x == '~'    = error "wrong input of value"
                       | x == '~'               = Lit ((-1) * (read xs :: Int))
                       | otherwise              = Lit (read xs :: Int)

isOperator :: Char -> Bool
isOperator    = flip elem "-+*/%"
