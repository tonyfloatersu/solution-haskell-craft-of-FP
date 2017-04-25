module CalcInpt where

import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Char
import           CalcInptType ( Expr (..), Ops (..), Parse)

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
parser    = _parser . filter (not . flip elem " \n\t")

_parser :: Parse Char Expr
_parser    = varParse `alt` litParse `alt` opExprParser

varParse :: Parse Char Expr
varParse    = neList (spot (\x -> x `elem` ['a' .. 'z'])) `build` Var

litParse :: Parse Char Expr
litParse    = (optional (token '~') >*> neList digit) `build` (charlistToLit . uncurry (++))

-- considered about how to change the charlist into a Int then to Lit
-- first the form might be "~123123" or "123123"
-- thus no null charlist
-- then if charlist is one, then no "~"
-- since all the variable are ['a' .. 'z']
-- thus if anything is wrong, then use Var "Fail" to show that it is error
charlistToLit :: String -> Expr
charlistToLit []                                = Var "Fail"
charlistToLit (x : xs) | null xs && x == '~'    = Var "Fail"
                       | x == '~'               = Lit ((-1) * (read xs :: Int))
                       | otherwise              = Lit (read (x : xs) :: Int)

isOperator :: Char -> Bool
isOperator    = flip elem "-+*/%:."

opExprParser :: Parse Char Expr
opExprParser    = (token '(' >*> _parser >*> spot isOperator >*> _parser >*> token ')')
                  `build` makeExpr

makeExpr :: (a1, (Expr, (Char, (Expr, a2)))) -> Expr
makeExpr (_, (e1, (op, (e2, _))))    = Op (symbToOper op) e1 e2

symbToOper :: Char -> Ops
symbToOper ch | ch == '-'    = Sub
              | ch == '+'    = Add
              | ch == '*'    = Mul
              | ch == '/'    = Div
              | ch == '%'    = Mod
              | ch == ':'    = Def
              | ch == '.'    = Frc
              | otherwise    = error "wrong symbol"

topLevel :: Parse a b -> b -> [a] -> b
topLevel p defaultVal inp    = case results of
                                 [] -> defaultVal
                                 _  -> head results
  where results              = [ f | (f, []) <- p inp ]

exprParser :: String -> Expr
exprParser    = topLevel parser (Var "Fail")

data Command = Eval { interpreted :: Expr }
             | Assign { varName :: String
                      , varValu :: Expr }
             | Exit { }
             | Null { }
             deriving (Show, Eq)

commandParse :: Parse Char Command
commandParse []                           = [(Null, [])]
commandParse xs | subres == Var "Fail"    = [(Null, [])]
                | subres == Var "exit"    = [(Exit, [])]
                | otherwise               = [(defExprCommandTrans subres, [])]
  where subres                            = exprParser xs :: Expr

defExprCommandTrans :: Expr -> Command
defExprCommandTrans (Op Def (Var e1) e2)    = Assign e1 e2
defExprCommandTrans other                   = Eval other

commandLine :: String -> Command
commandLine    = topLevel commandParse Null
