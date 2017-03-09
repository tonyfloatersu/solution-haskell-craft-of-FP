module Chapter_10_my_note where

import           Prelude hiding (unzip, last, init, getLine)
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Data.Typeable

doubleAllv1 :: [Integer] -> [Integer]
doubleAllv1 ls    = [x * 2 | x <- ls]

doubleAllv2 :: [Integer] -> [Integer]
doubleAllv2 []          = []
doubleAllv2 (x : xs)    = (2 * x) : doubleAllv2 xs

doubleAllv3 :: [Integer] -> [Integer]
doubleAllv3    = map (\x -> 2 * x)

lengthMapSum :: [a] -> Int
lengthMapSum    = sum . map (\x -> 1)

addUpv1 :: [Integer] -> [Integer]
addUpv1 ns    = filter (> 1) (map (+ 1) ns)

addUpv2 :: [Integer] -> [Integer]
addUpv2 ns    = map (+ 1) (filter (> 0) ns)

squareList :: [Integer] -> [Integer]
squareList    = map (\x -> x * x)

squareListSum :: [Integer] -> Integer
squareListSum    = sum . map (\x -> x * x)

allGreaterZero :: [Integer] -> Bool
allGreaterZero    = and . map (> 0)

minFunScope :: (Ord t, Num t) => (t -> t) -> [t] -> t
minFunScope func ls    = minimum (map func ls)

allFunEqual :: (Ord t, Num t) => (t -> t) -> [t] -> Bool
allFunEqual func ls    = null (filter (/= func (head ls)) (map func ls))

allGreaterFunZero :: (Ord t, Num t) => (t -> t) -> [t] -> Bool
allGreaterFunZero func ls    = and (map ((> 0) . func) ls)

funIssorted :: (Ord t, Num t) => (t -> t) -> [t] -> Bool
funIssorted func ls    = isSorted (map func ls)

isSorted :: (Ord t, Num t) => [t] -> Bool
isSorted []              = True
isSorted [_]             = True
isSorted (x : y : xs)    = x < y && isSorted (y : xs)

twice :: (t -> t) -> t -> t
twice func    = func . func

iter :: Integer -> (t -> t) -> t -> t
iter times func val
    | times < 0     = error "wrong time data given"
    | times == 0    = val
    | otherwise     = iter (times - 1) func (func val)

base2 :: Integer -> Integer
base2 n
    | n >= 0       = iter n (\x -> 2 * x) 1
    | otherwise    = error "time negative"

propFilter :: Fun Integer Bool -> [Integer] -> Bool
propFilter func ls    = filter (apply func) ls == filter (apply func) (filter (apply func) ls)

propFilterTestEnv :: (Fun Integer Bool -> [Integer] -> Bool) -> IO ()
propFilterTestEnv    = quickCheck

rev :: [a] -> [a]
rev    = foldr snoc []

snoc :: a -> [a] -> [a]
snoc x xs    = xs ++ [x]

toNSquareSum :: Integer -> Integer
toNSquareSum val
    | val > 0      = foldr (+) 0 (map (\x -> x * x) [1 .. val])
    | otherwise    = error "scope error"

listSquareSum :: [Integer] -> Integer
listSquareSum ls    = foldr (+) 0 (map (\x -> x * x) ls)

unzip :: [(a, b)] -> ([a], [b])
unzip orig    = foldr fragUnzip ([], []) orig

fragUnzip :: (a, b) -> ([a], [b]) -> ([a], [b])
fragUnzip (p1, p2) (w1, w2)    = (p1 : w1, p2 : w2)

last1 :: [a] -> a
last1    = foldr1 (\_ x -> x)

last :: [a] -> a
last ls
    | not $ null ls    = head $ foldr (\x y -> if null y then [x] else y) [] ls
    | otherwise        = error "length 0"

init :: [a] -> [a]
init ls
    | null ls      = error "length 0"
    | otherwise    = tail (foldr tailToHead [] ls)
  where
    tailToHead :: a -> [a] -> [a]
    tailToHead frag lst    = if null lst then [frag] else [head lst] ++ [frag] ++ tail lst

type Line = String

formatLine :: Line -> String
formatLine xs    = xs

formatList :: (a -> String) -> [a] -> String
formatList formatItem ls    = foldr (++) [] (map formatItem ls)

formatLines :: [Line] -> String
formatLines    = formatList formatLine

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ []           = []
filterFirst filtCond ls    = if filtCond (head ls)
                             then tail ls
                             else head ls : filterFirst filtCond (tail ls)

-- returnLoan use filterFirst or all the book will come out...

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast _ []           = []
filterLast filtCond ls    = if filtCond (last ls)
                            then init ls
                            else filterLast filtCond (init ls) ++ [last ls]

filterLast1 :: (a -> Bool) -> [a] -> [a]
filterLast1 filtCond ls    = reverse (filterFirst filtCond (reverse ls))

switchMap :: (a -> b) -> (a -> b) -> [a] -> [b]
switchMap _ _ []          = []
switchMap fun1 _ [x]      = [fun1 x]
switchMap fun1 fun2 (x1 : x2 : rest)    = fun1 x1 : fun2 x2 : switchMap fun1 fun2 rest

split :: [a] -> ([a], [a])
split []                  = ([], [])
split [x]                 = ([x], [])
split (x1 : x2 : rest)    = (x1 : fst (split rest), x2 : snd (split rest))

merge :: ([a], [a]) -> [a]
merge (x : xs, y : ys)    = x : y : merge (xs, ys)
merge (x : _, [])         = [x]
merge (_, _)              = []

-- g (foldr1 g xs) (foldr1 g ys) == foldr1 g (xs ++ ys)

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ []             = []
dropUntil cond (l : ls)    = if cond l
                             then l : ls
                             else dropUntil cond ls

isSpace :: Char -> Bool
isSpace ch    = ch `elem` [' ', '\n', '\r', '\t']

dropSpace :: String -> String
dropSpace    = dropUntil (not . isSpace)

dropWord :: String -> String
dropWord    = dropUntil isSpace

getUntil :: (a -> Bool) -> [a] -> [a]
getUntil _ []          = []
getUntil f (x : xs)    = if f x
                         then []
                         else x : getUntil f xs

getWord :: String -> String
getWord line    = getUntil isSpace (dropSpace line)

dropLine :: String -> String
dropLine    = dropSpace . dropWord . dropSpace

strToLine :: String -> [String]
strToLine []      = []
strToLine line    = getWord line : strToLine (dropLine line)

getLine :: Int -> (a -> Int) -> [a] -> [a]
getLine _ _ []      = []
getLine len f (x : xs)
    | f x <= len    = x : getLine (len - f x - 1) f xs
    | otherwise     = []

multiDropLine :: Int -> (a -> Int) -> [a] -> [a]
multiDropLine _ _ []      = []
multiDropLine len f (x : xs)    = if len >= f x
                                  then multiDropLine (len - f x - 1) f xs
                                  else x : xs

multiSplitLines :: Int -> (a -> Int) -> [a] -> [[a]]
multiSplitLines _ _ []           = []
multiSplitLines len f (x : xs)   = getLine len f (x : xs)
                                   : multiSplitLines len f (multiDropLine len f (x : xs))

type Picture = [String]

invertColorDot :: Char -> Char
invertColorDot ch    = if ch == '#' then '.' else '#'

invertColorLine :: String -> String
invertColorLine    = map invertColorDot

invertColor :: Picture -> Picture
invertColor    = map invertColorLine

data PicSize = ReguPic Int Int |
               IrrePic Int
               deriving (Eq, Show)

isReg :: Picture -> Bool
isReg pic    = and [length line == length (head pic) | line <- pic]

picSizeGen :: Picture -> PicSize
picSizeGen pic    = if isReg pic
                    then ReguPic (length pic) (length (head pic))
                    else IrrePic (length pic)

-- picSizeComp :: Picture -> Picture -> Bool
-- picSizeComp p1 p2    = if isReg p1 == isReg p2
--                         then picSizeGen p1 == picSizeGen p2
--                         else error "there is a regular pic and a irregular one"

-- superimpose :: Picture -> Picture -> Picture
-- superimpose    = undefined

isSizeReg :: PicSize -> Bool
isSizeReg (ReguPic _ _)    = True
isSizeReg (IrrePic _)      = False

{-
compariation of figures:
-----> not same regularity: ------> False now temporary
  |
  ---> same regularity: ------> all regular ----> same size
                         |                    |
                         |                    ---> not same size
                         -----> all irregular
-}

figSizeComp :: PicSize -> PicSize -> Bool
figSizeComp size1 size2    = isSizeReg size1 == isSizeReg size2
                             && isSizeReg size1 && size1 == size2

colorPriority :: Char -> Char -> Char
colorPriority ch1 ch2    = if ch1 == '#' || ch2 == '#'
                           then '#'
                           else '.'

superimpose :: Picture -> Picture -> Picture
superimpose p1 p2    = if figSizeComp (picSizeGen p1) (picSizeGen p2)
                       then map zipColor (zip p1 p2)
                       else error "feature not supported... sorry!"

zipColor :: (String, String) -> String
zipColor (l1, l2)    = zipWith colorPriority l1 l2
