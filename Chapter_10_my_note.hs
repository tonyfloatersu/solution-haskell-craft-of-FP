module Chapter_10_my_note where

import           Prelude hiding (unzip, last, init, getLine)
import           Test.QuickCheck
import           Test.QuickCheck.Function

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
  ---> same regularity: ------> all regular ----> same size DONE
                         |                    |
                         |                    ---> not same size DONE
                         -----> all irregular
-}

figRegComp :: PicSize -> PicSize -> Bool
figRegComp size1 size2    = isSizeReg size1 == isSizeReg size2
                            && isSizeReg size1

colorPriority :: Char -> Char -> Char
colorPriority ch1 ch2    = if ch1 == '#' || ch2 == '#'
                           then '#'
                           else '.'

superimpose :: Picture -> Picture -> Picture
superimpose p1 p2    = if figRegComp (picSizeGen p1) (picSizeGen p2)
                       then if picSizeGen p1 == picSizeGen p2
                            then map zipColor (zip p1 p2)
                            else mergePics p1 p2
                       else error "feature not supported... sorry!"

zipColor :: (String, String) -> String
zipColor (l1, l2)    = zipWith colorPriority l1 l2

_regPicSize :: PicSize -> (Int, Int)
_regPicSize (ReguPic a b)    = (a, b)
_regPicSize _                = error "regpicsize error"

regPicSize :: Picture -> (Int, Int)
regPicSize    = _regPicSize . picSizeGen

largestSize :: Picture -> Picture -> (Int, Int)
largestSize p1 p2    = (max (fst $ regPicSize p1) (fst $ regPicSize p2),
                        max (snd $ regPicSize p1) (fst $ regPicSize p2))

blankLineGen :: Int -> String
blankLineGen val    = concat ["." | _ <- [1 .. val]]

changeSizeTo :: Picture -> (Int, Int) -> Picture -- first int is width, second is height
changeSizeTo pic (width, height)    =
    [line ++ blankLineGen (width - length (head pic)) | line <- pic]
    ++ [blankLineGen width | _ <- [1 .. height - length pic]]

changeToLargest :: Picture -> Picture -> Picture -> Picture
changeToLargest base cp1 cp2    = changeSizeTo base (largestSize cp1 cp2)

mergePics :: Picture -> Picture -> Picture
mergePics p1 p2    = map zipColor
                     (zip (changeToLargest p1 p1 p2) (changeToLargest p2 p1 p2))

getVert :: Int -> String -> String -> String
getVert val str remi    = str !! val : remi

rotate901 :: Picture -> Picture
rotate901 pic    = [foldr (getVert i) [] pic | i <- [0 .. length (head pic) - 1]]

rotate90 :: Picture -> Picture
rotate90 []        = []
rotate90 pic     = reverse (map head pic) : reverse (map tail pic)

type Person = String
type Book = String

type Database = [(Person, Book)]

books :: Database -> Person -> [Book]
books db name    = (fst . unzip) (filter (matchName name) db)
  where
    matchName :: Person -> (Person, Book) -> Bool
    matchName nm pt    = fst pt == nm

borrowers :: Database -> Book -> [Person]
borrowers db name    = (snd . unzip) (filter (matchName name) db)
  where
    matchName :: Book -> (Person, Book) -> Bool
    matchName nm pt    = snd pt == nm

isBorrowed :: Database -> Book -> Bool
isBorrowed db name    = name `elem` (snd . unzip) db

numBorrowed :: Database -> Book -> Int
numBorrowed db name    = length (filter (matchName name) db)
  where
    matchName :: Book -> (Person, Book) -> Bool
    matchName nm pt    = snd pt == nm

makeLoan :: Database -> Person -> Book -> Database
makeLoan db pname bname    = (pname, bname) : db

returnLoan :: Database -> Person -> Book -> Database
returnLoan db pname bname    = filterFirst (matchName (pname, bname)) db
  where
    matchName :: (Person, Book) -> (Person, Book) -> Bool
    matchName test part    = fst test == fst part && snd test == snd part

type DataBase = [(Person, [Book])]

_books :: DataBase -> Person -> [Book]
_books db name    = (snd . head) (filter (matchName name) db)
  where
    matchName :: Person -> (Person, [Book]) -> Bool
    matchName nm part    = fst part == nm

_borrowers :: DataBase -> Book -> [Person]
_borrowers db name    = (fst . unzip) (filter (matchName name) db)
  where
    matchName :: Book -> (Person, [Book]) -> Bool
    matchName bk pr    = bk `elem` snd pr

_borrowed :: DataBase -> Book -> Bool
_borrowed db name    = (not . null) (filter (matchName name) db)
  where
    matchName :: Book -> (Person, [Book]) -> Bool
    matchName bk pr    = bk `elem` snd pr

_numBorrowed :: DataBase -> Person -> Int
_numBorrowed db name    = (length . snd . unzip) (filter (matchName name) db)
  where
    matchName :: Person -> (Person, [Book]) -> Bool
    matchName nm pr    = nm == fst pr

_makeLoan :: DataBase -> Person -> Book -> DataBase
_makeLoan db pname bname    = map (insertBook pname bname) db
  where
    insertBook :: Person -> Book -> (Person, [Book]) -> (Person, [Book])
    insertBook pnm bnm pr    = if fst pr == pnm
                               then (pnm, bnm : snd pr)
                               else pr

_returnLoan :: DataBase -> Person -> Book -> DataBase
_returnLoan db pname bname    = map (filterOneBook pname bname) db
  where
    filterOneBook :: Person -> Book -> (Person, [Book]) -> (Person, [Book])
    filterOneBook pnm bnm pr    = if pnm == fst pr
                                  then (pnm, filterFirst (== bnm) (snd pr))
                                  else pr

data Move = Rock | Paper | Scissors
            deriving (Eq)

instance Show Move where
         show Rock        = "r"
         show Paper       = "p"
         show Scissors    = "s"

convertMove :: Char -> Move
convertMove 'r'    = Rock
convertMove 'R'    = Rock
convertMove 'p'    = Paper
convertMove 'P'    = Paper
convertMove 's'    = Scissors
convertMove 'S'    = Scissors
convertMove _      = error "do not match"

type Tournament = ([Move], [Move])

outcome :: Move -> Move -> Integer
outcome Rock Paper        = -1
outcome Paper Rock        = 1
outcome Paper Scissors    = -1
outcome Rock Scissors     = 1
outcome Scissors Paper    = 1
outcome Scissors Rock     = -1
outcome _ _               = 0

tournamentOutcome :: Tournament -> Integer
tournamentOutcome tour    = sum (zipWith outcome (fst tour) (snd tour))
