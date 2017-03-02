module Chapter_7_my_note where

import           Prelude hiding (Word, getLine)
import           Data.Char
import           Test.QuickCheck

head' :: [a] -> a
head' (x : _)    = x

tail' :: [a] -> [a]
tail' (_ : xs)    = xs

null' :: [a] -> Bool
null' []         = True
null' (_ : _)    = False

digits :: String -> String
digits st    = [ch | ch <- st, isDigit ch]

firstDigit :: String -> Char
firstDigit st    = case digits st of
                        []         -> '\0'
                        (x : _)    -> x

firstIntPlus :: [Integer] -> Integer
firstIntPlus (x : _)    = x + 1
firstIntPlus []         = 0

twoIntegerAddition :: [Integer] -> Integer
twoIntegerAddition (x : y : _)     = x + y
twoIntegerAddition [x]             = x
twoIntegerAddition []              = 0

twoIntegerAddition' :: [Integer] -> Integer
twoIntegerAddition' ls    = case length ls of
                                 0    -> 0
                                 1    -> ls !! 1
                                 _    -> (ls !! 0) + (ls !! 1)

product' :: [Integer] -> Integer
product' (x : xs)    = x * product' xs
product' []          = 1

prop_product :: [Integer] -> Bool
prop_product ls    = product ls == product' ls

check_product_result :: IO ()
check_product_result    = quickCheck prop_product

and' :: [Bool] -> Bool
and' (x : xs)    = x && and' xs
and' []          = True

prop_and :: [Bool] -> Bool
prop_and ls    = and ls == and' ls

check_and_result :: IO ()
check_and_result    = quickCheck prop_and

or' :: [Bool] -> Bool
or' (x : xs)    = x || or' xs
or' []          = False

prop_or :: [Bool] -> Bool
prop_or ls    = or ls == or' ls

check_or_result :: IO ()
check_or_result    = quickCheck prop_or

ins :: Integer -> [Integer] -> [Integer]
ins x []           = [x]
ins x (y : ys)
    | x <= y       = x : y : ys
    | otherwise    = y : ins x ys

iSort :: [Integer] -> [Integer]
iSort []          = []
iSort (x : xs)    = ins x (iSort xs)

elemNum :: Integer -> [Integer] -> Integer
elemNum _ []         = 0
elemNum target (x : xs)
    | target == x    = 1 + elemNum target xs
    | otherwise      = elemNum target xs

elemNum' :: Integer -> [Integer] -> Integer
elemNum' target storage    = toInteger (length [x | x <- storage, x == target])

unique :: [Integer] -> [Integer]
unique ls    = [x | x <- ls, elemNum x ls == 1]

unique' :: [Integer] -> [Integer]
unique' []          = []
unique' (x : xs)
    | elemNum x (x : xs) == 1    = x : unique' xs
    | otherwise                  = unique' (deleteAll x xs)
  where
    deleteAll :: Integer -> [Integer] -> [Integer]
    deleteAll target ls          = [_x | _x <- ls, _x /= target]

reverse' :: [a] -> [a]
reverse' []       = []
reverse' _list    = reverse' (drop 1 _list) ++ [head _list]

unzip' :: [(a, b)] -> ([a], [b])
unzip' []      = ([], [])
unzip' list    = (fst (head list) : fst (unzip' (tail list)),
                  snd (head list) : snd (unzip' (tail list)))

minAndMax :: [Integer] -> (Integer, Integer)
minAndMax ls    = (head (iSort ls), (iSort ls) !! (length ls - 1))

minAndMax' :: [Integer] -> (Integer, Integer)
minAndMax' []          = (0, 0)
minAndMax' [x]         = (x, x)
minAndMax' (x : xs)    = (if x < fst (minAndMax' xs) then x else fst (minAndMax' xs),
                          if x > snd (minAndMax' xs) then x else snd (minAndMax' xs))

isSorted :: [Integer] -> Bool
isSorted []          = True
isSorted [_]         = True
isSorted (x : xs)    = x <= (head xs) && isSorted xs

qSort :: [Integer] -> [Integer]
qSort []          = []
qSort (x : xs)    = qSort [lft | lft <- xs, lft <= x] ++ [x] ++ qSort [rht | rht <- xs, rht > x]

qSort_prop :: [Integer] -> Bool
qSort_prop    = isSorted . qSort

dicSort :: [(Integer, Integer)] -> [(Integer, Integer)]
dicSort []          = []
dicSort [x]         = [x]
dicSort (x : xs)    = insDic x (dicSort xs)
  where
    insDic :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)]
    insDic _x []                                    = [_x]
    insDic _x (ele : eles)
        | fst _x > fst ele                          = ele : insDic _x eles
        | fst _x < fst ele                          = _x : ele : eles
        | fst _x == fst ele && snd _x > snd ele     = ele : insDic _x eles
        | fst _x == fst ele && snd _x <= snd ele    = _x : ele : eles

dicSortIsSorted :: [(Integer, Integer)] -> Bool
dicSortIsSorted []                    = True
dicSortIsSorted [_]                   = True
dicSortIsSorted (x : xs)
    | (fst . head) xs - fst x > 0     = dicSortIsSorted xs
    | (fst . head) xs - fst x == 0    = (snd . head) xs - snd x >= 0 && dicSortIsSorted xs
    | (fst . head) xs - fst x < 0     = False

dicSortProp :: [(Integer, Integer)] -> Bool
dicSortProp    = dicSortIsSorted . dicSort

zip' :: [a] -> [b] -> [(a, b)]
zip' (x : xs) (y : ys)    = (x, y) : zip' xs ys
zip' _ _                  = []

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x : xs)
    | n > 0    = x : take' (n - 1) xs
take' _ _      = error "negative argument"

splitAt' :: Int -> [a] -> ([a], [a])
splitAt'    = undefined

drop' :: Int -> [a] -> [a]
drop' 0 _      = []
drop' _ []     = []
drop' n (_ : xs)
    | n > 0    = drop' (n - 1) xs
drop' _ _      = error "negative argument"

zip'' :: ([a], [b]) -> [(a, b)]
zip'' (xs, ys)    = zip xs ys

prop_zip''_unzip :: Eq a => Eq b => ([a], [b]) -> Property
prop_zip''_unzip (xs, ys)    = (length xs == length ys)
    ==> (xs, ys) == (unzip . zip'') (xs, ys)

another_prop_zip''_unzip :: Eq a => Eq b => ([a], [b]) -> Bool
another_prop_zip''_unzip (xs, ys)
    | length xs == length ys    = (xs, ys) == (unzip . zip'') (xs, ys)
    | otherwise                 = True

zip3' :: ([a], [b], [c]) -> [(a, b, c)]
zip3' (x : xs, y : ys, z : zs)    = (x, y, z) : zip3' (xs, ys, zs)
zip3' _                           = []

zip3'' :: ([a], [b], [c]) -> [(a, b, c)]
zip3'' (xs, ys, zs)    = [(xs !! i, ys !! i, zs !! i)
                          | i <- [0 .. (minThree (length xs - 1) (length ys - 1) (length zs - 1))]]
  where
    minThree :: Int -> Int -> Int -> Int
    minThree x y z
        | x >= y && y >= z    = z
        | y >= x              = x
        | otherwise           = y

subseq :: Eq a => [a] -> [a] -> Bool
subseq target base    = or [and [target !! (j - i) == base !! j | j <- [i .. i + length target - 1]]
                            | i <- [0 .. length base - length target]]

subseqRec :: Eq a => [a] -> [a] -> Bool
subseqRec [] _    = True
subseqRec _ []    = False
subseqRec (x : xs) (y : ys)
    | x /= y      = subseqRec (x : xs) ys
    | x == y      = subseqRec xs ys || subseqRec (x : xs) ys

sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _     = True
sublist _ []     = False
sublist (x : xs) (y : ys)
    | x == y     = sublist xs ys
    | x /= y     = sublist (x : xs) ys

whiteSpace :: String
whiteSpace = ['\n', ' ', '\t', '\0']

getWord :: String -> String
getWord []                   = []
getWord (x : xs)
    | x `elem` whiteSpace    = []
    | otherwise              = x : getWord xs

dropWord :: String -> String
dropWord []                  = []
dropWord (x : xs)
    | x `elem` whiteSpace    = x : xs
    | otherwise              = dropWord xs

dropHeadSpace :: String -> String
dropHeadSpace []             = []
dropHeadSpace (x : xs)
    | x `elem` whiteSpace    = dropHeadSpace xs
    | otherwise              = x : xs

type Word = String

splitWord :: String -> [Word]
splitWord str    = if str /= []
                   then (getWord . dropHeadSpace) str : splitWord ((dropWord . dropHeadSpace) str)
                   else []

lineWidth :: Int
lineWidth    = 80

type Line = [Word]

getLine :: Int -> [Word] -> Line
getLine _ []              = []
getLine len (wd : wds)
    | length wd <= len    = wd : getLine (len - (length wd + 1)) wds
    | otherwise           = []

dropLine :: Int -> [Word] -> [Word]
dropLine _ []             = []
dropLine len (wd : wds)
    | length wd <= len    = dropLine (len - (length wd + 1)) wds
    | otherwise           = wd : wds

splitLines :: [Word] -> [Line]
splitLines []    = []
splitLines ws    = getLine lineWidth ws : splitLines (dropLine lineWidth ws)

joinLine :: Line -> String
joinLine []            = []
joinLine [wd]          = wd
joinLine (wd : wds)    = wd ++ " " ++ joinLine wds

joinLines :: [Line] -> String
joinLines []           = []
joinLines (_line : _lines)    = joinLine _line ++ "\n" ++ joinLines _lines

joinLine2lineLen :: Line -> Int -> String
joinLine2lineLen [] _                  = []
joinLine2lineLen [wd] _                = wd
joinLine2lineLen (wd : wds) lineLen    = if lineLen < (length . joinLine) (wd : wds)
                                         then joinLine (wd : wds)
                                         else wd ++ blanks
                                              ++ joinLine2lineLen wds (lineLen - length wd - length blanks)
  where
    blank_total_len :: Line -> Int -> Int
    blank_total_len (wd' : wds') lineLen'    = lineLen' - sum [length _wd | _wd <- wd' : wds']
    blank_num :: Line -> Int
    blank_num (wd' : wds')    = length (wd' : wds') - 1
    const_blank_total_len :: Int
    const_blank_total_len    = blank_total_len (wd : wds) lineLen
    const_blank_num :: Int
    const_blank_num    = blank_num (wd : wds)
    blanks :: String
    blanks    = concat [" " | _ <- [1 .. div const_blank_total_len const_blank_num
                                     + if mod const_blank_total_len const_blank_num == 0 then 0 else 1]]

wc :: String -> (Int, Int, Int)
wc []      = (0, 0, 0)
wc str'    = (length str, (length . splitWord) str, (length . splitLines . splitWord) str)
  where
    str :: String
    str    = strSplit str'
    strSplit :: String -> String
    strSplit []    = []
    strSplit (c : cs)
        | c == '\n'    = ""
        | otherwise    = c : strSplit cs

isPalin :: String -> Bool
isPalin []     = True
isPalin [_]    = True
isPalin str    = head orig == last orig && isPalin (tail (init orig))
  where
    orig :: String
    orig    = [toLower c | c <- str, c /= ' ', c /= '\'']

subst :: String -> String -> String -> String
subst oldSub newSub st    = if subseqRec oldSub st
                            then replacer oldSub newSub st (subseqMarker_max oldSub st)
                            else st

-- here is a bug and I need to fix
-- check when the case is "change" "change"
subseqMarker :: String -> String -> [Int]
subseqMarker [] []        = [0]
subseqMarker _ []         = [-1]
subseqMarker [] remain    = [length remain]
subseqMarker (x : xs) (y : ys)
    | x /= y              = subseqMarker (x : xs) ys
    | x == y              = subseqMarker xs ys ++ subseqMarker (x : xs) ys

subseqMarker_max :: String -> String -> Int
subseqMarker_max xs ys    = maximum (subseqMarker xs ys)

replacer :: String -> String -> String -> Int -> String
replacer oldone newone base place    = fst (splitAt (length base - place - length oldone) base)
                                       ++ newone ++ snd (splitAt (length base - place) base)
