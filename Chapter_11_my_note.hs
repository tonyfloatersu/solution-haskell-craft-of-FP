module Chapter_11_my_note where

import           Test.QuickCheck
import           Data.Function

infixl 9 >.>

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)

g >.> f = f . g

composeList :: [a -> a] -> (a -> a)
composeList    = foldr (.) id

mapFuns :: [a -> b] -> a -> [b]
mapFuns fs val    = map (\f -> f val) fs

comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2 f g    = \x y -> g (f x) (f y)

isBlank :: Char -> Bool
isBlank    = \ch -> not (ch `elem` "\t\n")

total :: (Integer -> Integer) -> (Integer -> Integer)
total f    = \n -> foldr ((+) . f) 0 [0 .. n]

total2 :: (Integer -> Integer) -> (Integer -> Integer)
total2 f    = sum . map f . \n -> [0 .. n]

invseq :: (a -> b -> c) -> (b -> a -> c)
invseq f    = \v2 v1 -> f v1 v2

mapFuns2 :: [a -> b] -> a -> [b]
mapFuns2 fs val    = map ($ val) fs

_curry :: ((a, b) -> c) -> a -> b -> c
_curry f    = \x y -> f (x, y)

_uncurry :: (a -> b -> c) -> (a, b) -> c
_uncurry f    = \(x, y) -> f x y

_anotherCurry :: ((a, b) -> c) -> a -> b -> c
_anotherCurry f x y    = f (x, y)

_anotherUncurry :: (a -> b -> c) -> (a, b) -> c
_anotherUncurry f (x, y)    = f x y

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z    = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z)    = f x y z

curryList :: ([a] -> d) -> a -> [a] -> d
curryList f val ls    = f (val : ls)

uncurryList :: (a -> [a] -> d) -> [a] -> d
uncurryList f ls    = f (head ls) (tail ls)

prop_uncurryZip_unzip :: Eq a => Eq b => ([a], [b]) -> Property
prop_uncurryZip_unzip tester@ (lsa, lsb)    =
    (length lsa == length lsb) ==>
    (unzip . uncurry zip) tester == tester

_prop_uncurryZip_unzip :: Eq a => Eq b => ([a], [b]) -> Bool
_prop_uncurryZip_unzip tester@ (lsa, lsb)    = if length lsa == length lsb
                                               then (unzip . uncurry zip) tester == tester
                                               else True

twice :: (a -> a) -> (a -> a)
twice f    = f . f

_succ :: Integer -> Integer
_succ a    = a + 1

iter :: Integer -> (a -> a) -> (a -> a)
iter n f
    | n > 0        = f . iter (n - 1) f
    | otherwise    = id

double :: Integer -> Integer
double    = (* 2)

_sq :: Integer -> Integer
_sq d    = d * d

_iter :: Integer -> (a -> a) -> (a -> a)
_iter n f    = foldr (.) id (replicate (fromInteger n :: Int) f)

__iter :: Integer -> (a -> a) -> (a -> a)
__iter n f    = foldr (.) id (listcreate n f)

listcreate :: Integer -> a -> [a]
listcreate    = fix (\f n p -> if n > 0
                               then p : f (n - 1) p
                               else [])

_iter_ :: Integer -> (a -> a) -> (a -> a)
_iter_ n f    = foldr (.) id [f | _ <- [1 .. n]]

getEvens :: [Integer] -> [Integer]
getEvens    = filter ((== 0) . (`mod` 2))

mapFuns3 :: [a -> b] -> a -> [b]
mapFuns3 fs val    = map (applyVal val) fs
  where
    applyVal :: a -> (a -> b) -> b
    applyVal val' f    = f val'

slope :: (Float -> Float) -> (Float -> Float)
slope f    = \x -> (f (x + 1.0e-5) - f (x - 1.0e-5)) / 2.0e-5

integrate :: (Float -> Float) -> (Float -> Float -> Float)
integrate f x y    = sum $ map ((* 1.0e-5) . f) [x, x + 1.0e-5 .. y]
