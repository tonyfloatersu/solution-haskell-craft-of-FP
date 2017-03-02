module Chapter3_my_note where

import           Chapter3        hiding (isDigit)
import           Prelude         hiding (min)
import           Test.QuickCheck

mystery :: Integer -> Integer -> Integer -> Bool
mystery m n p = not((m == n) && (n == p))

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = (m /= n) && (n /= p) && (p /= m)

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual m n p q = threeEqual m n p && (m == q)

prop_fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
prop_fourEqual m n p q = (threeEqual m n p && (p == q)) == fourEqual m n p q

min :: Int -> Int -> Int
min m n
    | m > n        = n
    | otherwise    = m

min' :: Int -> Int -> Int
min' m n = if m < n then m else n

prop_compareMin :: Int -> Int -> Bool
prop_compareMin m n = min m n == min' m n

minThree :: Int -> Int -> Int -> Int
minThree m n p
    | (m < n) && (m < p)    = m
    | p < n                 = p
    | otherwise             = n

isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')

charToEnum :: Char -> Int
charToEnum ch
    | isDigit ch     = fromEnum ch - fromEnum '0'
    | otherwise      = -1

onThreeLines :: String -> String -> String -> String
onThreeLines m n p = m ++ "\n" ++ n ++ "\n" ++ p ++ "\n"

putString3 :: String -> String -> String -> IO()
putString3 m n p = putStr (onThreeLines m n p)

-- https://wiki.haskell.org/Roman_numerals
-- not so good at it now...

averageThree :: Integer -> Integer -> Integer -> Float
averageThree m n p = fromInteger( m + n + p ) / 3

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage m n p = (if fromInteger m > averageThree m n p then 1 else 0)
                            + (if fromInteger n > averageThree m n p then 1 else 0)
                            + (if fromInteger p > averageThree m n p then 1 else 0)

numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots m n p
    | n * n - 4 * m * p > 0     = 2
    | n * n - 4 * m * p == 0    = 1
    | otherwise                 = 0

numberRoots :: Float -> Float -> Float -> Integer
numberRoots m n p
    | m /= 0       = numberNDroots m n p
    | n /= 0       = 1
    | p /= 0       = 0
    | otherwise    = 3

smallerRoot :: Float -> Float -> Float -> Float
smallerRoot m n p
    | numberRoots m n p == 0
      || numberRoots m n p == 3    = 0
    | otherwise                    = (negate n - sqrt(n * n - 4 * m * p)) / (2 * m)

largerRoot :: Float -> Float -> Float -> Float
largerRoot m n p
    | numberRoots m n p == 0
      || numberRoots m n p == 3    = 0
    | otherwise                    = (negate n + sqrt(n * n - 4 * m * p)) / (2 * m)
