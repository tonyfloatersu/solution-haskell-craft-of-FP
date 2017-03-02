module Chapter_4_my_note where

import           Chapter4        hiding (between, blackChess, blackWhite,
                                  maxThree, whiteBlack, whiteChess)
import           PicturesSVG     hiding (test2)
import           Test.HUnit
import           Test.QuickCheck hiding (Result)


maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = max (max x y) z

type MyNum2 = Int

middleNumber :: MyNum2 -> MyNum2 -> MyNum2 -> MyNum2
middleNumber x y z
    | between y x z    = x
    | between x y z    = y
    | otherwise        = z

between :: MyNum2 -> MyNum2 -> MyNum2 -> Bool
between x y z
    | (x - y) * (y - z) >= 0    = True
    | otherwise                 = False

equal :: Integer -> Integer -> Integer
equal x y
    | x == y       = 1
    | otherwise    = 0

howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual x y z
    | equal x y + equal y z + equal x z == 1    = 1
    | otherwise                                 = equal x y + equal y z + equal x z

squareSum :: Integer -> Integer -> Integer
squareSum n m
    = sq n + sq m
  where
    sq p = p * p

triAreaMy :: Float -> Float -> Float -> Float
triAreaMy n m p
    | possible n m p    = sqrt ( s * (s - n) * (s - m) * (s - p) )
    | otherwise         = -1.0
  where
    s = (n + m + p) / 2
    possible :: Float -> Float -> Float -> Bool
    possible a b c = a + b > c
                     && b + c > a
                     && a + c > b

isOdd' :: Integer -> Bool
isOdd' n
    | n <= 0       = False
    | otherwise    = isEven' (n - 1)

isEven' :: Integer -> Bool
isEven' n
    | n < 0        = False
    | n == 0       = True
    | otherwise    = isOdd' (n - 1)

fourPics1' :: Picture -> Picture
fourPics1' pic =
    left `beside` right
  where
    left     = pic `above` invertColour pic
    right    = invertColour $ flipV pic `above` flipV pic

fourPics2' :: Picture -> Picture
fourPics2' pic =
    left `beside` right
  where
    left     = pic `above` invertColour pic
    right    = invertColour $ flipV left

data Result = Win | Draw | Lose
              deriving (Show, Eq)

outcome :: Move -> Move -> Result
outcome mov1 mov2
    | lose mov1 == mov2     = Win
    | mov1 == mov2          = Draw
    | otherwise             = Lose

propertyOutcome :: Move -> Bool
propertyOutcome mov1 = lose mov1 /= beat mov1

data Temp = Cold | Hot
            deriving (Eq, Show, Ord)

data Season = Spring | Summer | Autumn | Winter
              deriving (Eq, Show, Ord)

temperatureUGB :: Season -> Temp
temperatureUGB Spring = Cold
temperatureUGB Summer = Hot
temperatureUGB Autumn = Cold
temperatureUGB Winter = Cold

data Month = January | February | March | April | May | June |
     July | August | September | October | November | December
     deriving (Eq, Show, Ord)

monthToSeason :: Month -> Season
monthToSeason mon
    | mon <= March        = Spring
    | mon <= June         = Summer
    | mon <= September    = Autumn
    | otherwise           = Winter

facMy :: Integer -> Integer
facMy n
    | n == 0       = 1
    | n > 0        = facMy (n - 1) * n
    | otherwise    = error "fac only for integers greater than 0"

rangeProd :: Integer -> Integer -> Integer
rangeProd n m
    | n > m && m > 0     = rangeProd m n
    | n < m && n > 0     = n * rangeProd (n + 1) m
    | n == m && n > 0    = m
    | otherwise          = error "smaller than 0 not expected"

facVerRange :: Integer -> Integer
facVerRange n
    | n == 0       = 1
    | n < 0        = error "smaller than 0 not expected"
    | otherwise    = rangeProd 1 n

multiUseAddRecur :: Integer -> Integer -> Integer
multiUseAddRecur n m
    | n > 0        = multiUseAddRecur (n - 1) m + m
    | n == 0       = 0
    | otherwise    = multiUseAddRecur ((-1) * n) ((-1) * m)

intSqrtLowBound :: Integer -> Integer
intSqrtLowBound n = tester n n
  where
    tester m p
        | p < 0        = -1
        | m * m > p    = tester (m - 1) p
        | otherwise    = m

testf :: Integer -> Integer
testf 0 = 0
testf 1 = 44
testf 2 = 17
testf _ = 0

testingStructure :: (Integer -> Integer) -> Integer -> Integer -> Integer
testingStructure tf bound1 bound2
    | bound1 > bound2     = testingStructure tf bound2 bound1
    | bound1 == bound2    = tf bound1
    | otherwise           = max (tf bound1) (testingStructure tf (bound1 + 1) bound2)

boolTestingStructure :: (Integer -> Integer) -> Integer -> Integer -> Bool
boolTestingStructure tf bd1 bd2
    | zeroCounter tf bd1 bd2 > 1    = True
    | otherwise                     = False
  where
    zeroCounter :: (Integer -> Integer) -> Integer -> Integer -> Integer
    zeroCounter tf' bd1' bd2'
        | bd1' > bd2'     = zeroCounter tf' bd2' bd1'
        | bd1' == bd2'    = judger tf' bd1'
        | otherwise       = judger tf' bd1' + zeroCounter tf' (bd1' + 1) bd2'
      where
        judger :: (Integer -> Integer) -> Integer -> Integer
        judger tempf bd
            | tempf bd == 0      = 1
            | otherwise          = 0

regions' :: Integer -> Integer
regions' num = sumFun id num + 1

plains :: Integer -> Integer
plains num
    | num > 3      = num
    | otherwise    = -1

blackWhite :: Integer -> Picture
blackWhite num
    | num <= 1     = black
    | otherwise    = black `beside` whiteBlack (num - 1)

whiteBlack :: Integer -> Picture
whiteBlack num
    | num <= 1     = white
    | otherwise    = white `beside` blackWhite (num - 1)

blackChess :: Integer -> Integer -> Picture
blackChess n m
    | n <= 1       = blackWhite m
    | otherwise    = blackWhite m `above` whiteChess (n - 1) m

whiteChess :: Integer -> Integer -> Picture
whiteChess n m
    | n <= 1       = whiteBlack m
    | otherwise    = whiteBlack m `above` blackChess (n - 1) m

column :: Picture -> Integer -> Picture
column p n
    | n <= 1       = p
    | otherwise    = p `above` column p (n - 1)

columnBoard1 :: Integer -> Picture
columnBoard1 num = chunk num num
  where
    chunk :: Integer -> Integer -> Picture
    chunk num1 num2
        | num1 == 1 && num2 == 1    = black
        | num1 <= 1                 = layer num1 num2
        | otherwise                 = layer num1 num2 `above` chunk (num1 - 1) num2
      where
        layer :: Integer -> Integer -> Picture
        layer num1' num2'
            | num1' == 1        = whiteLine (num2' - 1) `beside` black
            | num1' == num2'    = black `beside` whiteLine (num2' - 1)
            | otherwise         = whiteLine (num2' - num1') `beside` black
                                  `beside` whiteLine (num1' - 1)
          where
            whiteLine :: Integer -> Picture
            whiteLine num'
                | num' <= 1    = white
                | otherwise    = whiteLine (num' - 1) `beside` white

columnBoard2 :: Integer -> Picture
columnBoard2 num = chunk num num
  where
    chunk :: Integer -> Integer -> Picture
    chunk num1 num2
        | num1 == 1 && num2 == 1    = black
        | num1 <= 1                 = layer num1 num2
        | otherwise                 = layer num1 num2 `above` chunk (num1 - 1) num2
      where
        layer :: Integer -> Integer -> Picture
        layer num1' num2'
            | num1' == 1        = black `beside` whiteLine (num2' - 1)
            | num1' == num2'    = whiteLine (num2' - 1) `beside` black
            | otherwise         = whiteLine (num1' - 1) `beside` black
                                  `beside` whiteLine (num2' - num1')
          where
            whiteLine :: Integer -> Picture
            whiteLine num'
                | num' <= 1    = white
                | otherwise    = whiteLine (num' - 1) `beside` white

crossBoard :: Integer -> Picture
crossBoard num = chunk num num
  where
    chunk num1 num2
        | num1 == 1 && num2 == 1    = black
        | num1 == 2 && num2 == 2    = (black `beside` black) `above` (black `beside` black)
        | num1 <= 1                 = layer num1 num2
        | otherwise                 = layer num1 num2 `above` chunk (num1 - 1) num2
      where
        layer :: Integer -> Integer -> Picture
        layer num1' num2'
            | num1' == 1
              || num1' == num2'           = black `beside` whiteLine (num2' - 2) `beside` black
            | (num1' - 1) * 2 == num2'
              || num2' == num1' * 2       = whiteLine (divide num2' 2 - 1) `beside` black
                                            `beside` black `beside` whiteLine (divide num2' 2 - 1)
            | 2 * num1'- 1 == num2'       = whiteLine (divide num2' 2) `beside` black
                                            `beside` whiteLine (divide num2' 2)
            | 2 * num1' < num2'           = whiteLine (num1' - 1) `beside` black
                                            `beside` whiteLine (num2' - num1' * 2)
                                            `beside` black `beside` whiteLine (num1' - 1)
            | otherwise                   = whiteLine (num2' - num1') `beside` black
                                            `beside` whiteLine (2 * num1' - num2' - 2)
                                            `beside` black `beside` whiteLine (num2' - num1')
          where
            whiteLine :: Integer -> Picture
            whiteLine num'
                | num' <= 1    = white
                | otherwise    = whiteLine (num' - 1) `beside` white

chessBoard :: Integer -> Picture
chessBoard num
    | num <= 1                = black
    | remainder num 2 == 0    = chessBoard (num - 1) `above` whiteBlack (num - 1)
                                `beside` whiteBlackV num
    | otherwise               = chessBoard (num - 1) `above` blackWhite (num - 1)
                                `beside` blackWhiteV num
  where
    whiteBlackV :: Integer -> Picture
    whiteBlackV num'
        | num' <= 1    = white
        | otherwise    = white `above` blackWhiteV (num' - 1)
    blackWhiteV :: Integer -> Picture
    blackWhiteV num''
        | num'' <= 1    = black
        | otherwise     = black `above` whiteBlackV (num'' - 1)

testMax :: Test
testMax = TestList [TestCase (assertEqual "for: maxThree 6 4 1" 6 (maxThree 6 4 1)),
                    TestCase (assertEqual "for: maxThree 6 6 6" 6 (maxThree 6 6 6)),
                    TestCase (assertEqual "for: maxThree 2 6 6" 6 (maxThree 2 6 6)),
                    TestCase (assertEqual "for: maxThree 2 2 6" 6 (maxThree 2 2 6)),
                    TestCase (assertEqual "for: maxThree 6 6 2" 6 (maxThree 6 6 2))
                   ]

allEqual :: Integer -> Integer -> Integer -> Bool
allEqual n m p
    | n == m && n == p    = True
    | otherwise           = False

testAllEqual :: Test
testAllEqual = TestList [TestCase (assertEqual "for: allEqual 1 2 3" False (allEqual 1 2 3)),
                         TestCase (assertEqual "for: allEqual 2 2 2" True (allEqual 2 2 2)),
                         TestCase (assertEqual "for: allEqual 2 3 3" False (allEqual 2 3 3))
                         ]

allDifferent :: Integer -> Integer -> Integer -> Bool
allDifferent n m p
    | n /= m && m /= p && p /= n    = True
    | otherwise                     = False

testAllDifferent :: Test
testAllDifferent = TestList [TestCase (assertEqual "for: allDifferent 1 2 2" False (allDifferent 1 2 2))
                            , TestCase (assertEqual "for: allDifferent 1 1 1" False (allDifferent 1 1 1))
                            , TestCase (assertEqual "for: allDifferent 1 2 3" True (allDifferent 1 2 3))]
