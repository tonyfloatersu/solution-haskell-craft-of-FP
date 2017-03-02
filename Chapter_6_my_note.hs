module Chapter_6_my_note where

import           Prelude         hiding (fst, id, snd, lookup)
import           Test.QuickCheck hiding (scale, collect)

horse :: [String]
horse = [".......##...",
         ".....##..#..",
         "...##.....#.",
         "..#.......#.",
         "..#...#...#.",
         "..#...###.#.",
         ".#....#..##.",
         "..#...#.....",
         "...#...#....",
         "....#..#....",
         ".....#.#....",
         "......##...."]

id :: a -> a
id x    = x

fst :: (a, b) -> a
fst (x, _)    = x

snd :: (a, b) -> b
snd (_, y)    = y

mystery :: (Bool, a) -> Char
mystery (x, _)    = if x
                    then 'c'
                    else 'd'

sing :: a -> [a]
sing x    = [x]

shift :: ((a, b), c) -> (a, (b, c))
shift ((x, y), z)    = (x, (y, z))

type Picture = [String]

flipH :: Picture -> Picture
flipH    = reverse

above :: Picture -> Picture -> Picture
above    = (++)

flipV :: Picture -> Picture
flipV _pic   = [reverse line | line <- _pic]

beside :: Picture -> Picture -> Picture
beside pic1 pic2    = [line1 ++ line2 | (line1, line2) <- zip pic1 pic2]

invertChar :: Char -> Char
invertChar ch    = if ch == '.'
                   then '#'
                   else '.'

invertLine :: String -> String
invertLine line    = [invertChar ch | ch <- line]

invertColour :: Picture -> Picture
invertColour pic    = [invertLine line | line <- pic]

invertColour' :: Picture -> Picture
invertColour' pic    = [[invertChar ch | ch <- line] | line <- pic]

prop_AboveFlipV :: Picture -> Picture -> Bool
prop_AboveFlipV pic1 pic2    = flipV (above pic1 pic2) == above (flipV pic1) (flipV pic2)

tester1 :: IO ()
tester1    = quickCheck prop_AboveFlipV

prop_AboveFlipH :: Picture -> Picture -> Bool
prop_AboveFlipH pic1 pic2    = flipH (above pic1 pic2) == above (flipH pic2) (flipH pic1)

tester2 :: IO ()
tester2    = quickCheck prop_AboveFlipH

superimposeChar :: Char -> Char -> Char
superimposeChar ch1 ch2    = if ch1 == ch2 && ch1 == '.'
                             then '.'
                             else '#'

superimposeString :: String -> String -> String
superimposeString line1 line2    = [superimposeChar (fst _pair) (snd _pair) | _pair <- zip line1 line2]

superimpose :: Picture -> Picture -> Picture
superimpose pic1 pic2    = [superimposeString (fst _p) (snd _p) | _p <- zip pic1 pic2]

printPicture :: Picture -> IO ()
printPicture pic    = putStr (concat [line ++ "\n" | line <- pic])

rotatePos90 :: Picture -> Picture
rotatePos90 pic    = flipV [concat [[line !! i] | line <- pic] | i <- [0 .. length (head pic) - 1]]

rotateNeg90 :: Picture -> Picture
rotateNeg90 pic    = [concat [[line !! i] | line <- pic] | i <- [length (head pic) - 1, length (head pic) - 2 .. 0]]

scale :: Picture -> Integer -> Picture
scale _pic _num
    | _num <= 0     = [""]
    | otherwise     = rotateNeg90 (orig (rotatePos90 (orig _pic _num)) _num)
  where
    orig :: Picture -> Integer -> Picture
    orig pic num    = [concat [concat [[ch] | _ <- [1 .. num]] | ch <- line] | line <- pic]

prop_BesideFlipV :: Picture -> Picture -> Bool
prop_BesideFlipV pic1 pic2    = flipV (beside pic1 pic2) == beside (flipV pic2) (flipV pic1)

tester3 :: IO ()
tester3    = quickCheck prop_BesideFlipV

prop_BesideFlipH :: Picture -> Picture -> Bool
prop_BesideFlipH pic1 pic2
    | length pic1 == length pic2    = flipH (beside pic1 pic2) == beside (flipH pic1) (flipH pic2)
    | otherwise                     = True

tester4 :: IO ()
tester4    = quickCheck prop_BesideFlipH

isRectangular :: Picture -> Bool
isRectangular pic    = False `notElem` [maxLen pic == length line | line <- pic]
  where
    maxLen :: Picture -> Int
    maxLen _pic    = maximum [length line | line <- _pic]

fixToRectangular :: Picture -> Picture
fixToRectangular pic    = if isRectangular pic
                          then pic
                          else [line ++ concat ["." | _ <- [0 .. maxLen pic - length line - 1]] | line <- pic]
  where
    maxLen :: Picture -> Int
    maxLen _pic    = maximum [length line | line <- _pic]

type Position = (Int, Int)
type Image = (Picture, Position)

makeImage :: Picture -> Position -> Image
makeImage pic pos    = (pic, pos)

changePosition :: Image -> Position -> Image
changePosition  (pic, _) _pos    = (pic, _pos)

printImage :: Image -> IO ()
printImage (pic, (_x, _y))
    | _x < 0 || _y < 0    = putStr "position is not in the first part.\n"
    | otherwise           = printPicture ([concat ["x" | _ <- [0 .. _x - 1]] ++ line | line <- pic]
                            ++ [concat ["x" | _ <- [0 .. _x + maxLen pic - 1]] | _ <- [0 .. _y - 1]])
  where
    maxLen :: Picture -> Int
    maxLen _pic    = maximum [length line | line <- _pic]

aboveImg :: Image -> Image -> Image
aboveImg (pic1, (_x1, _y1)) (pic2, (_x2, _y2))    = (above pic1 pic2, (_x1, _y1))

type Name        = String
type Price       = Int
type BarCode     = Int
type Database    = [(BarCode, Name, Price)]

codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers", 121)
            , (5643, "Nappies", 1010)
            , (3814, "Orange Jelly", 56)
            , (1111, "Hula Loops", 21)
            , (1112, "Big Hula Loops", 122)
            , (1234, "Dry Sherry", 540) ]

type TillType    = [BarCode]
type BillType    = [(Name, Price)]

formatPence :: Price -> String
formatPence _price    = show (div _price 100) ++ "." ++ if mod _price 100 >= 10
                                                        then show (mod _price 100)
                                                        else "0" ++ show (mod _price 100)

formatLine :: (Name, Price) -> String
formatLine (_n, _p)    = _n ++ concat ["." | _ <- [1 .. lineLength - length _n - length (formatPence _p)]]
                         ++ formatPence _p ++ "\n"
  where
    lineLength :: Int
    lineLength    = 30

formatLines :: [(Name, Price)] -> String
formatLines list    = concat [formatLine line | line <- list]

makeTotal :: BillType -> Price
makeTotal _tp    = (sum . snd . unzip) _tp - makeDiscount _tp

formatTotal :: Price -> String
formatTotal num    = "\nTotal" ++ concat ["." | _ <- [1 .. lineLength - (length . formatPence) num - 5]]
                     ++ formatPence num
  where
    lineLength :: Int
    lineLength    = 30

look :: Database -> BarCode -> (Name, Price)
look _db _bc = if placelist _db _bc /= []
               then (mid3 (_db !! head (placelist _db _bc)), lst3 (_db !! head (placelist _db _bc)))
               else ("Unknown Item", 0)
  where
    placelist :: Database -> BarCode -> [Int]
    placelist _db' _bc'    = [i | i <- [0 .. length _db' - 1], fst3 (_db' !! i) == _bc']
    fst3 :: (a, b, c) -> a
    fst3 (_t, _, _)    = _t
    mid3 :: (a, b, c) -> b
    mid3 (_, _t, _)    = _t
    lst3 :: (a, b, c) -> c
    lst3 (_, _, _t)    = _t

lookup :: BarCode -> (Name, Price)
lookup    = look database
  where
    database :: Database
    database    = codeIndex

makeBill :: TillType -> BillType
makeBill _tp    = [lookup bc | bc <- _tp]

makeDiscount :: BillType -> Price
makeDiscount _tp    = 100 * div (length [i | i <- _tp, fst i == dsctgt]) satisfytgt
  where
    dsctgt :: String
    dsctgt    = "Dry Sherry"
    satisfytgt :: Int
    satisfytgt    = 2

formatDiscount :: Price -> String
formatDiscount _pr    = "\nDiscount" ++ concat ["." | _ <- [1 .. lineLength - (length . formatPence) _pr - 8]]
                        ++ formatPence _pr ++ "\n"
  where
    lineLength :: Int
    lineLength    = 30

formatBill :: BillType -> String
formatBill bill    = formatLines bill ++ (formatDiscount . makeDiscount) bill
                     ++ (formatTotal . makeTotal) bill ++ "\n"

formatTill :: TillType -> String
formatTill    = formatBill . makeBill

data Suit = Spade | Heart | Diamond | Club
            deriving (Show, Eq)

data Value = One | Two | Three | Four | Five | Six |
             Seven | Eight | Nine | Ten | Jack | Queen |
             King | Ace
             deriving (Show, Eq, Ord)

data Card = CardConstruct Suit Value
            deriving (Show, Eq)

type Deck = [Card]

data Player = East | South | West | North
              deriving (Show, Eq, Read)

data Action = ActConstruct Card Player
              deriving (Show, Eq)

type Trick = [Action]

actionCard :: Action -> Card
actionCard (ActConstruct _card _)    = _card

actionPlayer :: Action -> Player
actionPlayer (ActConstruct _ _player)    = _player

cardSuit :: Card -> Suit
cardSuit (CardConstruct _suit _)    = _suit

cardValue :: Card -> Value
cardValue (CardConstruct _ _value)    = _value

maxAndIndex :: [Value] -> (Value, Int)
maxAndIndex _ls    = (maximum _ls, head [index | index <- [length _ls - 1, length _ls - 2 .. 0],
                                                 _ls !! index == maximum _ls])

winNT :: Trick -> Player
winNT _list    = actionPlayer (_list !! winnerIndex _list (cardSuit $ actionCard $ winSuitAction _list))
  where
    winSuitAction :: Trick -> Action
    winSuitAction    = head
    winSatisList :: Trick -> Suit -> Trick
    winSatisList _ls _suit    = [act | act <- _ls, (cardSuit . actionCard) act == _suit]
    winSatisValue :: Trick -> Suit -> [Value]
    winSatisValue _ls _suit    = [(cardValue . actionCard) act
                                  | act <- winSatisList _list ((cardSuit . actionCard . winSuitAction) _list)]
    winnerIndex :: Trick -> Suit -> Int
    winnerIndex _ls _suit    = snd (maxAndIndex (winSatisValue _ls _suit))

winT :: Trick -> Suit -> Player
winT _list _suit    = head [actionPlayer act | act <- _list, (cardSuit . actionCard) act == _suit]

data Hand = HandConstruct [Card] Player
            deriving (Show, Eq)

handPlayer :: Hand -> Player
handPlayer (HandConstruct _ _player)    = _player

handCards :: Hand -> [Card]
handCards (HandConstruct _cards _)    = _cards

type Hands = [Hand]

checkPlay :: Hands -> Trick -> Bool
checkPlay _hands _trick    = allInside _hands _trick && seqTotalCheck _trick _hands
  where
    collect :: Trick -> Player -> Hand
    collect _tr _player    = HandConstruct [actionCard act | act <- _tr, actionPlayer act == _player] _player
    allCollect :: Trick -> Hands -> Hands
    allCollect trick _hands'   = [collect trick i | i <- [handPlayer i | i <- _hands']]
    valueTable :: [Value]
    valueTable    = [One, Two, Three, Four, Five, Six,
                     Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
    countValue :: Value -> Hand -> Int
    countValue val _hand    = length [i | i <- handCards _hand, cardValue i == val]
    countValueList :: Hand -> [Int]
    countValueList _hand    = [countValue val _hand | val <- valueTable]
    compareOrigColl :: Hand -> Hand -> Bool
    compareOrigColl orig coll    = and [(countValueList orig) !! i >= (countValueList coll) !! i | i <- [0 .. 13]]
    hashTable :: Hands -> Hands -> Bool
    hashTable _hands _collect    = and [compareOrigColl (_hands !! i) (_collect !! i)
                                        | i <- [0 .. length _hands - 1]]
    allInside :: Hands -> Trick -> Bool
    allInside _hands _trick    = hashTable _hands (allCollect _trick _hands)
    seqCheck :: Action -> Action -> Hands -> Bool
    seqCheck _prev _next _store    = if suitSame _prev _next
                                     then True
                                     else not (findSuit (findHand _store (actionPlayer _next))
                                                        ((cardSuit . actionCard) _prev))
    -- if same then True to continue, not True then check whether it has the prev suit, has then False
    findHand :: Hands -> Player -> Hand
    findHand _stor _play    = head [hand | hand <- _stor, handPlayer hand == _play]
    suitSame :: Action -> Action -> Bool
    suitSame _prev _next    = (cardSuit . actionCard) _prev == (cardSuit . actionCard) _next
    findSuit :: Hand -> Suit -> Bool
    findSuit _hand _suit    = and [cardSuit card == _suit | card <- handCards _hand]
    seqTotalCheck :: Trick -> Hands -> Bool
    seqTotalCheck _trick _hands    = and [seqCheck (_trick !! i) (_trick !! (i - 1)) _hands
                                          | i <- [1 .. length _trick - 1]]
