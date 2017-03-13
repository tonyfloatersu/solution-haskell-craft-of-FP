module Chapter_12_my_note where

import           Pictures (horse)
import           Test.QuickCheck
import           Chapter_8_my_note (randInt)

type Picture = [String]

_flipH :: Picture -> Picture
_flipH    = reverse

_flipV :: Picture -> Picture
_flipV    = map reverse

_above :: Picture -> Picture -> Picture
_above    = (++)

_beside :: Picture -> Picture -> Picture
_beside    = zipWith (++)

_invertColor :: Picture -> Picture
_invertColor    = map (map (\ch -> if ch == '.' then '#' else '.'))

_superImpose :: Picture -> Picture -> Picture
_superImpose    = zipWith (zipWith (\c1 c2 -> if c1 == c2 then c1 else '#'))

_printPicture :: Picture -> IO ()
_printPicture    = putStr . concat . map (++ "\n")

_chessBoard :: Int -> Picture
_chessBoard size    = zipWith ($)
    ((fst . splitAt size . concat . replicate size) [fst, reverse . snd])
    ((replicate size . splitAt size . concat . replicate size) "#.")

type Bicture = [[Bool]]

_binvertColor :: Bicture -> Bicture
_binvertColor    = map (map not)

_bsuperImpose :: Bicture -> Bicture -> Bicture
_bsuperImpose    = zipWith (zipWith (\b1 b2 -> (b1 == b2) && b2))

_bprintPicture :: Bicture -> IO ()
_bprintPicture    = putStr . concat . map ((++ "\n") . map (\b -> if b
                                                                  then '#'
                                                                  else '.'))

createPic :: Int -> Int -> Picture
createPic h w    = replicate h ((concat . replicate w) ".")

addPt :: (Int, Int) -> Picture -> Picture
addPt (h, w) pic    = take h pic ++ [take w (pic !! h) ++ "#" ++ drop (w + 1) (pic !! h)]
                      ++ drop (h + 1) pic

addPts :: [(Int, Int)] -> Picture -> Picture
addPts pts pic    = foldr addPt pic pts

qualify :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
qualify ptl sz    = filter (\(y, x) -> (fst sz >= y) && (x >= 0)
                                       && (snd sz >= x) && (y >= 0)) ptl

makePicture :: Int -> Int -> [(Int, Int)] -> Picture
makePicture w h pts    = addPts (qualify pts (h, w)) (createPic h w)

picSize :: Picture -> (Int, Int)
picSize pic    = (length (head pic), length pic)

pointsCreate :: Int -> Int -> [(Int, Int)]
pointsCreate w h    = zip
    ((concat . (map (concat . replicate w))) (map (\x -> [x]) [0 .. (h - 1)]))
    ((concat . replicate h) [0 .. (w - 1)])

isBlack :: (Int, Int) -> Picture -> Bool
isBlack (h, w) pic    = (pic !! h) !! w == '#'

blackList :: Picture -> [(Int, Int)]
blackList pic    = filter (`isBlack` pic) (uncurry pointsCreate (picSize pic))

picToRep :: Picture -> (Int, Int, [(Int, Int)])
picToRep pic    = ((fst . picSize) pic, (snd . picSize) pic, blackList pic)

type Rep = (Int, Int, [(Int, Int)])

repRotate :: Rep -> Rep
repRotate (w, h, locs)    = (h, w, map (\(y, x) -> (x, h - 1 - y)) locs)

data Move = Rock | Paper | Scissors
            deriving (Show, Eq)

type Strategy = [Move] -> Move

alternate :: Strategy -> Strategy -> [Move] -> Move
alternate str1 str2 moves    = map ($ moves) [str1, str2] !! (length moves `mod` 2)

sToss :: Strategy -> Strategy -> Strategy
sToss str1 str2    = [str1, str2] !! (fromInteger (randInt 2) :: Int)

sTossList :: [Strategy] -> Strategy
sTossList []    = \moves -> head moves
sTossList ls    = ls !! (fromInteger (randInt ((toInteger . length) ls)) :: Int)

alternativeList :: [Strategy] -> [Move] -> Strategy
alternativeList strs moves    = strs !! (length moves `mod` length strs)
