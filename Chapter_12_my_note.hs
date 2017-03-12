module Chapter_12_my_note where

import           Pictures (horse)

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
