module Frequency (frequency) where

frequency :: String -> [(Char, Int)]
frequency    = mergeSort frequencySort . mergeSort alphabetSort . numberThem

numberThem :: String -> [(Char, Int)]
numberThem    = map (\x -> (x, 1))

mergeSort :: (Ord a) => ([a] -> [a] -> [a]) -> [a] -> [a]
mergeSort merge xs
    | length xs < 2    = xs
    | otherwise        = merge (mergeSort merge firstHalf) (mergeSort merge secondHalf)
  where firstHalf      = take halflen xs
        secondHalf     = drop halflen xs
        halflen        = div (length xs) 2

alphabetSort :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
alphabetSort xs []    = xs
alphabetSort [] ys    = ys
alphabetSort ((xc, xi) : xs) ((yc, yi) : ys)
    | xc == yc        = (xc, xi + yi) : alphabetSort xs ys
    | xc < yc         = (xc, xi) : alphabetSort xs ((yc, yi) : ys)
    | otherwise       = (yc, yi) : alphabetSort ((xc, xi) : xs) ys

frequencySort :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
frequencySort xs []    = xs
frequencySort [] ys    = ys
frequencySort ((xc, xv) : xs) ((yc, yv) : ys)
    | xv == yv         = (if xc < yc
                          then [(xc, xv), (yc, yv)]
                          else [(yc, yv), (xc, xv)]) ++ frequencySort xs ys
    | xv < yv          = (xc, xv) : frequencySort xs ((yc, yv) : ys)
    | otherwise        = (yc, yv) : frequencySort ((xc, xv) : xs) ys
