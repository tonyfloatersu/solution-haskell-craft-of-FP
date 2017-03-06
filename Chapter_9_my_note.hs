module Chapter_9_my_note where

import           Pictures

frac :: Integer -> Integer
frac n
    | n == 0       = 1
    | otherwise    = n * frac (n - 1)

mult :: Integer -> Integer -> Integer
mult a b
    | a == 0 || b == 0    = 0
    | otherwise           = a * b
-- see that static finding, so check a first then check b

{-
solution 9.3
Base:
    Since flipH [] == flipV [] == []
    Thus:
        Left hand side == Right hand side

Ind:
    Since we can use the picline, and it goes like that
    pic ++ picline as a picture
    ---------------------------------------------------
    Left hand side:
        flipV (flipH (pic ++ picline))
                                                | according to the reverse on book
        == flipV (picline ++ reverse pic)
                                                | then based on definition on chapter 6
        == (reverse picline) ++ [reverse line | line <- (reverse pic)]

    Right hand side:
        flipH (flipV (pic ++ picline))
                                                | according to chapter 6
        == flipH ([reverse line | line <- pic] ++ (reverse picline))
                                                | according to reverse on book
        == (reverse picline) ++ reverse [reverse line | line <- pic]
        == (reverse picline) ++ [reverse line | line <- (reverse pic)]

    Left hand side == Right hand side
    Q.E.D

The rest two are easy.
Ind:
    flipV (flipV pic)
    == flipV [reverse line | line <- pic]
    == [reverse (reverse line) | line <- pic]
    == pic

    flipH (flipH pic)
    == reverse (reverse pic)
    == pic
-}

{-
solution 9.4
-}

{-
solution 9.5
-}

{-
solution 9.6
-}

{-
solution 9.7
-}

{-
solution 9.8
-}

{-
solution 9.9
-}

{-
solution 9.10
-}

{-
solution 9.11
-}

{-
solution 9.12
-}

{-
solution 9.13
-}

{-
solution 9.14
-}

{-
solution 9.15
-}
