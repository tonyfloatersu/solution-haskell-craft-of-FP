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
flipV (above pic1 pic2) == above (flipV pic2) (flipV pic1)
Base:
    flipV (above [] pic2) == flipV pic2 == [reverse line | line <- pic2]
    above (flipV pic2) [] == flipV pic2 == [reverse line | line <- pic2]
    LFT == RHT

Ind:
    flipV (above (p:pic1) pic2) == flipV ((p:pic1) ++ pic2)
    above (flipV (p:pic1)) (flipV pic2) == flipV ((p:pic1) ++ pic2)
    LHT == RHT
-}

{-
solution 9.5
Base:
    LHS:
        sum ([] ++ ys) == sum ys
    RHS:
        sum [] + sum ys == 0 + sum ys == sum ys
    LHS == RHS

Ind:
    LHS:
        sum ((x : xs) ++ ys) == sum ([x] ++ xs ++ ys) == x + sum (xs ++ ys)
        == x + sum xs + sum ys
    RHS:
        sum (x : xs) + sum ys == x + sum xs + sum ys == x + sum (xs ++ ys)
    LHS == RHS
-}

{-
solution 9.6
xs ++ [] == xs
Base:
    [] ++ [] == LHS
    [] == RHS
    [] == [] ++ []
    LHS == RHS
Ind:
    (x : xs) ++ [] == LHS
    == (x : (xs ++ []))
    hypothsis: \forall xs ++ [] == xs
    hypothsis apply
    == (x : xs) == RHS

xs ++ (ys ++ zs) == (xs ++ ys) ++ zs

Base:
    [] ++ (ys ++ zs) == (ys ++ zs) == ([] ++ ys) ++ zs
Ind:
    theorem here: (x : xs) ++ ps == x : (xs ++ ps)
    (x : xs) ++ (ys ++ zs) == x : (xs ++ (ys ++ zs)) == x : ((xs ++ ys) ++ zs)
    == (x : (xs ++ ys) ++ zs) == (((x : xs) ++ ys) ++ zs)
-}

{-
solution 9.7
sum (reverse xs) == sum xs

Base:
    sum (reverse []) == 0 == sum []
Ind:
    sum (reverse (x : xs)) == sum (xs ++ [x]) == x + sum xs
    sum (x : xs) == x + sum xs
    LHS == RHS

length (reverse xs) == length xs

Base:
    length (reverse []) == length [] == 0
Ind:
    length (reverse (x : xs)) == length (xs ++ [x]) == 1 + length xs
    length (x : xs) == 1 + length xs
    LHS == RHS
-}

{-
solution 9.8
elem z (xs ++ ys) == elem z xs || elem z ys

Base:
    elem z ([] ++ ys) == elem z ys == elem z [] || elem z ys

Ind:
    elem z ((x : xs) ++ ys) == elem z (x : (xs ++ ys)) == elem z [x] || elem z (xs ++ ys)
    == elem z [x] || elem z xs || elem z ys
-}

{-
solution 9.9
zip (fst (unzip ps)) (snd (unzip ps)) == ps

Base:
    zip (fst (unzip ([], []))) (snd (unzip ([], []))) == zip [] [] == ([], [])

Ind:
    zip (fst (unzip (x : xs, y : ys))) (snd (unzip (x : xs, y : ys)))
    == zip (x : xs) (y : ys) == (x : xs, y : ys)
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