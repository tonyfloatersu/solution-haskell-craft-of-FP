module Chapter_9_my_note where

import           Pictures
import           Test.QuickCheck

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
take n xs ++ drop n xs == xs

Base:
    take n [] ++ drop n [] == [] no matter what n is

Ind:
    take n (x : xs) ++ drop n (y : ys)
    if n <= 0   then take n (x : xs) ++ drop n (y : ys) == drop n (y : ys) == y : ys == x : xs
                where x : xs == y : ys
    if n > 0    then take n (x : xs) ++ drop n (y : ys)
                     == x : take (n - 1) xs ++ drop (n - 1) ys
-}

prop_flipv_fliph :: Picture -> Bool
prop_flipv_fliph pic    = (flipV . flipH) pic == (flipH . flipV) pic

prop_flipv :: Picture -> Bool
prop_flipv pic    = (flipV . flipV) pic == pic

prop_fliph :: Picture -> Bool
prop_fliph pic    = (flipH . flipH) pic == pic

tester_env :: (Picture -> Bool) -> IO ()
tester_env    = quickCheck

prop_sum_list :: [Integer] -> [Integer] -> Bool
prop_sum_list xs ys    = sum (xs ++ ys) == sum xs + sum ys

test_sum_env :: ([Integer] -> [Integer] -> Bool) -> IO ()
test_sum_env    = quickCheck

prop_sum_rev_list :: [Integer] -> Bool
prop_sum_rev_list ls    = (sum . reverse) ls == sum ls

test_sum_rev_env :: ([Integer] -> Bool) -> IO ()
test_sum_rev_env    = quickCheck

prop_length_rev_list :: [a] -> Bool
prop_length_rev_list ls    = (length . reverse) ls == length ls

test_length_rev_env :: Show a => Arbitrary a => ([a] -> Bool) -> IO ()
test_length_rev_env    = quickCheck

prop_elem_list :: Eq a => a -> [a] -> [a] -> Bool
prop_elem_list z xs ys    = elem z (xs ++ ys) == elem z xs || elem z ys

test_elem_env :: Arbitrary a => Show a => (a -> [a] -> [a] -> Bool) -> IO ()
test_elem_env    = quickCheck

prop_zip_list :: Eq a => Eq b => [(a, b)] -> Bool
prop_zip_list ps    = zip (fst (unzip ps)) (snd (unzip ps)) == ps

test_zip_env :: Arbitrary a => Arbitrary b => Show a => Show b => ([(a, b)] -> Bool) -> IO ()
test_zip_env    = quickCheck

prop_take_drop :: Eq a => Int -> [a] -> Bool
prop_take_drop n xs    = take n xs ++ drop n xs == xs

test_take_drop :: Arbitrary a => Show a => (Int -> [a] -> Bool) -> IO ()
test_take_drop    = quickCheck

shunt :: [a] -> [a] -> [a]
shunt [] ys          = ys
shunt (x : xs) ys    = shunt xs (x : ys)

rev :: [a] -> [a]
rev xs    = shunt xs []

{-
solution 9.12
rev (xs ++ ys) == shunt (xs ++ ys) [] == shunt ys [] ++ shunt xs []

Base:
    shunt ([] ++ ys) [] == shunt ys [] == rev ys == RHS
Ind:
1.
    rev ((x : xs) ++ ys) == shunt ((x : xs) ++ ys) [] == shunt (x : (xs ++ ys)) [] == shunt (xs ++ ys) [x]
    rev ys ++ shunt xs [x] ?== shunt (xs ++ ys) [x]
2.
    change: shunt (xs ++ ys) [] == shunt ys [] ++ shunt xs []
        <=> shunt ys (shunt xs zs) == shunt ys [] ++ shunt xs [] ++ zs
    then:
    shunt ys (shunt [] zs) == shunt ys zs
    shunt ys [] ++ shunt xs [] ++ zs == shunt ys [] ++ zs == shunt ys zs
    shunt ys (shunt (x : xs) zs) == shunt ys (shunt xs (x : zs)) == shunt ys [] ++ shunt xs [] ++ (x : zs)
    shunt ys [] ++ shunt (x : xs) [] ++ zs == shunt ys [] ++ shunt xs [x] ++ zs == shunt ys [] ++ shunt xs [] ++ x : zs
    let zs == []
    then shunt ys [] ++ shunt xs [] == rev ys ++ rev xs
         shunt ys (shunt xs []) == shunt ys (rev xs) == rev ys ++ rev xs
    addition:
    proof for shunt xs [] ++ ys == shunt xs ys
    stronger: shunt xs zs ++ ys == shunt xs (zs ++ ys)
    shunt [] zs ++ ys == zs ++ ys == shunt [] (zs ++ ys)
    shunt (x : xs) zs ++ ys == shunt xs (x : zs) ++ ys == shunt xs (x : (zs ++ ys))
    shunt (x : xs) (zs ++ ys) == shunt xs (x : (zs ++ ys))
    Q.E.D
-}

facAux :: Integer -> Integer -> Integer
facAux 0 p    = p
facAux n p    = facAux (n - 1) (n * p)

fac2 :: Integer -> Integer
fac2 n    = facAux n 1

{-
solution 9.13
fac2 n == frac n

Base:
fac2 0 == 1 == frac 0

Ind:
    frac (n + 1) = (n + 1) * frac n
    fac2 (n + 1) = facAux n (n + 1) * p
    p * frac n = facAux n p
    fac2 (n + 1) = p * facAux n (n + 1) == (n + 1) * p * frac n == (n + 1) * frac n
-}

prop_old_new_rev :: Eq a => [a] -> Bool
prop_old_new_rev ls    = rev ls == reverse ls

test_old_new_rev_env :: Arbitrary a => Show a => ([a] -> Bool) -> IO ()
test_old_new_rev_env    = quickCheck

prop_fac2 :: Integer -> Property
prop_fac2 n    = (n > 0) ==> fac2 n == frac n

test_fac2_env :: (Integer -> Property) -> IO ()
test_fac2_env    = quickCheck
