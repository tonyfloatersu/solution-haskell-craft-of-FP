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
