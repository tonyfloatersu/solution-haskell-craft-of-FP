module Chapter_15_my_note where

-- import           Prelude hiding (map)

-- import qualified Prelude as P

-- tester_q :: [Int] -> [Int]
-- tester_q    = P.map (+ 1)

-- tester_q2 :: [Int] -> Int
-- tester_q2    = foldr (*) 0

import           Coding ( codeMessage
                        , decodeMessage )

import           Types ( Bit (L, R)
                       , HCode
                       , Table
                       , Tree (Leaf, Node) )

import           MakeCode ( codes
                          , codeTable )
