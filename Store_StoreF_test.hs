module Store_StoreF_test where

import           Store ( Store
                       , initial
                       , update
                       , value )

import           StoreF ( StoreF
                        , initialF
                        , updateF
                        , valueF )

import           Test.QuickCheck

prop_Initial :: Char -> Bool
prop_Initial ch    = value initial ch == 0

initialTest :: IO ()
initialTest    = quickCheck prop_Initial

prop_Update :: Char -> Integer -> Store -> Bool
prop_Update str int sto    = value (update sto str int) str == int

updateTest :: IO ()
updateTest    = quickCheck prop_Update
