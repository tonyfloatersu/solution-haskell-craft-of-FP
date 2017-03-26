module DequeDL ( DequeDL
               , emptyDQDL
               , isEmptyDQDL
               , addFDQDL
               , addBDQDL
               , remFDQDL
               , remBDQDL ) where

data DequeDL a = DequeDL [a] [a]

emptyDQDL :: DequeDL a
emptyDQDL    = DequeDL [] []

isEmptyDQDL :: DequeDL a -> Bool
isEmptyDQDL (DequeDL [] [])    = True
isEmptyDQDL _                  = False

addFDQDL :: a -> DequeDL a -> DequeDL a
addFDQDL vl (DequeDL xs ys)    = DequeDL (vl : xs) ys

addBDQDL :: a -> DequeDL a -> DequeDL a
addBDQDL vl (DequeDL xs ys)    = DequeDL xs (vl : ys)

remFDQDL :: DequeDL a -> DequeDL a
remFDQDL (DequeDL [] [])          = error "empty deque"
remFDQDL (DequeDL [] ys)          = remFDQDL (DequeDL [] (init ys))
remFDQDL (DequeDL (_ : xs) ys)    = DequeDL xs ys

remBDQDL :: DequeDL a -> DequeDL a
remBDQDL (DequeDL [] [])    = error "empty deque"
remBDQDL (DequeDL xs [])    = remBDQDL (DequeDL (init xs) [])
remBDQDL (DequeDL xs ys)    = DequeDL xs (init ys)
