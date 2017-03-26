module StoreF ( StoreF
              , initialF
              , valueF
              , updateF
              , equstoreF
              , setAll ) where

type Var = Char

newtype StoreF = StoreF (Var -> Integer)

initialF :: StoreF
initialF    = StoreF (\_ -> 0)

valueF :: StoreF -> Var -> Integer
valueF (StoreF sto)    = sto

updateF :: StoreF -> Var -> Integer -> StoreF
updateF (StoreF sto) v int    = StoreF (\w -> if w == v then int else sto w)

charLs :: [Char]
charLs    = ['a' .. 'z'] ++ ['0' .. '9'] ++ ['A' .. 'Z']

resLs :: StoreF -> [Integer]
resLs sto    = map (valueF sto) charLs

equstoreF :: StoreF -> StoreF -> Bool
equstoreF sto1 sto2    = resLs sto1 == resLs sto2

setAll :: Integer -> StoreF
setAll x    = StoreF (\_ -> x)
