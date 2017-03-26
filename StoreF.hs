module StoreF ( StoreF
              , initialF
              , valueF
              , updateF ) where

type Var = Char

newtype StoreF = StoreF (Var -> Integer)

initialF :: StoreF
initialF    = StoreF (\_ -> 0)

valueF :: StoreF -> Var -> Integer
valueF (StoreF sto)    = sto

updateF :: StoreF -> Var -> Integer -> StoreF
updateF (StoreF sto) v int    = StoreF (\w -> if w == v then int else sto w)
