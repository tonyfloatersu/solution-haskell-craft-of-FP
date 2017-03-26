module StoreSeq ( StoreS
                , initialS
                , valueS
                , updateS ) where

type Var = Char

newtype StoreS = StoreS [(Integer, Var)]

initialS :: StoreS
initialS    = StoreS []

valueS :: StoreS -> Var -> Integer
valueS (StoreS []) _    = 0
valueS (StoreS ((int, chr) : rst)) vr
    | vr == chr         = int
    | vr < chr          = valueS (StoreS rst) vr
    | vr > chr          = 0

updateS :: StoreS -> Var -> Integer -> StoreS
updateS (StoreS []) vr int    = StoreS [(int, vr)]
updateS (StoreS ((it, cr) : rst)) vr int
    | vr == cr                = StoreS ((int, vr) : rst)
    | vr < cr                 = StoreS ((int, vr) : (it, cr) : rst)
    | vr > cr                 = updateS (StoreS rst) vr int
