module Store ( Store
             , initial
             , value
             , update
             , equstore
             , valueE
             , valueEM
             , valueM ) where

import Test.QuickCheck

-- initial :: Store
-- value   :: Store -> Var -> Integer
-- update  :: Store -> Var -> Integer-> Store

type Var = Char

newtype Store = Store [(Integer, Var)]

initial :: Store
initial    = Store []

value :: Store -> Var -> Integer
value (Store []) _                        = 0
value (Store ((inte, tbdv) : rst)) val    = if val == tbdv
                                            then inte
                                            else value (Store rst) val

valueE :: Store -> Var -> Integer
valueE (Store []) _                        = error "no value"
valueE (Store ((inte, tbdv) : rst)) val    = if val == tbdv
                                             then inte
                                             else valueE (Store rst) val

valueEM :: Store -> Var -> Integer
valueEM sto var    = maybe (error "no value") id (valueM sto var)

valueM :: Store -> Var -> Maybe Integer
valueM (Store []) _                        = Nothing
valueM (Store ((inte, tbdv) : rst)) val    = if val == tbdv
                                               then Just inte
                                               else valueM (Store rst) val

update :: Store -> Var -> Integer -> Store
update (Store rst) vr inte    = Store ((inte, vr) : rst)

instance Eq Store where
    (Store lhs) == (Store rhs)    = lhs == rhs

instance Show Store where
    showsPrec n (Store st)    = showsPrec n st

instance Arbitrary Store where
    arbitrary = do list <- listOf element
                   return $ Store list
      where element = do n <- arbitrary
                         v <- elements ['a' .. 'z']
                         return (n, v)

equstore :: Store -> Store -> Bool
equstore stora storb    = atestb stora storb && atestb storb stora

atestb :: Store -> Store -> Bool
atestb stora storb    = all (\(int, var) -> value storb var == int) (leavesugar stora)

leavesugar :: Store -> [(Integer, Var)]
leavesugar (Store what)    = what
