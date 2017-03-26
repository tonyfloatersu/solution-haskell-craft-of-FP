module Store ( Store
             , initial
             , value
             , update ) where

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
