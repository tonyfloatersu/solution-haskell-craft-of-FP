module Deque ( Deque
             , emptyDQ
             , isEmptyDQ
             , addFDQ
             , addBDQ
             , remFDQ
             , remBDQ ) where

newtype Deque a = Deque { unDeque :: [a] }

emptyDQ :: Deque a
emptyDQ    = Deque []

isEmptyDQ :: Deque a -> Bool
isEmptyDQ (Deque [])    = True
isEmptyDQ _             = False

addFDQ :: a -> Deque a -> Deque a
addFDQ va (Deque ls)    = Deque (va : ls)

addBDQ :: a -> Deque a -> Deque a
addBDQ va (Deque ls)    = Deque (ls ++ [va])

remFDQ :: Deque a -> (a, Deque a)
remFDQ (Deque [])          = error "empty deque"
remFDQ (Deque (x : xs))    = (x, Deque xs)

remBDQ :: Deque a -> (a, Deque a)
remBDQ (Deque [])    = error "empty deque"
remBDQ (Deque xs)    = (last xs, Deque (init xs))
