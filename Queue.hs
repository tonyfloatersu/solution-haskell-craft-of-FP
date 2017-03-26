module Queue ( Queue
             , emptyQ
             , isEmptyQ
             , addQ
             , remQ ) where

data Queue a = Queue [a] [a]

emptyQ :: Queue a
emptyQ    = Queue [] []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue [] [])    = True
isEmptyQ _                = False

addQ :: a -> Queue a -> Queue a
addQ x (Queue xs ys)    = Queue xs (x : ys)

remQ :: Queue a -> (a, Queue a)
remQ (Queue [] [])          = error "empty queue"
remQ (Queue [] ys)          = remQ (Queue (reverse ys) [])
remQ (Queue (x : xs) ys)    = (x, Queue xs ys)
