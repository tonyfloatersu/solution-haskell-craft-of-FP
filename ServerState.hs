module ServerState ( ServerState
                   , addToQueue
                   , serverStep
                   , simulationStep
                   , serverStart
                   , serverSize
                   , shortestQueue ) where

import           QueueState ( QueueState
                            , addMessage
                            , queueStep
                            , queueStart
                            , queueLength
                            , queueEmpty )

import           Chapter_14_my_note ( Inmess (..)
                                    , Outmess (..) )

newtype ServerState = SS { unServerState :: [QueueState] } deriving (Show, Eq)

addToQueue :: Int -> Inmess -> ServerState -> ServerState
addToQueue n im (SS qsl)    = SS (take newLocation qsl ++ [addMessage im (qsl !! newLocation)]
                                  ++ drop (newLocation + 1) qsl)
  where serverQueues        = serverSize (SS qsl) :: Int
        newLocation         = n `mod` serverQueues :: Int

serverStep :: ServerState -> (ServerState, [Outmess])
serverStep (SS [])          = (SS [], [])
serverStep (SS (q : qs))    = (SS (_q : _qs), mess ++ messes)
  where (_q, mess)          = queueStep q
        (SS _qs, messes)    = serverStep (SS qs)

simulationStep :: ServerState -> Inmess -> (ServerState, [Outmess])
simulationStep servSt im      = (addNewObject im servSt_, outmess)
  where (servSt_, outmess)    = serverStep servSt

serverSize :: ServerState -> Int
serverSize (SS xs)    = length xs

addNewObject :: Inmess -> ServerState -> ServerState
addNewObject No servSt              = servSt
addNewObject (Yes arr wa) servSt    = addToQueue (shortestQueue servSt) (Yes arr wa) servSt

serverStart :: Int -> ServerState
serverStart threads    = SS (replicate threads queueStart)

shortestQueue :: ServerState -> Int
shortestQueue (SS [])                  = error "no thread"
shortestQueue (SS [_])                 = 0
shortestQueue (SS (q : qs))
    | queueLength (qs !! shortestQueue (SS qs))
                   <= queueLength q    = 1 + shortestQueue (SS qs)
    | otherwise                        = undefined
