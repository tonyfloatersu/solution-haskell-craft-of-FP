module QueueState ( QueueState
                  , addMessage
                  , queueStep
                  , queueStart
                  , queueLength
                  , queueEmpty ) where

import           Chapter_14_my_note ( Inmess (..)
                                    , Outmess (..) )

type Time = Integer

type Service = Integer

data QueueState = QS Time Service [Inmess]
                deriving (Eq, Show)

addMessage :: Inmess -> QueueState -> QueueState
addMessage im (QS time serv ml)    = QS time serv (ml ++ [im])

queueStep :: QueueState -> (QueueState, [Outmess])
queueStep (QS t s [])          = (QS (t + 1) s [], [])
queueStep (QS _ _ (No : _))    = error "NO?! WTF!"
queueStep (QS time servSoFar (Yes arr serv : inRest))
    | servSoFar < serv         = (QS (time + 1) (servSoFar + 1) (Yes arr serv : inRest), [])
    | otherwise                = ( QS (time + 1) 0 inRest
                                 , [Discharge arr serv (time - serv - arr)] )

queueStart :: QueueState
queueStart    = QS 0 0 []

queueLength :: QueueState -> Int
queueLength (QS _ _ q)    = length q

queueEmpty :: QueueState -> Bool
queueEmpty qss    = queueLength qss /= 0
