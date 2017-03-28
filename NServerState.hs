module NServerState ( NServerState
                    , naddToQueue
                    , nserverStep
                    , nsimulationStep
                    , nserverStart
                    , nserverSize
                    , nshortestQueue ) where

import           QueueState ( QueueState
                            , addMessage
                            , queueStep
                            , queueStart
                            , queueLength
                            , queueEmpty )

import           Chapter_14_my_note ( Inmess (..)
                                    , Outmess (..) )

data NServerState = NSS { index :: Int, threads :: [QueueState] } deriving (Show, Eq)

naddToQueue :: Inmess -> NServerState -> NServerState
naddToQueue wtf nsv@ (NSS ind threadq)
    = NSS (if ind + 1 == nserverSize nsv then 0 else ind + 1) (modify wtf ind threadq)

modify :: Inmess -> Int -> [QueueState] -> [QueueState]
modify inms loc thrads
    = take loc thrads ++ [addMessage inms (thrads !! loc)] ++ drop (loc + 1) thrads

nserverStep :: NServerState -> (NServerState, [Outmess])
nserverStep (NSS idx [])          = (NSS idx [], [])
nserverStep (NSS idx (q : qs))    = (NSS idx (_q : _qs), oms ++ omsl)
  where (_q, oms)                 = queueStep q
        (NSS _ _qs, omsl)         = nserverStep (NSS idx qs)

nsimulationStep :: NServerState -> Inmess -> (NServerState, [Outmess])
nsimulationStep nsv inm     = (naddToQueue inm servModf, omsl)
  where (servModf, omsl)    = nserverStep nsv

nserverStart :: Int -> NServerState
nserverStart thread_num    = NSS 0 (replicate thread_num queueStart)

nserverSize :: NServerState -> Int
nserverSize (NSS _ threadq)    = length threadq

nshortestQueue :: NServerState -> Int
nshortestQueue (NSS _ [])           = error "no thread"
nshortestQueue (NSS _ [_])          = 0
nshortestQueue (NSS idx (q : qs))
    | rstShortestLen <= currQlen    = 1 + rstShortest
    | otherwise                     = 0
  where currQlen                    = queueLength q :: Int
        rstShortest                 = nshortestQueue (NSS idx qs) :: Int
        rstShortestLen              = queueLength (qs !! rstShortest)
