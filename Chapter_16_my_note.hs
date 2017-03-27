module Chapter_16_my_note where

import           Prelude hiding (Word)

import qualified Prelude as P

import           StoreSeq ( StoreS
                          , initialS
                          , updateS
                          , valueS )

import           Store ( Store
                       , initial
                       , update
                       , value
                       , equstore
                       , valueEM
                       , valueE
                       , valueM )

import           StoreF ( StoreF
                        , initialF
                        , updateF
                        , valueF
                        , setAll )

import           LibraryADT ( Database
                            , initDB
                            , books
                            , borrowers
                            , borrowed
                            , numborrowed
                            , makeLoan
                            , returnLoan )

import           Queue ( Queue
                       , emptyQ
                       , isEmptyQ
                       , addQ
                       , remQ )

import           Deque ( Deque
                       , emptyDQ
                       , isEmptyDQ
                       , addFDQ
                       , addBDQ
                       , remFDQ
                       , remBDQ )

import           DequeDL ( DequeDL
                         , emptyDQDL
                         , isEmptyDQDL
                         , addFDQDL
                         , addBDQDL
                         , remFDQDL
                         , remBDQDL )

import           PropertyQue ( PropertyQue
                             , emptyPQ
                             , isEmptyPQ
                             , addPQ
                             , remPQ )

import           TreeADT ( Tree
                         , nil
                         , isNil
                         , isNode
                         , leftSub
                         , rightSub
                         , treeVal
                         , insTree
                         , delete
                         , minTree
                         , elemT )

import           Chapter_12_my_note ( Doc
                                    , Line
                                    , Word
                                    , makeIndex
                                    , shorten
                                    , amalgamate
                                    , makeLists
                                    , sortLs
                                    , orderPair
                                    , smallerThan
                                    , allNumWords
                                    , whiteSpace
                                    , cleanHead
                                    , _splitWds
                                    , numWords
                                    , _lines )

import           QueueState ( QueueState
                            , addMessage
                            , queueStep
                            , queueStart
                            , queueLength
                            , queueEmpty )

import           ServerState ( ServerState
                             , addToQueue
                             , serverStep
                             , simulationStep
                             , serverStart
                             , serverSize
                             , shortestQueue )
