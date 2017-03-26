module Chapter_16_my_note where

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
