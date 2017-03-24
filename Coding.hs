module Coding ( codeMessage
              , decodeMessage ) where

import           Types ( Tree (Leaf, Node)
                       , Bit (L, R)
                       , HCode
                       , Table )

codeMessage :: Table -> String -> HCode
codeMessage tbl    = concatMap (lookupTable tbl)

lookupTable :: Table -> Char -> HCode
lookupTable [] _                   = error "***from Coding: lookUptable -> table empty"
lookupTable ((ch, hcd) : rs) fd    = if fd == ch
                                     then hcd
                                     else lookupTable rs fd

decodeMessage :: Tree -> HCode -> String
decodeMessage trt    = decodemsg trt
  where decodemsg :: Tree -> HCode -> String
        decodemsg _ []                       = []
        decodemsg (Node _ tl _) (L : rst)    = decodemsg tl rst
        decodemsg (Node _ _ tr) (R : rst)    = decodemsg tr rst
        decodemsg (Leaf ch _) rst            = ch : decodemsg trt rst
