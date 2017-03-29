module STree ( STree
             , snil
             , sisNil
             , sisNode
             , sleftSub
             , srightSub
             , streeVal
             , sinsTree
             , sdelete
             , sminTree
             , sindexT
             , successor
             , ancientor
             , closest ) where

data STree a = SNil | SNode a Int (STree a) (STree a) deriving (Eq, Show)

snil :: STree a
snil    = SNil

sisNil :: STree a -> Bool
sisNil SNil        = True
sisNil SNode {}    = False

sisNode :: STree a -> Bool
sisNode    = not . sisNil

sleftSub :: STree a -> STree a
sleftSub SNil                 = SNil
sleftSub (SNode _ _ lft _)    = lft

srightSub :: STree a -> STree a
srightSub SNil                 = SNil
srightSub (SNode _ _ _ rht)    = rht

streeVal :: STree a -> a
streeVal SNil                 = error "empty tree"
streeVal (SNode val _ _ _)    = val

sinsTree :: Ord a => a -> STree a -> STree a
sinsTree val SNil    = SNode val 1 SNil SNil
sinsTree v (SNode val sz tl tr)
    | v == val       = SNode val sz tl tr
    | val > v        = SNode val (sz + 1) (sinsTree v tl) tr
    | otherwise      = SNode val (sz + 1) tl (sinsTree v tr)

sdelete :: Ord a => a -> STree a -> STree a
sdelete _ SNil         = SNil
sdelete targ (SNode val sz tl tr)
    | targ < val                = SNode val sz (sdelete targ tl) tr
    | targ > val                = SNode val sz tl (sdelete targ tr)
    | sisNil tl                 = tr
    | sisNil tr                 = tl
    | otherwise                 = sjoin tl tr
  where sjoin :: Ord a => STree a -> STree a -> STree a
        sjoin lt rt             = SNode sminr (ssize lt + ssize rt) lt (sdelete sminr rt)
          where (Just sminr)    = sminTree rt

sminTree :: Ord a => STree a -> Maybe a
sminTree tree
    | sisNil tree               = Nothing
    | sisNil (sleftSub tree)    = Just (streeVal tree)
    | otherwise                 = (sminTree . sleftSub) tree

ssize :: STree a -> Int
ssize SNil               = 0
ssize (SNode _ n _ _)    = n

sindexT :: Int -> STree a -> a
sindexT n tr
    | sisNil tr                  = error "index error fail to fetch"
    | n == ssize tr              = streeVal tr
    | n < ssize (sleftSub tr)    = sindexT n (sleftSub tr)
    | otherwise                  = sindexT (n - 1 - (ssize . sleftSub) tr) (srightSub tr)

successor :: Ord a => a -> STree a -> Maybe a
successor _ SNil     = Nothing
successor v (SNode val _ SNil SNil)
    | v <= val       = Just val
    | otherwise      = Nothing
successor fdtg (SNode val _ tl tr)
    | fdtg >= val    = successor fdtg tr
    | otherwise      = successor fdtg (sinsTree val tl)

ancientor :: Ord a => a -> STree a -> Maybe a
ancientor _ SNil     = Nothing
ancientor v (SNode val _ SNil SNil)
    | v <= val       = Nothing
    | otherwise      = Just val
ancientor v (SNode val _ tl tr)
    | v < val        = ancientor v tl
    | otherwise      = ancientor v (sinsTree val tr)

closest :: Int -> STree Int -> Int
closest val tr
    | Nothing == suc                     = maybe (error "fuck you") id anc
    | Nothing == anc                     = maybe (error "fuck you") id suc
    | otherwise                          = clos anc suc val
  where suc                              = successor val tr
        anc                              = ancientor val tr
        clos :: Maybe Int -> Maybe Int -> Int -> Int
        clos (Just vl) (Just vr) _val    = if _val - vl > vr - _val
                                           then vr
                                           else vl
