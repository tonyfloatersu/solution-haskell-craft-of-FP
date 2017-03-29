module STree ( STree, snil
             , sisNil, sisNode
             , sleftSub
             , srightSub
             , streeVal
             , sinsTree
             , sdelete
             , sminTree ) where

data STree a = SNil | SNode a Int (STree a) (STree a)

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
srightSub SNil                = SNil
srightSub (SNode _ _ _ rht)    = rht

streeVal :: STree a -> a
streeVal SNil                 = error "empty tree"
streeVal (SNode val _ _ _)    = val

sinsTree :: Ord a => a -> STree a -> STree a
sinsTree val SNil    = SNode val 1 SNil SNil
sinsTree v (SNode val sz tl tr)
    | v == val       = SNode val sz tl tr
    | val > v        = SNode val (ssize tl + 1 + ssize (sinsTree val tr)) tl (sinsTree val tr)
    | otherwise      = SNode val (ssize tr + 1 + ssize (sinsTree val tl)) (sinsTree val tl) tr

sdelete :: Ord a => a -> STree a -> STree a
sdelete _ SNil         = SNil
sdelete targ (SNode val sz tl tr)
    | targ < val                = SNode val sz (sdelete targ tl) tr
    | targ > val                = SNode val sz tl (sdelete targ tr)
    | sisNil tl                 = tr
    | sisNil tr                 = tl
    | otherwise                 = sjoin tl tr
  where sjoin :: Ord a => STree a -> STree a -> STree a
        sjoin lt rt             = SNode sminr (1 + ssize lt + ssize (sdelete sminr rt))
                                        lt (sdelete sminr rt)
          where (Just sminr)    = sminTree rt

sminTree :: Ord a => STree a -> Maybe a
sminTree tree
    | sisNil tree               = Nothing
    | sisNil (sleftSub tree)    = Just (streeVal tree)
    | otherwise                 = (sminTree . sleftSub) tree

ssize :: STree a -> Int
ssize SNil               = 0
ssize (SNode _ n _ _)    = n
