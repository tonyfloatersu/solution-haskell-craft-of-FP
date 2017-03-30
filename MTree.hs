module MTree ( MTree
             , mnil
             , misNil
             , misNode
             , mleftSub
             , mrightSub
             , mtreeVal
             , minsTree
             , mfindVal
             , mdelete
             , mminTree
             , mmaxTree
             , mindexT
             , msuccessor
             , mancientor
             , mcloset ) where

data Decode a = Decode { element_nd :: a
                       , occurance :: Int
                       , treesize :: Int }
                       deriving (Eq, Show)

data MTree a = MNil | MNode (Decode a) (MTree a) (MTree a) deriving (Show, Eq)

mnil :: MTree a
mnil    = MNil

misNil :: MTree a -> Bool
misNil MNil        = False
misNil MNode {}    = True

misNode :: MTree a -> Bool
misNode    = not . misNil

mleftSub :: MTree a -> MTree a
mleftSub MNil             = MNil
mleftSub (MNode _ l _)    = l

mrightSub :: MTree a -> MTree a
mrightSub MNil             = MNil
mrightSub (MNode _ _ r)    = r

mtreeVal :: MTree a -> Maybe a
mtreeVal MNil                        = Nothing
mtreeVal (MNode (Decode v _ _) _ _)    = Just v

mfindVal :: (Eq a, Ord a) => a -> MTree a -> Maybe (a, Int)
mfindVal v MNil    = Nothing
mfindVal v (MNode (Decode ndv oc _) rht lht)
    | v == ndv     = Just (ndv, oc)
    | v < ndv      = mfindVal v rht
    | otherwise    = mfindVal v lht

mminTree :: (Eq a, Ord a) => MTree a -> Maybe a
mminTree MNil       = Nothing
mminTree (MNode (Decode ndv _ _) rht _)
    | misNil rht    = Just ndv
    | otherwise     = mminTree rht

mmaxTree :: (Eq a, Ord a) => MTree a -> Maybe a
mmaxTree MNil       = Nothing
mmaxTree (MNode (Decode ndv _ _) _ lht)
    | misNil lht    = Just ndv
    | otherwise     = mmaxTree lht

minsTree :: (Eq a, Ord a) => a -> MTree a -> MTree a
minsTree v MNil    = MNode (Decode v 1 1) MNil MNil
minsTree v (MNode (Decode ndv ocr sz) lhs rhs)
    | v == ndv     = MNode (Decode ndv (ocr + 1) sz) lhs rhs
    | v > ndv      = MNode (Decode ndv ocr (sz + 1)) lhs (minsTree v rhs)
    | otherwise    = MNode (Decode ndv ocr (sz + 1)) (minsTree v lhs) rhs

mdelete :: (Ord a, Eq a) => a -> MTree a -> MTree a
mdelete _ MNil      = MNil
mdelete v (MNode (Decode ndv occ trsz) lht rht)
    | v < ndv       = MNode (Decode ndv occ (trsz - 1)) (mdelete v lht) rht
    | v > ndv       = MNode (Decode ndv occ (trsz - 1)) lht (mdelete v rht)
    | misNil rht    = lht
    | misNil lht    = rht
    | otherwise     = undefined
  where mjoin                                = MNode (Decode _max occ twosz)
                                                      lht (mdelete _max rht)
        twosz                                = treesz lht + treesz rht :: Int
        (Just max)                           = mmaxTree rht
        (Just (_max, occ))                   = mfindVal max rht

treesz :: MTree a -> Int
treesz (MNode (Decode _ _ s) _ _)    = s

mindexT :: Int -> MTree a -> Maybe a
mindexT _ MNil    = Nothing
mindexT n tr@ (MNode (Decode ndv _ sz) lh rh)
    | n > sz      = Nothing
    | n == sz     = Just ndv
