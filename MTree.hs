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
                       , occurance :: Int }
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
mtreeVal (MNode (Decode v _) _ _)    = Just v

mfindVal :: (Eq a, Ord a) => a -> MTree a -> Maybe (a, Int)
mfindVal v MNil    = Nothing
mfindVal v (MNode (Decode ndv oc) rht lht)
    | v == ndv     = Just (ndv, oc)
    | v < ndv      = mfindVal v rht
    | otherwise    = mfindVal v lht

mminTree :: (Eq a, Ord a) => MTree a -> Maybe a
mminTree MNil       = Nothing
mminTree (MNode (Decode ndv _) rht _)
    | misNil rht    = Just ndv
    | otherwise     = mminTree rht

mmaxTree :: (Eq a, Ord a) => MTree a -> Maybe a
mmaxTree MNil       = Nothing
mmaxTree (MNode (Decode ndv _) _ lht)
    | misNil lht    = Just ndv
    | otherwise     = mmaxTree lht
