module MTree ( MTree
             , mnil
             , misNil
             , misNode
             , mleftSub
             , mrightSub
             , mtreeVal
             , mdelete
             , mminTree
             , mmaxTree
             , mindexT
             , msuccessor
             , mancientor
             , mcloset ) where

-- make this tree is to accomplish the search/count/category tree

-- decode message with minTree maxTree element_occurance

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
