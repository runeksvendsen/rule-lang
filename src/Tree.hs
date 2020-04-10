module Tree
( Tree(..)
, NodeData(..)
, leaves
)
where

import LangPrelude
import Types
import qualified Data.List.NonEmpty as NE


-- data Tree leaf =
--       Node (FieldName, FieldValue) [Tree leaf]
--     | Leaf leaf

data Tree leafLabel =
      Node (NodeData [Tree leafLabel])
    | TermNode (NodeData leafLabel)
        deriving (Eq, Show)

data NodeData a = NodeData (FieldName, FieldValue) a
    deriving (Eq, Show)

leaves :: Tree leafLabel -> [leafLabel]
leaves tree =
    go [] tree
  where
    go accum (Node (NodeData _ subTree)) = concat $ map (go accum) subTree
    go accum (TermNode (NodeData _ leaf)) = leaf : accum

-- instance Functor NodeData where
--     fmap f (NodeData lab lst) = NodeData lab (NE.map f lst)

-- instance Functor Tree where
--     fmap f (Node nodeData) = Node (fmap (fmap f) nodeData)
--     fmap f (TermNode nodeData) = TermNode (fmap f nodeData)



-- test =
--     Node ("Portfolio", "ForeignBonds")
--         [ Node ("Country", "DK")
--             [ Node ("Issuer", "I2")
--                 [ Leaf $ Pos "P7"
--                 ]
--             ]
--         , Node ("Country", "US")
--             [ Node ("Issuer", "I1")
--                 [ Leaf $ Pos "P2"
--                 , Leaf $ Pos "P5"
--                 ]
--             , Node ("Issuer", "I3")
--                 [ Leaf $ Pos "P3"
--                 ]
--             , Node ("Issuer", "I6")
--                 [ Leaf $ Pos "P8"
--                 ]
--             ]
--         , Node ("Country", "GB")
--             [ Node ("Issuer", "I4")
--                 [ Leaf $ Pos "P1"
--                 , Leaf $ Pos "P4"
--                 ]
--             , Node ("Issuer", "I5")
--                 [ Leaf $ Pos "P6"
--                 ]
--             ]
--         ]

data Pos = Pos String
