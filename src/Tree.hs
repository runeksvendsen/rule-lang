{-# LANGUAGE DeriveFunctor #-}
module Tree
( Tree(..)
, NodeData(..)
, rootToLeafFold
, mapTermNode
, termNodes
, addGrouping
)
where

import LangPrelude
import Types
import qualified Data.HashMap.Strict as M


data Tree termLabel =
      Node (NodeData [Tree termLabel])
    | TermNode (NodeData termLabel)
        deriving (Eq, Show, Functor, Generic)

data NodeData a = NodeData (FieldName, FieldValue) a
    deriving (Eq, Show, Functor, Generic)

instance NFData a => NFData (Tree a)
instance NFData a => NFData (NodeData a)

-- Collect all term node labels
termNodes :: Tree termLabel -> [termLabel]
termNodes tree =
    go [] tree
  where
    go accum (Node (NodeData _ subTree)) = concat $ map (go accum) subTree
    go accum (TermNode (NodeData _ leaf)) = leaf : accum

-- Accumulate a state for each path from the root node to a TermNode.
-- Add the accumulated state to the TermNode label.
rootToLeafFold
    -- Accumulating function.
    -- Applied to the contents of each node from the root node to each TermNode.
    :: (state -> Tree a -> (FieldName, FieldValue) -> state)
    -- Initial state
    -> state
    -- Input tree
    -> Tree a
    -- Output tree
    -> Tree (a, state)
rootToLeafFold f accum tree@(Node (NodeData label subTree)) =
    let newAccum = f accum tree label
        newSubTree = map (rootToLeafFold f newAccum) subTree
    in Node (NodeData label newSubTree)
rootToLeafFold f accum tree@(TermNode (NodeData label a)) =
    let newAccum = f accum tree label
    in TermNode (NodeData label (a, newAccum))

-- | Modify the TermNode labels of a tree
mapTermNode
    :: (a -> b)
    -> Tree a
    -> Tree b
mapTermNode f (Node (NodeData label subTree)) =
    let newSubTree = map (mapTermNode f) subTree
    in Node (NodeData label newSubTree)
mapTermNode f (TermNode (NodeData label a)) =
    TermNode (NodeData label (f a))

addGrouping
    :: (FieldName -> Position -> FieldValue)
    -> FieldName
    -> Tree [Position]
    -> Tree [Position]
addGrouping lookupFun fieldName tree =
    go tree
  where
    mkTermNode :: (FieldValue, [Position]) -> Tree [Position]
    mkTermNode (fieldValue, positions) = TermNode $ NodeData (fieldName, fieldValue) positions
    go tn@(TermNode (NodeData _ [])) = tn
    go (Node (NodeData lab subTree)) = do
        let newSubTree = map go subTree
        Node $ NodeData lab newSubTree
    go (TermNode (NodeData lab posList)) = do
        let grouping = mkGrouping (lookupFun fieldName) posList
        Node $ NodeData lab (map mkTermNode (M.toList grouping))

mkGrouping
    :: Groupable key
    => (val -> key)
    -> [val]
    -> HashMap key [val]
mkGrouping f =
    foldr folder emptyMap
    where
        folder val groupingMap =
            M.insertWith insertFunc (f val) [val] groupingMap
        -- NB: 'insertFunc' requires two values of the same type,
        --  so we wrap the new value in a list above and take it
        --  out using 'head' below
        insertFunc new old = head new : old
        -- Look up a variable and throw exception if not found
