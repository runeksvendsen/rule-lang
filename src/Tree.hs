module Tree
( Tree(..)
, NodeData(..)
, accumMap
, termNodes
, addGrouping
)
where

import LangPrelude
import Types
import qualified Data.HashMap.Strict as M


data Tree leafLabel =
      Node (NodeData [Tree leafLabel])
    | TermNode (NodeData leafLabel)
        deriving (Eq, Show)

data NodeData a = NodeData (FieldName, FieldValue) a
    deriving (Eq, Show)

-- Collect all term nodes
termNodes :: Tree leafLabel -> [leafLabel]
termNodes tree =
    go [] tree
  where
    go accum (Node (NodeData _ subTree)) = concat $ map (go accum) subTree
    go accum (TermNode (NodeData _ leaf)) = leaf : accum

-- | Apply the accumulating function to all paths in the tree
--    that start from the root ("Portfolio") node and ends
--    at a 'TermNode' (the innermost grouping).
accumMap
    -- Accumulating function
    -- Applied to the contents of a Node
    :: (state -> Tree a -> (FieldName, FieldValue) -> state)
    -- Mapping function
    -- Applied to the contents of a TermNode
    -> (a -> state -> b)
    -- Initial state
    -> state
    -- Input tree
    -> Tree a
    -- Output tree
    -> Tree b
accumMap f mkRes accum tree@(Node (NodeData label subTree)) =
    let newAccum = f accum tree label
        newSubTree = map (accumMap f mkRes newAccum) subTree
    in Node (NodeData label newSubTree)
accumMap f mkRes accum tree@(TermNode (NodeData label a)) =
    let newAccum = f accum tree label
    in TermNode (NodeData label (mkRes a newAccum))

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
    -> Map key [val]
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
