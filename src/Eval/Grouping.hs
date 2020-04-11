{-# LANGUAGE ScopedTypeVariables #-}
module Eval.Grouping
( addGrouping
)
where

import LangPrelude
import Absyn
import Eval.Types
import Eval.Common

import qualified Data.HashMap.Strict        as M


addGrouping
    :: FieldName
    -> EvalTree
    -> EvalTree
addGrouping fieldName tree =
    go tree
  where
    mkTermNode :: (FieldValue, [Position]) -> EvalTree
    mkTermNode (fieldValue, positions) = TermNode $ NodeData (fieldName, fieldValue) positions
    go tn@(TermNode (NodeData _ [])) = tn
    go (Node (NodeData lab subTree)) = do
        let newSubTree = map go subTree
        Node $ NodeData lab newSubTree
    go (TermNode (NodeData lab posList)) = do
        let grouping = mkCurrentLevelGrouping fieldName posList
        Node $ NodeData lab (map mkTermNode (M.toList grouping))

mkCurrentLevelGrouping
    :: FieldName
    -> [Position]
    -> Map FieldValue [Position]
mkCurrentLevelGrouping fieldName currentLevelPositions =
    mkGrouping (lookup' fieldName) currentLevelPositions
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