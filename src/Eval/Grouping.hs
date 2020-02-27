{-# LANGUAGE ScopedTypeVariables #-}
module Eval.Grouping
( mkGroupingMaybe
)
where

import LangPrelude

import qualified Data.List.NonEmpty         as NE
import qualified Data.HashMap.Strict        as M


mkGroupingMaybe
    :: Groupable key
    => (val -> Maybe key)
    -> NE.NonEmpty val
    -> (Maybe (NonEmpty val), Map key (NE.NonEmpty val))   -- ^ (not_found_items, grouping)
mkGroupingMaybe f =
    foldr folder (Nothing, emptyMap) . NE.toList
  where
    folder val (errorValsM, groupingMap) =
        case f val of
            Just key -> (errorValsM, M.insertWith insertFunc key (NE.fromList [val]) groupingMap)
            Nothing  -> (consMaybeNE val errorValsM, groupingMap)
    insertFunc new old = (NE.head new) `NE.cons` old
