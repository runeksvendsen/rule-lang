{-# LANGUAGE ScopedTypeVariables #-}
module Eval.Grouping
( mkGrouping
, mkGroupingMaybe
)
where

import LangPrelude

import qualified Data.List.NonEmpty         as NE
import qualified Data.HashMap.Strict        as M


mkGrouping
    :: Groupable key
    => (val -> Either e key)
    -> NE.NonEmpty val
    -> ([e], Map key (NE.NonEmpty val))
mkGrouping f =
    foldr folder ([], emptyMap) . NE.toList
  where
    folder item (errors, map) =
        case f item of
            Right val  -> (errors, M.insertWith insertFunc val (NE.fromList [item]) map)
            Left error -> (error : errors, map)
    insertFunc new old = (NE.head new) `NE.cons` old

mkGroupingMaybe
    :: Groupable key
    => (val -> Maybe key)
    -> NE.NonEmpty val
    -> (Maybe (NonEmpty val), Map key (NE.NonEmpty val))   -- ^ (not_found_items, grouping)
mkGroupingMaybe f =
    foldr folder (Nothing, emptyMap) . NE.toList
  where
    folder val (errorValsM, map) =
        case f val of
            Just key -> (errorValsM, M.insertWith insertFunc key (NE.fromList [val]) map)
            Nothing  -> (consMaybeNE val errorValsM, map)
    insertFunc new old = (NE.head new) `NE.cons` old
