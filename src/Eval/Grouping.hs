{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module Eval.Grouping
( mkGrouping
, mkGroupingMaybe
--   Grouping
-- , GroupedBy
-- , mkGroupedBy
-- * Re-exports
, Groupable
)
where

import LangPrelude

import qualified Data.List.NonEmpty         as NE
import qualified Data.HashMap.Strict        as Map
import           Data.Hashable              (Hashable)


type Groupable a = (Eq a, Hashable a)

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
            Right val  -> (errors, Map.insertWith insertFunc val (NE.fromList [item]) map)
            Left error -> (error : errors, map)
    insertFunc new old = (NE.head new) `NE.cons` old

mkGroupingMaybe
    :: Groupable key
    => (val -> Maybe key)
    -> NE.NonEmpty val
    -> ([val], Map key (NE.NonEmpty val))   -- ^ (not_found_items, grouping)
mkGroupingMaybe f =
    foldr folder ([], emptyMap) . NE.toList
  where
    folder val (errorVals, map) =
        case f val of
            Just key -> (errorVals, Map.insertWith insertFunc key (NE.fromList [val]) map)
            Nothing  -> (val : errorVals, map)
    insertFunc new old = (NE.head new) `NE.cons` old



-- newtype Grouping f key val = Grouping { unGroup :: (key, f val) }

-- instance Functor f => Functor (Grouping f key) where
--     fmap f = Grouping . fmap (NE.map f) . unGroup

-- newtype GroupedBy f key val = GroupedBy { groupedBy :: f (Grouping f key val) }

-- instance Functor f => Functor (GroupedBy f key) where
--     fmap f = GroupedBy . NE.map (fmap f) . groupedBy

-- mkGroupedBy
--     :: forall val key.
--        Ord key
--     => (val -> key)
--     -> NE.NonEmpty val
--     -> GroupedBy key val
-- mkGroupedBy f =
--     someFunc . NE.fromList . NE.groupAllWith f . NE.toList
--   where
--     someFunc :: NE.NonEmpty (NE.NonEmpty val) -> GroupedBy key val
--     someFunc = GroupedBy . NE.map pairUp
--     pairUp :: NE.NonEmpty val -> Grouping key val
--     pairUp nonEmpty = Grouping (f $ NE.head nonEmpty, nonEmpty)

-- addGrouping
--     :: forall val key.
--        Ord key
--     => (val -> key)
--     -> GroupedBy key val
--     -> GroupedBy key (GroupedBy key val)
-- addGrouping f (GroupedBy neGrouping) =
--     NE.map
--   where
--     someFunc (Grouping (key, ne)) = (Grouping (key, mkGroupedBy f ne))
