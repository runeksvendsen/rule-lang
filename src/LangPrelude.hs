{-# LANGUAGE ConstraintKinds #-}
module LangPrelude
( module Prelude
, module LangPrelude
, module CM
, module Maybe
, module Text
, module Map
, module NonEmpty
, module Conv
, module Generic
)
where

import           Prelude                  as Prelude  hiding (lookup)
import           Control.Monad            as CM
import           Data.Maybe               as Maybe    hiding ()
import           Data.Text                as Text     (Text)
import           Data.HashMap.Strict      as Map      (HashMap, lookup, member)
import qualified Data.HashMap.Strict      as Map
import           Data.List.NonEmpty       as NonEmpty (NonEmpty, NonEmpty((:|)), (<|), cons)
import           Protolude.Conv           as Conv
import           Data.Hashable            (Hashable)
import           GHC.Generics             as Generic  (Generic)


type Map = HashMap
emptyMap = Map.empty

insert env key value = Map.insert key value env
nonEmpty item = item :| []

consMaybeNE :: a -> Maybe (NonEmpty a) -> Maybe (NonEmpty a)
consMaybeNE item Nothing     = Just (item :| [])
consMaybeNE item (Just list) = Just (item <| list)

replaceHead :: NonEmpty a -> a -> NonEmpty a
replaceHead = undefined

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust _        _ = return ()

type Groupable a = (Eq a, Hashable a)
