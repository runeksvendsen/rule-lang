{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LangPrelude
( module Prelude
, module LangPrelude
, module CM
, module Maybe
, module Text
, module HashMap
, module NonEmpty
, module Conv
, module Generic
, module Printf
, module Hashable
, module Void
, module List
, module Data
, module String
)
where

import           Prelude                  as Prelude  hiding (lookup)
import           Control.Monad            as CM
import           Data.Maybe               as Maybe    hiding ()
import           Data.Text                as Text     (Text)
import           Text.Printf              as Printf
import           Data.HashMap.Strict      as HashMap      (HashMap, lookup, member)
import qualified Data.HashMap.Strict      as M
import           Data.List.NonEmpty       as NonEmpty (NonEmpty, NonEmpty((:|)), (<|), cons, fromList)
import           Protolude.Conv           as Conv
import           Data.Hashable            as Hashable (Hashable)
import           GHC.Generics             as Generic  (Generic)
import qualified Data.Aeson               as Json
import qualified Data.Text                as T
import           Data.Void                as Void
import           Data.List                as List     (foldl')
import qualified Data.List.NonEmpty       as NE
import           Data.Data                as Data
import           Data.String              as String   (IsString(fromString))
import qualified Data.List.NonEmpty as NE (nonEmpty)


show' :: Show a => a -> Text
show' = toS . show

emptyMap :: HashMap k v
emptyMap = M.empty

insert
    :: Groupable k
    => HashMap k v
    -> k
    -> v
    -> HashMap k v
insert env key value = M.insert key value env

nonEmpty :: a -> NonEmpty a
nonEmpty item = item :| []

neConcat :: NonEmpty (NonEmpty a) -> NonEmpty a
neConcat = fromMaybe (error "empty non-empty list") . NE.nonEmpty . concat . fmap NE.toList

consMaybeNE :: a -> Maybe (NonEmpty a) -> Maybe (NonEmpty a)
consMaybeNE item Nothing     = Just (item :| [])
consMaybeNE item (Just list) = Just (item <| list)

replaceHead :: NonEmpty a -> a -> NonEmpty a
replaceHead (_ :| xs) x' = x' :| xs

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust _        _ = return ()

type Groupable a = (Eq a, Hashable a)

showValue :: Json.Value -> Text
showValue (Json.Object _) = "[object]"
showValue (Json.Array _) = "[array]"
showValue (Json.String txt) = txt
showValue (Json.Number num) = T.pack $ printf "%f" (realToFrac num :: Double)
showValue (Json.Bool b) = if b then "true" else "false"
showValue Json.Null = "(null)"

neText :: Text -> NonEmpty Char
neText = fromList . T.unpack

