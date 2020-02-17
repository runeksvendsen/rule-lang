module LangPrelude
( module Prelude
, module LangPrelude
-- , module List
, module Maybe
, module Text
, module Map
, module NonEmpty
, module Conv
)
where

import           Prelude                  as Prelude  hiding (lookup)
-- import Data.List                as List     hiding (insert)
import           Data.Maybe               as Maybe    hiding ()
import           Data.Text                as Text     (Text)
import           Data.HashMap.Strict      as Map      (HashMap, lookup, member)
import qualified Data.HashMap.Strict      as Map
import           Data.List.NonEmpty       as NonEmpty (NonEmpty, nonEmpty)
import           Protolude.Conv           as Conv


type Map = HashMap
emptyMap = Map.empty
-- mapFromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
-- mapFromList = Map.fromList

insert env key value = Map.insert key value env
