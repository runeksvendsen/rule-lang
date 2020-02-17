module Eval.Env
( Env
, lookupEnv
)
where

import LangPrelude
import Absyn
import qualified Data.HashMap.Strict        as Map

lookupEnv :: FieldName -> Env a -> Maybe a
lookupEnv = Map.lookup

type Env a = HashMap FieldName a
