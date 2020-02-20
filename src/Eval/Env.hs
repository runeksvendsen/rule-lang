module Eval.Env
( Env
, lookupEnv
)
where

import LangPrelude
import Absyn
import qualified Data.HashMap.Strict        as M

lookupEnv :: FieldName -> Env a -> Maybe a
lookupEnv = M.lookup

type Env a = Map FieldName a
