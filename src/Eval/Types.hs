
module Eval.Types where

import LangPrelude
import Absyn
import qualified Data.Aeson                 as Json


type Position = HashMap Text Json.Value
type Env a = HashMap FieldName a
