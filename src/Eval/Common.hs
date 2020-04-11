module Eval.Common
( lookup'
)
where

import LangPrelude
import Eval.Types


-- Look up a variable and throw exception if not found
lookup' :: Text -> Env b -> b
lookup' varName env =
    maybe (error $ "Variable '" ++ toS varName ++ "' not found")
          id
          (lookup varName env)
