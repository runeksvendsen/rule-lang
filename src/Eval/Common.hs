module Eval.Common
( getField
)
where

import           LangPrelude
import           Eval.Types
import           Eval.Result
import           Eval.Monad


-- TODO: return 'MissingField'
getField :: FieldName -> Position -> EvalM (Maybe FieldValue)
getField fieldName pos = do
    when (isNothing resM) $
        logResult (MissingField fieldName) (nonEmpty pos)
    return resM
  where
    resM = lookup fieldName pos
