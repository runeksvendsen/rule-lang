module Eval.Common
( getField
)
where

import           LangPrelude


getField fieldName pos =
    fromMaybe (notFoundError pos) $ lookup fieldName pos
  where
    notFoundError =
        error $ "ERROR: '" ++ toS fieldName ++ "' key not found for: " ++ show pos
