module Eval.GroupValueExpr where

import LangPrelude
import AbsynFun
import Eval.Types
import Eval.GroupFold
import qualified Eval.DataExpr


eval :: Env DataExpr -> GroupValueExpr -> GroupValue
eval env expr =
    go expr
  where
    go (Literal groupValue) = groupValue
    go (GroupFold groupFold dataExpr) = undefined
    go (RelativeComparison a b) = undefined
