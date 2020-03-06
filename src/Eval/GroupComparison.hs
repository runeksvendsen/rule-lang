{-# LANGUAGE ScopedTypeVariables #-}
module Eval.GroupComparison
( eval
)
where

import LangPrelude
import Comparison
import AbsynFun
import Eval.Types
import Eval.Monad
import qualified Eval.GroupValueExpr


eval
    :: Env DataExpr
    -> GroupComparison
    -> Bool
eval env (GroupComparison valueIn bCompare valueExp) =
    let fCompare = comparator bCompare
        eval = Eval.GroupValueExpr.eval env
    in eval valueIn `fCompare` eval valueExp
