module Eval.RuleExpr
( eval
)
where

import LangPrelude
import AbsynFun
import Eval.Types
import Eval.Monad

eval
    :: Env EvalTree
    -> RuleExpr
    -> EvalM a
eval = undefined
