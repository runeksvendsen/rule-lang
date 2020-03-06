module Eval.RuleExpr
( eval
)
where

import LangPrelude
import AbsynFun
import Eval.Types
import Eval.Monad

eval
    :: Env DataExpr
    -> RuleExpr
    -> EvalM Bool
eval = undefined
