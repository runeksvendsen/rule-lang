module Eval.LangExpr where

import LangPrelude
import AbsynFun
import Eval.Types
import Eval.Monad
import qualified Eval.RuleExpr
import qualified Eval.DataExpr


eval
    :: Env EvalTree
    -> LangExpr
    -> EvalM [a]
eval varEnv expr =
    case expr of
        Let name rhs scope -> do
            dataTree <- Eval.DataExpr.eval varEnv rhs
            eval (insert varEnv name dataTree) scope

        Foreach varName dataExpr scope -> do
            dataTree <- Eval.DataExpr.eval varEnv dataExpr
            let termNodeTrees = collectTermNodeTrees dataTree
            results <- forM termNodeTrees $ \termNodeTree -> do
                let newVarEnv = insert varEnv varName termNodeTree
                eval newVarEnv scope
            return (concat results)

        RuleExpr ruleExpr -> Eval.RuleExpr.eval varEnv ruleExpr
