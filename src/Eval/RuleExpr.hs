module Eval.RuleExpr
( eval
, runEvalRule
)
where

import LangPrelude
import Absyn
import Eval.Types
import Eval.Monad
import qualified Eval.DataExpr


eval
    :: Env EvalTree
    -> RuleExpr
    -> EvalM Bool
eval varEnv ruleExpr =
    case ruleExpr of
        Let name rhs scope -> do
            dataTree <- Eval.DataExpr.eval varEnv rhs
            eval (insert varEnv name dataTree) scope

        Foreach dataExpr scope -> do
            dataTree <- Eval.DataExpr.eval varEnv dataExpr

            let iterateTree :: Env EvalTree -> EvalTree -> EvalM Bool
                iterateTree varEnv' tree =
                    let updateVarEnv fieldName = insert varEnv' fieldName tree in
                    case tree of
                        Node     (fieldName, _) subTree ->
                            all (== True) <$> mapM (iterateTree $ updateVarEnv fieldName) subTree
                        TermNode (fieldName, _) _ ->
                            eval (updateVarEnv fieldName) scope
            iterateTree varEnv dataTree

        And e1 e2 -> do
            b1 <- eval varEnv e1
            b2 <- eval varEnv e2
            return $ b1 && b2

        Rule groupComparison ->
            Eval.DataExpr.evalGroupComparison varEnv groupComparison


runEvalRule :: NonEmpty Position -> RuleExpr -> Either Text (Bool, [Result])
runEvalRule portfolioPositions expr =
    runEvalM $ eval (initialVarEnv portfolioPositions) expr

