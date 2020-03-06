module Eval.LangExpr where

import LangPrelude
import AbsynFun
import Eval.Types
import Eval.Monad


eval
    :: Env DataExpr
    -> LangExpr
    -> EvalM Bool
eval varEnv expr =
    case expr of
        Let name rhs scope -> do
            dataTree <- evalData varEnv rhs
            evalRec (insert varEnv name dataTree) scope

        -- for each country in (group portfolio by Country): <scope>
        Foreach varName dataExpr scope -> do
            dataTree <- evalData varEnv dataExpr
            let termNodeTrees = collectTermNodeTrees dataTree
            boolList <- forM termNodeTrees $ \termNodeTree -> do
                let newVarEnv = insert varEnv varName termNodeTree
                evalRec newVarEnv scope
            return $ all (== True) boolList

        Rule ruleExpr -> do
            compRes <- evalComparisonGroup groupValueExpr (comparator bCompare) groupValue undefined
            whenJust (compareTrue compRes) rulePassed
            case compareFalse compRes of
                Nothing -> return True
                Just positions -> do
                    ruleViolated positions
                    return False
