{-# LANGUAGE ScopedTypeVariables #-}
module Eval.DataExpr
( eval
, evalGroupComparison
, evalGroupValueExpr
, runEvalData
, Position
, Tree
, EvalTree
, Result(..)
)
where

import LangPrelude
import Eval.Types
import Eval.Result
import Eval.Monad
import Tree
import Eval.GroupFold                       (getFold)
import Eval.Common                          (getField)
import Absyn


runEvalData :: NonEmpty Position -> DataExpr -> Either Text (EvalTree, [Result])
runEvalData portfolioPositions expr =
    runEvalM $ eval (initialVarEnv portfolioPositions) expr

eval
    :: Env EvalTree -- ^ let bindings
    -> DataExpr
    -> EvalM EvalTree
eval varEnv dataExpr =
    case dataExpr of
        GroupBy field input -> do
            evalTree <- eval varEnv input
            addGrouping field evalTree

        Filter comparison input -> do
            evalTree <- eval varEnv input
            forTermNodeM evalTree $ \tree -> do
                let [((fieldName, _), posList)] = collectTermNodes tree
                let newVarEnv = insert varEnv fieldName tree
                case comparison of
                    FilterGroup groupComparison -> do
                        res <- evalGroupComparison newVarEnv groupComparison
                        if res
                            then return posList
                            else return []
                    FilterPos posComparison -> do
                        let removeNothing = maybe False id
                        filterM (fmap removeNothing . evalPosComparison posComparison) posList

        Var name -> do
            let varNotFound = "Variable '" <> name <> "' not defined"
            maybe (fatalError varNotFound) return (lookup name varEnv)

evalPosComparison :: PosComparison -> Position -> EvalM (Maybe Bool)
evalPosComparison (PosComparison fieldName bCompare fieldValue) pos =
    let fCompare = comparator bCompare
    in fmap (`fCompare` fieldValue) <$> getField fieldName pos

-- Eval.GroupComparison
evalGroupComparison
    :: Env EvalTree
    -> GroupComparison
    -> EvalM Bool
evalGroupComparison env (GroupComparison valueIn bCompare valueExp) =
    let fCompare = comparator bCompare
        eval' = evalGroupValueExpr env
    in do
        rIn <- eval' valueIn
        rExp <- eval' valueExp
        return $ rIn `fCompare` rExp

-- Eval.GroupValueExpr
evalGroupValueExpr :: Env EvalTree -> GroupValueExpr -> EvalM GroupValue
evalGroupValueExpr varEnv expr =
    go expr
  where
    go (Literal groupValue) =
        return groupValue
    go (GroupFold groupFold fieldName dataExpr) = do
        dataTree <- Eval.DataExpr.eval varEnv dataExpr
        let foldFun = getFold groupFold fieldName
        foldFun (concat $ map snd $ collectTermNodes dataTree)
    go (RelativeComparison e1 e2) =
        let eval' = evalGroupValueExpr varEnv
        in do
            r1 <- eval' e1
            r2 <- eval' e2
            let (numerator, denominator) = case (r1,r2) of
                    (Count c1, Count c2)     -> (realToFrac c1, realToFrac c2)
                    (Sum d1, Sum d2)         -> (d1, d2)
                    (Percent d1, Percent d2) -> (d1, d2)
                    bad -> error $ "evalGroupValueExpr: invalid relative comparison: " ++ show bad
            return $ Percent (numerator * 100 / denominator)

