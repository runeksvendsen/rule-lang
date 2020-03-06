{-# LANGUAGE ScopedTypeVariables #-}
module Eval.DataExpr
( eval
, runEvalData
, DataEnv
, Position
)
where

import LangPrelude
import Eval.Types
import Eval.Result
import Eval.Monad
import Tree
import Eval.Common                          (getField)
import AbsynFun

import qualified Data.HashMap.Strict        as M
import qualified Data.Aeson                 as Json
import qualified Data.List.NonEmpty         as NE


type DataEnv = Map Text EvalTree

runEvalData :: NonEmpty Position -> DataExpr -> Either Text (EvalTree, [Result])
runEvalData portfolioPositions expr =
    runEvalM (nonEmpty $ initialScope) $ eval initialVarEnv expr
  where
    -- initialScopeData = nonEmpty $ LevelPos initialScope portfolioPositions
    initialScope = Level "Portfolio" (Json.String "")
    initialVarEnv = M.fromList [("portfolio", initialTree)]
    initialTree = -- TODO: GroupBy "PortfolioName" --> Tree
        TermNode ("PortfolioName", "Test portfolio 123") (NE.toList portfolioPositions)

eval
    :: Env EvalTree -- ^ Variables
    -> DataExpr
    -> EvalM EvalTree
eval varEnv dataExpr =
    case dataExpr of
        GroupBy field input -> do
            evalTree <- eval varEnv input
            addGrouping field evalTree

        Filter comparison input -> do
            evalTree <- eval varEnv input
            let forMTermNode = flip mapMTermNode
            forMTermNode evalTree $ \posList ->
                return $ case comparison of
                    Right groupComparison ->
                        if evalGroupComparison varEnv groupComparison
                            then NE.toList posList
                            else []
                    Left posComparison ->
                        filter (evalPosComparison posComparison) (NE.toList posList)

        Var name -> do
            let varNotFound = "Variable '" <> name <> "' not defined"
            scopeDataList <- maybe (fatalError varNotFound) return (lookup name varEnv)
            return scopeDataList

evalPosComparison :: PosComparison -> Position -> Bool
evalPosComparison (PosComparison fieldName bCompare fieldValue) pos =
    let fCompare = comparator bCompare
    in getField fieldName pos `fCompare` fieldValue


-- Eval.GroupComparison
evalGroupComparison
    :: DataEnv
    -> GroupComparison
    -> Bool
evalGroupComparison env (GroupComparison valueIn bCompare valueExp) =
    let fCompare = comparator bCompare
        eval = evalGroupValueExpr env
    in eval valueIn `fCompare` eval valueExp

-- Eval.GroupValueExpr
evalGroupValueExpr :: DataEnv -> GroupValueExpr -> GroupValue
evalGroupValueExpr env expr =
    go expr
  where
    go (Literal groupValue) = groupValue
    go (GroupFold groupFold dataExpr) = undefined
    go (RelativeComparison a b) = undefined

