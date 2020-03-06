{-# LANGUAGE ScopedTypeVariables #-}
module Eval.DataExpr
( evalData
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
import qualified Eval.GroupComparison

import qualified Data.HashMap.Strict        as M
import qualified Data.Aeson                 as Json
import qualified Data.List.NonEmpty         as NE


type DataEnv = Map Text EvalTree

runEvalData :: NonEmpty Position -> DataExpr -> Either Text (EvalTree, [Result])
runEvalData portfolioPositions expr =
    runEvalM (nonEmpty $ initialScope) $ evalData initialVarEnv expr
  where
    -- initialScopeData = nonEmpty $ LevelPos initialScope portfolioPositions
    initialScope = Level "Portfolio" (Json.String "")
    initialVarEnv = M.fromList [("portfolio", initialTree)]
    initialTree = -- TODO: GroupBy "PortfolioName" --> Tree
        TermNode ("PortfolioName", "Test portfolio 123") (NE.toList portfolioPositions)

evalData
    :: DataEnv -- ^ Variables
    -> DataExpr
    -> EvalM EvalTree
evalData varEnv dataExpr =
    case dataExpr of
        GroupBy field input -> do
            evalTree <- evalData varEnv input
            addGrouping field evalTree

        Filter comparison input -> do
            evalTree <- evalData varEnv input
            let forMTermNode = flip mapMTermNode
            forMTermNode evalTree $ \posList ->
                return $ case comparison of
                    Right groupComparison ->
                        if Eval.GroupComparison.eval varEnv groupComparison
                            then posList
                            else []
                    Left posComparison ->
                        filter (evalPos posComparison) posList

        Var name -> do
            let varNotFound = "Variable '" <> name <> "' not defined"
            scopeDataList <- maybe (fatalError varNotFound) return (lookup name varEnv)
            return scopeDataList

evalPos (PosComparison fieldName bCompare fieldValue) pos =
    let fCompare = comparator bCompare
    in getField fieldName pos `fCompare` fieldValue
