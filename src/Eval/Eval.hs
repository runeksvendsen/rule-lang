module Eval.Eval where

import LangPrelude
import Eval.Types
-- import Eval.Result
import Eval.Grouping
import Eval.Monad
import Absyn

import           Control.Monad
import           Control.Error
import qualified Data.HashMap.Strict        as Map
import qualified Data.Aeson                 as Json
import qualified Data.List.NonEmpty         as NE



-- eval :: DataExpr a -> NonEmpty Position -> Either Text (Result Position)
-- eval expr portfolioPositions =
--     evalRec mempty ("Portfolio", portfolioPositions) emptyMap emptyMap expr

evalRec
    :: GroupName                            -- Current group name
    -> Env (Json.Value, NonEmpty Position)  -- groupName -> (groupKey, positions)
    -> Env (DataExpr a)                     -- Variables
    -> DataExpr a
    -> EvalM ()
evalRec currentGroupName groups varEnv expr = case expr of
    Both a b -> do
           let evalCurrentScope = evalRec currentGroupName groups varEnv
           evalCurrentScope a
           res2 <- throwLeft $ runEvalM $ evalCurrentScope b
           addResult res2
    Let name rhs scope ->
        evalRec currentGroupName groups (insert varEnv name rhs) scope
    Var var ->
        let varNotFound = "Variable '" <> var <> "' doesn't exist"
        in maybe (fatalError varNotFound) (evalRec currentGroupName groups varEnv) (lookup var varEnv)
    GroupBy field scope ->
        let Just (currentGroupKey, currentGroupPositions) = lookup currentGroupName groups
            eval' (fieldValue, positions) =
                evalRec field (insert groups field (fieldValue, positions)) varEnv scope
        in do
            newGrouping <- mkGroupingM field (lookup field) currentGroupPositions
            mapM_ eval' (Map.toList newGrouping)
    Filter comparison exprOpt -> error "Not implemented"
    Rule comparison -> error "Not implemented"


evalComparison
    :: GroupName
    -> Env (Json.Value, NonEmpty Position)
    -> Comparison
    -> EvalM ()
evalComparison currentGroupName groups (Comparison valueExpr fCompare value) = do
    case valueExpr of
        CountDistinct fieldName groupName ->
            evalCountDistinct currentGroup fieldName groupName >>= compareM (`fCompare` value)
        Forall fieldName ->
            currentPositionsField currentGroup fieldName >>= mapM_ (\(pos,val) -> compareM (`fCompare` value) ([pos], Field val))
        SumOver fieldName groupName groupNameOpt -> do
            undefined

  where
    compareM test (positions, value) = do
        if test value
            then addPassed positions
            else addFailed positions

evalSumOver currentGroupName fieldName groupNameOpt = do
    sumValue <- evalSum currentGroup fieldName
    return $ case groupNameOpt of
        Nothing -> Field sumValue
        Just groupName -> do
            undefined

evalSum currentGroupName fieldName = do
    positions <- currentPositionsField currentGroup fieldName
    sumNumber (map snd positions)
  where
    sumNumber = foldM addNumber (Json.Number 0)
    -- TODO: prevent runtime failure
    addNumber (Json.Number v1) (Json.Number v2) = return $ Json.Number (v1+v2)
    addNumber (Json.Number _) f2 = throwError $ "sum: invalid field: " ++ show f2
    addNumber f1 (Json.Number _) = throwError $ "sum: invalid field: " ++ show f1
    addNumber f1 f2 = throwError $ "sum: invalid fields: " ++ show (f1,f2)
    throwError str = fatalError (toS str)

evalCountDistinct (currentGroupName, currentGroupPositions) fieldName groupName
    | groupName /= currentGroupName = fatalError $ "Grouping '" <> groupName <> "' not found"
    | otherwise = do
        newGrouping <- mkGroupingM fieldName (lookup fieldName) currentGroupPositions
        let result = fromIntegral $ length $ Map.keys newGrouping
        return (NE.toList currentGroupPositions, Count result)

currentPositionsField (_, currentGroupPositions) fieldName = do
    let fieldValuesE :: [Either Position (Position, Json.Value)]
        fieldValuesE = NE.toList $ NE.map lookupField currentGroupPositions
        lookupField pos = maybe (Left pos) (\v -> Right (pos, v)) (lookup fieldName pos)
        dataMisses = lefts fieldValuesE
    when (not $ null dataMisses) $
        addDataMiss (fieldName, dataMisses)
    return $ rights fieldValuesE





-- 35-30-6
{-
    for each Issuer:
        where value Issuer > 35%:
            count distinct SecurityID >= 6
            for each SecurityID:
                value SecurityID <= 30%
-}
