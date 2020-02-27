{-# LANGUAGE ScopedTypeVariables #-}
module Eval.Eval
( eval
, Position
)
where

import LangPrelude
import Eval.Types
import Eval.Env
import Eval.Result
import Eval.Monad
import Absyn

import qualified Data.HashMap.Strict        as M
import qualified Data.Aeson                 as Json
import qualified Data.List.NonEmpty         as NE


-- | The boolean value indicates the following:
--      * True: no data contradicts the rule
--      * False: some data contradicts the rule
--
-- E.g. if every single position in the portfolio is missing data
--  required to determine whether a rule is broken the result will
--  be True.
--
-- In short:
--  True = "rule not violated", False = "rule violated".
eval :: NonEmpty Position -> RuleExpr -> Either Text (Bool, [Result])
eval portfolioPositions expr =
    runEvalM portfolioPositions $ evalRec emptyMap expr

evalRec
    :: Env RuleExpr     -- Variables
    -> RuleExpr
    -> EvalM Bool
evalRec varEnv expr =
    case expr of
    And a b -> do
        -- NB: order of evaluation does not matter
        resA <- evalRec varEnv a
        resB <- evalRec varEnv b
        return (resA && resB)

    Let name rhs scopeExpr ->
        evalRec (insert varEnv name rhs) scopeExpr

    Var var ->
        let varNotFound = "Variable '" <> var <> "' doesn't exist"
        in maybe (fatalError varNotFound) (evalRec varEnv) (lookup var varEnv)

    GroupBy field scopeExpr -> do
        newGrouping <- mkCurrentLevelGroupingM field (lookup field)
        boolList <- forM (M.toList newGrouping) $ \(fieldValue, positions) -> do
            withLevel (LevelPos (Level field fieldValue) positions) $
                evalRec varEnv scopeExpr
        return $ all (== True) boolList

    Filter comparison scopeExpr -> do
        compRes <- evalComparison comparison
        whenJust (compareFalse compRes) notConsidered
        -- if there are any positions in "compareTrue":
        --      then: evaluate "scopeExpr" for these positions
        --      else: just return True
        whenJustOr (compareTrue compRes) True $ \positions -> do
            replaceCurrentLevelPos positions
            evalRec varEnv scopeExpr

    Rule comparison -> do
        compRes <- evalComparison comparison
        whenJust (compareTrue compRes) rulePassed
        -- if there are any positions in "compareFalse":
        --      then: add these positions as "violated rule" and return "False"
        --      else: just return True
        whenJustOr (compareFalse compRes) True $ \positions -> do
            ruleViolated positions
            return False


evalComparison
    :: Comparison
    -> EvalM (ComparisonResult Position)
evalComparison (Comparison valueExpr bCompare value) =
    let fCompare = comparator bCompare in
    case valueExpr of
        GroupValueExpr groupValueExpr ->
            evalComparisonGroup groupValueExpr fCompare value
        PosValueExpr posValueExpr ->
            evalComparisonPos posValueExpr fCompare value

evalComparisonGroup
    :: GroupValueExpr
    -> (Value -> Value -> Bool)
    -> Value
    -> EvalM (ComparisonResult Position)
evalComparisonGroup groupValueExpr fCompare value = do
    case groupValueExpr of
        CountDistinct fieldName -> do
            (positions, count) <- evalCountDistinct fieldName
            return $ groupCompare count positions
        SumOver fieldName groupNameOpt -> do
            (positions, sumValue) <- evalSumOver fieldName groupNameOpt
            return $ groupCompare sumValue positions
  where
    groupCompare calculatedValue positions = do
        if fCompare calculatedValue value
            then ComparisonResult (Just positions) Nothing
            else ComparisonResult Nothing (Just positions)

evalComparisonPos
    :: PosValueExpr
    -> (t -> Value -> Bool)
    -> t
    -> EvalM (ComparisonResult Position)
evalComparisonPos posValueExpr fCompare value = do
    case posValueExpr of
        Get fieldName -> do
            currentLevelPositions <- currentLevelPosM
            valueFields <- lookupFields fieldName currentLevelPositions
            return $ addManyResults (fCompare value) (NE.map (fmap Field) valueFields)
  where
    addManyResults :: (Value -> Bool) -> NonEmpty (Position, Value) -> ComparisonResult Position
    addManyResults f =
        foldr testAdd (ComparisonResult Nothing Nothing)
      where
        testAdd :: (Position, Value) -> ComparisonResult Position -> ComparisonResult Position
        testAdd (item, testVal) cr =
            if f testVal
                then cr { compareTrue  = consMaybeNE item (compareTrue cr) }
                else cr { compareFalse = consMaybeNE item (compareFalse cr) }

data ComparisonResult a = ComparisonResult
    { compareTrue   :: Maybe (NonEmpty a)
    , compareFalse  :: Maybe (NonEmpty a)
    }

evalSumOver
    :: FieldName
    -> Maybe GroupName
    -> EvalM (NonEmpty Position, Value)
evalSumOver fieldName groupNameOpt = do
    (positions, sumValue) <- evalSum fieldName =<< currentLevelPosM
    addPositions positions =<< case groupNameOpt of
        Nothing -> return (Sum sumValue)    -- Absolute
        Just groupName -> do                -- Relative
            groupPositions <- lookupLevel groupName
            groupSumValue <- snd <$> evalSum fieldName groupPositions
            return $ Percent (sumValue * 100 / groupSumValue)
  where
    addPositions positions value = return (positions, value)

-- TODO: prevent runtime failure
evalSum :: FieldName -> NonEmpty Position -> EvalM (NonEmpty Position, Double)
evalSum fieldName positions = do
    jsonFields <- lookupFields fieldName positions
    sumValue <$> mapM pairToDouble jsonFields
  where
    sumValue :: NonEmpty (Position, Double) -> (NonEmpty Position, Double)
    sumValue posValueList =
        let unzipped = NE.unzip posValueList
        in (fst unzipped, sum (snd unzipped))
    pairToDouble :: (a, Json.Value) -> EvalM (a, Double)
    pairToDouble (a, val) = toDouble val >>= \double -> return (a, double)
    toDouble :: Json.Value -> EvalM Double
    toDouble (Json.Number s) = return (realToFrac s)
    toDouble v = fatalError $ "sum: expected Number for '" <> fieldName <> "' found: " <> toS (show v)

evalCountDistinct
    :: FieldName
    -> EvalM (NonEmpty Position, Value)
evalCountDistinct fieldName = do
    currentLevelPositions <- currentLevelPosM
    newGrouping <- mkCurrentLevelGroupingM fieldName (lookup fieldName)
    let result = fromIntegral $ length $ M.keys newGrouping
    return (currentLevelPositions, Count result)
