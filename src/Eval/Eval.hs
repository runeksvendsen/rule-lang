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
    runEvalM (nonEmpty initialScope) $ evalRec emptyMap initialScopeData expr
  where
    initialScopeData = nonEmpty $ LevelPos initialScope portfolioPositions
    initialScope = Level "Portfolio" (Json.String "")

evalRec
    :: Env RuleExpr     -- Variables
    -> ScopeData
    -> RuleExpr
    -> EvalM Bool
evalRec varEnv scopeData expr = do
    -- Used for logging a result that contains the current scope
    setCurrentScope (groupScope scopeData)
    case expr of
        Both a b -> do
            -- NB: order of evaluation does not matter
            resA <- evalRec varEnv scopeData a
            resB <- evalRec varEnv scopeData b
            return (resA && resB)

        Let name rhs scope ->
            evalRec (insert varEnv name rhs) scopeData scope

        Var var ->
            let varNotFound = "Variable '" <> var <> "' doesn't exist"
            in maybe (fatalError varNotFound) (evalRec varEnv scopeData) (lookup var varEnv)

        GroupBy field scope -> do
            newGrouping <- mkCurrentLevelGroupingM field (currentLevelPos scopeData)
            boolList <- forM (M.toList newGrouping) $ \(fieldValue, positions) -> do
                let newLevel = LevelPos (Level field fieldValue) positions
                evalRec varEnv (newLevel `cons` scopeData) scope
            return $ all (== True) boolList

        Filter _          Nothing      -> error "Not implemented"
        Filter comparison (Just fExpr) -> do
            compRes <- evalComparison comparison scopeData
            whenJust (compareFalse compRes) notConsidered
            whenJustOr (compareTrue compRes) True $ \positions -> do
                let newCurrentLevel = (currentLevel scopeData) { lpPositions = positions }
                evalRec varEnv (replaceHead scopeData newCurrentLevel) fExpr

        Rule comparison -> do
            compRes <- evalComparison comparison scopeData
            whenJust (compareTrue compRes) rulePassed
            -- if there are any positions in "compareFalse":
            --      then: add these positions as "violated rule" and return "False"
            --      else: just return True
            whenJustOr (compareFalse compRes) True $ \positions -> do
                ruleViolated positions
                return False

evalComparison
    :: Comparison
    -> ScopeData
    -> EvalM (ComparisonResult Position)
evalComparison (Comparison valueExpr bCompare value) scopeData =
    let fCompare = comparator bCompare in
    case valueExpr of
        GroupValueExpr groupValueExpr ->
            evalComparisonGroup groupValueExpr fCompare value scopeData
        PosValueExpr posValueExpr ->
            evalComparisonPos posValueExpr fCompare value (currentLevelPos scopeData)

evalComparisonGroup
    :: GroupValueExpr
    -> (Value -> Value -> Bool)
    -> Value
    -> ScopeData
    -> EvalM (ComparisonResult Position)
evalComparisonGroup groupValueExpr fCompare value scopeData = do
    case groupValueExpr of
        CountDistinct fieldName -> do
            count <- evalCountDistinct currentLevelPositions fieldName
            return $ groupCompare count currentLevelPositions
        SumOver fieldName groupNameOpt -> do
            (positions, sumValue) <- evalSumOver scopeData fieldName groupNameOpt
            return $ groupCompare sumValue positions
  where
    currentLevelPositions = currentLevelPos scopeData
    groupCompare calculatedValue positions = do
        if fCompare calculatedValue value
            then ComparisonResult (Just positions) Nothing
            else ComparisonResult Nothing (Just positions)

evalComparisonPos
    :: PosValueExpr
    -> (t -> Value -> Bool)
    -> t
    -> NonEmpty Position
    -> EvalM (ComparisonResult Position)
evalComparisonPos posValueExpr fCompare value currentLevelPositions = do
    case posValueExpr of
        Get fieldName -> do
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
    :: ScopeData
    -> FieldName
    -> Maybe GroupName
    -> EvalM (NonEmpty Position, Value)
evalSumOver scopeData fieldName groupNameOpt = do
    (positions, sumValue) <- evalSum fieldName (currentLevelPos scopeData)
    addPositions positions =<< case groupNameOpt of
        Nothing -> return (Sum sumValue)    -- Absolute
        Just groupName -> do                -- Relative
            groupPositions <- lookupLevel scopeData groupName
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

-- |
evalCountDistinct
    :: NonEmpty Position    -- ^ Current level positions
    -> FieldName
    -> EvalM Value
evalCountDistinct currentLevelPositions fieldName = do
    newGrouping <- mkCurrentLevelGroupingM fieldName currentLevelPositions
    let result = fromIntegral $ length $ M.keys newGrouping
    return $ Count result
