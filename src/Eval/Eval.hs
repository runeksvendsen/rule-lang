{-# LANGUAGE ScopedTypeVariables #-}
module Eval.Eval
( eval
, runEvalData
, Position
)
where

import LangPrelude
import Eval.Types
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
    runEvalM (nonEmpty $ initialScope) $ evalRec initialVarEnv expr
  where
    initialScopeData = nonEmpty $ LevelPos initialScope portfolioPositions
    initialScope = Level "Portfolio" (Json.String "")
    initialVarEnv = M.fromList [("portfolio", [initialScopeData])]

-- | TODO: document environment
evalRec
    :: Map Text [ScopeData] -- ^ Mapping of: variable names --> grouped data (environment)
    -> RuleExpr
    -> EvalM Bool
evalRec varEnv expr = undefined
    -- -- Used for logging a result that contains the current scope
    -- setCurrentScope (groupScope scopeData)
    -- case expr of
    --     And a b -> do
    --         -- NB: order of evaluation does not matter
    --         resA <- evalRec varEnv scopeData a
    --         resB <- evalRec varEnv scopeData b
    --         return (resA && resB)


    --     Let name rhs scope -> undefined
    --         -- evalRec (insert varEnv name rhs) scopeData scope

    --     Foreach var scope -> do
    --         let varNotFound = "Variable '" <> var <> "' doesn't exist"
    --         dataExprList <- maybe (fatalError varNotFound) return (lookup var varEnv)
    --         scopeDataList <- evalData scopeData dataExprList
    --         boolList <- forM scopeDataList (\scopeData' -> evalRec varEnv scopeData' scope)
    --         return $ all (== True) boolList

    --     Rule comparison -> do
    --         compRes <- evalComparison comparison scopeData
    --         whenJust (compareTrue compRes) rulePassed
    --         case compareFalse compRes of
    --             Nothing -> return True
    --             Just positions -> do
    --                 ruleViolated positions
    --                 return False

runEvalData :: NonEmpty Position -> DataExpr -> Either Text ([ScopeData], [Result])
runEvalData portfolioPositions expr =
    runEvalM (nonEmpty $ initialScope) $ evalData initialVarEnv expr
  where
    initialScopeData = nonEmpty $ LevelPos initialScope portfolioPositions
    initialScope = Level "Portfolio" (Json.String "")
    initialVarEnv = M.fromList [("portfolio", [initialScopeData])]

evalData
    :: Map Text [ScopeData] -- ^ Variables
    -> DataExpr
    -> EvalM [ScopeData]
evalData varEnv dataExpr =
    case dataExpr of
        GroupBy field input -> do
            scopeDataList <- evalData varEnv input
            fmap concat . forM scopeDataList $ \scopeData -> do
                newGrouping <- mkCurrentLevelGroupingM field (currentLevelPos scopeData)
                forM (M.toList newGrouping) $ \(fieldValue, positions) -> do
                    let newLevel = LevelPos (Level field fieldValue) positions
                    return (newLevel `cons` scopeData)

        Filter comparison input -> do
            scopeDataList <- evalData varEnv input
            fmap concat . forM scopeDataList $ \scopeData -> do
                compRes <- evalComparison comparison scopeData
                whenJust (compareFalse compRes) notConsidered
                case compareTrue compRes of
                    Nothing -> return []
                    Just positions -> do
                        let newCurrentLevel = (currentLevel scopeData) { lpPositions = positions }
                        return [replaceHead scopeData newCurrentLevel]

        Var name -> do
            let varNotFound = "Variable '" <> name <> "' not defined"
            scopeDataList <- maybe (fatalError varNotFound) return (lookup name varEnv)
            return scopeDataList


evalComparison
    :: Comparison
    -> ScopeData
    -> EvalM (ComparisonResult Position)
evalComparison (GroupComparison groupValueExpr bCompare groupValue) scopeData =
    evalComparisonGroup groupValueExpr (comparator bCompare) groupValue scopeData
evalComparison (PosComparison fieldName bCompare fieldValue) scopeData =
    evalComparisonPos fieldName (comparator bCompare) fieldValue (currentLevelPos scopeData)

evalComparisonGroup
    :: GroupValueExpr
    -> (GroupValue -> GroupValue -> Bool)
    -> GroupValue
    -> ScopeData
    -> EvalM (ComparisonResult Position)
evalComparisonGroup groupValueExpr fCompare expectedValue scopeData = do
    case groupValueExpr of
        CountDistinct fieldName -> do
            count <- evalCountDistinct currentLevelPositions fieldName
            return $ groupCompare count currentLevelPositions
        SumOver fieldName relativeToOpt -> do
            (positions, sumValue) <- evalSumOver scopeData fieldName undefined
            return $ groupCompare sumValue positions
  where
    currentLevelPositions = currentLevelPos scopeData
    groupCompare calculatedValue positions = do
        if fCompare calculatedValue expectedValue
            then ComparisonResult (Just positions) Nothing
            else ComparisonResult Nothing (Just positions)

evalComparisonPos
    :: FieldName
    -> (FieldValue -> FieldValue -> Bool)
    -> FieldValue
    -> NonEmpty Position
    -> EvalM (ComparisonResult Position)
evalComparisonPos fieldName fCompare expectedValue currentLevelPositions = do
    valueFields <- lookupFields fieldName currentLevelPositions
    return $ addManyResults (`fCompare` expectedValue) valueFields
  where
    addManyResults :: (FieldValue -> Bool) -> NonEmpty (Position, FieldValue) -> ComparisonResult Position
    addManyResults f =
        foldr testAdd (ComparisonResult Nothing Nothing)
      where
        testAdd :: (Position, FieldValue) -> ComparisonResult Position -> ComparisonResult Position
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
    -> EvalM (NonEmpty Position, GroupValue)
evalSumOver scopeData fieldName groupNameOpt = do
    (positions, sumValue) <- evalSum fieldName (currentLevelPos scopeData)
    addPositions positions =<< case groupNameOpt of
        Nothing -> return (Sum sumValue)    -- Absolute
        Just groupName -> do                -- Relative
            groupPositions <- lookupLevel scopeData groupName
            -- TODO:
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
    -> EvalM GroupValue
evalCountDistinct currentLevelPositions fieldName = do
    newGrouping <- mkCurrentLevelGroupingM fieldName currentLevelPositions
    let result = fromIntegral $ length $ M.keys newGrouping
    return $ Count result
