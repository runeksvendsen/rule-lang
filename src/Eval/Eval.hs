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
import Tree
import Eval.DataExpr
import AbsynFun

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
    -- initialScopeData = nonEmpty $ LevelPos initialScope portfolioPositions
    initialScope = Level "Portfolio" (Json.String "")
    initialVarEnv = undefined -- M.fromList [("portfolio", portfolioPositions)]

type DataEnv = Map Text EvalTree


evalRec
    :: DataEnv
    -> RuleExpr
    -> EvalM Bool
evalRec varEnv expr =
    -- Used for logging a result that contains the current scope
    -- setCurrentScope (groupScope scopeData)
    case expr of
        And a b -> do
            -- NB: order of evaluation does not matter
            resA <- evalRec varEnv a
            resB <- evalRec varEnv b
            return (resA && resB)

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

        Rule (groupValueExpr, bCompare, groupValue) dataExpr -> do
            compRes <- evalComparisonGroup groupValueExpr (comparator bCompare) groupValue undefined
            whenJust (compareTrue compRes) rulePassed
            case compareFalse compRes of
                Nothing -> return True
                Just positions -> do
                    ruleViolated positions
                    return False


-- input `compareFun` expected
------------------------------
-- input            :: a
-- expected         :: b
-- groupValueExpr   :: a -> b
-- fieldName        ::
-- compareFun       :: b -> b -> Bool



-- groupCompare :: [a] -> b -> Bool
-- posCompare   ::  b  -> b -> Bool
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
            (positions, sumValue) <- evalSumOver scopeData fieldName relativeToOpt
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
    -> Maybe DataExpr
    -> EvalM (NonEmpty Position, GroupValue)
evalSumOver scopeData fieldName groupNameOpt = do
    (positions, sumValue) <- evalSum fieldName (currentLevelPos scopeData)
    addPositions positions =<< case groupNameOpt of
        Nothing -> return (Sum sumValue)    -- Absolute
        Just dataExpr -> do                -- Relative
            -- groupPositions <- evalData varEnv dataExpr
            groupPositions <- lookupLevel scopeData undefined

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
