{-# LANGUAGE ScopedTypeVariables #-}
module Eval.Eval
( eval
)
where

import LangPrelude
import Eval.Types
import Eval.Env
import Eval.Result
import Eval.Monad
import Absyn

import qualified Data.HashMap.Strict        as Map
import qualified Data.Aeson                 as Json
import qualified Data.List.NonEmpty         as NE


eval :: DataExpr a -> NonEmpty Position -> Either Text [Result]
eval expr portfolioPositions =
    runEvalM $ evalRec (nonEmpty initialLevel) emptyMap expr
  where
    initialLevel = LevelPos (Level "Portfolio" (Json.String "")) portfolioPositions

evalRec
    :: NonEmpty LevelPos    -- Current level/group and previous levels/groups
    -> Env (DataExpr a)     -- Variables
    -> DataExpr a
    -> EvalM ()
evalRec levels varEnv expr = case expr of
    Both a b ->
        let evalCurrentScope = evalRec levels varEnv
        in do
            evalCurrentScope a
            res2 <- throwLeft $ runEvalM $ evalCurrentScope b
            addResults res2

    Let name rhs scope ->
        evalRec levels (insert varEnv name rhs) scope

    Var var ->
        let varNotFound = "Variable '" <> var <> "' doesn't exist"
        in maybe (fatalError varNotFound) (evalRec levels varEnv) (lookup var varEnv)

    GroupBy field scope -> do
        newGrouping <- mkGroupingM field (lookup field) (currentLevelPos levels)
        forM_ (Map.toList newGrouping) $ \(fieldValue, positions) -> do
            let newLevelPos = LevelPos (Level field fieldValue) positions
            enterScope (lpLevel newLevelPos) -- Used to associate results with the current level
            evalRec (newLevelPos `cons` levels) varEnv scope

    Filter _          Nothing      -> error "Not implemented"
    Filter comparison (Just fExpr) -> do
        compRes <- evalComparison levels comparison
        whenJust (compareFalse compRes) notConsidered
        whenJust (compareTrue compRes) $ \positions -> do
            let newCurrentLevel = (currentLevel levels) { lpPositions = positions }
            evalRec (replaceHead levels newCurrentLevel) varEnv fExpr

    Rule comparison -> do
        compRes <- evalComparison levels comparison
        whenJust (compareFalse compRes) ruleViolated
        whenJust (compareTrue compRes) rulePassed


evalComparison
    :: -- | Levels/groups
       NonEmpty LevelPos
    -> Comparison
    -> EvalM (ComparisonResult Position)
evalComparison levels (Comparison valueExpr fCompare value) = do
    case valueExpr of
        GroupValueExpr (CountDistinct fieldName) -> do
            (positions, count) <- evalCountDistinct (currentLevelPos levels) fieldName
            return $ groupCompare count positions
        GroupValueExpr (SumOver fieldName groupNameOpt) -> do
            (positions, sumValue) <- evalSumOver levels fieldName groupNameOpt
            return $ groupCompare sumValue positions
        PosValueExpr (Get fieldName) -> do
            valueFields <- NE.map (fmap Field) <$> lookupFields fieldName (currentLevelPos levels)
            return $ addManyResults (fCompare value) valueFields
  where
    groupCompare calculatedValue positions = do
        if fCompare calculatedValue value
            then ComparisonResult (Just positions) Nothing
            else ComparisonResult Nothing (Just positions)
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
    :: NonEmpty LevelPos
    -> FieldName
    -> Maybe GroupName
    -> EvalM (NonEmpty Position, Value)
evalSumOver levels fieldName groupNameOpt = do
    (positions, sumValue) <- evalSum (currentLevelPos levels) fieldName
    addPositions positions =<< case groupNameOpt of
        Nothing -> return (Sum sumValue)    -- Absolute
        Just groupName -> do                -- Relative
            groupPositions <- lookupLevel groupName levels
            groupSumValue <- snd <$> evalSum groupPositions fieldName
            return $ Percent (sumValue * 100 / groupSumValue)
  where
    addPositions positions value = return (positions, value)

-- TODO: prevent runtime failure
evalSum :: NonEmpty Position -> FieldName -> EvalM (NonEmpty Position, Double)
evalSum currentLevelPos fieldName = do
    jsonFields <- lookupFields fieldName currentLevelPos
    sumValue <$> mapM pairToDouble jsonFields
  where
    sumValue :: NonEmpty (Position, Double) -> (NonEmpty Position, Double)
    sumValue posValueList = let unzipped = NE.unzip posValueList in (fst unzipped, sum (snd unzipped))
    pairToDouble :: (a, Json.Value) -> EvalM (a, Double)
    pairToDouble (a, val) = toDouble val >>= \double -> return (a, double)
    toDouble :: Json.Value -> EvalM Double
    toDouble (Json.Number s) = return (realToFrac s)
    toDouble v = fatalError $ "sum: expected Number for '" <> fieldName <> "' found: " <> toS (show v)

evalCountDistinct
    :: NonEmpty Position
    -> FieldName
    -> EvalM (NonEmpty Position, Value)
evalCountDistinct currentLevelPos fieldName = do
    newGrouping <- mkGroupingM fieldName (lookup fieldName) currentLevelPos
    let result = fromIntegral $ length $ Map.keys newGrouping
    return (currentLevelPos, Count result)
