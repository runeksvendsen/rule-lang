module Eval.Monad
( EvalM
, runEvalM
, fatalError
, throwLeft
, withLevel
, currentLevelPosM
, portfolioPosM
, replaceCurrentLevelPos
, addResults
, rulePassed
, ruleViolated
, notConsidered
, mkCurrentLevelGroupingM
, lookupField
, lookupFields
, lookupLevel
  -- * Re-exports
, R.Result
)
where

import LangPrelude
import Absyn
import Eval.Types
import qualified Eval.Result                            as R
import qualified Eval.Grouping                          as G

import qualified Data.Aeson                             as Json
import qualified Data.List.NonEmpty                     as NE
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except             as E
import qualified Control.Monad.Trans.State.Strict       as S
import qualified Control.Monad.Trans.Writer.Strict      as W


type EvalM = S.StateT (NonEmpty LevelPos) (W.WriterT [R.Result] (E.Except Text))

runEvalM :: NonEmpty Position -> EvalM a -> Either Text (a, [R.Result])
runEvalM portfolioPositions m =
    E.runExcept . W.runWriterT $ S.evalStateT m (nonEmpty initialLevel)
  where
    initialLevel = LevelPos (Level "Portfolio" (Json.String "")) portfolioPositions

fatalError :: Text -> EvalM a
fatalError = throwLeft . Left

throwLeft :: Either Text a -> EvalM a
throwLeft = lift . lift . E.except

allLevelsM :: EvalM (NonEmpty LevelPos)
allLevelsM = S.get

-- TODO: replace with "withLevel" function
withLevel :: LevelPos -> EvalM a -> EvalM a
withLevel lp action = do
    enterLevel lp
    res <- action
    exitCurrentLevel
    return res

enterLevel :: LevelPos -> EvalM ()
enterLevel lp = S.modify (lp `cons`) -- (\levels -> logString levels `trace` (lp `cons` levels))
  where
    logString levels = unlines
      [ "Entering level: " ++ show (lpLevel lp)
      , "levels: " ++ concat (NE.intersperse "," (NE.map (show . lpLevel) levels))
      ]

exitCurrentLevel :: EvalM ()
exitCurrentLevel =
    S.modify removeLevel
  where
    errorMessage = "BUG: 'exitCurrentLevel' at Portfolio level"
    removeLevel =
        fromMaybe (error errorMessage) . NE.nonEmpty . NE.tail -- $ logString levels `trace` levels
    logString levels = unlines
      [ "Exiting from levels: " ++ concat (NE.intersperse "," (NE.map (show . lpLevel) levels))
      , ""
      ]

currentLevelPosM :: EvalM (NonEmpty Position)
currentLevelPosM = currentLevelPos <$> S.get

-- | Get Portfolio-level positions (may be filtered)
portfolioPosM :: EvalM (NonEmpty Position)
portfolioPosM = lpPositions . NE.last <$> S.get

-- | Used when filtering
replaceCurrentLevelPos :: NonEmpty Position -> EvalM ()
replaceCurrentLevelPos positions =
    S.modify (\levels -> replaceHead levels (newCurrentLevel levels))
  where
    newCurrentLevel levels = (currentLevel levels) { lpPositions = positions }

logResult :: R.ResultStatus -> NonEmpty Position -> EvalM ()
logResult status positions = do
    scope <- S.get
    addResults [R.Result positions (NE.map lpLevel scope) status]

addResults :: [R.Result] -> EvalM ()
addResults = lift . W.tell

rulePassed :: NonEmpty Position -> EvalM ()
rulePassed = logResult R.RulePassed

ruleViolated :: NonEmpty Position -> EvalM ()
ruleViolated = logResult R.RuleViolated

notConsidered :: NonEmpty Position -> EvalM ()
notConsidered = logResult R.NotConsidered

mkCurrentLevelGroupingM
    :: Groupable fieldValue
    => FieldName
    -> (Position -> Maybe fieldValue)
    -> EvalM (Map fieldValue (NonEmpty Position))
mkCurrentLevelGroupingM fieldName f = do
    currentLevelPositions <- currentLevelPosM
    let (dataMissesM, grouping) = G.mkGroupingMaybe f currentLevelPositions
    whenJust dataMissesM $ logResult (R.MissingField fieldName)
    return grouping

lookupField :: FieldName -> Position -> EvalM (Maybe FieldValue)
lookupField fieldName pos =
  case lookup fieldName pos of
      Nothing -> logResult (R.MissingField fieldName) (nonEmpty pos) >> return Nothing
      Just value -> return (Just value)

lookupFields :: FieldName -> NonEmpty Position -> EvalM (NonEmpty (Position, FieldValue))
lookupFields fieldName positions = do
    results <- forM positions (\pos -> addPos pos <$> lookupField fieldName pos)
    let finalResultM = NE.nonEmpty . catMaybes . NE.toList $ results
    case finalResultM of
        Nothing -> fatalError $ "No positions with field name '" <> fieldName <> "'"
        Just nonEmpty -> return nonEmpty
  where
    addPos pos valueM = fmap (\val -> (pos, val)) valueM

lookupLevel :: GroupName -> EvalM (NonEmpty Position)
lookupLevel groupName = do
    currentLevel <- allLevelsM
    lookupLevel' groupName (NE.toList currentLevel)

lookupLevel' :: GroupName -> [LevelPos] -> EvalM (NonEmpty Position)
lookupLevel' groupName [] = fatalError $ "Grouping '" <> groupName <> "' doesn't exist"
lookupLevel' groupName (LevelPos (Level groupName' _) positions : levelPositions)
    | groupName == groupName' = return positions
    | otherwise = lookupLevel' groupName levelPositions

