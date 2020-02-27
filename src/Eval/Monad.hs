module Eval.Monad
( EvalM
, runEvalM
, fatalError
, throwLeft
, setCurrentScope
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

import qualified Data.List.NonEmpty                     as NE
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except             as E
import qualified Control.Monad.Trans.State.Strict       as S
import qualified Control.Monad.Trans.Writer.Strict      as W


type EvalM = S.StateT GroupScope (W.WriterT [R.Result] (E.Except Text))

runEvalM :: GroupScope -> EvalM a -> Either Text (a, [R.Result])
runEvalM initialScope m =
    E.runExcept . W.runWriterT $ S.evalStateT m initialScope

fatalError :: Text -> EvalM a
fatalError = throwLeft . Left

throwLeft :: Either Text a -> EvalM a
throwLeft = lift . lift . E.except

-- Used for logging with a reference to the current scope
setCurrentScope :: GroupScope -> EvalM ()
setCurrentScope = S.put

logResult :: R.ResultStatus -> NonEmpty Position -> EvalM ()
logResult status positions = do
    scope <- S.get
    addResults [R.Result positions scope status]

addResults :: [R.Result] -> EvalM ()
addResults = lift . W.tell

rulePassed :: NonEmpty Position -> EvalM ()
rulePassed = logResult R.RulePassed

ruleViolated :: NonEmpty Position -> EvalM ()
ruleViolated = logResult R.RuleViolated

notConsidered :: NonEmpty Position -> EvalM ()
notConsidered = logResult R.NotConsidered

mkCurrentLevelGroupingM
    :: FieldName
    -> NonEmpty Position
    -> EvalM (Map FieldValue (NonEmpty Position))
mkCurrentLevelGroupingM fieldName currentLevelPositions = do
    let (dataMissesM, grouping) = G.mkGroupingMaybe (lookup fieldName) currentLevelPositions
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
        Just posValues -> return posValues
  where
    addPos pos valueM = fmap (\val -> (pos, val)) valueM

lookupLevel :: ScopeData -> GroupName -> EvalM (NonEmpty Position)
lookupLevel scopeData groupName =
    lookupLevel' groupName (NE.toList scopeData)

lookupLevel' :: GroupName -> [LevelPos] -> EvalM (NonEmpty Position)
lookupLevel' groupName [] = fatalError $ "Grouping '" <> groupName <> "' doesn't exist"
lookupLevel' groupName (LevelPos (Level groupName' _) positions : levelPositions)
    | groupName == groupName' = return positions
    | otherwise = lookupLevel' groupName levelPositions

