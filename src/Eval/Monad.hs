module Eval.Monad
( EvalM
, runEvalM
, fatalError
, throwLeft
, enterScope
, addResults
, rulePassed
, ruleViolated
, notConsidered
, mkGroupingM
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


type EvalM = S.StateT [R.Level] (W.WriterT [R.Result] (E.Except Text))

runEvalM :: EvalM () -> Either Text [R.Result]
runEvalM m = E.runExcept . W.execWriterT $ S.runStateT m []

fatalError :: Text -> EvalM a
fatalError = throwLeft . Left

throwLeft :: Either Text a -> EvalM a
throwLeft = lift . lift . E.except

enterScope :: R.Level -> EvalM ()
enterScope l = S.modify (l :)

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

mkGroupingM
    :: Groupable fieldValue
    => FieldName
    -> (Position -> Maybe fieldValue)
    -> NonEmpty Position
    -> EvalM (Map fieldValue (NonEmpty Position))
mkGroupingM fieldName f values =
    let (dataMissesM, grouping) = G.mkGroupingMaybe f values
    in do maybe (return ()) (logResult (R.MissingField fieldName)) dataMissesM
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

lookupLevel :: GroupName -> NonEmpty LevelPos -> EvalM (NonEmpty Position)
lookupLevel groupName = lookupLevel' groupName . NE.toList

lookupLevel' :: GroupName -> [LevelPos] -> EvalM (NonEmpty Position)
lookupLevel' groupName [] = fatalError $ "Grouping '" <> groupName <> "' doesn't exist"
lookupLevel' groupName (LevelPos (Level groupName' _) positions : levelPositions)
    | groupName == groupName' = return positions
    | otherwise = lookupLevel' groupName levelPositions

