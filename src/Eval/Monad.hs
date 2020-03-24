module Eval.Monad
( EvalM
, EvalTree
, runEvalM
, initialVarEnv
, initialScope
, fatalError
, throwLeft
, setCurrentScope
, logResult
, addResults
, rulePassed
, ruleViolated
, notConsidered
, addGrouping
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
import qualified Data.HashMap.Strict                    as M
import qualified Data.Aeson                             as Json
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except             as E
import qualified Control.Monad.Trans.State.Strict       as S
import qualified Control.Monad.Trans.Writer.Strict      as W


-- ### Types
type EvalM = S.StateT GroupScope (W.WriterT [R.Result] (E.Except Text))

-- ### Runners
runEvalM :: EvalM a -> Either Text (a, [R.Result])
runEvalM m =
    E.runExcept . W.runWriterT $ S.evalStateT m (nonEmpty initialScope)

initialVarEnv :: NonEmpty a -> HashMap Text (Tree [a])
initialVarEnv portfolioPositions =
    M.fromList [("portfolio", initialTree)]
  where
    initialTree = -- TODO: GroupBy "PortfolioName" --> Tree
        TermNode ("PortfolioName", "Test portfolio 123") (NE.toList portfolioPositions)

initialScope :: Level
initialScope = Level "Portfolio" (Json.String "")

-- ### Helper functions
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

-- Group positions in a TermNode.
-- A TermNode is transformed into a Node that
--  contains a set of TermNodes (one for each created group).
addGrouping
    :: FieldName
    -> EvalTree
    -> EvalM EvalTree
addGrouping fieldName tree =
    go tree
  where
    mkTermNode :: (FieldValue, NonEmpty Position) -> EvalTree
    mkTermNode (fieldValue, positions) = TermNode (fieldName, fieldValue) (NE.toList positions)
    go (Node lab subTree) = do
        newSubTree <- mapM go subTree
        return $ Node lab newSubTree
    go tn@(TermNode _ []) = return tn
    go (TermNode lab posList) = do
        grouping <- mkCurrentLevelGroupingM fieldName (NE.fromList posList)
        return $ Node lab (map mkTermNode (M.toList grouping))


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

lookupLevel :: ScopeData -> FieldName -> EvalM (NonEmpty Position)
lookupLevel scopeData groupName =
    lookupLevel' groupName (NE.toList scopeData)

lookupLevel' :: FieldName -> [LevelPos] -> EvalM (NonEmpty Position)
lookupLevel' groupName [] = fatalError $ "Grouping '" <> groupName <> "' doesn't exist"
lookupLevel' groupName (LevelPos (Level groupName' _) positions : levelPositions)
    | groupName == groupName' = return positions
    | otherwise = lookupLevel' groupName levelPositions

