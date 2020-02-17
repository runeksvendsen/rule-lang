module Eval.Monad
( EvalM
, runEvalM
, fatalError
, throwLeft
, addResult
, addPassed
, addFailed
, addIgnored
, addDataMiss
, mkGroupingM
  -- * Re-exports
, R.Result
)
where

import LangPrelude
import Eval.Types
import qualified Eval.Result                            as R
import qualified Eval.Grouping                          as G

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except             as E
import qualified Control.Monad.Trans.State.Strict       as S


type EvalM = S.StateT (R.Result Position) (E.Except Text)


runEvalM :: EvalM () -> Either Text (R.Result Position)
runEvalM m = fmap snd . E.runExcept $ S.runStateT m mempty

fatalError :: Text -> EvalM a
fatalError = throwLeft . Left

throwLeft :: Either Text a -> EvalM a
throwLeft = lift . E.except

addResult :: R.Result Position -> EvalM ()
addResult a = S.modify (<> a)

addPassed :: [Position] -> EvalM ()
addPassed a = S.modify (`R.addPassed` a)

addFailed :: [Position] -> EvalM ()
addFailed a = S.modify (`R.addFailed` a)

addIgnored :: [Position] -> EvalM ()
addIgnored a = S.modify (`R.addIgnored` a)

addDataMiss :: (Text, [Position]) -> EvalM ()
addDataMiss a = S.modify (`R.addDataMiss` a)

mkGroupingM fieldName f values =
    let (dataMisses, grouping) = G.mkGroupingMaybe f values
    in do addDataMiss (fieldName, dataMisses)
          return grouping
