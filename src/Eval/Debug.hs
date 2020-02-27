module Eval.Debug where

import LangPrelude
import Absyn
import Eval.Monad
import qualified Control.Monad.Trans.State.Strict       as S
import Debug.Trace

debugPrint :: RuleExpr -> EvalM a -> EvalM a
debugPrint expr evalM = do
    levels <- S.get
    unlines [ "Evaluating: " ++ show expr
            , "Levels: " ++ show levels
            ] `trace` evalM

