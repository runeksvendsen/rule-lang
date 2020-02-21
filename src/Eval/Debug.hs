module Eval.Debug where

import LangPrelude
import Eval.Types
import qualified Control.Monad.Trans.State.Strict       as S
import Debug.Trace

debugPrint expr evalM = do
    levels <- fmap lpLevel <$> S.get
    unlines [ "Evaluating: " ++ show expr
            , "Levels: " ++ show levels
            ] `trace` evalM

