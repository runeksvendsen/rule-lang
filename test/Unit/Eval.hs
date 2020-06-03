module Unit.Eval
( main
)
where

import Prelude
import Absyn
import qualified Eval
import qualified Examples.Rules
import qualified Examples.Test.Pos

import Test.QuickCheck
import Control.Monad (forM_)


main :: IO ()
main =
    forM_ positionCounts $ \posCount ->
        forM_ Examples.Rules.allRules (testRule posCount)
  where
    positionCounts = [1, 10, 100, 1000, 10000]

testRule :: Int -> (Rule, String) -> IO ()
testRule numPositions (rule, name) = do
    testPositions <- generate $ vectorOf numPositions arbitrary
    let positions = map Examples.Test.Pos.toPos testPositions
    let lbl = "Rule " <> name <> ": (" <> show numPositions <> ")"
    let passed = Eval.eval (Eval.mkInitialEnv positions) rule
    -- We're not interested in whether an arbitrary set of positions passes a rule,
    --  rather, we just want to catch runtime errors in the evaluator.
    quickCheck $ label lbl $ passed || True
