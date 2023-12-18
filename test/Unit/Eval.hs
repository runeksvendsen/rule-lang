module Unit.Eval
( main
)
where

import Prelude
import Absyn
import qualified Eval
import qualified Examples.Rules
import qualified Examples.Test.Pos
import Test.Tasty

import Test.QuickCheck
import Data.Functor ((<&>))
import Test.Tasty.QuickCheck  as QC


main :: IO ()
main = defaultMain $
    testGroup "Eval: example rules" $ concat $
        positionCounts <&> \posCount ->
            Examples.Rules.allRules <&> \r@(_, name) -> 
                QC.testProperty name $ testRule posCount r
  where
    positionCounts = [1, 10, 100, 1000, 10000]

testRule :: Int -> (Rule, String) -> Gen Property
testRule numPositions (rule, name) = do
    testPositions <- vectorOf numPositions arbitrary
    let positions = map Examples.Test.Pos.toPos testPositions
    let lbl = "Rule " <> name <> ": (" <> show numPositions <> ")"
    let passed = Eval.eval (Eval.mkInitialEnv positions) rule
    -- We're not interested in whether an arbitrary set of positions passes a rule,
    --  rather, we just want to catch runtime errors in the evaluator.
    let passed' = passed || True
    pure $ property $ label lbl passed'
