{-# LANGUAGE CPP, BangPatterns #-}
module Main where

import Prelude
import qualified Examples.Test.Pos
import qualified Eval
import qualified Examples.Rules

import Criterion.Main
import Test.QuickCheck (generate, vectorOf, arbitrary)


main :: IO ()
main =
    defaultMain
      [
        bgroup "Rule I" $
          map (mkBench ruleI) benchmarkIterations
      ]
  where
    ruleI = fst $ head Examples.Rules.allRules
    benchmarkIterations = map (\n -> n * 100000) [1..10]
    mkBench rule count =
        let eval' env' = Eval.eval env' rule
        in env (mkEnv count) (\env' -> bench (show count) $ nf eval' env')

mkEnv numPositions = do
    testPositions <- generate $ vectorOf numPositions arbitrary
    return $ Eval.mkInitialEnv (map Examples.Test.Pos.toPos testPositions)
