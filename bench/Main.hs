{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Main where

import Prelude
import qualified Examples.Test.Pos
import qualified Eval
import qualified Examples.Rules

import qualified Criterion.Main as Crit
import Test.QuickCheck (generate, vectorOf, arbitrary)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import System.IO (hFlush, stdout)


main :: IO ()
main = do
    logInfo "Generating test data ... "
    envList <- evaluate . force =<< mapM genEnv counts
    logInfo "done!\n"
    Crit.defaultMain
      [ Crit.bgroup "Rule I"   $ map (mkBench Examples.Rules.ruleI) envList
      , Crit.bgroup "Rule Ia"  $ map (mkBench Examples.Rules.ruleIa) envList
      , Crit.bgroup "Rule II"  $ map (mkBench Examples.Rules.ruleII) envList
      , Crit.bgroup "Rule III" $ map (mkBench Examples.Rules.ruleIII) envList
      , Crit.bgroup "Rule IV"  $ map (mkBench Examples.Rules.ruleIV) envList
      , Crit.bgroup "Rule V"   $ map (mkBench Examples.Rules.ruleV) envList
      , Crit.bgroup "Rule VI"  $ map (mkBench Examples.Rules.ruleVI) envList
      ]
  where
    logInfo str = putStr str >> hFlush stdout
    counts = map (* 10000) [1..10]
    mkBench rule (env, count) = Crit.bench (show count) $ Crit.nf (flip Eval.eval $ rule) env
    mkEnv posList = Eval.mkInitialEnv (map Examples.Test.Pos.toPos posList)
    genEnv count = do
        posList <- generate (vectorOf count arbitrary)
        return (mkEnv posList, count)
