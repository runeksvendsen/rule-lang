{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Main where

import Prelude
import qualified Examples.Test.Pos
import qualified Eval
import qualified Examples.Rules

import qualified Criterion.Main as Crit
import Test.QuickCheck (generate, vectorOf, arbitrary)
import Data.List (group, sort)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import System.IO (hFlush, stdout)


main :: IO ()
main = do
    logInfo "Generating test data ... "
    posLists <- evaluate . force =<< mapM genPos counts
    logInfo "done!\n"
    Crit.defaultMain
      [ Crit.bgroup "Rule I"   $ map (mkBench Examples.Test.Pos.issuer Examples.Rules.ruleI) posLists
      , Crit.bgroup "Rule Ia"  $ map (mkBench Examples.Test.Pos.issuer Examples.Rules.ruleIa) posLists
      , Crit.bgroup "Rule II"  $ map (mkBench issuerIssue Examples.Rules.ruleII) posLists
      , Crit.bgroup "Rule III" $ map (mkBench issuerIssue Examples.Rules.ruleIII) posLists
      , Crit.bgroup "Rule IV"  $ map (mkBench Examples.Test.Pos.counterparty Examples.Rules.ruleIV) posLists
      , Crit.bgroup "Rule V"   $ map (mkBench Examples.Test.Pos.securityID Examples.Rules.ruleV) posLists
      , Crit.bgroup "Rule VI"  $ map (mkBench Examples.Test.Pos.country Examples.Rules.ruleVI) posLists
      ]
  where
    logInfo str = putStr str >> hFlush stdout
    genPos :: Int -> IO [Examples.Test.Pos.TestPosition]
    genPos count = generate (vectorOf count arbitrary)
    counts = map (\n -> 100000 + n * 10000) [1..10]
    eval rule env = Eval.eval env rule
    mkLabel f posList = show (countUnique f posList) <> "/" <> show (length posList)
    mkEnv posList = Eval.mkInitialEnv (map Examples.Test.Pos.toPos posList)
    mkBench f rule (force -> !posList) = Crit.bench (mkLabel f posList) (Crit.nf (eval rule) (mkEnv posList))
    countUnique f = fromIntegral . length . group . sort . map f
    issuerIssue p = (Examples.Test.Pos.issuer p, Examples.Test.Pos.issue p)
