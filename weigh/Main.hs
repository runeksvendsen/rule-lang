{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Main where

import Prelude
import Absyn (Rule)
import qualified Eval
import qualified Examples.Rules
import qualified Examples.Test.Pos

import Test.QuickCheck (generate, vectorOf, arbitrary)
import Data.List (group, sort)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import System.IO (hFlush, stdout)
import qualified Weigh


main :: IO ()
main = do
    logInfo "Generating test data ... "
    posLists <- evaluate . force =<< mapM genPos counts
    logInfo "done!\n"
    Weigh.mainWith $ do
        Weigh.setColumns [Weigh.Case, Weigh.Allocated, Weigh.Max, Weigh.Live, Weigh.GCs]
        Weigh.wgroup "Rules" $ do
            weighRule "Rule I"   Examples.Test.Pos.issuer Examples.Rules.ruleI posLists
            weighRule "Rule Ia"  Examples.Test.Pos.issuer Examples.Rules.ruleIa posLists
            weighRule "Rule II"  issuerIssue Examples.Rules.ruleII posLists
            weighRule "Rule III" issuerIssue Examples.Rules.ruleIII posLists
            weighRule "Rule IV"  Examples.Test.Pos.counterparty Examples.Rules.ruleIV posLists
            weighRule "Rule V"   Examples.Test.Pos.securityID Examples.Rules.ruleV posLists
            weighRule "Rule VI"  Examples.Test.Pos.country Examples.Rules.ruleVI posLists
  where
    logInfo str = putStr str >> hFlush stdout
    genPos :: Int -> IO [Examples.Test.Pos.TestPosition]
    genPos count = generate (vectorOf count arbitrary)
    counts = map (\n -> n * 100000) [1..10]
    issuerIssue p = (Examples.Test.Pos.issuer p, Examples.Test.Pos.issue p)

weighRule
    :: Ord a
    => String
    -> (Examples.Test.Pos.TestPosition -> a)
    -> Rule
    -> [[Examples.Test.Pos.TestPosition]]
    -> Weigh.Weigh ()
weighRule name f rule posLists =
    Weigh.wgroup name $ mapM_ mkWeigh (posLists)
  where
    eval env = Eval.eval env rule
    mkLabel posList = show (countUnique posList) <> "/" <> show (length posList)
    mkEnv posList = Eval.mkInitialEnv (map Examples.Test.Pos.toPos posList)
    mkWeigh posList = Weigh.func (mkLabel posList) eval (mkEnv posList)
    countUnique = length . group . sort . map f

