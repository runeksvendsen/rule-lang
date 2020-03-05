{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import LangPrelude
import Absyn
import Data.List
import qualified Data.Text as T
--TEST
import qualified Rules.FiveTenForty         as Rule2

main :: IO ()
main = putStrLn $ toS $ Pretty.pp "  " Rule2.ruleExpr

pp :: Text -> RuleExpr -> Text
pp indentation expr' =
    T.unlines
    . map (\(level, line) -> T.concat (replicate (fromIntegral level) indentation) <> line)
        $ go 0 expr'
  where
    go :: Word -> RuleExpr -> [(Word, Text)]
    go level (And a b) = go level a <> go level b
    go level (Let name rhs scope) =
        (level, "let " <> (name :: Text) <> " = " <> ppData rhs)
            : go level scope
    go level (Foreach dataExpr scope) =
        (level, "for all " <> ppData dataExpr <> ":")
        : go (level+1) scope
    go level (Rule groupComparison) =
        [(level, "rule: " <> ppGroupComparison groupComparison)]

ppData :: DataExpr -> Text
ppData =
    T.concat . intersperse " " . go
  where
    go :: DataExpr -> [Text]
    go (Var varName) = [varName]
    go (GroupBy fieldName dataExpr) =
        go dataExpr ++ ["grouped by " <> fieldName]
    go (Filter filterComparison dataExpr) =
        go dataExpr ++ ["where: " <> ppFilterComparison filterComparison]


ppGroupValue :: GroupValue -> Text
ppGroupValue (Count count) = show' count
ppGroupValue (Sum sum') = show' sum'
ppGroupValue (Percent percent) = show' percent <> "%"

ppGroupValueExpr :: GroupValueExpr -> Text
ppGroupValueExpr (Literal groupValue) =
    ppGroupValue groupValue
ppGroupValueExpr (GroupFold groupFold fieldName dataExpr) =
    T.unwords [ppGroupFold groupFold, fieldName, "of", ppData dataExpr]
ppGroupValueExpr (RelativeComparison e1 e2) =
    T.unwords [ppGroupValueExpr e1, "relative to", ppGroupValueExpr e2]

ppGroupFold :: GroupFold -> Text
ppGroupFold CountDistinct = "count"
ppGroupFold SumOver = "sum over"
ppGroupFold Average = "average"
ppGroupFold Max = "maximum"
ppGroupFold Min = "minimum"

ppGroupComparison :: GroupComparison -> Text
ppGroupComparison (GroupComparison e1 bCompare e2) =
    T.unwords [ppGroupValueExpr e1, show' bCompare, ppGroupValueExpr e2]

ppPosComparison :: PosComparison -> Text
ppPosComparison (PosComparison fieldName bCompare fieldValue) =
    T.unwords [fieldName, show' bCompare, showValue fieldValue]

ppFilterComparison :: FilterComparison -> Text
ppFilterComparison (FilterGroup groupComparison) =
    ppGroupComparison groupComparison
ppFilterComparison (FilterPos posComparison) =
    ppPosComparison posComparison
