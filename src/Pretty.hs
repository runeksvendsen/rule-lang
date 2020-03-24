{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import LangPrelude
import Absyn

import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T


pp :: Text -> RuleExpr -> Text
pp indentation expr' =
    T.unlines
    . map (\(level, line) -> T.concat (replicate (fromIntegral level) indentation) <> line)
        $ go 0 expr'
  where
    go :: Word -> RuleExpr -> [(Word, Text)]
    go level (And a b) = go level a <> concat (map (go level) (NE.toList b))
    go level (Let name rhs scope) =
        (level, "let " <> (name :: Text) <> " = " <> ppGroupValueExpr rhs)
            : go level scope
    go level (Foreach fieldName dataExpr scope) =
        (level, "for each " <> ppGroupValueExpr fieldName <> " in " <> ppGroupValueExpr dataExpr <> " {")
        : go (level+1) scope ++ [(level, "}")]
    go level (Rule groupComparison) =
        [(level, "rule: " <> ppComparison groupComparison)]

ppDataExpr :: DataExpr -> Text
ppDataExpr =
    T.unwords . go
  where
    go :: DataExpr -> [Text]
    go (GroupBy fieldName dataExpr) =
        [ppGroupValueExpr dataExpr, "grouped by", ppGroupValueExpr fieldName]
    go (Filter filterComparison dataExpr) =
        [ppGroupValueExpr dataExpr, "where", ppComparison filterComparison]


ppLiteral :: Literal -> Text
ppLiteral (Integer count) = show' count
ppLiteral (Percent num) = show' num <> "%"
ppLiteral (FieldName text) = text
ppLiteral (FieldValue fieldValue) = ppFieldValue fieldValue


ppFieldValue :: FieldValue -> Text
ppFieldValue (Number num) = show' num
ppFieldValue (String str) = "\"" <> str <> "\""
ppFieldValue (Bool b) = show' b

ppGroupValueExpr :: GroupValueExpr -> Text
ppGroupValueExpr (GroupOp groupOp) = ppGroupOp groupOp
ppGroupValueExpr (DataExpr dataExpr) = ppDataExpr dataExpr
ppGroupValueExpr (Var name) = name
ppGroupValueExpr (Literal lit) = ppLiteral lit

ppGroupOp :: GroupOp -> Text
ppGroupOp (GroupCount groupValueExpr) = "count " <> ppGroupValueExpr groupValueExpr
ppGroupOp (PositionFold positionFold fieldName groupValueExpr) =
    T.unwords [ppPositionFold positionFold, ppGroupValueExpr fieldName, "of", ppGroupValueExpr groupValueExpr]
ppGroupOp (Relative e1 e2) =
    T.unwords [ppGroupValueExpr e1, "relative to", ppGroupValueExpr e2]

ppPositionFold :: PositionFold -> Text
ppPositionFold SumOver = "sum"
ppPositionFold Average = "average"
ppPositionFold Max = "maximum"
ppPositionFold Min = "minimum"

ppComparison :: Comparison -> Text
ppComparison (Comparison e1 bCompare e2) =
    T.unwords
        [ ppGroupValueExpr e1
        , toS $ fromMaybe (error $ "BUG: 'valueToString': " ++ show bCompare) $
            Data.List.lookup bCompare valueToString
        , ppGroupValueExpr e2
        ]
