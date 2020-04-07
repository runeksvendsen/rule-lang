{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import LangPrelude
import Absyn

import Data.List
import qualified Data.Text as T


pp :: Text -> RuleExpr -> Text
pp indentation expr' =
    T.unlines
    . map (\(level, line) -> T.concat (replicate (fromIntegral level) indentation) <> line)
        $ go 0 expr'
  where
    go :: Word -> RuleExpr -> [(Word, Text)]
    go level (Let name rhs scope) =
        (level, "let " <> (name :: Text) <> " = " <> ppVarOr ppValueExpr rhs)
            : go level scope
    go level (Foreach fieldName dataExpr scope) =
        (level, "for each " <> ppVarOr ppFieldName fieldName <> " in " <> ppDataExpr dataExpr <> " {")
        : go (level+1) scope ++ [(level, "}")]
    go level (Rule boolExpr) =
        [(level, "rule: " <> ppVarOr ppBoolExpr boolExpr)]

ppDataExpr :: DataExpr -> Text
ppDataExpr =
    T.unwords . go
  where
    go :: DataExpr -> [Text]
    go (GroupBy fieldName dataExpr) =
        [ppDataExpr dataExpr, "grouped by", ppVarOr ppFieldName fieldName]
    go (Filter boolExpr dataExpr) =
        [ppDataExpr dataExpr, "where", ppBoolExpr boolExpr]


ppVarOr :: (t -> Text) -> VarOr t -> Text
ppVarOr _ (Var var) = var
ppVarOr ppFun (NotVar a) = ppFun a

ppLiteral :: Literal -> Text
ppLiteral (Integer count) = show' count
ppLiteral (Percent num) = show' (realToFrac num :: Double) <> "%"
ppLiteral (FieldName text) = ppFieldName text
ppLiteral (FieldValue fieldValue) = ppFieldValue fieldValue

ppFieldName text = text

ppFieldValue :: FieldValue -> Text
ppFieldValue (Number num) = show' num
ppFieldValue (String str) = "\"" <> str <> "\""
ppFieldValue (Bool b) = show' b

ppValueExpr :: ValueExpr -> Text
ppValueExpr (GroupOp groupOp) = ppGroupOp groupOp
ppValueExpr (DataExpr dataExpr) = ppDataExpr dataExpr
ppValueExpr (Literal lit) = ppLiteral lit

ppGroupOp :: GroupOp -> Text
ppGroupOp (GroupCount dataExpr) = "count " <> ppDataExpr dataExpr
ppGroupOp (PositionFold positionFold fieldName dataExpr) =
    T.unwords [ppPositionFold positionFold, ppVarOr ppFieldName fieldName, "of", ppDataExpr dataExpr]
ppGroupOp (Relative e1 e2) =
    T.unwords [ppVarOr ppGroupOp e1, "relative to", ppVarOr ppGroupOp e2]

ppPositionFold :: PositionFold -> Text
ppPositionFold SumOver = "sum"
ppPositionFold Average = "average"
ppPositionFold Max = "maximum"
ppPositionFold Min = "minimum"

ppBoolExpr :: BoolExpr -> Text
ppBoolExpr (Comparison e1 bCompare e2) =
    ppComparison e1 bCompare e2
ppBoolExpr (And e1 e2) = T.unwords
    [ ppVarOr ppBoolExpr e1
    , "AND"
    , ppVarOr ppBoolExpr e2
    ]
ppBoolExpr (Or e1 e2) = T.unwords
    [ ppVarOr ppBoolExpr e1
    , "OR"
    , ppVarOr ppBoolExpr e2
    ]
ppBoolExpr (Not expr) = T.unwords
    ["NOT", ppVarOr ppBoolExpr expr]

ppComparison :: VarOr ValueExpr -> BoolCompare -> VarOr ValueExpr -> Text
ppComparison e1 bCompare e2 =
    T.unwords
        [ ppVarOr ppValueExpr e1
        , toS $ fromMaybe (error $ "BUG: 'valueToString': " ++ show bCompare) $
            Data.List.lookup bCompare valueToString
        , ppVarOr ppValueExpr e2
        ]
