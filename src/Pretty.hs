{-# LANGUAGE OverloadedStrings #-}
module Pretty
( pp
, ppLines
, ppExpr
)
where

import LangPrelude
import Absyn

import Data.List
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE


pp indentation = T.unlines . ppLines indentation

-- ppLines :: Text -> NE.NonEmpty RuleExpr -> Text
ppLines indentation exprList =
    map (\(level, line) -> T.concat (replicate (fromIntegral level) indentation) <> line)
        $ foldr (go 0) [] exprList
  where
    go :: Word -> RuleExpr -> [(Word, Text)] -> [(Word, Text)]
    go level (Let name rhs) accum =
        (level, "let " <> (name :: Text) <> " = " <> ppExpr rhs) : accum
    go level (Forall dataExpr scope) accum =
        (level, "forall " <> ppExpr dataExpr <> " {")
        : foldr (go (level+1)) [] scope ++ [(level, "}")] ++ accum
    go level (If boolExpr scope) accum =
        (level, T.unwords ["if", ppExpr boolExpr, "{"])
        : foldr (go (level+1)) [] scope ++ [(level, "}")] ++ accum
    go level (Rule boolExpr) accum =
        (level, T.unwords ["require", ppExpr boolExpr]) : accum

ppExpr :: Expr -> Text
ppExpr (Literal lit) = ppLiteral lit
ppExpr (Var txt) = txt
ppExpr (ValueExpr valueExpr) = ppValueExpr valueExpr
ppExpr (DataExpr dataExpr) = ppDataExpr dataExpr
ppExpr (BoolExpr boolExpr) = ppBoolExpr boolExpr
ppExpr (Map fieldName dataExpr) = T.unwords [ppExpr fieldName, "of", ppExpr dataExpr]

ppDataExpr :: DataExpr -> Text
ppDataExpr =
    T.unwords . go
  where
    go :: DataExpr -> [Text]
    go (GroupBy dataExpr fieldName) =
        [ppExpr dataExpr, "grouped by", ppExpr fieldName]
    go (Filter dataExpr boolExpr) =
        [ppExpr dataExpr, "where", multiWordParens $ ppExpr boolExpr]

ppValueExpr :: ValueExpr -> Text
ppValueExpr (GroupCount dataExpr) = "count " <> ppExpr dataExpr
ppValueExpr (FoldMap positionFold mapExpr) = T.unwords $
    [ ppPositionFold positionFold
    , ppExpr mapExpr
    ]
ppValueExpr (Relative e1 e2) = T.unwords
    [ multiWordParens $ ppExpr e1, "relative to", multiWordParens $ ppExpr e2 ]

ppPositionFold :: Fold -> Text
ppPositionFold Sum = "sum"
ppPositionFold Average = "average"
ppPositionFold Max = "maximum"
ppPositionFold Min = "minimum"

ppBoolExpr :: BoolExpr -> Text
ppBoolExpr (Comparison e1 bCompare e2) =
    ppComparison e1 bCompare e2
ppBoolExpr (And e1 e2) = T.unwords
    [ multiWordParens $ ppExpr e1
    , "AND"
    , multiWordParens $ ppExpr e2
    ]
ppBoolExpr (Or e1 e2) = T.unwords
    [ ppExpr e1
    , "OR"
    , ppExpr e2
    ]
ppBoolExpr (Not expr) = T.unwords
    ["NOT", ppExpr expr]

ppComparison :: Expr -> BoolCompare -> Expr -> Text
ppComparison e1 bCompare e2 =
    T.unwords
        [ ppExpr e1
        , fromMaybe (error $ "BUG: 'valueToString': " ++ show bCompare) $
            Data.List.lookup bCompare valueToString
        , multiWordParens $ ppExpr e2
        ]

ppFieldName :: FieldName -> Text
ppFieldName fieldName = "." <> toS fieldName

ppLiteral :: Literal -> Text
ppLiteral (Percent num) = ppNumber num <> "%"
ppLiteral (FieldName text) = ppFieldName text
ppLiteral (FieldValue fieldValue) = ppFieldValue fieldValue

ppFieldValue :: FieldValue -> Text
ppFieldValue (Number num) = ppNumber num
ppFieldValue (String str) = "\"" <> str <> "\""
ppFieldValue (Bool True) = "true"
ppFieldValue (Bool False) = "false"

ppNumber :: Number -> Text
ppNumber num =
    let numStr = show' (realToFrac num :: Double)
    -- Remove (optional) trailing ".0"
    in fromMaybe numStr $ T.stripSuffix ".0" numStr

-- ######### Helpers

parenthesize :: Text -> Text
parenthesize txt = "(" <> txt <> ")"

multiWordParens :: Text -> Text
multiWordParens txt =
    if length (T.splitOn " " txt) > 1
        then parenthesize txt
        else txt
