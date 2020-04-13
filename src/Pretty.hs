{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import LangPrelude
import Absyn

import Data.List
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE


pp :: Text -> NE.NonEmpty RuleExpr -> Text
pp indentation exprList =
    T.unlines
    . map (\(level, line) -> T.concat (replicate (fromIntegral level) indentation) <> line)
        $ foldr (go 0) [] exprList
  where
    go :: Word -> RuleExpr -> [(Word, Text)] -> [(Word, Text)]
    go level (Let name rhs) accum =
        (level, "let " <> (name :: Text) <> " = " <> ppVarExpr rhs) : accum
    go level (Foreach dataExpr scope) accum =
        (level, "forall " <> ppVarOr ppDataExpr dataExpr <> " {")
        : foldr (go (level+1)) [] scope ++ [(level, "}")] ++ accum
    go level (If boolExpr scope) accum =
        (level, T.unwords ["if", ppVarOr ppBoolExpr boolExpr, "{"])
        : foldr (go (level+1)) [] scope ++ [(level, "}")] ++ accum
    go level (Rule boolExpr) accum =
        (level, T.unwords ["require", ppVarOr ppBoolExpr boolExpr]) : accum

ppDataExpr :: DataExpr -> Text
ppDataExpr =
    T.unwords . go
  where
    go :: DataExpr -> [Text]
    go (GroupBy fieldName dataExpr) =
        [ppVarOr ppDataExpr dataExpr, "grouped by", ppVarOr ppFieldName fieldName]
    go (Filter boolExpr dataExpr) =
        [ppVarOr ppDataExpr dataExpr, "where", parenthesize $ ppBoolExpr boolExpr]

ppVarOr :: (t -> Text) -> VarOr t -> Text
ppVarOr _ (Var var) = var
ppVarOr ppFun (NotVar a) = ppFun a

ppVarExpr :: VarExpr -> Text
ppVarExpr (ValueExpr valueExpr) = ppValueExpr valueExpr
ppVarExpr (DataExpr dataExpr) = ppDataExpr dataExpr
ppVarExpr (BoolExpr boolExpr) = ppBoolExpr boolExpr

ppValueExpr :: ValueExpr -> Text
ppValueExpr (GroupOp groupOp) = ppGroupOp groupOp
ppValueExpr (Literal lit) = ppLiteral lit

ppGroupOp :: GroupOp -> Text
ppGroupOp (GroupCount dataExpr) = "count " <> ppVarOr ppDataExpr dataExpr
ppGroupOp (PositionFold positionFold fieldName dataExpr relativeM) = T.unwords $
    [ ppPositionFold positionFold
    , ppVarOr ppFieldName fieldName
    , "of"
    , ppVarOr ppDataExpr dataExpr
    ] ++
    maybe [] (\relativeData -> ["relative to", ppVarOr ppDataExpr relativeData]) relativeM

ppPositionFold :: PositionFold -> Text
ppPositionFold SumOver = "sum"
ppPositionFold Average = "average"
ppPositionFold Max = "maximum"
ppPositionFold Min = "minimum"

ppBoolExpr :: BoolExpr -> Text
ppBoolExpr (Comparison e1 bCompare e2) =
    ppComparison e1 bCompare e2
ppBoolExpr (And e1 e2) = parenthesize $ T.unwords
    [ ppVarOr ppBoolExpr e1
    , "AND"
    , ppVarOr ppBoolExpr e2
    ]
ppBoolExpr (Or e1 e2) = parenthesize $ T.unwords
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
        , fromMaybe (error $ "BUG: 'valueToString': " ++ show bCompare) $
            Data.List.lookup bCompare valueToString
        , ppVarOr ppValueExpr e2
        ]

ppFieldName :: Text -> Text
ppFieldName text = "." <> text

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
    let string = show' (realToFrac num :: Double)
    -- Remove (optional) trailing ".0"
    in fromMaybe string $ T.stripSuffix ".0" string




-- ######### Helpers

parenthesize :: Text -> Text
parenthesize txt = "(" <> txt <> ")"
