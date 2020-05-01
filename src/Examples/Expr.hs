{-# LANGUAGE QuasiQuotes #-}

{- Code examples from the report -}
module Examples.Expr
( allExpressions
)
where

import LangPrelude
import qualified Absyn
import QuasiQuote (rulelangExpr)


allExpressions :: [(Absyn.Expr, String)]
allExpressions =
    [ (groupingExpressionPos, "Abstract syntax/Grouping expression (position comparison)")
    , (groupingExpressionGroup, "Abstract syntax/Grouping expression (group comparison)")
    ]

groupingExpressionPos :: Absyn.Expr
groupingExpressionPos =
    [rulelangExpr|Portfolio where (.InstrumentType == "OTC") grouped by .Counterparty
    |]

groupingExpressionGroup :: Absyn.Expr
groupingExpressionGroup =
    [rulelangExpr|Portfolio grouped by Country where (sum .Value of Country relative to Portfolio >= 10%)
    |]
