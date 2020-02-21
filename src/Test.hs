module Test where

import Prelude
import Absyn
import Syntax

-- 35-30-6
thirtyfiveThirtySix :: RuleExpr a
thirtyfiveThirtySix =
    forEach "IssuerName" $:
        where' (sumOf value (relativeTo "Portfolio")) Gt (Percent 35) $:
            rule (numberOf issue) GtE (Count 6)
            +++
            forEach issue |:
                rule (sumOf value (relativeTo "Portfolio")) LtE (Percent 30)

value = "DirtyValueTotalRC"
issue = "SecurityID"

{-
    for each Issuer:
        where Value of Issuer (relative to portfolio) > 35%:
            rule number of distinct SecurityID (at Portfolio level) >= 6
            for each Issue (group by SecurityID):
                rule Value of Issue (relative to Portfolio value) <= 30%
-}

absyn =
    GroupBy "IssuerName" $
        Filter (Comparison (GroupValueExpr (SumOver value (Just "Portfolio"))) Gt (Percent 35)) $ Just $ Both
            (Rule $ Comparison (GroupValueExpr (CountDistinct "SecurityID")) GtE (Count 6))
            (GroupBy "SecurityID" $
                (Rule $ Comparison (GroupValueExpr (SumOver value (Just "Portfolio"))) LtE (Percent 30))
            )
