module Test where

import Prelude
import Absyn
import Syntax

-- 35-30-6
thirtyfiveThirtySix :: RuleExpr a
thirtyfiveThirtySix =
    forEach "IssuerName" $:
        where' (sumOf value (relativeTo "Portfolio")) (>) (Percent 35) $:
            rule (numberOf issue) (>=) (Count 6)
            +++
            forEach issue |:
                rule (sumOf value (relativeTo "Portfolio")) (<=) (Percent 30)
  where
    value = "DirtyValueTotalRC"
    issue = "SecurityID"

{-
    for each Issuer:
        where Value of Issuer (relative to portfolio) > 35%:
            rule number of distinct SecurityID (at Portfolio level) >= 6
            for each Issue (group by SecurityID):
                rule Value of Issue (relative to Portfolio value) <= 30%
-}
