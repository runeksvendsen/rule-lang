{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Rules.ThirtyfiveSixtySix
( ruleExpr
)
where

import LangPrelude
import AbsynFun
import Rules.Syntax

-- 35-30-6
{-
    number of distinct SecurityID (at Portfolio level) >= 6
    for each Issuer:
        where Value of Issuer (relative to portfolio) > 35%:
            for each Issue (group by SecurityID):
                Value of Issue (relative to Portfolio value) <= 30%
-}
ruleExpr :: RuleExpr
ruleExpr = undefined
    -- let issuerNameValue = [GroupBy "IssuerName", where' (sumOf value (relativeTo "Portfolio") Gt (Percent 35))] in
    -- let' "issuerGt35Pct" issuerNameValue $:
    -- let' "issues" (issuerNameValue ++ [GroupBy issue]) $:
    -- forEach "issuerGt35Pct" $:
    --     rule (numberOf issue GtEq (Count 6))
    --     +++
    --     forEach "issues" |:
    --         rule (sumOf value (relativeTo "Portfolio") LtEq (Percent 30))

    -- forEach "IssuerName" $:
    --     where' (sumOf value (relativeTo "Portfolio") Gt (Percent 35)) $:
    --         rule (numberOf issue GtEq (Count 6))
    --         +++
    --         forEach issue |:
    --                 rule (sumOf value (relativeTo "Portfolio") LtEq (Percent 30))

value = "DirtyValueTotalRC"
issue = "SecurityID"
