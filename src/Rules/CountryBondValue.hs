{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Rules.CountryBondValue
( ruleExpr
)
where

import LangPrelude
import AbsynFun
import Rules.Syntax


ruleExpr :: RuleExpr
ruleExpr = undefined
    -- let' "countryIssuers" [ where' (forall "InstrumentType" Eq "Bond")
    --                       , GroupBy "Country"
    --                       , GroupBy "IssuerName"
    --                       ] $:
    -- forEach "countryIssuers" $:
    --     rule (sumOf value (relativeTo "Country") LtEq (Percent 5))

value = "DirtyValueTotalRC"
