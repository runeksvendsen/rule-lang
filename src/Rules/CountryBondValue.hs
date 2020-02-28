{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Rules.CountryBondValue
( ruleExpr
)
where

import Absyn
import Rules.Syntax


ruleExpr :: RuleExpr
ruleExpr =
    where' (forall "InstrumentType" Eq "Bond") $:
        forEach "Country" $:
            forEach "IssuerName" $:
                rule (sumOf value (relativeTo "Country") LtE (Percent 5))

value = "DirtyValueTotalRC"
