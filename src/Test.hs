{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test where

import Absyn
import Syntax

-- 35-30-6
{-
    for each Issuer:
        where Value of Issuer (relative to portfolio) > 35%:
            number of distinct SecurityID (at Portfolio level) >= 6
            for each Issue (group by SecurityID):
                Value of Issue (relative to Portfolio value) <= 30%
-}
thirtyfiveThirtySix :: RuleExpr
thirtyfiveThirtySix =
    forEach "IssuerName" $:
        where' (sumOf value (relativeTo "Portfolio")) Gt (Percent 35) $:
            rule (numberOf issue) GtE (Count 6)
            +++
            forEach issue |:
                    rule (sumOf value (relativeTo "Portfolio")) LtE (Percent 30)

value = "DirtyValueTotalRC"
issue = "SecurityID"

bondValueRelativeByCountry :: RuleExpr
bondValueRelativeByCountry =
    where' (forall "InstrumentType") Eq "Bond" $:
        forEach "Country" $:
            forEach "IssuerName" $:
                rule (sumOf value (relativeTo "Country")) LtE (Percent 5)
