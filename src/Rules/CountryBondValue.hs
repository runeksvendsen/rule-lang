{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Rules.CountryBondValue
( ruleExpr
, countryIssuers
)
where

import LangPrelude
import Absyn
import Rules.Syntax


countryIssuers = (Var "portfolio" `groupedBy` "Country") `groupedBy` "IssuerID"

ruleExpr =
    -- NB: no "where InstrumentType == Bond"
    let' "countryIssuer" countryIssuers $:
    Foreach (Var "countryIssuer")
        (Rule $ GroupComparison
            (            GroupFold SumOver value (Var "IssuerID")
            `relativeTo` GroupFold SumOver value (Var "Country")
            )
            LtEq
            (percent 5)
        )

value = "DirtyValueTotalRC"
