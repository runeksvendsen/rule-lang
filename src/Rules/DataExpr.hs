module Rules.DataExpr
( issuersAbove5Pct
)
where

import LangPrelude
import Rules.Syntax                                 (relativeTo, where')
import AbsynFun


-- Examples
noCashPositions =
    Filter (Left $ PosComparison "InstrumentType" NEq "Cash") $
        Var "portfolio"

issuersExcludingCash =
    GroupBy "IssuerID" noCashPositions

issuersAbove5Pct = undefined
    Filter
        (where'
            (GroupFold (SumOver "DirtyValueRC") (Var "IssuerID") `relativeTo` GroupFold (SumOver "DirtyValueRC") noCashPositions)
            Gt
            (Literal $ Percent 5)
        )
        issuersExcludingCash

