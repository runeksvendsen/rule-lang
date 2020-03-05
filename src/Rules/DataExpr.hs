module Rules.DataExpr
( issuersAbove5Pct
)
where

import LangPrelude
import Rules.Syntax                                 (relativeTo, where')
import Absyn


-- Examples
noCashPositions =
    Filter (PosComparison "InstrumentType" NEq "Cash") $
        Var "portfolio"

issuersExcludingCash =
    GroupBy "Issuer" noCashPositions

issuersAbove5Pct =
    Filter (where' ("DirtyValue" `relativeTo` noCashPositions) Gt (Percent 5))
        issuersExcludingCash

