module Rules.DataExpr
( issuersAbove5Pct
)
where

import LangPrelude
-- import Rules.Syntax                                 (relativeTo, where')
import AbsynFun


-- Examples
-- noCashPositions =
--     Filter (PosComparison "InstrumentType" NEq "Cash") $
--         Var "portfolio"

-- issuersExcludingCash =
--     GroupBy "IssuerID" noCashPositions

issuersAbove5Pct = undefined
    -- Filter (where' ("DirtyValueRC" `relativeTo` noCashPositions) Gt (Percent 5))
    --     issuersExcludingCash


