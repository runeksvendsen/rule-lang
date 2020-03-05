module Rules.FiveTenForty
( ruleExpr
)
where

import LangPrelude
import Absyn
import Rules.Syntax


ruleExpr =
    Let "noCashPositions" (Var "portfolio" `where'` FilterPos (PosComparison "InstrumentType" NEq "Cash")) $:
    Let "issuersExcludingCash" (Var "noCashPositions" `groupedBy` "IssuerID") $:
    Let "issuersAbove5Pct" (Var "issuersExcludingCash" `where'`
        FilterGroup (GroupComparison
            (GroupFold SumOver "DirtyValueRC" (Var "IssuerID")
                `relativeTo` GroupFold SumOver "DirtyValueRC" (Var "noCashPositions"))
            Gt
            (percent 5))
            ) $:
    Foreach (Var "issuersExcludingCash")
        (Rule $ GroupComparison
            (GroupFold SumOver "DirtyValueRC" (Var "IssuerID")
                    `relativeTo` GroupFold SumOver "DirtyValueRC" (Var "noCashPositions"))
            LtEq
            (percent 10)
        )
    +++
    (Rule $ GroupComparison
        (GroupFold SumOver "DirtyValueRC" (Var "issuersAbove5Pct")
                `relativeTo` GroupFold SumOver "DirtyValueRC" (Var "noCashPositions"))
        LtEq
        (percent 40)
    )
