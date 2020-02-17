module Test where

import Prelude
import Absyn
import Syntax

-- 35-30-6
thirtyfiveThirtySix =
    forEach "Issuer" $:
        where' (sumOf "Value") (>) (Percent 35.0) $:
            rule (numberOf "SecurityID") (>=) (Count 6)
            +++
            forEach "Issue" |:
                rule (sumOf "Value") (<=) (Percent 30)

{-
    for each Issuer:
        where value of Issuer (relative to portfolio) > 35%:
            rule number of distinct SecurityID (at Portfolio level) >= 6
            for each Issue (group by SecurityID):
                rule value of Issue (relative to Portfolio value) <= 30%
-}
