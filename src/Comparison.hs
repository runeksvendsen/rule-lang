module Comparison
( BoolCompare(..)
, comparator
)
where

import LangPrelude


comparator :: Ord a => BoolCompare -> a -> a -> Bool
comparator Eq  = (==)
comparator NEq = (/=)
comparator Lt  = (<)
comparator Gt  = (>)
comparator LtE = (<=)
comparator GtE = (>=)

data BoolCompare
    = Eq
    | NEq
    | Lt
    | Gt
    | LtE
    | GtE

instance Show BoolCompare where
    show Eq  = "=="
    show NEq = "!="
    show Lt  = "<"
    show Gt  = ">"
    show LtE = "<="
    show GtE = ">="
