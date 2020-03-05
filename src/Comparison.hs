module Comparison
( BoolCompare(..)
, comparator
)
where

import LangPrelude


comparator :: Ord a => BoolCompare -> a -> a -> Bool
comparator Eq   = (==)
comparator NEq  = (/=)
comparator Lt   = (<)
comparator Gt   = (>)
comparator LtEq = (<=)
comparator GtEq = (>=)

data BoolCompare
    = Eq
    | NEq
    | Lt
    | Gt
    | LtEq
    | GtEq

instance Show BoolCompare where
    show Eq   = "=="
    show NEq  = "!="
    show Lt   = "<"
    show Gt   = ">"
    show LtEq = "<="
    show GtEq = ">="

deriving instance Eq BoolCompare
