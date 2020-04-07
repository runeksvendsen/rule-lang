module Comparison
( BoolCompare(..)
, comparator
, valueToString -- Print
, stringToValue -- Parse
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
        deriving (Eq, Show, Read, Generic)

valueToString :: [(BoolCompare, String)]
valueToString =
    [ (Eq   , "==")
    , (NEq  , "!=")
    , (Lt   , "<")
    , (Gt   , ">")
    , (LtEq , "<=")
    , (GtEq , ">=")
    ]

stringToValue :: [(String, BoolCompare)]
stringToValue =
    map swap valueToString
  where
    swap (a,b) = (b,a)
