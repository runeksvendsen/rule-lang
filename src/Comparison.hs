{-# LANGUAGE DeriveDataTypeable #-}
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
        deriving (Eq, Show, Read, Generic, Data)

-- | Used by pretty-printer
valueToString :: [(BoolCompare, Text)]
valueToString =
    [ (Eq   , "==")
    , (NEq  , "!=")
    , (Lt   , "<")
    , (Gt   , ">")
    , (LtEq , "<=")
    , (GtEq , ">=")
    ]

-- | Used by parser
stringToValue :: [(Text, BoolCompare)]
stringToValue =
    map swap valueToString
  where
    swap (a,b) = (b,a)
