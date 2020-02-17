module Absyn where

import LangPrelude
import qualified Data.Aeson                 as Json

type FieldName = Text
type FieldValue = Json.Value
type GroupName = Text

data Value =
      Count Word
    | Sum Double
    | Field FieldValue
    | Percent Double
        deriving Show

-- A value for a position
data PosValueExpr =
      -- Violator: (FieldName, Position)
      -- "InstrumentType"
      Get FieldName

-- A value for a group
data GroupValueExpr =
      -- Violator: (FieldName, GroupName)
      -- "count SecurityID"
      CountDistinct FieldName

      -- Violator: (FieldName, GroupName, Maybe GroupName)
      -- "{Exposure} {(relative to Country)}"
    | SumOver FieldName (Maybe GroupName)

data ValueExpr =
      GroupValueExpr GroupValueExpr
    | PosValueExpr PosValueExpr

-- Examples:
--     *Input*                          *ValueExpr*                       *Compare*       *Value*
--  |------------|----------------------------------------------------|---------------|-------------|
--     position     InstrumentType                                      ==              Bond
--
--     grouping     count   SecurityID                                  >=              6
--     grouping     sum     Value                                       <=              2M EUR
--     grouping     sum     Value           (relative to Portfolio)     >               5%
--     grouping     sum     Value           (relative to Country)       <               20%
data Comparison = -- eval: Bool
      Comparison ValueExpr (Value -> Value -> Bool) Value

data DataExpr a
    -- Two DataExpr in the same context.
    -- Example (two "Rule"):
    --      for each X:
    --          value of Y <= 10%
    --          number of distinct Z >= 6
    = Both (DataExpr a) (DataExpr a)

    -- let varName = exprA in varNameScopeExpr
    | Let Text (DataExpr a) (DataExpr a)   -- name rhs scope

    -- varName
    | Var Text

    -- for each SomeField: expr
    | GroupBy FieldName (DataExpr a)           -- field scope

    -- where ... (NB: ending colon signifies 'Just DataExpr')
    | Filter Comparison (Maybe (DataExpr a))

    -- <some conditions that must be true>
    | Rule  Comparison





{- ####  EXAMPLES #### -}

-- 5-10-40
{-

    let min5PercentHoldings =
            for each Issuer:
                where Value > 5%
    for each Issuer:
        Value <= 5%
        UNLESS
        Value <= 10% AND Value min5PercentHoldings <= 40%
-}

-- 35-30-6
{-
    let portfolioLevelIssueCount = count distinct SecurityID >= 6
    for each Issuer:
        where Value > 35%:
            portfolioLevelIssueCount >= 6
            for each SecurityID:
                Value <= 30%
-}

-- Max 20% in bonds per issuer
{-
    for each Country:
        where InstrumentType == Bond:
            for each Issuer:
                    Value relative to Country <= 20%

    variations: relative to CountryBonds vs relative to CountryAllAssets
-}

-- 75-5
{-
    A minimum 75% the portfolio value must comprise
    securities whose value (relative to total portfolio value)
    does not exceed 5%.
-}
{-
    let max5PercentPositions =
            for each SecurityID:
                where Value SecurityID <= 5%
    limit Value max5PercentPositions >= 75%
-}

-- home-made
{-
    let tenPctCountryIssuers =
            for each Country:
                for each Issuer:
                    where Value relative to Country >= 10%
    if Value of tenPctCountryIssuers relative to Portfolio > 50%
        then count distinct tenPctCountryIssuers >= 7
    if Value of tenPctCountryIssuers relative to Portfolio > 40%
        then count distinct tenPctCountryIssuers >= 6
    if Value of tenPctCountryIssuers relative to Portfolio > 30%
        then count distinct tenPctCountryIssuers >= 5
    else if Value of tenPctCountryIssuers relative to Portfolio <= 30%
        then count distinct tenPctCountryIssuers >= 4
-}


{- #### TYPE CLASS INSTANCES #### -}

-- TODO: static check of invalid comparisons
instance Eq Value where
    (Count   a) == (Count   b) = a == b
    (Sum     a) == (Sum     b) = a == b
    (Percent a) == (Percent b) = a == b
    -- Below throws an error
    (Field _) == (Field _) = error "Not implemented: compare field value"
    (==) a b = error $ "Invalid comparison: " ++ show (a, b)

-- TODO: static check of invalid comparisons
instance Ord Value where
    (Count   a) `compare` (Count   b) = a `compare` b
    (Sum     a) `compare` (Sum     b) = a `compare` b
    (Percent a) `compare` (Percent b) = a `compare` b
    -- Below throws an error
    (Field _) `compare` (Field _) = error "Not implemented: compare field value"
    compare a b = error $ "Invalid comparison: " ++ show (a, b)
