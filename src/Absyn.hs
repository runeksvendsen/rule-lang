module Absyn
( -- * Abstract syntax
  RuleExpr(..)
, Comparison(..)
, ValueExpr(..)
, GroupValueExpr(..)
, PosValueExpr(..)
, Value(..)
  -- * Value types
, FieldName
, FieldValue
, GroupName
  -- * Re-exports
, module Comparison
)
where

import LangPrelude
import Comparison                           as Comparison
import qualified Data.Aeson                 as Json
import           Data.String                (IsString(..))


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
        deriving (Eq, Show)

-- A value for a group
data GroupValueExpr =
      -- Violator: (FieldName, GroupName)
      -- "count SecurityID"
      CountDistinct FieldName

      -- Violator: (FieldName, GroupName, Maybe GroupName)
      -- "{Exposure} {(relative to Country)}"
    | SumOver FieldName (Maybe GroupName)
        deriving (Eq, Show)

data ValueExpr =
      GroupValueExpr GroupValueExpr
    | PosValueExpr PosValueExpr
        deriving (Eq, Show)

-- Examples:
--     *Input*                          *ValueExpr*                       *Compare*       *Value*
--  |------------|----------------------------------------------------|---------------|-------------|
--     position     InstrumentType                                      ==              Bond
--
--     grouping     count   SecurityID                                  >=              6
--     grouping     sum     Value                                       <=              2M EUR
--     grouping     sum     Value           (relative to Portfolio)     >               5%
--     grouping     sum     Value           (relative to Country)       <               20%
data Comparison =
      Comparison ValueExpr BoolCompare Value

data RuleExpr
    -- Two RuleExpr in the same context.
    -- Example (two "Rule"):
    --      for each X:
    --          value of Y <= 10%
    --          number of distinct Z >= 6
    = And RuleExpr RuleExpr

    -- let varName = exprA in varNameScopeExpr
    | Let Text RuleExpr RuleExpr   -- name rhs scope

    -- varName
    | Var Text

    -- for each SomeField: expr
    | GroupBy FieldName RuleExpr           -- field scope

    -- where ... (NB: ending colon signifies 'Just RuleExpr')
    | Filter Comparison (Maybe RuleExpr)

    -- <some conditions that must be true>
    | Rule Comparison
        deriving Show




{- ####  EXAMPLES #### -}

-- complex
{-
    let homeCountry = "DK"
    let foreignCountries = group by Country: where Country <> homeCountry
    for each (Country in) foreignCountries:
        let countryValue = Value of Country relative to Portfolio
        let numCountrySecurities = count distinct SecurityId at Country level
        if countryValue > 60%:
            numCountrySecurities >= 20
            (AND)
            for each SecurityID:
                Value of SecurityID relative to Country <= 5%
        else if countryVal > 40%:
            numCountrySecurities >= 10

-}

-- 5-10-40
{-

    let min5PercentHoldings =
            for each Issuer:
                where Value > 5%
    for each Issuer:
        Value <= 5%
        UNLESS
        (Value <= 10% AND Value min5PercentHoldings <= 40%)
-}

-- 35-30-6
{-
    let portfolioLevelIssueCount = count distinct SecurityID (at Portfolio level) >= 6
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
    Value max5PercentPositions >= 75%
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

instance IsString Value where
    fromString str = Field $ Json.String (toS str)

-- TODO: static check of invalid comparisons
instance Eq Value where
    (Count   a) == (Count   b) = a == b
    (Sum     a) == (Sum     b) = a == b
    (Percent a) == (Percent b) = a == b
    (Field (Json.String strA)) == (Field (Json.String strB)) =
        strA == strB
    -- Below throws an error
    (Field a) == (Field b) =
        error $ "Field comparison not implemented: " ++ show (a,b)
    (==) a b = error $ "Invalid comparison: " ++ show (a, b)

-- TODO: static check of invalid comparisons
instance Ord Value where
    (Count   a) `compare` (Count   b) = a `compare` b
    (Sum     a) `compare` (Sum     b) = a `compare` b
    (Percent a) `compare` (Percent b) = a `compare` b
    -- Below throws an error
    (Field (Json.String strA)) `compare` (Field (Json.String strB)) =
        strA `compare` strB
    -- Below throws an error
    (Field a) `compare` (Field b) =
        error $ "Field comparison not implemented: " ++ show (a,b)
    compare a b = error $ "Invalid comparison: " ++ show (a, b)

-- TMP:
instance Show Comparison where
    show (Comparison valueExpr _ value) =
        "(Comparison (" ++ show valueExpr ++ ") (" ++ show value ++ "))"
