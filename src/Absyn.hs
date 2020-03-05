module Absyn
( -- * Abstract syntax
  RuleExpr(..)
, DataExpr(..)
, GroupComparison(..)
, PosComparison(..)
, FilterComparison(..)
, GroupValueExpr(..)
, GroupValue(..)
, GroupFold(..)
  -- * Value types
, FieldName
, FieldValue
  -- * Re-exports
, module Comparison
, module Tree
)
where

import LangPrelude
import Types
import Tree                                 as Tree
import Comparison                           as Comparison
import qualified Data.Aeson                 as Json


data GroupValue =
      Count Word        -- 'CountDistinct'
    | Sum Double        -- 'SumOver'
    | Percent Double    -- 'RelativeComparison'
        deriving (Show)

data GroupValueExpr =
      Literal GroupValue
    | GroupFold GroupFold FieldName DataExpr
    | RelativeComparison GroupValueExpr GroupValueExpr  -- 'Percent'
        deriving (Eq, Show)

-- Input:  grouping
-- Output: 'GroupValue'
data GroupFold =
      CountDistinct     -- 'Count'          (Equality)
    | SumOver           -- 'Sum'            (+)
    | Average           -- <any type>       (+ /)
    | Max               -- <any type>       (Order)
    | Min               -- <any type>       (Order)
        deriving (Eq, Ord, Generic, Show)

data GroupComparison =
      GroupComparison GroupValueExpr BoolCompare GroupValueExpr
        deriving (Eq, Show)

-- Input:  (Position, fieldName, fieldValue)
data PosComparison = PosComparison FieldName BoolCompare FieldValue
    deriving (Eq, Show)

data FilterComparison =
      FilterGroup GroupComparison
    | FilterPos PosComparison
        deriving (Eq, Show)

type VarName = Text

data RuleExpr
    = Let VarName DataExpr RuleExpr     -- ^ name rhs scope
    | Foreach DataExpr RuleExpr -- ^ for each country in dataExpr: <scope>
    | Rule GroupComparison              -- ^ a condition that must be true
    | And RuleExpr RuleExpr             -- ^ logical "and"
        deriving (Eq, Show)

data DataExpr
    = GroupBy FieldName DataExpr        -- ^ groupingField input
    | Var VarName
    | Filter FilterComparison DataExpr  -- ^ comparison input
        deriving (Eq, Show)




-- VI
{-

    let foreignCountrySecurities =
            portfolio grouped by Country where Country <> Dk
    let dirtyValueForeignSecurities =
            sum over DirtyValue of foreignCountrySecurities
    let relativeDirtyValueForeignSecurities =
            dirtyValueForeignSecurities relative to sum over DirtyValue of portfolio
    let numberOfForeignCountries = groupCount foreignCountrySecurities

    if relativeDirtyValueForeignSecurities >= 80%:
        numberOfForeignCountries >= 5
    else if relativeDirtyValueForeignSecurities >= 60%
        numberOfForeignCountries >= 4
    else if relativeDirtyValueForeignSecurities >= 40%
        numberOfForeignCountries >= 3
    else if relativeDirtyValueForeignSecurities < 40%
        numberOfForeignCountries >= 2

-}

-- complex
{-
    let homeCountry = "DK"
    let foreignCountries = group portfolio by Country: where Country <> homeCountry
    for each country in foreignCountries:
        let relativeCountryValue = sum over Value of country relative to Value of Portfolio
        let numCountrySecurities = count (group country by SecurityId)
        if relativeCountryValue > 60%:
            numCountrySecurities >= 20
            for each SecurityID:
                sum over Value of SecurityID relative to sum over Value of Country <= 5%
        else if relativeCountryValue > 40%:
            numCountrySecurities >= 10

-}

-- 5-10-40
{-

    define min5PercentHoldings =
            for each Issuer:
                where Value > 5% relative to portfolio
    for each Issuer:
        define relativeValue = sum Value of Issuer relative to portfolio
        relativeValue <= 5%
        UNLESS
        (relativeValue <= 10% AND Value min5PercentHoldings <= 40%)
-}













-- 35-30-6
{-
    for each IssueName:
        where Issuer = "Danske Bank":
            Value >= 500M USD




    define portfolioLevelIssueCount = count distinct SecurityID in Portfolio
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


-- #### TYPE CLASS INSTANCES #### --

-- TODO: static check of invalid comparisons
instance Eq GroupValue where
    (Count   a) == (Count   b) = a == b
    (Sum     a) == (Sum     b) = a == b
    (Percent a) == (Percent b) = a == b
    (==) a b = error $ "Invalid comparison: " ++ show (a, b)

-- instance Eq GroupValue where
--     (Field (Json.String strA)) == (Field (Json.String strB)) =
--         strA == strB
--     -- Below throws an error
--     (Field a) == (Field b) =
--         error $ "Field comparison not implemented: " ++ show (a,b)
--     (==) a b = error $ "Invalid comparison: " ++ show (a, b)

-- TODO: static check of invalid comparisons
instance Ord GroupValue where
    (Count   a) `compare` (Count   b) = a `compare` b
    (Sum     a) `compare` (Sum     b) = a `compare` b
    (Percent a) `compare` (Percent b) = a `compare` b
    compare a b = error $ "Invalid comparison: " ++ show (a, b)

instance Ord Json.Value where
    (Json.Number a) `compare` (Json.Number b) = a `compare` b
    -- Below throws an error
    compare a b = error $ "Field comparison not implemented: " ++ show (a,b)

instance Hashable GroupFold



-- complex
{-

    let homeCountry = "DK"
    let foreignCountries = group by Country: where Country <> homeCountry

    for each country in foreignCountries:
        let countryValue = Value of country relative to Portfolio
        let numCountrySecurities = count distinct SecurityId at Country level

        if countryValue > 60%:
            numCountrySecurities >= 20
            for each SecurityID in country:
                Value of SecurityID relative to Country <= 5%
        else if countryVal > 40%:
            numCountrySecurities >= 10







    for each Country:
        for each Issuer:
            Value of Issuer relative to Country <= 10%

-}













