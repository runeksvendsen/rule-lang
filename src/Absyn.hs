{-# OPTIONS_GHC -fno-warn-orphans #-}
module Absyn
( -- * Abstract syntax
  RuleExpr(..)
, DataExpr(..)
, FieldValue(..)
, Comparison(..)
, GroupValueExpr(..)
, GroupOp(..)
, Literal(..)
, PositionFold(..)
  -- * Value types
, FieldName
, Number
, fromReal
  -- * Re-exports
, module Comparison
, module Tree
)
where

import LangPrelude
import Types
import Tree                                 as Tree
import Comparison                           as Comparison


-- TODO: move Number+String+Bool out of "FieldValue"?
data Literal =
      Integer Integer
    | Percent Number
    | FieldName Text            -- the name of a field in a Position
    | FieldValue FieldValue     -- the contents of a field in a Position
        deriving (Show, Generic)

data GroupValueExpr
    = Literal Literal
    | GroupOp GroupOp
    | DataExpr DataExpr
    | Var VarName
        deriving (Eq, Show, Generic)

data DataExpr
    = GroupBy GroupValueExpr GroupValueExpr   -- ^ (groupingField :: Literal FieldName) (input :: DataExpr)
    | Filter Comparison GroupValueExpr        -- ^ comparison (input :: DataExpr)
        deriving (Eq, Show, Generic)

data GroupOp
    = GroupCount GroupValueExpr -- grouping
    | PositionFold PositionFold GroupValueExpr GroupValueExpr   -- foldType fieldName input
    | Relative GroupValueExpr GroupValueExpr -- numeratorGroupOp denominatorInput
        deriving (Eq, Show, Generic)

-- [Position] -> 'Literal'
-- Output: 'Number'
data PositionFold =
      SumOver           -- (+)
    | Average           -- (+ /)
    | Max               -- (Order)
    | Min               -- (Order)
        deriving (Eq, Ord, Generic, Show)

-- Input:  'TermNode'
-- Output: 'Bool'
data Comparison =
    Comparison GroupValueExpr BoolCompare GroupValueExpr
        deriving (Eq, Show, Generic)

type VarName = Text

data RuleExpr
    = Let VarName GroupValueExpr RuleExpr               -- ^ name rhs scope
    | Foreach GroupValueExpr GroupValueExpr RuleExpr    -- ^ fieldName dataExpr scope
    | Rule Comparison                                   -- ^ a condition that must be true
    | And RuleExpr (NonEmpty RuleExpr)                  -- ^ logical "and"
        deriving (Eq, Show, Generic)





{-
    count (group Portfolio by IssuerName) >= 10
    for each Country in Portfolio:
        sum DirtyValue of Country relative to Portfolio <= 15%
        for each IssuerName in Country:
            count (group IssuerName by IssueID) >= 3
-}

-- VI
{-

    let foreignCountrySecurities =                                                      -- GroupBy "Country:" $: Filter (FilterPos $ PosComparison "Country" NEq "DK")
            portfolio grouped by Country: where Country <> Dk
    let dirtyValueForeignSecurities =                                                   -- PositionFold SumOver "DirtyValue" (Var "foreignCountrySecurities")
            sum over DirtyValue of foreignCountrySecurities
    let relativeDirtyValueForeignSecurities =                                           -- Relative (Var "dirtyValueForeignSecurities") (PositionFold SumOver "DirtyValue" (Var "Portfolio"))
            dirtyValueForeignSecurities relative to sum over DirtyValue of Portfolio
    let numberOfForeignCountries = groupCount foreignCountrySecurities                  -- GroupCount (Var "foreignCountrySecurities")

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

        if Value of Issuer > 35%:
            portfolioLevelIssueCount >= 6
            for each SecurityID:
                Value <= 30%

-}


--     a minimum of five different issues per issuer
-- AND value of each issue <= 5% (relative to Portoflio)
{-
    let issuers = Portfolio grouped by IssuerName
    for all issuers:
        let issues = IssuerName grouped by IssueID
        count issues >= 5
        for all issues:
            sum DirtyValue of IssueID relative to Portfolio <= 5%
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
                    where Value of Issuer relative to Country >= 10%
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
instance Eq Literal where
    (Integer   a) == (Integer   b) = a == b
    (Percent a) == (Percent b) = a == b
    (==) a b = error $ "Invalid comparison: " ++ show (a, b)

-- instance Eq Literal where
--     (Field (Json.String strA)) == (Field (Json.String strB)) =
--         strA == strB
--     -- Below throws an error
--     (Field a) == (Field b) =
--         error $ "Field comparison not implemented: " ++ show (a,b)
--     (==) a b = error $ "Invalid comparison: " ++ show (a, b)

-- TODO: static check of invalid comparisons
instance Ord Literal where
    (Integer   a) `compare` (Integer   b) = a `compare` b
    (Percent a) `compare` (Percent b) = a `compare` b
    compare a b = error $ "Invalid comparison: " ++ show (a, b)

instance Ord FieldValue where
    (Number a) `compare` (Number b) = a `compare` b
    -- Below throws an error
    compare a b = error $ "Field comparison not implemented: " ++ show (a,b)

instance Hashable PositionFold



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













