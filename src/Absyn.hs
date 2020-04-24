{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Absyn
( -- * Abstract syntax
  RuleExpr(..)
, Rule
, Expr(..)
, DataExpr(..)
, FieldValue(..)
, BoolExpr(..)
, ValueExpr(..)
, Literal(..)
, Fold(..)
  -- * Re-exports (literals)
, FieldName
, Number
, fromReal
  -- * Re-exports
, module Comparison
)
where

import LangPrelude
import Types
import Comparison                           as Comparison


-- | The result of evaluating a 'ValueExpr'
data Literal
    = Percent Number
    | FieldName FieldName            -- the name of a field in a Position
    | FieldValue FieldValue     -- the contents of a field in a Position
        deriving (Show, Generic, Data)

data ValueExpr
    -- Count the number of groups
    = GroupCount Expr
    | FoldMap Fold Expr
    | Relative Expr Expr
        deriving (Eq, Show, Ord, Generic, Data)

data Fold
    -- Required operations:
    = Sum       -- (+)
    | Average   -- (+ /)
    | Max       -- (==, >)
    | Min       -- (==, >)
        deriving (Eq, Ord, Generic, Show, Data)

data BoolExpr
    = Comparison Expr BoolCompare Expr  -- ^ compare two things
    | And Expr Expr                     -- ^ logical AND
    | Or Expr Expr                      -- ^ logical OR
    | Not Expr                          -- ^ logical NOT
        deriving (Eq, Show, Ord, Generic, Data)

data DataExpr
    = GroupBy Expr Expr -- ^ (input :: DataExpr) (groupingField :: Literal FieldName)
    | Filter Expr Expr  -- ^ (input :: DataExpr) (comparison :: BoolExpr)
        deriving (Eq, Show, Ord, Generic, Data)

-- rhs of let-binding
data Expr
    = Literal Literal
    | ValueExpr ValueExpr
    | BoolExpr BoolExpr
    | DataExpr DataExpr
    -- | A "map" over a tree ('DataExpr').
    --   E.g. 'Map (Literal $ FieldName "Value") (Var "Portfolio")
    --     transforms a Position in the leaf of the tree (resulting
    --     from evaluating 'Var "Portfolio"') into the "Value"
    --     property of that Position.
    | Map Expr Expr
    | Var Text
        deriving (Eq, Show, Ord, Generic, Data)

type Rule = NonEmpty RuleExpr

data RuleExpr
    = Let Text Expr
    | Forall Expr [RuleExpr]  -- ^ (input :: DataExpr) scope
    | If Expr [RuleExpr]
    | Rule Expr              -- ^ a condition that must be true
        deriving (Eq, Show, Ord, Generic, Data)














-- #### TYPE CLASS INSTANCES #### --

instance Eq Literal where
    (Percent a) == (Percent b) = a == b
    (FieldName a) == (FieldName b) = a == b -- TODO: do we need this?
    (FieldValue a) == (FieldValue b) = a == b
    _ == _ = False

-- TODO: static check of invalid comparisons
instance Ord Literal where
    (Percent a) `compare` (Percent b) = a `compare` b
    (FieldValue a) `compare` (FieldValue b) = a `compare` b
    compare a b = error $ "Invalid comparison: " ++ show (a, b)

instance Ord FieldValue where
    (Number a) `compare` (Number b) = a `compare` b
    -- Below throws an error
    compare a b = error $ "Field comparison not implemented: " ++ show (a,b)

instance Hashable Fold
instance Hashable DataExpr
instance Hashable Expr
instance Hashable BoolExpr
-- instance Hashable VarExpr
instance Hashable ValueExpr
-- instance Hashable  a => Hashable (Map a)
instance Hashable Literal
instance Hashable BoolCompare



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
    let dirtyValueForeignSecurities =                                                   -- TreeFold Sum "DirtyValue" (Var "foreignCountrySecurities")
            sum over DirtyValue of foreignCountrySecurities
    let relativeDirtyValueForeignSecurities =                                           -- Relative (Var "dirtyValueForeignSecurities") (TreeFold Sum "DirtyValue" (Var "Portfolio"))
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
        relativeValue <= 10%

        AND Value min5PercentHoldings <= 40%
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
    forall issuers:
        let issues = IssuerName grouped by IssueID
        count issues >= 5
        forall issues:
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







{-


    Portfolio where
        (position.InstrumentType == "GovernmentBond" OR position.InstrumentType == "StateBond")

    Portfolio grouped by Country where
        (position.InstrumentType == "OTC" OR sum Value of Country < 10M)


    eval boolExpr = \position -> bool

-}




-- let otcPositions = Portfolio where (InstrumentType == "OTC")
-- forall (otcPositions grouped by Counterparty) {
--    let counterpartyExposure = sum Exposure of Counterparty relative to Portfolio
--    // non-approved credit institutions
--    if (Counterparty == "SmallCompanyX" OR (Counterparty == "SmallCompanyY" OR Counterparty == "SmallCompanyZ") {
--        require counterpartyExposure <= 5%
--    }
--    // approved credit institutions
--    if (Counterparty == "HugeCorporationA" OR (Counterparty == "HugeCorporationB" OR Counterparty == "HugeCorporationC") {
--        require counterpartyExposure <= 10%
--    }
-- }

