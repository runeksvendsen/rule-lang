module Absyn where

import LangPrelude
import qualified Data.Aeson                 as Json

type FieldName = Text
type FieldValue = Json.Value
type GroupName = Text

data Value =
      Count Word
    | Field FieldValue
    | Percent Double
        deriving Show



data ValueExpr =
      -- Violator: (FieldName, GroupName)
      -- "count SecurityID at Portfolio level"
      CountDistinct FieldName GroupName                 -- length $ groupBy (lookup fieldName) groupNamePositions

      -- Violator: (FieldName, Position, GroupName)
      -- "InstrumentType"
    | Forall FieldName                                  -- forall . map (lookup fieldName)

      -- Violator: (FieldName, GroupName, Maybe GroupName)
      -- "{Exposure} of {Issuer} {(relative to Country)}"
    | SumOver FieldName GroupName (Maybe GroupName)     -- [relativeTo groupName] . sum . map (lookup fieldName)



-- Examples:
--                              ValueExpr                                  Compare         Value
--  |-----------------------------------------------------------------|---------------|--------------|
--    count             SecurityID                                      >=              6
--    InstrumentType                                                    ==              Bond
--    Value             Issuer                                          <=              2M EUR
--    Value             SecurityID       (relative to Portfolio)        >               5%
--    Value             Issuer           (relative to Country)          <               20%
data Comparison = -- eval: Bool
    Comparison ValueExpr (Value -> Value -> Bool) Value


data DataExpr a =
    -- Two DataExpr in the same context.
    -- Example (two "limit"):
    --      for each X:
    --          limit value of Y <= 10%
    --          limit number of distinct Z >= 6
      Both (DataExpr a) (DataExpr a)
    -- let varName = exprA in varNameScopeExpr
    | Let Text (DataExpr a) (DataExpr a)   -- (name, rhs, scope)
    -- varName
    | Var Text
    -- for each SomeField: expr
    | GroupBy FieldName (DataExpr a)           -- (field, scope)
    -- where ... (NB: ending colon signifies 'Just DataExpr')
    | Filter Comparison (Maybe (DataExpr a))
    -- <some conditions that must be true>
    | Rule  Comparison








{- ####  NOTES #### -}
{-

initial input data:
    Grouping "portfolio" [position1, position2, ..., positionN]


<begin>             -> has access to "portfolio"
GroupBy ("country") -> has access to "portfolio" + "country"
GroupBy ("issuer")  -> has access to "portfolio" + "country" + issuer

-}





{- ####  EXAMPLES #### -}

-- 5-10-40
{-

    let min5PercentHoldings =
            for each Issuer:
                where Value Issuer > 5%
    for each Issuer:
        Value Issuer <= 5%
        UNLESS
        Value Issuer <= 10%
            AND
                Value min5PercentHoldings <= 40%
-}

-- 35-30-6
{-
    for each Issuer:
        where Value of Issuer > 35%:
            count distinct SecurityID >= 6
            for each SecurityID:
                Value of SecurityID <= 30%
-}

-- Max 20% in bonds per issuer
{-
    for each Country:
        where InstrumentType == Bond:
            for each Issuer:
                    value Issuer relative to Country <= 20%

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



{- #### TYPE CLASS INSTANCES #### -}

-- TODO: static check of invalid comparisons
instance Eq Value where
    (Count a) == (Count b) = a == b
    (Field _) == (Field _) = error "Not implemented: compare field access"
    (Percent a) == (Percent b) = a == b
    (==) a b = error $ "Invalid comparison: " ++ show (a, b)

-- TODO: static check of invalid comparisons
instance Ord Value where
    (Count a) `compare` (Count b) = a `compare` b
    (Field _) `compare` (Field _) = error "Not implemented: compare field access"
    (Percent a) `compare` (Percent b) = a `compare` b
    compare a b = error $ "Invalid comparison: " ++ show (a, b)
