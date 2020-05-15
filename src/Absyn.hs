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


data Literal
    = Percent Number
    | FieldName FieldName       -- the name of a field in a Position
    | FieldValue FieldValue     -- the contents of a field in a Position
        deriving (Show, Generic, Data)

data ValueExpr
    = GroupCount Expr
    | FoldMap Fold Expr
    | Relative Expr Expr
        deriving (Eq, Show, Ord, Generic, Data)

data Fold
    -- Required operations:
    = Sum   -- (+)
    | Avg   -- (+ /)
    | Max   -- (==, >)
    | Min   -- (==, >)
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

data RuleExpr
    = Let Text Expr
    | Forall Expr [RuleExpr]  -- ^ (input :: DataExpr) scope
    | If Expr [RuleExpr]
    | Rule Expr              -- ^ a condition that must be true
        deriving (Eq, Show, Ord, Generic, Data)

-- A rule corresponds to a list of RuleExpr
type Rule = [RuleExpr]



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
instance Hashable ValueExpr
instance Hashable Literal
instance Hashable BoolCompare
