{-# LANGUAGE DeriveGeneric #-}
module Eval.Result
( Level(..)
, Result(..)
, ResultStatus(..)
)
where

import LangPrelude
import Absyn
import Eval.Types


data Result = Result
    { rPosition :: NonEmpty Position
    , rScope    :: NonEmpty Level       -- ^ The head of the list defines the innermost scope.
    , rStatus   :: ResultStatus
    } deriving (Show, Eq)

-- |
data ResultStatus
    = RulePassed        -- ^ Passed a rule
    | RuleWarning       -- ^ (Unused) Warning level for a rule
    | RuleViolated      -- ^ Violated a rule
    | NotConsidered     -- ^ Filtered away (by "when")
    | MissingField FieldName    -- ^ No such field exists (e.g. no such field as "Issuer")
    | FieldTypeError PositionFold FieldName FieldValue
      -- ^ Field data type incompatible with operation (e.g. "sum" on a field containing a 'String')
        deriving (Eq, Show, Ord, Generic)
