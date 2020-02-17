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
    , rScope    :: [Level]  -- ^ The empty list is the top level. The head of the list defines the innermost scope.
    , rStatus   :: ResultStatus
    } deriving Show

-- |
data ResultStatus
    = RulePassed        -- ^ Passed a rule
    | RuleWarning       -- ^ (Unused) Warning level for a rule
    | RuleViolated      -- ^ Violated a rule
    | NotConsidered     -- ^ Filtered away (by "when")
    | MissingField FieldName    -- ^ No such field exists (e.g. no such field as "Issuer")
    | FieldTypeError    -- ^ Field data type incompatible with operation (e.g. "sum" on a field containing a 'String')
        deriving (Eq, Show, Generic)
