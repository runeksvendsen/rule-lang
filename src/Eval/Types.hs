
module Eval.Types
( Env
, Level(..)
, LevelPos
, LevelPos'(..)
, Position
, ScopeData
, GroupScope
, groupScope
, currentLevel
, currentLevelPos
, portfolioLevelPos
, FieldName
, FieldValue
)
where

import LangPrelude
import Types
import qualified Data.Aeson                             as Json
import qualified Data.List.NonEmpty                     as NE


type Env = Map Text

-- | Information about a group of elements in a "group by".
--   Associated with one or more items whose field name ('lGroupName')
--    all contain the same value ('lGroupValue').
data Level = Level
    { lGroupName    :: FieldName    -- e.g. "Country" or "SecurityID"
    , lGroupValue   :: Json.Value   -- e.g. 'String "DK"' or 'Number 12323535'
    }  deriving (Eq, Show, Generic)

type LevelPos = LevelPos' Position

data LevelPos' a = LevelPos
    { lpLevel       :: Level            -- e.g. "Country"
    , lpPositions   :: NonEmpty a
    } deriving (Eq, Show, Generic)

type ScopeData = NonEmpty LevelPos
type GroupScope = NonEmpty Level

groupScope :: ScopeData -> NonEmpty Level
groupScope = NE.map lpLevel

currentLevel :: ScopeData -> LevelPos
currentLevel = NE.head

currentLevelPos :: ScopeData -> NonEmpty Position
currentLevelPos = lpPositions . currentLevel

-- | Get positions in the "Portfolio" (outermost) scope
portfolioLevelPos :: ScopeData -> NonEmpty Position
portfolioLevelPos = lpPositions . NE.last

--- TMP ---
instance Json.ToJSON Level
instance Json.ToJSON a => Json.ToJSON (LevelPos' a)

instance Functor LevelPos' where
    fmap f (LevelPos level neA) = LevelPos level (NE.map f neA)
