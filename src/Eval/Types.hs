
module Eval.Types
( Env
, Level(..)
, LevelPos(..)
, Position
, ScopeData
, GroupScope
, groupScope
, currentLevel
, currentLevelPos
, portfolioLevelPos
)
where

import LangPrelude
import Absyn
import qualified Data.Aeson                             as Json
import qualified Data.List.NonEmpty                     as NE


type Env a = Map FieldName a

-- | Information about a group of elements in a "group by".
--   Associated with one or more items whose field name ('lGroupName')
--    all contain the same value ('lGroupValue').
data Level = Level
    { lGroupName    :: GroupName    -- e.g. "Country" or "SecurityID"
    , lGroupValue   :: Json.Value   -- e.g. 'String "DK"' or 'Number 12323535'
    }  deriving (Eq, Show)

data LevelPos = LevelPos
    { lpLevel       :: Level            -- e.g. "Country"
    , lpPositions   :: NonEmpty Position
    }

type Position = Map Text Json.Value
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
