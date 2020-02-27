
module Eval.Types
( Level(..)
, LevelPos(..)
, Position
, levelPosLevel
, currentLevel
, currentLevelPos
)
where

import LangPrelude
import Absyn
import qualified Data.Aeson                             as Json
import qualified Data.List.NonEmpty                     as NE


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

levelPosLevel :: NonEmpty LevelPos -> NonEmpty Level
levelPosLevel = NE.map lpLevel

currentLevel :: NonEmpty LevelPos -> LevelPos
currentLevel = NE.head

currentLevelPos :: NonEmpty LevelPos -> NonEmpty Position
currentLevelPos = lpPositions . currentLevel
