
module Eval.Types
( Level(..)
, LevelPos(..)
, Position
, levelPosLevel
, currentLevel
, currentLevelPos
, showLevel
)
where

import LangPrelude
import Absyn
import qualified Data.Aeson                             as Json
import qualified Data.List.NonEmpty                     as NE


data Level = Level
    { lGroupName    :: GroupName    -- e.g. "Country" or "SecurityID"
    , lGroupValue   :: Json.Value   -- e.g. 'String "DK"' or 'Number 12323535'
    }  deriving (Eq, Show)

instance Json.ToJSON Level where
    toJSON (Level name val) =
        let
        in Json.String $ name <> ": " <> showValue val

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

showLevel (Level name val) =
    name <> "=" <> showValue val
