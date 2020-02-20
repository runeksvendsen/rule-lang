
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
import qualified Data.Text                              as T

data Level = Level
    { lGroupName    :: GroupName    -- e.g. "Country" or "SecurityID"
    , lGroupValue   :: Json.Value   -- e.g. 'String "DK"' or 'Number 12323535'
    }  deriving Show

instance Json.ToJSON Level where
    toJSON (Level name val) =
        let showField (Json.Object _) = "<object>"
            showField (Json.Array _) = "<array>"
            showField (Json.String txt) = txt
            showField (Json.Number num) = T.pack $ printf "%f" (realToFrac num :: Double)
            showField (Json.Bool b) = if b then "true" else "false"
            showField Json.Null = "<null>"
        in Json.String $ name <> "/" <> showField val

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
