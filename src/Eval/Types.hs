
module Eval.Types
( Level(..)
, LevelPos(..)
, Position
, levelPosLevel
, currentLevel
, currentLevelPos
  -- * Helper functions (TODO: move somewhere else)
, showValue
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
    }  deriving Eq

instance Show Level where
    show (Level name val) = "(" ++ show name ++ ", " ++ toS (Json.encode val) ++ ")"

instance Json.ToJSON Level where
    toJSON (Level name val) =
        let
        in Json.String $ name <> " > " <> showValue val

showValue :: Json.Value -> Text
showValue (Json.Object _) = "[object]"
showValue (Json.Array _) = "[array]"
showValue (Json.String txt) = txt
showValue (Json.Number num) = T.pack $ printf "%f" (realToFrac num :: Double)
showValue (Json.Bool b) = if b then "true" else "false"
showValue Json.Null = "(null)"

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
