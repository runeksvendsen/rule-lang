{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Types
( FieldName
, FieldValue(..)
, fromJsonValue
, Position
, Number
, fromReal
)
where

import LangPrelude
import qualified Data.Aeson                             as Json
import Data.String (IsString(fromString))
import Text.Read (Read(..))


type FieldName = Text

data FieldValue
    = Number Number
    | String Text
    | Bool Bool
        deriving (Eq, Show, Generic)

instance Hashable FieldValue

instance IsString FieldValue where
    fromString = String . toS

fromJsonValue :: Json.Value -> Maybe FieldValue
fromJsonValue (Json.String txt) =
    Just $ String txt
fromJsonValue (Json.Number num) =
    Just $ Number $ fromReal num
fromJsonValue (Json.Bool b) =
    Just $ Bool b
fromJsonValue _ =
    Nothing

newtype Number = Number' Double
    deriving (Eq,Ord,Enum,Floating,Fractional,Num,Real,RealFloat,RealFrac,Hashable)

instance Show Number where
    show (Number' double) = show double

instance Read Number where
    readPrec = Number' <$> readPrec

fromReal :: Real a => a -> Number
fromReal = Number' . realToFrac

type Position = Map Text FieldValue

