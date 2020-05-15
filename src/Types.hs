{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Data.Aeson as Json
import Text.Read (Read(..))
import Data.List


newtype FieldName = FieldName Text
    deriving (Eq, Show, Generic, Ord, Data)

instance StringConv FieldName Text where
    strConv _ (FieldName text) = text

instance StringConv FieldName String where
    strConv _ (FieldName text) = toS text

instance Hashable FieldName
instance IsString FieldName where
    fromString = FieldName . toS

data FieldValue
    = Number Number
    | String Text
    | Bool Bool
        deriving (Eq, Show, Generic, Data)

instance Hashable FieldValue
instance IsString FieldValue where
    fromString = String . toS
-- NB: Only for allowing FieldValue as literal
instance Num FieldValue where
    fromInteger = Number . realToFrac

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
    deriving (Eq,Ord,Enum,Floating,Fractional,Num,Real,RealFloat,RealFrac,Hashable,Data)

instance Show Number where
    show (Number' double) =
        let dropSuffix suffix input =
                if suffix `isSuffixOf` input
                    then take (length input - length suffix) input
                    else input
        -- Remove (optional) trailing ".0"
        in dropSuffix ".0" (show double)

instance Read Number where
    readPrec = Number' <$> readPrec

fromReal :: Real a => a -> Number
fromReal = Number' . realToFrac

type Position = HashMap Text FieldValue

