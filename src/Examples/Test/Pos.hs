module Examples.Test.Pos
( TestPosition(..)
, toPos
)
where

import LangPrelude
import qualified Eval
import qualified Types

import qualified Data.Char as C
import qualified Data.Aeson as Json
import Test.QuickCheck


-- |
data TestPosition = TestPosition
    { value             :: Double
    , exposure          :: Double
    , issuer            :: Text
    , issue             :: Word
    , instrumentType    :: Text
    , counterparty      :: Text
    , securityID        :: Word
    , rating            :: Text
    , country           :: Text
    } deriving Generic

instance NFData TestPosition

-- | Position data tailored for 'Examples.Rules.allRules'
instance Arbitrary TestPosition where
    arbitrary = TestPosition
        <$> arbitrary
        <*> arbitrary
        <*> elements counterparties
        <*> arbitrary
        <*> elements instrumentTypes
        <*> elements counterparties
        <*> arbitrary
        <*> elements ratings
        <*> elements countries
      where
        counterparties =
            [ "SmallCompanyX"
            , "SmallCompanyY"
            , "SmallCompanyZ"
            , "HugeCorpA"
            , "HugeCorpB"
            , "HugeCorpC"
            , "Novo"
            , "Maersk"
            , "Google"
            , "Tesla"
            ]
        instrumentTypes =
            [ "OTC"
            , "GovernmentBond"
            , "StateBond"
            , "Option"
            , "Future"
            ]
        ratings =
            [ "AAA"
            , "AA"
            , "A"
            , "BBB"
            , "BB"
            , "B"
            , "CCC"
            , "CC"
            , "C"
            , "D"
            ]
        countries =
            [ "DK"
            , "US"
            , "GB"
            , "JP"
            , "DE"
            , "FR"
            ]

-- | Generate an arbitrary 'Eval.Position' using:
--
-- >  toPos <$> arbitrary
toPos :: TestPosition -> Eval.Position
toPos =
    fmap fromJson . getHashMap . Json.toJSON
  where
    fromJson a = fromMaybe (error $ "toHashMap: invalid input: " ++ show a) $
        Types.fromJsonValue a
    getHashMap (Json.Object m) = m
    getHashMap _ = error "BUG: toHashMap"

-- | For internal use
instance Json.ToJSON TestPosition where
    toJSON =
        let capitalizeFirstLetter [] = []
            capitalizeFirstLetter (first:rest) = C.toUpper first : rest
        in Json.genericToJSON Json.defaultOptions
            { Json.fieldLabelModifier = capitalizeFirstLetter }
