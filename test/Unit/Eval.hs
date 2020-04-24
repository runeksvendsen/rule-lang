module Unit.Eval
( main
)
where

import Prelude
import Absyn
import qualified Eval
import qualified Types
import qualified Examples.Rules

import qualified Data.Char as C
import qualified Data.Aeson as Json
import GHC.Generics (Generic)
import Data.Maybe
import Test.QuickCheck
import Control.Monad (forM_)
import Data.List.NonEmpty (toList)


main :: IO ()
main =
    forM_ positionCounts $ \posCount ->
        forM_ Examples.Rules.allRules (testRule posCount)
  where
    positionCounts = [1, 10, 100, 1000, 10000, 100000]

testRule :: Int -> (Rule, String) -> IO ()
testRule numPositions (rule, name) = do
    testPositions <- generate $ vectorOf numPositions arbitrary
    let positions = map toHashMap testPositions
    let lbl = "Rule " <> name <> ": (" <> show numPositions <> ")"
    let passed = Eval.eval (Eval.mkInitialEnv positions) (toList rule)
    -- We're not interested in whether an arbitrary set of positions passes a rule,
    --  rather, we just want to catch runtime errors in the evaluator.
    quickCheck $ label lbl $ passed || True

data TestPosition = TestPosition
    { value             :: Double
    , exposure          :: Double
    , issuer            :: String
    , issue             :: Integer
    , instrumentType    :: String
    , counterparty      :: String
    } deriving Generic

instance Arbitrary TestPosition where
    arbitrary = TestPosition
        <$> arbitrary
        <*> arbitrary
        <*> elements counterparties
        <*> arbitrary
        <*> elements instrumentTypes
        <*> elements counterparties
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

toHashMap :: TestPosition -> Eval.Position
toHashMap =
    fmap fromJson . getHashMap . Json.toJSON
  where
    fromJson a = fromMaybe (error $ "toHashMap: invalid input: " ++ show a) $
        Types.fromJsonValue a
    getHashMap (Json.Object m) = m
    getHashMap _ = error "BUG: toHashMap"

instance Json.ToJSON TestPosition where
    toJSON =
        let capitalizeFirstLetter [] = []
            capitalizeFirstLetter (first:rest) = C.toUpper first : rest
        in Json.genericToJSON Json.defaultOptions
            { Json.fieldLabelModifier = capitalizeFirstLetter }
