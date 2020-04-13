{-# LANGUAGE QuasiQuotes #-}
module Unit
( test
)
where

import LangPrelude
import qualified Parse
import qualified Pretty
import qualified Text.Megaparsec
import qualified Data.Text as T
import qualified Hedgehog
import Control.Monad.Trans.Class (lift)
import NeatInterpolation (text)


test :: IO Bool
test =
    Hedgehog.checkParallel $ Hedgehog.Group "Example rules"
        (map mkTest allRules)

mkTest
    :: (Hedgehog.PropertyName, Text)
    -> (Hedgehog.PropertyName, Hedgehog.Property)
mkTest (name, code) =
    let prop = Hedgehog.property $ do
            absyn <- parse' code
            let prettyPrinted = T.lines $ Pretty.pp "   " absyn
            Hedgehog.diff (map T.stripEnd prettyPrinted) (==) (map T.stripEnd $ T.lines code)
    in (name, prop)
  where
    parse' input =
        case Text.Megaparsec.parse Parse.documentParser "" input of
            Left  e -> do
                Hedgehog.footnote (Text.Megaparsec.errorBundlePretty e)
                Hedgehog.failure
            Right x -> return x

allRules :: [(Hedgehog.PropertyName, Text)]
allRules =
    [ ("I", ruleI)
    , ("II", ruleII)
    , ("III", ruleIII)
    , ("IV", ruleIV)
    ]

ruleI :: Text
ruleI =
    [text|
let issuers = Portfolio grouped by .Issuer
forall issuers {
   require sum .Value of Issuer relative to Portfolio <= 10%
}
let issuersAbove5Pct = issuers where (sum .Value of Issuer relative to Portfolio > 5%)
require sum .Value of issuersAbove5Pct <= 40%
    |]

ruleII :: Text
ruleII =
    [text|
let issuers = Portfolio grouped by .Issuer
forall issuers {
   let issuerValue = sum .Value of Issuer relative to Portfolio
   require issuerValue <= 35.0%
   let issueCount = count Issuer grouped by .Issue
   if issuerValue > 30% {
      require issueCount >= 6
   }
}
    |]

ruleIII :: Text
ruleIII =
    [text|
let govtSecurities = Portfolio where ((.InstrumentType == "GovernmentBond" OR .InstrumentType == "StateBond"))
forall govtSecurities grouped by .Issuer {
   let issuerValue = sum .Value of Issuer relative to Portfolio
   if issuerValue > 35% {
      let issues = Issuer grouped by .Issue
      require count issues >= 6
      forall issues {
         require sum .Value of Issue relative to Portfolio <= 30%
      }
   }
}
    |]

ruleIV :: Text
ruleIV =
    [text|
let otcPositions = Portfolio where (.InstrumentType == "OTC")
let nonApprovedCounterparties = otcPositions where ((.Counterparty == "SmallCompanyX" OR (.Counterparty == "SmallCompanyY" OR .Counterparty == "SmallCompanyZ")))
let approvedCounterparties = otcPositions where ((.Counterparty == "HugeCorpA" OR (.Counterparty == "HugeCorpB" OR .Counterparty == "HugeCorpC")))
forall nonApprovedCounterparties grouped by .Counterparty {
   require sum .Exposure of Counterparty relative to Portfolio <= 5.0%
}
forall approvedCounterparties grouped by .Counterparty {
   require sum .Exposure of Counterparty relative to Portfolio <= 10.0%
}
    |]
