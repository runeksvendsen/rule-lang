{-# LANGUAGE QuasiQuotes #-}
module Main
( main
)
where

import LangPrelude
import qualified Parse
import qualified Pretty
import qualified Text.Show.Pretty
import qualified Text.Megaparsec
import qualified Data.Text as T
import NeatInterpolation (text)
-- TMP
import Data.Void


everything :: [Text]
everything =
    [ ruleI
    , ruleII
    , ruleIII
    , ruleIV
    ]

main :: IO ()
main = forM_ everything $ \code -> do
    let absyn = parse' code
        prettyPrinted = Pretty.pp "   " absyn
    if prettyPrinted /= code
        then error . toS $ T.unlines
                [ "### ERROR. Input:"
                , code
                , "### Printed incorrectly:"
                , prettyPrinted
                , "### Absyn:"
                , toS $ Text.Show.Pretty.ppShow absyn
                ]
        else putStrLn "success"
  where
    parse' input = case Text.Megaparsec.parse Parse.documentParser "" input of
        Left  e -> error (Text.Megaparsec.errorBundlePretty e)
        Right x -> x

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
forall otcPositions grouped by .Counterparty {
   let counterpartyExposure = sum .Exposure of Counterparty relative to Portfolio
   // non-approved credit institutions
   if (Counterparty == "SmallCompanyX" OR (Counterparty == "SmallCompanyY" OR Counterparty == "SmallCompanyZ")) {
      require counterpartyExposure <= 5.0%
   }
   // approved credit institutions
   if (Counterparty == "HugeCorpA" OR (Counterparty == "HugeCorpB" OR Counterparty == "HugeCorpC")) {
      require counterpartyExposure <= 10.0%
   }
}
    |]

test999 :: Text
test999 = T.unlines
    [ "let homeCountry = \"DK\""
    , "let foreignCountries = portfolio grouped by Country where (Country != homeCountry)"
    , "for each country in foreignCountries {"
    , "    let relativeCountryValue = sum Value of country relative to Portfolio"
    , "    let numCountrySecurities = count country grouped by SecurityId"

    , "    if relativeCountryValue > 60% {"
    , "        numCountrySecurities >= 20"
    , "        for each SecurityID:"
    , "            sum Value of SecurityID relative to Country <= 5%"
    , "    } else if relativeCountryValue > 40% {"
    , "        numCountrySecurities >= 10"
    , "    }"
    , "}"
    ]

test22 :: Text
test22 =
    T.unlines
        [ "let countryCount = count Portfolio grouped by Country"
        , "for each Country in portfolio              "
        , "{"
        , "   for each Issuer in Country {              "
        , "      rule: (average Exposure of Issuer < 3 AND sum Exposure of Issuer relative to Country <= 5%)   "
        , "   }                                         "
        , "}                                            "
        , "rule: sum DirtyValue of Country >= 100000    "
        ]

test0 :: Text
test0 =
    T.unlines
        [ "rule: sum DirtyValue of Country >= 100000"
        , "rule: sum DirtyValue of Country >= 100000"
        ]

test1 :: Text
test1 =
    T.unlines
        [ "for each Country in portfolio {"
        , "   rule: sum DirtyValue of Country >= 100000"
        , "}"
        , "AND"
        , "rule: average Exposure of portfolio < 3"
        ]

test2 :: Text
test2 =
    T.unlines
        [ "for each Country in (portfolio grouped by Country where sum DirtyValue of Country >= 10000):"
        , "   rule: sum DirtyValue of Country >= 100000"
        ]

test3 :: Text
test3 =
    T.unlines
        [ "let portfolioValue = sum DirtyValue of portfolio"
        , "for each Country in portfolio:"
        , "   rule: sum DirtyValue of Country <= 90000000"
        ]

test4 :: Text
test4 =
    T.unlines
        [ "let portfolioValue = sum DirtyValue of portfolio"
        , "for each Country in portfolio:"
        , "   rule: sum DirtyValue of Country relative to portfolioValue <= 10%"
        ]

