module Main where

import LangPrelude
import qualified Parse
import qualified Text.Show.Pretty
import qualified Text.Megaparsec
import qualified Data.Text as T


parse :: Text -> IO ()
parse input =
    case Text.Megaparsec.parse Parse.documentParser "" input of
        Left  e -> putStr (Text.Megaparsec.errorBundlePretty e)
        Right x -> Text.Show.Pretty.pPrint x

main = parse test999


mainTest = test999

test999 = T.unlines
    [ "let homeCountry = \"DK\""
    , "let foreignCountries = portfolio grouped by Country where Country != homeCountry"
    , "for each country in foreignCountries {"
    , "    let relativeCountryValue = sum Value of country relative to Portfolio"
    , "    let numCountrySecurities = count (country grouped by SecurityId)"
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
        [ "let countryCount = count (portfolio grouped by Country)"
        , "for each Country in portfolio              "
        , "{"
        , "   for each Issuer in Country {              "
        , "      rule: average Exposure of Issuer < 3   "
        , "      rule: sum Exposure of Issuer relative to Country <= 5%"
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

