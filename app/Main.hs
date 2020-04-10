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


parse :: Text -> IO ()
parse input =
    case Text.Megaparsec.parse Parse.documentParser "" input of
        Left  e -> putStr (Text.Megaparsec.errorBundlePretty e)
        Right x -> Text.Show.Pretty.pPrint x

main :: IO ()
main = forM_ everything $ \code -> do
    let absyn = parse' code
    putStrLn (toS $ Pretty.pp "  " absyn)
    putStrLn ""
    Text.Show.Pretty.pPrint absyn
    putStrLn ""
  where
    parse' input = case Text.Megaparsec.parse Parse.documentParser "" input of
        Left  e -> error (Text.Megaparsec.errorBundlePretty e)
        Right x -> x

ruleI = T.unlines
    [ "let issuers = Portfolio grouped by Issuer"
    , "for all issuers {"
    , "  require sum Value of Issuer relative to Portfolio <= 10%"
    , "}"
    , "let greaterThan5PctIssuers = issuers where (sum Value of Issuer relative to Portfolio > 5%)"
    , "require sum Value of greaterThan5PctIssuers <= 40%"
    ]

ruleII = T.unlines
    [ "let issuers = Portfolio grouped by Issuer"
    , "for all issuers {"
    , "  let issuerValue = sum Value of Issuer relative to Portfolio"
    , "  require issuerValue <= 35%"
    , "  let issueCount = count Issuer grouped by Issue"
    , "  if issuerValue > 30% {"
    , "    require issueCount >= 6"
    , "  }"
    , "}"
    ]

ruleIII = T.unlines
    [ "let govtSecurities = Portfolio where ((InstrumentType == \"GovernmentBond\" OR InstrumentType == \"StateBond\"))"
    , "for all govtSecurities grouped by Issuer {"
    , "  let issuerValue = sum Value of Issuer relative to Portfolio"
    , "  if issuerValue > 35% {"
    , "    let issues = Issuer grouped by Issue"
    , "    require count issues >= 6"
    , "    for all issues {"
    , "      require sum Value of Issue relative to Portfolio <= 30%"
    , "    }"
    , "  }"
    , "}"
    ]

everything :: [Text]
everything =
    [ ruleIII
    ]

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

