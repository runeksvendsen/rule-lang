{-# LANGUAGE QuasiQuotes #-}
module Examples.Rules
( allRules
, ruleI
, ruleIa
, ruleII
, ruleIII
, ruleIV
, ruleV
, ruleVI
)
where

import LangPrelude
import qualified Absyn
import QuasiQuote (rulelang)


allRules :: [([Absyn.RuleExpr], String)]
allRules =
    [ (ruleI, "I")
    , (ruleII, "II")
    , (ruleIII, "III")
    , (ruleIV, "IV")
    , (ruleV, "V")
    , (ruleVI, "VI")
    ]

ruleI :: [Absyn.RuleExpr]
ruleI =
    [rulelang|
let issuers = Portfolio grouped by .Issuer
let portfolioValue = sum .Value of Portfolio
forall issuers {
   require sum .Value of Issuer relative to portfolioValue <= 10%
}
let issuersAbove5Pct = issuers where (sum .Value of Issuer relative to portfolioValue > 5%)
require sum .Value of issuersAbove5Pct relative to portfolioValue <= 40%
    |]

ruleIa :: [Absyn.RuleExpr]
ruleIa =
    [rulelang|
let issuers = Portfolio grouped by .Issuer
forall issuers {
   let portfolioValue = sum .Value of Portfolio
   require sum .Value of Issuer relative to portfolioValue <= 10%
}
let portfolioValue = sum .Value of Portfolio
let issuersAbove5Pct = issuers where (sum .Value of Issuer relative to portfolioValue > 5%)
require sum .Value of issuersAbove5Pct relative to portfolioValue <= 40%
    |]

ruleII :: [Absyn.RuleExpr]
ruleII =
    [rulelang|
let issuers = Portfolio grouped by .Issuer
let portfolioValue = sum .Value of Portfolio
forall issuers {
   let issuerValue = sum .Value of Issuer relative to portfolioValue
   require issuerValue <= 35%
   let issueCount = count Issuer grouped by .Issue
   if issuerValue > 30% {
      require issueCount >= 6
   }
}
    |]

ruleIII :: [Absyn.RuleExpr]
ruleIII =
    [rulelang|
let govtSecurities = Portfolio where (.InstrumentType == "GovernmentBond" OR .InstrumentType == "StateBond")
let portfolioValue = sum .Value of Portfolio
forall govtSecurities grouped by .Issuer {
   let issuerValue = sum .Value of Issuer relative to portfolioValue
   if issuerValue > 35% {
      let issues = Issuer grouped by .Issue
      require count issues >= 6
      forall issues {
         require sum .Value of Issue relative to portfolioValue <= 30%
      }
   }
}
    |]

ruleIV :: [Absyn.RuleExpr]
ruleIV =
    [rulelang|
let otcPositions = Portfolio where (.InstrumentType == "OTC")
let nonApprovedCounterparties = otcPositions where (.Counterparty == "SmallCompanyX" OR .Counterparty == "SmallCompanyY" OR .Counterparty == "SmallCompanyZ")
let approvedCounterparties = otcPositions where (.Counterparty == "HugeCorpA" OR .Counterparty == "HugeCorpB" OR .Counterparty == "HugeCorpC")
let portfolioExposure = sum .Exposure of Portfolio
forall nonApprovedCounterparties grouped by .Counterparty {
   require sum .Exposure of Counterparty relative to portfolioExposure <= 5%
}
forall approvedCounterparties grouped by .Counterparty {
   require sum .Exposure of Counterparty relative to portfolioExposure <= 10%
}
    |]

ruleV :: [Absyn.RuleExpr]
ruleV =
    [rulelang|
let securities = Portfolio grouped by .SecurityID
let betterThanBBB = securities where (.Rating == "AAA" OR .Rating == "AA" OR .Rating == "A")
let notBetterThanBBB = securities where (NOT (.Rating == "AAA" OR .Rating == "AA" OR .Rating == "A"))
let portfolioValue = sum .Value of Portfolio
forall betterThanBBB {
   require sum .Value of SecurityID relative to portfolioValue <= 5%
}
forall notBetterThanBBB {
   require sum .Value of SecurityID relative to portfolioValue <= 1%
}
    |]

ruleVI :: [Absyn.RuleExpr]
ruleVI =
    [rulelang|
let homeCountry = "DK"
let foreignCountryPositions = Portfolio where (.Country != homeCountry)
let portfolioValue = sum .Value of Portfolio
let foreignCountryValue = sum .Value of foreignCountryPositions relative to portfolioValue
let foreignCountryCount = count (foreignCountryPositions grouped by .Country)
if foreignCountryValue >= 80% {
    require foreignCountryCount >= 5
}
if foreignCountryValue >= 60% {
    require foreignCountryCount >= 4
}
if foreignCountryValue >= 40% {
    require foreignCountryCount >= 3
}
if foreignCountryValue <  40% {
    require foreignCountryCount >= 2
}
    |]
