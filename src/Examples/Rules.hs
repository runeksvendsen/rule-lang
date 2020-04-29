{-# LANGUAGE QuasiQuotes #-}
module Examples.Rules
( allRules
)
where

import LangPrelude
import qualified Absyn
import QuasiQuote (rulelang)


allRules :: [([Absyn.RuleExpr], String)]
allRules =
    [
      (ruleI, "I")
    , (ruleII, "II")
    , (ruleIII, "III")
    , (ruleIV, "IV")
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

ruleII :: [Absyn.RuleExpr]
ruleII =
    [rulelang|
let issuers = Portfolio grouped by .Issuer
let portfolioValue = sum .Value of Portfolio
forall issuers {
   let issuerValue = sum .Value of Issuer relative to portfolioValue
   require issuerValue <= 35.0%
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
   require sum .Exposure of Counterparty relative to portfolioExposure <= 5.0%
}
forall approvedCounterparties grouped by .Counterparty {
   require sum .Exposure of Counterparty relative to portfolioExposure <= 10.0%
}
    |]
