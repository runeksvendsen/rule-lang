{-# LANGUAGE QuasiQuotes #-}
module Main
( main
)
where

import LangPrelude
import qualified Absyn
import QuasiQuote (rulelang)
import qualified Text.Show.Pretty


main :: IO ()
main = Text.Show.Pretty.pPrint lol

lol =
    [rulelang|
let issuers = Portfolio grouped by .Issuer
forall issuers {
   require sum .Value of Issuer relative to Portfolio <= 10%
}
    |]
