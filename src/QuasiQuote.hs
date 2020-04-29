{-# LANGUAGE TemplateHaskell #-}
module QuasiQuote
( rulelang
)
where

import LangPrelude
import Parse
import qualified Data.Text as T
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import qualified Text.Megaparsec.Error


parse' :: Text -> Either (Text.Megaparsec.Error.ParseErrorBundle Text Void) [RuleExpr]
parse' = Parse.parse Parse.ruleParserDoc ""

rulelang :: QuasiQuoter
rulelang = QuasiQuoter {
      quoteExp = \str -> do
          absyn <- case parse' (toS str) of
              Left e -> do
                  fail $ Text.Megaparsec.Error.errorBundlePretty e
              Right absyn ->
                  return absyn
          dataToExpQ (\a -> liftText <$> cast a) absyn
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
  where
    -- Source: https://stackoverflow.com/a/38182444/700597
    liftText :: T.Text -> Q Exp
    liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)
