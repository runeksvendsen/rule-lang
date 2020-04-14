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
import Language.Haskell.TH.Syntax (CharPos, lift, qReport)
import qualified Text.Megaparsec.Error
import qualified Text.Megaparsec.Pos as Pos
import qualified Data.List.NonEmpty as NE


parse' :: Text -> Either (Text.Megaparsec.Error.ParseErrorBundle Text Void) (NonEmpty RuleExpr)
parse' = Parse.parse Parse.documentParser ""

rulelang :: QuasiQuoter
rulelang = QuasiQuoter {
      quoteExp = \str -> do
          (filename, start, end) <- location'
          let resE = parse' (toS str)
          absyn <- case resE of
              Left e -> do
                  qReport True $ unlines
                      [ "Filename: " ++ filename
                      , "LOC start: " ++ show start
                      , "LOC end: " ++ show end
                      , Text.Megaparsec.Error.errorBundlePretty e
                      ]
                  return (NE.fromList [])
              Right absyn -> return absyn
          dataToExpQ (\a -> liftText <$> cast a) absyn
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
  where
    -- Source: https://stackoverflow.com/a/38182444/700597
    liftText :: T.Text -> Q Exp
    liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

location' :: Q (String, CharPos, CharPos)
location' =
    aux <$> location
  where
    aux loc = (loc_filename loc, loc_start loc, loc_end loc)
