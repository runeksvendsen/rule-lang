{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import LangPrelude
import qualified Output
import qualified Analyze.Check
import qualified Eval.GroupComparison --                        as Eval
import qualified Eval.Types
import qualified Tree                        as Tree
-- import qualified Rules.CountryBondValue           as Rule
import qualified Rules.DataExpr
import           System.Environment               (getArgs)
import           System.IO                        (stderr, hPutStrLn)
import qualified Data.Text                        as T
import qualified Data.Aeson                       as Json
import qualified Data.ByteString.Lazy.Char8       as Char8
import qualified Data.List.NonEmpty               as NE


main :: IO ()
main = do
    inputFile <- argOrFail <$> getArgs
    positions <- toNonEmpty . value . handleDecodeResult <$> Json.eitherDecodeFileStrict' inputFile
    let (tree, res) = handleEvalResult $
            Eval.runEvalData positions Rules.DataExpr.issuersAbove5Pct
    printJson res
    hPutStrLn stderr . Tree.drawTree $ tree
  where
    toNonEmpty = fromMaybe (error "ERROR: Empty input data") . NE.nonEmpty
    printJson = Char8.putStrLn . Json.encode . Output.toObjectSecId . NE.fromList
    handleEvalResult (Left e) = error $ "An error occurred evaluating the rule:\n" ++ T.unpack e
    handleEvalResult (Right r) = r
    handleDecodeResult (Left e) = error $ "ERROR: JSON decoding error: \n" ++ e
    handleDecodeResult (Right r) = r
    argOrFail [inputFile] = inputFile
    argOrFail _ = error "ERROR: provide JSON input file name as argument"

mainCheck :: IO ()
mainCheck = do
    let errors = Analyze.Check.checkData Rule.ruleExpr
    let errorCount = length errors
    putStrLn $ printf ("%d error(s)%s") errorCount (if errorCount > 0 then ":" else "" :: String)
    forM_ errors (\s -> putStrLn . T.unpack $ "\t" <> s)

data JsonData = JsonData
    { value   :: [Eval.Position]
    } deriving Generic

instance Json.FromJSON JsonData
