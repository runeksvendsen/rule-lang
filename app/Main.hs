{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import LangPrelude
import qualified Output
import qualified Sunburst.D3
import qualified Eval.RuleExpr
import qualified Eval.DataExpr
import qualified Eval.Types
import qualified Tree                             as Tree
import qualified Rules.CountryBondValue
import qualified Rules.FiveTenForty
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
    evalRule "CountryBondValue" positions Rules.CountryBondValue.ruleExpr
    evalRule "FiveTenForty" positions Rules.FiveTenForty.ruleExpr
  where
    evalRule name positions rule = do
        hPutStrLn stderr $ "Rule: " <> name
        let (rulePassed, langErrors) = handleEvalResult $
                Eval.RuleExpr.runEvalRule positions rule
        hPutStrLn stderr $ "Passed: " ++ show rulePassed
        hPutStrLn stderr $ "Errors:"
        mapM_ (hPutStrLn stderr . ("\t" ++)) (map showResult langErrors)
        hPutStrLn stderr $ "\n"
    securityIdTree :: Tree.Tree [Eval.Types.Position] -> Tree.Tree [Text]
    securityIdTree = fmap (map (showValue . Output.getSecurityIdOrFail))
    toNonEmpty = fromMaybe (error "ERROR: Empty input data") . NE.nonEmpty
    printJson = Char8.putStrLn . Json.encode . Output.toObjectSecId . NE.fromList
    handleDecodeResult (Left e) = error $ "ERROR: JSON decoding error: \n" ++ e
    handleDecodeResult (Right r) = r
    argOrFail [inputFile] = inputFile
    argOrFail _ = error "ERROR: provide JSON input file name as argument"

handleEvalResult (Left e) = error $ "An error occurred evaluating the rule:\n" ++ T.unpack e
handleEvalResult (Right r) = r

showResult (Eval.DataExpr.Result posList _ status) =
    unwords [ toS $ T.concat . NE.toList $ NE.map (showValue . Output.getSecurityIdOrFail) posList
            , ":"
            , show status
            ]

sunburstTree positions = do
    hPutStrLn stderr $ "DataExpr:"
    let (tree, dataErrors) = handleEvalResult $
            Eval.DataExpr.runEvalData positions Rules.CountryBondValue.countryIssuers
    Char8.putStrLn . Json.encode $
        Sunburst.D3.convert (fromMaybe 100000 . fmap jsonDouble . LangPrelude.lookup "DirtyValue") tree
    -- hPutStrLn stderr $ Tree.drawTree $ securityIdTree tree
    hPutStrLn stderr $ "\nDataExpr errors:"
    mapM_ (hPutStrLn stderr) (map showResult dataErrors)
  where
    jsonDouble (Json.Number a) = realToFrac a
    jsonDouble a = error $ "BUG: " ++ show a

data JsonData = JsonData
    { value   :: [Eval.Types.Position]
    } deriving Generic

instance Json.FromJSON JsonData
