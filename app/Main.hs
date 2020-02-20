{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import LangPrelude
import qualified Output
import qualified Analyze.Check
import qualified Eval.Eval                        as Eval
import qualified Test

import           Text.Printf                      (printf)
import           System.Environment               (getArgs)
import qualified Data.Text                        as T
import qualified Data.Aeson                       as Json
import qualified Data.ByteString.Lazy.Char8       as Char8
import qualified Data.List.NonEmpty               as NE


main :: IO ()
main = do
    inputFile <- argOrFail <$> getArgs
    positions <- value . handleDecodeResult <$> Json.eitherDecodeFileStrict' inputFile
    printJson . handleEvalResult $ Eval.eval (nonEmpty positions) Test.thirtyfiveThirtySix
  where
    nonEmpty = fromMaybe (error "ERROR: Empty input data") . NE.nonEmpty
    printJson = Char8.putStrLn . Json.encode . Output.toObjectSecId
    handleEvalResult (Left e) = error $ "An error occurred evaluating the rule:\n" ++ T.unpack e
    handleEvalResult (Right r) = r
    handleDecodeResult (Left e) = error $ "ERROR: JSON decoding error: \n" ++ e
    handleDecodeResult (Right r) = r
    argOrFail [inputFile] = inputFile
    argOrFail _ = error "ERROR: provide JSON input file name as argument"

mainCheck :: IO ()
mainCheck = do
    let errors = Analyze.Check.checkData Test.thirtyfiveThirtySix
    let errorCount = length errors
    putStrLn $ printf ("%d error(s)%s") errorCount (if errorCount > 0 then ":" else "" :: String)
    forM_ errors (\s -> putStrLn . T.unpack $ "\t" <> s)

data JsonData = JsonData
    { value   :: [Eval.Position]
    } deriving Generic

instance Json.FromJSON JsonData
