{-# LANGUAGE QuasiQuotes #-}
module Main
( main
)
where

import LangPrelude
import Absyn
import qualified Pretty
import qualified Eval
import qualified Examples.Rules
import qualified Text.Show.Pretty


main :: IO ()
main = forM_ Examples.Rules.allRules $ \(rule, name) -> do
    putStrLn $ "Rule " ++ name ++ ":"
    forM_ (Pretty.ppLines "   " rule) $ \line -> do
        putStrLn $ "\t" ++ (toS line)
    putStrLn ""
