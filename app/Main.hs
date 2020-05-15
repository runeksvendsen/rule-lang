{-# LANGUAGE QuasiQuotes #-}
module Main
( main
)
where

import LangPrelude
import qualified Pretty
import qualified Examples.Rules
import qualified Examples.Expr
import qualified Text.Show.Pretty


main :: IO ()
main = do
    printExampleRules
    printExampleExpressions

printExampleRules :: IO ()
printExampleRules = forM_ Examples.Rules.allRules $ \(rule, name) -> do
    putStrLn $ "Rule " ++ name ++ ":"
    forM_ (Pretty.ppLines "   " rule) $ \line -> do
        putStrLn $ "\t" ++ (toS line)
    putStrLn ""

printExampleExpressions :: IO ()
printExampleExpressions = forM_ Examples.Expr.allExpressions $ \(expr, name) -> do
    putStrLn $ "#### " <> name <> ":"
    Text.Show.Pretty.pPrint expr
    putStrLn ""
