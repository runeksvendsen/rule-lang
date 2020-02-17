module Main where

import Prelude
import qualified Analyze.Check
import qualified Test

import           Text.Printf            (printf)
import           Control.Monad          (forM_)
import qualified Data.Text              as T


main :: IO ()
main = do
    let errors = Analyze.Check.checkData Test.thirtyfiveThirtySix
    let errorCount = length errors
    putStrLn $ printf ("%d error(s)%s") errorCount (if errorCount > 0 then ":" else "" :: String)
    forM_ errors (\s -> putStrLn . T.unpack $ "\t" <> s)
