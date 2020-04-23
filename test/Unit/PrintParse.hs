module Unit.PrintParse where

import LangPrelude
import qualified Parse
import qualified Pretty
import qualified Spec.Parse
import qualified Unit.Parse
import Types
import Absyn
import qualified Text.Megaparsec as M
import           Data.String              as String   (IsString(fromString))
import qualified Data.Text                        as T
import qualified Text.Megaparsec.Error
import qualified Text.Show.Pretty


mkStr str = Literal (FieldValue $ fromString str)

test1 = BoolExpr $ BoolExpr (mkStr "a" `Or` mkStr "b") `And` mkStr "c"

main :: IO ()
main =
    case runTest test1 of
        Left e -> do putStrLn "ERROR!"
                     putStrLn e
        Right r -> do putStrLn r

runTest :: Expr -> Either String String
runTest expected =
    let prettyPrinted = Pretty.ppExpr expected
        parse = Parse.parse Parse.pExpr ""
    in case parse prettyPrinted of
        Left e ->
            Left $ T.unpack $ T.unlines
                [ "this fails to parse:"
                , ""
                , prettyPrinted
                , "error:"
                , T.pack $ Text.Megaparsec.Error.errorBundlePretty e
                ]
        Right result ->
            handleResult expected result

handleResult expected actual =
    if actual == expected
        then Right $ unlines [ "Success:   " <> toS (Text.Show.Pretty.ppShow expected) ]
        else Left $ unlines $
             [ "Expected:  " <> toS (Text.Show.Pretty.ppShow expected)
             , "Found:     " <> toS (Text.Show.Pretty.ppShow actual)
             ]
