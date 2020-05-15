module Spec.Parse
( scProps
, spec
  -- * Used by other tests
, printParseExpr
, printParse
)
where

import Prelude
import qualified Parse
import qualified Pretty

import Orphans ()

import Test.Tasty
import Test.Tasty.SmallCheck  as SC
import qualified Data.Text                        as T
import qualified Text.Megaparsec.Error
import qualified Data.List.NonEmpty as NE

import Test.Hspec
import qualified Test.Hspec.SmallCheck  as SC
import Debug.Trace (trace)


printParseRuleExpr :: [Parse.RuleExpr] -> Either String String
printParseRuleExpr =
    printParse (T.unlines . Pretty.ppLines "  ") Parse.ruleParserDoc

printParseExpr :: Parse.Expr -> Either String String
printParseExpr =
    printParse Pretty.ppExpr Parse.pExpr

printParseLiteral :: Parse.Literal -> Either String String
printParseLiteral =
    printParseExpr . Parse.Literal

printParseValueExpr :: Parse.ValueExpr -> Either String String
printParseValueExpr =
    printParseExpr . Parse.ValueExpr

printParseBoolExpr :: Parse.BoolExpr -> Either String String
printParseBoolExpr =
    printParseExpr . Parse.BoolExpr

printParseDataExpr :: Parse.DataExpr -> Either String String
printParseDataExpr =
    printParseExpr . Parse.DataExpr

printParseMap :: Parse.FieldName -> Parse.DataExpr -> Either String String
printParseMap fieldName dataExpr =
    printParseExpr $ Parse.Map (Parse.Literal $ Parse.FieldName fieldName) (Parse.DataExpr dataExpr)


-- | "parse (prettyPrint absyn) == absyn"
printParse
    :: (Eq absyn, Show absyn)
    => (absyn -> T.Text)
    -> Parse.Parser absyn
    -> absyn
    -> Either String String
printParse pp parser absyn =
    let prettyPrinted = pp absyn
        parse = Parse.parse parser ""
        debugOn pp' = T.unpack ("\n" <> pp') `trace` pp'
        debugOff = id
    in case parse (debugOff prettyPrinted) of
        Left e ->
            Left $ T.unpack $ T.unlines
                [ "this fails to parse:"
                , ""
                , prettyPrinted
                , "error:"
                , T.pack $ Text.Megaparsec.Error.errorBundlePretty e
                ]
        Right res ->
            if res == absyn
                then Right (T.unpack "")
                else Left $ T.unpack $ T.unlines
                        ["this is parsed incorrectly:"
                        , ""
                        , prettyPrinted
                        , "as:"
                        , ""
                        , T.pack $ show res
                        ]

scProps :: TestTree
scProps = testGroup "parse (prettyPrint absyn) == absyn"
    [
      depth 3 $ SC.testProperty "Literal" printParseLiteral
    , depth 2 $ SC.testProperty "ValueExpr" printParseValueExpr
    , depth 2 $ SC.testProperty "BoolExpr" printParseBoolExpr
    , depth 3 $ SC.testProperty "DataExpr" printParseDataExpr
    , depth 3 $ SC.testProperty "Map" printParseMap
    , depth 2 $ SC.testProperty "Expr" printParseExpr
    , depth 2 $ SC.testProperty "[RuleExpr]" printParseRuleExpr
    ]
  where
    depth d = localOption (SC.SmallCheckDepth d)

spec :: Spec
spec =
    describe "parse (prettyPrint absyn) == absyn" $ do
        it "Literal" $
            SC.property printParseLiteral
        it "ValueExpr" $
            SC.property printParseValueExpr
        it "BoolExpr" $
            SC.property printParseBoolExpr
        it "DataExpr" $
            SC.property printParseDataExpr
        it "Map" $
            SC.property printParseMap
        it "Expr" $
            SC.property printParseExpr
        it "[RuleExpr]" $
            SC.property printParseRuleExpr
