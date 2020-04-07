module Spec.Parse
( scProps
, spec
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

import Test.Hspec
import qualified Test.Hspec.SmallCheck  as SC
import Debug.Trace (trace)


-- | "parse (prettyPrint absyn) == absyn"
printParse :: Parse.RuleExpr -> Either String String
printParse absyn =
    let prettyPrint = Pretty.pp "  "
        prettyPrinted = prettyPrint absyn
        parse = Parse.parse Parse.documentParser ""
        debugTrace str = id
    in case T.unpack ("\n" <> prettyPrinted) `debugTrace` parse prettyPrinted of
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
scProps = testGroup "Properties"
  [ SC.testProperty "parse (prettyPrint absyn) == absyn" printParse
  ]

spec :: Spec
spec = parallel $ do
    describe "Properties" $ do
      it "parse (prettyPrint absyn) == absyn" $
         SC.property printParse
