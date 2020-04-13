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
import qualified Data.List.NonEmpty as NE

import Test.Hspec
import qualified Test.Hspec.SmallCheck  as SC
import Debug.Trace (trace)


-- | "parse (prettyPrint absyn) == absyn"
printParse :: NE.NonEmpty Parse.RuleExpr -> Either String String
printParse absyn =
    let prettyPrint = Pretty.pp "  "
        prettyPrinted = prettyPrint absyn
        parse = Parse.parse Parse.documentParser ""
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
scProps = testGroup "Properties"
  [ SC.testProperty "parse (prettyPrint absyn) == absyn" printParse
  ]

spec :: Spec
spec = parallel $ do
    describe "Properties" $ do
      it "parse (prettyPrint absyn) == absyn" $
         SC.property printParse
