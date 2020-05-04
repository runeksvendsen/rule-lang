{-# LANGUAGE QuasiQuotes #-}
module Unit.Parse
( main
, expectedButFound
)
where

import LangPrelude
import qualified Parse
import qualified Pretty
import Types
import Absyn as Absyn
import qualified Text.Megaparsec as M
import qualified Data.Text.ANSI as Color
import qualified Data.Text as T


main :: IO ()
main = do
    forM_ exprTests (uncurry $ runTests Pretty.ppExpr)
    forM_ ruleTests (uncurry $ runTests (Pretty.pp "   "))
  where
    exprTests =
        [ (testsNumComp,  Parse.pExpr)
        , (testsBoolComp, Parse.pExpr)
        , (testsGrouping, Parse.pExpr)
        , (testsCombined, Parse.pExpr)
        ]
    ruleTests =
        [ (testsSimple, Parse.ruleParserDoc)
        ]

testsSimple :: [(Text, [RuleExpr])]
testsSimple =
    [ ( T.unlines ["let a = true", "require a"]
      , [ Let "a" (Literal (FieldValue (Bool True)))
        , Rule (Var "a")
        ]
      )
    ]

testsNumComp :: [(Text, Expr)]
testsNumComp =
    [ ( ".Hello == \"LOL\""
      , BoolExpr $ Comparison (mkField "Hello") Eq (Literal $ FieldValue $ String "LOL")
      )
    , ( "sum .Value of Country >= 10000"
      , BoolExpr $ Comparison (ValueExpr $ FoldMap Sum (mkField "Value" `Map` Var "Country")) GtEq (Literal $ FieldValue 10000)
      )
    ]
  where
    mkField = Literal . FieldName

testsBoolComp :: [(Text, Expr)]
testsBoolComp =
    [ ( ".Hello == \"LOL\" AND sum .Value of Country >= 10000"
      , BoolExpr $ snd (head testsNumComp) `And` snd (second testsNumComp)
      )
    , ( "sum .Value of Country relative to Portfolio >= 10%"
      , BoolExpr $ Comparison
          (ValueExpr $ (ValueExpr $ FoldMap Sum (mkField "Value" `Map` Var "Country")) `Relative` Var "Portfolio")
          GtEq
          (Literal (Percent 10))
      )
    ]
  where
    second (_:x:_) = x
    mkField = Literal . FieldName

testsGrouping :: [(Text, Expr)]
testsGrouping =
    [ ( "x grouped by .Hello"
      , DataExpr $ GroupBy (Var "x") (mkField "Hello")
      )
    , ( "x where (.Hello == \"LOL\")"
      , xWhereHelloLol
      )
    , ( "x where (.Hello == \"LOL\") grouped by .Issuer"
      , DataExpr $ GroupBy xWhereHelloLol (Literal (FieldName "Issuer"))
      )
    , ( "Portfolio grouped by .Country where (sum .Value of Country relative to Portfolio >= 10%)"
      , DataExpr $
          let input = GroupBy (Var "Portfolio") (Literal $ FieldName "Country")
          in Filter (DataExpr input) (snd (second testsBoolComp))
      )
    ]
  where
    xWhereHelloLol =
        DataExpr $ Filter (Var "x") (BoolExpr $ Comparison (Literal (FieldName "Hello")) Eq (Literal $ FieldValue $ String "LOL"))
    mkField = Literal . FieldName
    second (_:x:_) = x

testsCombined :: [(Text, Expr)]
testsCombined =
    [ ( ".Hello == \"LOL\" AND sum .Value of Country where (.InstrumentType != \"Bond\") >= 10000"
      , BoolExpr $ (BoolExpr $ Comparison (Literal (FieldName "Hello")) Eq (Literal $ FieldValue $ String "LOL")) `And`
            (BoolExpr $ Comparison (ValueExpr foldValue) GtEq (Literal $ FieldValue 10000))
      )
    , ( ".Hello == \"LOL\" AND sum .Value of Country where (.InstrumentType != \"Bond\") relative to average .Exposure of Portfolio >= 5%"
      , BoolExpr $ (BoolExpr $ Comparison (Literal (FieldName "Hello")) Eq (Literal $ FieldValue $ String "LOL")) `And`
            (BoolExpr $ Comparison (ValueExpr $ ValueExpr foldValue `Relative` ValueExpr foldExposure) GtEq (Literal $ Percent 5))
      )
    ]
  where
    foldValue = FoldMap Sum (mkField "Value" `Map` DataExpr grouping)
    foldExposure = FoldMap Avg (mkField "Exposure" `Map` Var "Portfolio")
    grouping = Var "Country" `Filter` (BoolExpr $ Comparison (Literal (FieldName "InstrumentType")) NEq (Literal $ FieldValue $ String "Bond"))
    mkField = Literal . FieldName

runTests
    :: Eq a
    => (a -> Text)
    -> [(Text, a)]
    -> Parse.Parser a
    -> IO ()
runTests pp tests p =
    forM_ tests runTest
  where
    parse' = M.parse p ""
    runTest (txt, expected) = do
        putStrLn . toS $ "Parsing:   " <> txt
        putStrLn . toS $ case parse' txt of
            Left e -> Color.red (toS $ M.errorBundlePretty e)
            Right absyn -> either id id (expectedButFound pp expected absyn)

expectedButFound
  :: Eq a
  => (a -> Text)  -- Pretty-printing function
  -> a
  -> a
  -> Either Text Text
expectedButFound pp expected actual =
    if actual == expected
        then Right . Color.green $ T.unlines [ "Success:   " <> pp expected ]
        else Left . Color.red . T.unlines $
             [ "Expected:  " <> toS (pp expected)
             , "Found:     " <> toS (pp actual)
             ]
