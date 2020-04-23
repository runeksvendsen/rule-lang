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


main :: IO ()
main =
    forM_ allTests (uncurry runTests
  )
  where
    allTests =
        [ (testsNumComp,  Parse.pExpr)
        , (testsBoolComp, Parse.pExpr)
        , (testsGrouping, Parse.pExpr)
        , (testsCombined, Parse.pExpr)
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
    ]
  where
    second (_:x:_) = x

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
    ]
  where
    xWhereHelloLol =
        DataExpr $ Filter (Var "x") (BoolExpr $ Comparison (Literal (FieldName "Hello")) Eq (Literal $ FieldValue $ String "LOL"))
    mkField = Literal . FieldName

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
    foldExposure = FoldMap Average (mkField "Exposure" `Map` Var "Portfolio")
    grouping = Var "Country" `Filter` (BoolExpr $ Comparison (Literal (FieldName "InstrumentType")) NEq (Literal $ FieldValue $ String "Bond"))
    mkField = Literal . FieldName

runTests
    :: [(Text, Expr)]
    -> Parse.Parser Expr
    -> IO ()
runTests tests p =
    forM_ tests runTest
  where
    parse' = M.parse p ""
    runTest (txt, expected) = do
        putStrLn . toS $ "Parsing:   " <> txt
        putStrLn $ case parse' txt of
            Left e -> M.errorBundlePretty e
            Right absyn -> either id id (expectedButFound expected absyn)

expectedButFound :: Expr -> Expr -> Either String String
expectedButFound expected actual =
    if actual == expected
        then Right $ unlines [ "Success:   " <> toS (Pretty.ppExpr expected) ]
        else Left $ unlines $
             [ "Expected:  " <> toS (Pretty.ppExpr expected)
             , "Found:     " <> toS (Pretty.ppExpr actual)
             ]
