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
    title "Expression"
    forM_ exprTests $ runTests Pretty.ppExpr (Parse.pExpr <* M.eof)
    title "Expression (failure)"
    forM_ failureTestsExpr $ runTestsFail Pretty.ppExpr (Parse.pExpr <* M.eof)
    title "Rule (success)"
    forM_ ruleTests $ runTests (Pretty.pp "   ") Parse.ruleParserDoc
    title "Rule (failure)"
    forM_ failureTestsStmt $ runTestsFail (Pretty.pp "   ") Parse.ruleParserDoc
  where
    title str = putStrLn $ "\n##### " <> str <> " #####"
    exprTests =
        [ testsNumbers
        , testsStrings
        , testsNumComp
        , testsParens
        , testsBoolComp
        , testsGrouping
        , testsCombined
        ]
    failureTestsExpr =
        [ testsExprFailure
        ]
    ruleTests =
        [ testsSimple
        , testsBlockStmt
        ]
    failureTestsStmt =
        [ testsStmtFailure
        ]

testsSimple :: [(Text, [RuleExpr])]
testsSimple =
    [ ( T.unlines ["let a = true", "require a"]
      , [ Let "a" (Literal (FieldValue (Bool True)))
        , Rule (Var "a")
        ]
      )
    , (T.unlines ["let a=true"]
      , [ Let "a" (Literal (FieldValue (Bool True)))]
      )
    , (T.unlines ["let a= true"]
      , [ Let "a" (Literal (FieldValue (Bool True)))]
      )
    , (T.unlines ["let a =true"]
      , [ Let "a" (Literal (FieldValue (Bool True)))]
      )
    ]

testsBlockStmt :: [(Text, [RuleExpr])]
testsBlockStmt =
    [ ( T.unlines ["if a {", "}"]
      , [ If (Var "a") []
        ]
      )
    , ( T.unlines ["if a{", "}"]
      , [ If (Var "a") []
        ]
      )
    , ( T.unlines ["if a {", "", "", "", "}"]
      , [ If (Var "a") []
        ]
      )
    , ( T.unlines ["if a {", "require b", "}"]
      , [ If (Var "a") [Rule $ Var "b"]
        ]
      )
    , ( T.unlines ["if a", "{", "require b", "}"]
      , [ If (Var "a") [Rule $ Var "b"]
        ]
      )
    , ( T.unlines ["if a {", "require b", "}", "require c"]
      , [ If (Var "a") [Rule $ Var "b"]
        , Rule $ Var "c"
        ]
      )
    , ( T.unlines ["if a", "", "", "{", "", "", "require b", "", "", "}", "", ""]
      , [ If (Var "a") [Rule $ Var "b"]
        ]
      )
    ]

testsStmtFailure :: [Text]
testsStmtFailure =
    [ T.unlines ["if a {    require b     }"]
    , T.unlines ["if a {    require b ", "}"]
    , T.unlines ["if a {", "require b }    "]
    , T.unlines ["if a {", "require b ", "} require c"]
    ]

testsExprFailure :: [Text]
testsExprFailure =
    [ "5relative to5"
    , "5relative to 5"
    , "5%relative to 5"
    , "\"hey\"relative to 5"
    , "5 relative to5"
    , "5where5"
    , "5where 5"
    , "5 where5"
    ]

testsNumbers :: [(Text, Expr)]
testsNumbers =
    [ ( "12"
      , Literal (FieldValue (Number 12))
      )
    , ( "17.5"
      , Literal (FieldValue (Number 17.5))
      )
    , ( "7%"
      , Literal (Percent 7)
      )
    , ( "7.5%"
      , Literal (Percent 7.5)
      )
    , ( "-12"
      , Literal (FieldValue (Number $ -12))
      )
    , ( "-17.5"
      , Literal (FieldValue (Number $ -17.5))
      )
    , ( "-7%"
      , Literal (Percent $ -7)
      )
    , ( "-7.5%"
      , Literal (Percent $ -7.5)
      )
    ]

testsStrings :: [(Text, Expr)]
testsStrings =
    [ ( "\"hello\""
      , Literal (FieldValue (String "hello"))
      )
    , ( "\"HELLO\""
      , Literal (FieldValue (String "HELLO"))
      )
      -- escaped double quote
    , ( toS ['"', 'h', 'e', '\\', '"', 'y', '"'] -- he\"y
      , Literal (FieldValue (String $ toS ['h', 'e', '"', 'y']))
      )
      -- keyword-prefixed variables
    , ( "trueness"
      , Var "trueness"
      )
    , ( "counterParty"
      , Var "counterParty")
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

testsParens :: [(Text, Expr)]
testsParens =
    [ ( "a AND b OR c"
      , BoolExpr $ (BoolExpr $ Var "a" `And` Var "b") `Or` Var "c"
      )
    , ( "a AND (b OR c)"
      , aAnd_bOrC
      )
    , ( "a AND ( b OR c)"
      , aAnd_bOrC
      )
    , ( "a AND (b OR c )"
      , aAnd_bOrC
      )
    , ( "a AND ( b OR c )"
      , aAnd_bOrC
      )
    , ( "a  AND  ( b  OR  c  )  "
      , aAnd_bOrC
      )
    ]
  where
    aAnd_bOrC = BoolExpr $ Var "a" `And` (BoolExpr $ Var "b" `Or` Var "c")

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
    , ( "5 relative to 5"
      , ValueExpr $ num 5 `Relative` num 5
      )
    , ( "5 where 5"
      , DataExpr $ num 5 `Filter` num 5
      )
    , ("5 > 4"
      , BoolExpr $ Comparison (num 5) Gt (num 4)
      )
    , ("5>4"
      , BoolExpr $ Comparison (num 5) Gt (num 4)
      )
    , ("5> 4"
      , BoolExpr $ Comparison (num 5) Gt (num 4)
      )
    , ("5 >4"
      , BoolExpr $ Comparison (num 5) Gt (num 4)
      )
    , ("\"hey\">4"
      , BoolExpr $ Comparison (Literal $ FieldValue "hey") Gt (num 4)
      )
    , (".Value>.Exposure"
      , BoolExpr $ Comparison (mkField "Value") Gt (mkField "Exposure")
      )
    , ("true>4"
      , BoolExpr $ Comparison (Literal $ FieldValue (Bool True)) Gt (num 4)
      )
    , ("a>b"
      , BoolExpr $ Comparison (Var "a") Gt (Var "b")
      )
    ]
  where
    num = Literal . FieldValue
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
    -> Parse.Parser a
    -> [(Text, a)]
    -> IO ()
runTests pp p tests =
    forM_ tests runTest
  where
    parse' = M.parse p ""
    runTest (txt, expected) = do
        putStrLn . toS $ "Parsing:   " <> txt
        putStrLn . toS $ case parse' txt of
            Left e -> Color.red (toS $ M.errorBundlePretty e)
            Right absyn -> either id id (expectedButFound pp expected absyn)

-- Run tests expected to fail
runTestsFail
    :: Eq a
    => (a -> Text)
    -> Parse.Parser a
    -> [Text]
    -> IO ()
runTestsFail pp p tests =
    forM_ tests runTestFail
  where
    parse' = M.parse p ""
    runTestFail txt = do
        putStrLn . toS $ "Parsing:   " <> txt
        putStrLn . toS $ case parse' txt of
            Left e -> Color.green $ T.unlines ["Failed with: ", toS $ M.errorBundlePretty e]
            Right absyn -> Color.red $ T.unlines ["Expected failure, succeeded with: " <> toS (pp absyn)]

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
