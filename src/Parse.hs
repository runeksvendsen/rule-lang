{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parse
( documentParser
, module Absyn
, Text.Megaparsec.parse
)
where

import LangPrelude
import qualified Prelude
import qualified Comparison
import Types
import Absyn as Absyn

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char
import qualified Text.Megaparsec.Debug as D

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.List (foldl')

type Parser = Text.Megaparsec.Parsec Void Text

-- ######### Helpers

-- Line comments start with //
lineComment :: Parser ()
lineComment = L.skipLineComment "//"

-- Block comments are inside /* */
blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

-- A space-consumer that consumes newlines
scn :: Parser ()
scn = L.space (void Text.Megaparsec.Char.spaceChar) lineComment blockComment

-- A space-consumer that does NOT consume newlines
sc :: Parser ()
sc = L.space (void $ Text.Megaparsec.oneOf (" \t" :: String)) lineComment blockComment

skipTrailingWhitespace :: Parser a -> Parser a
skipTrailingWhitespace = L.lexeme sc

skipTrailingNewline :: Parser a -> Parser a
skipTrailingNewline = L.lexeme scn

keyword :: Text -> Parser ()
keyword = void . skipTrailingWhitespace . chunk

-- #########

documentParser :: Parser RuleExpr
documentParser = pRuleExpr <* eof

pRuleExpr :: Parser RuleExpr
pRuleExpr = D.dbg "pRuleExpr" $
    pBoolRuleExpr <|> pLet

-- |
pBoolRuleExpr :: Parser RuleExpr
pBoolRuleExpr = D.dbg "pBoolRuleExpr" $ do
    oneOrMore <- some $ skipTrailingNewline (pForEach <|> pRule)
    case oneOrMore of
        [] -> error "BUG: 'some' returned empty list"
        [one] -> return one
        first:nonEmptyRest -> return $ And first (fromList nonEmptyRest)

pLet :: Parser RuleExpr
pLet = do
    (varName, groupExpr) <- skipTrailingNewline pLetHeader
    scope <- pRuleExpr
    return $ Let varName groupExpr scope

pLetHeader :: Parser (Text, GroupValueExpr)
pLetHeader = D.dbg "pLetHeader" $ do
    keyword "let"
    varName <- skipTrailingWhitespace pVar
    keyword "="
    groupExpr <- skipTrailingWhitespace pGroupValueExpr
    return (varName, groupExpr)

forEachHeader :: Parser (GroupValueExpr, GroupValueExpr)
forEachHeader = D.dbg "forEachHeader" $ do
    keyword "for"
    keyword "each"
    fieldName <- skipTrailingWhitespace pGroupValueExpr
    keyword "in"
    dataExpr <- skipTrailingWhitespace pGroupValueExpr
    return (fieldName, dataExpr)

pForEach :: Parser RuleExpr
pForEach = D.dbg "pForEach" $ do
    (fieldNameExpr, dataExpr) <- forEachHeader
    scope <- braces pRuleExpr
    return $ Foreach fieldNameExpr dataExpr scope

-- pIfThenElse :: Parser RuleExpr
-- pIfThenElse = D.dbg "pIfThenElse" $ do
--     keyword "if"
--     comparison <- pComparison
--     thenExpr <- braces pRuleExpr
--     elseIfs <- many $ do
--         keyword "else"
    -- if <comparison> {
    --         ...
    --     } else if <comparison> {
    --         ...
    --     }

braces :: Parser a -> Parser a
braces = Text.Megaparsec.between
    (discardSurroundingWhitespace $ Text.Megaparsec.Char.char '{')  -- discard whitespace+newline(s) after '{'
    (discardSurroundingWhitespace $ Text.Megaparsec.Char.char '}')  -- discard whitespace+newline(s) after '}'
  where
    discardSurroundingWhitespace p = scn >> p >> scn

pRule :: Parser RuleExpr
pRule = D.dbg "pRule" $ do
    D.dbg "rule:" $ keyword "rule:"
    Rule <$> pComparison

pBoolCompare :: Parser BoolCompare
pBoolCompare = D.dbg "pBoolCompare" $ do
    let bCompareSymbols = foldr1 (<|>) (map Text.Megaparsec.Char.char "=!><")
    string <- some bCompareSymbols
    case Prelude.lookup string stringToValue of
        Just bComp -> return bComp
        Nothing -> failParse "Unknown operator" (map (toS . fst) Comparison.stringToValue)

-- TODO: document "reserved" words, e.g. "sum" or "average".
-- 'Literal' is last because otherwise e.g. "sum" is parsed as variable.
pGroupValueExpr :: Parser GroupValueExpr
pGroupValueExpr = D.dbg "pGroupValueExpr" $
        try pGroupOp
    <|> try pDataExpr
    <|> try (Literal <$> pLiteral)
    <|> try (Var <$> pVar)

pLiteral :: Parser Literal
pLiteral = D.dbg "pLiteral" $
        try pPercentage
    <|> try pFieldName
    -- NB: 'Number' (FieldValue) will also match integers
    <|> try (Integer <$> L.decimal)
    <|> try (FieldValue <$> pFieldValue)


pPercentage :: Parser Literal
pPercentage = do
    num <- pNumber
    void $ Text.Megaparsec.Char.char '%'
    return $ Percent num

pNumber :: Parser Number
pNumber = D.dbg "pNumber" $
    try (fromReal @Double <$> L.float) <|> fromIntegral <$> L.decimal

pFieldValue :: Parser FieldValue
pFieldValue = D.dbg "pFieldValue" $
        String . toS <$> pStringLiteral
    <|> Number <$> pNumber
  where
    pStringLiteral :: Parser String
    pStringLiteral = Text.Megaparsec.Char.char '\"' *> manyTill L.charLiteral (Text.Megaparsec.Char.char '\"')

pGroupOp :: Parser GroupValueExpr
pGroupOp = D.dbg "pGroupOp" $ fmap GroupOp $
        try (GroupCount <$> (keyword "count" >> pGroupValueExpr))
    <|> try pPositionFoldRelative
    <|> try pPositionFold

-- Parser of both 'PositionFold' and 'Relative'
pPositionFold :: Parser GroupOp
pPositionFold = D.dbg "pPositionFold" $ do
    fold <- pFold
    fieldName <- skipTrailingWhitespace pGroupValueExpr
    keyword "of"
    input <- skipTrailingWhitespace pGroupValueExpr
    return $ PositionFold fold fieldName input

pPositionFoldRelative :: Parser GroupOp
pPositionFoldRelative = do
    positionFold <- pPositionFold
    keyword "relative"
    keyword "to"
    relativeTo <- skipTrailingWhitespace pGroupValueExpr
    return $ Relative (GroupOp positionFold) relativeTo

pFold :: Parser PositionFold
pFold = D.dbg "pFold" $
        try (keyword "sum" >> return SumOver)
    <|> try (keyword "average" >> return Average)
    <|> try (keyword "minimum" >> return Min)
    <|> try (keyword "maximum" >> return Max)

pDataExpr :: Parser GroupValueExpr
pDataExpr =
    parens pDataExpr' <|> pDataExpr'
  where
    parens p = do
        void $ Text.Megaparsec.Char.char '('
        res <- p
        void $ Text.Megaparsec.Char.char ')'
        return res

pDataExpr' :: Parser GroupValueExpr
pDataExpr' = D.dbg "pDataExpr" $ do
    var <- skipTrailingWhitespace pVar
    dataExprList <- many $ pGroupBy <|> pFilter
    return $ foldl' (\input filterOrGroup -> DataExpr $ mkDataExpr filterOrGroup input) (Var var) dataExprList
  where
    mkDataExpr (Left filterComp) = Filter filterComp
    mkDataExpr (Right fieldName) = GroupBy fieldName

pFilter :: Parser (Either Comparison GroupValueExpr)
pFilter = D.dbg "pFilter" $ do
    keyword "where"
    Left <$> pComparison

pGroupBy :: Parser (Either Comparison GroupValueExpr)
pGroupBy = D.dbg "pGroupBy" $ do
    keyword "grouped"
    keyword "by"
    groupField <- skipTrailingWhitespace pGroupValueExpr
    return $ Right groupField

pComparison :: Parser Comparison
pComparison = D.dbg "pComparison" $ do
    lhs <- skipTrailingWhitespace pGroupValueExpr
    bComp <- skipTrailingWhitespace pBoolCompare
    rhs <- skipTrailingWhitespace pGroupValueExpr
    return $ Comparison lhs bComp rhs

pVar :: Parser Text
pVar = D.dbg "pVar" $ do
    word@(firstChar : remainingChars) <- pWord
    let lowerFirstChar = C.toLower firstChar
    if firstChar == lowerFirstChar
        then return (toS word)
        else failParse "Variable name must start with lower case letter" [toS $ lowerFirstChar : remainingChars]

pWord :: Parser String
pWord =
    some Text.Megaparsec.Char.letterChar

pFieldName :: Parser Literal
pFieldName = D.dbg "pFieldName" $
    FieldName <$> pascalCaseWord
  where
    pascalCaseWord :: Parser Text
    pascalCaseWord = do
        word@(firstChar : remainingChars) <- pWord
        let upperFirstChar = C.toUpper firstChar
        if firstChar == upperFirstChar
            then return (toS word)
            else failParse "Field name must start with upper case letter" [toS $ upperFirstChar : remainingChars]

failParse
    :: Text     -- ^ Error message for user
    -> [Text]   -- ^ Expected items
    -> Parser a
failParse msg expected = failure (Just $ Label $ neText msg) (Set.fromList es)
  where
    -- Example:
    --   [ Tokens (NE.fromList "Country")
    --   , Tokens (NE.fromList "b")
    --   ]
    es = map (Tokens . NE.fromList . T.unpack) expected
