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
documentParser = skipTrailingNewline pRuleExpr <* eof

pRuleExpr :: Parser RuleExpr
pRuleExpr = D.dbg "pRuleExpr" $
    try pForEach <|> try pRule <|> try pLet

pLet :: Parser RuleExpr
pLet = do
    (varName, groupExpr) <- skipTrailingNewline pLetHeader
    scope <- pRuleExpr
    return $ Let varName groupExpr scope

pLetHeader = D.dbg "pLetHeader" $ do
    keyword "let"
    varName <- skipTrailingWhitespace pVar
    keyword "="
    groupExpr <- skipTrailingWhitespace (pVarOr pValueExpr)
    return (varName, groupExpr)

-- forEachHeader :: Parser (ValueExpr, ValueExpr)
forEachHeader = D.dbg "forEachHeader" $ do
    keyword "for"
    keyword "each"
    fieldName <- skipTrailingWhitespace (pVarOr pFieldName)
    keyword "in"
    -- supports:
    --  var
    -- (var)
    --  var grouped_by/where x
    --  (var grouped_by/where x)
    dataExpr <- skipTrailingWhitespace (try (parens pDataExpr) <|> try pDataExpr)
    return (fieldName, dataExpr)

pForEach :: Parser RuleExpr
pForEach = D.dbg "pForEach" $ do
    (fieldNameExpr, dataExpr) <- forEachHeader
    scope <- braces pRuleExpr
    return $ Foreach fieldNameExpr dataExpr scope

braces :: Parser a -> Parser a
braces = Text.Megaparsec.between
    (discardSurroundingWhitespace $ Text.Megaparsec.Char.char '{')
    (discardSurroundingWhitespace $ Text.Megaparsec.Char.char '}')
  where
    -- discard whitespace (including newlines) before and after
    discardSurroundingWhitespace p = scn >> p >> scn

pRule :: Parser RuleExpr
pRule = D.dbg "pRule" $ do
    D.dbg "rule:" $ keyword "rule:"
    Rule <$> (pVarOr pBoolExpr)

pBoolCompare :: Parser BoolCompare
pBoolCompare = D.dbg "pBoolCompare" $ do
    let bCompareSymbols = foldr1 (<|>) (map Text.Megaparsec.Char.char "=!><")
    string <- some bCompareSymbols
    case Prelude.lookup string stringToValue of
        Just bComp -> return bComp
        Nothing -> failParse "Unknown operator" (map (toS . fst) Comparison.stringToValue)

-- TODO: document reserved keywords, e.g. "sum" or "average".
-- 'Var' is last because otherwise e.g. "sum" is parsed as
--   a variable instead of as a 'PositionFold'
pValueExpr :: Parser ValueExpr
pValueExpr = D.dbg "pValueExpr" $
        try (GroupOp <$> pGroupOp)
    <|> try (DataExpr <$> pDataExpr)
    -- TODO: newlines between "AND" in rules but no for let-bindings?
    <|> try (BoolExpr <$> pBoolExpr)
    <|> try (Literal <$> pLiteral)

pVarOr :: Parser a -> Parser (VarOr a)
pVarOr pA = try (NotVar <$> pA) <|> try (Var <$> pVar)

pLiteral :: Parser Literal
pLiteral = D.dbg "pLiteral" $
        try pPercentage
    <|> try (FieldName <$> pFieldName)
    -- TODO: 'Number' (FieldValue) will also match integers
    <|> try (Integer <$> L.decimal)
    <|> try (FieldValue <$> pFieldValue)

pPercentage :: Parser Literal
pPercentage = do
    num <- pNumber
    void $ Text.Megaparsec.Char.char '%'
    return $ Percent num

pNumber :: Parser Number
pNumber = D.dbg "pNumber" $
    try (fromReal @Double <$> L.float) <|> fromIntegral @Integer <$> L.decimal

pFieldValue :: Parser FieldValue
pFieldValue = D.dbg "pFieldValue" $
        String . toS <$> pStringLiteral
    <|> Number <$> pNumber
  where
    pStringLiteral :: Parser String
    pStringLiteral = Text.Megaparsec.Char.char '\"' *> manyTill L.charLiteral (Text.Megaparsec.Char.char '\"')

-- NB: relative "Count" NOT supported
pGroupOp :: Parser GroupOp
pGroupOp =
        try (GroupCount <$> (keyword "count" >> pVarOr pDataExpr))
    <|> try pPositionFoldRelative
    <|> try pPositionFold

-- Parser of 'PositionFold' and 'Relative'
pPositionFold :: Parser GroupOp
pPositionFold = D.dbg "pPositionFold" $ do
    fold <- pFold
    fieldName <- skipTrailingWhitespace (pVarOr pFieldName)
    keyword "of"
    input <- skipTrailingWhitespace (pVarOr pDataExpr)
    return $ PositionFold fold fieldName input

-- Parser of 'Relative'
pPositionFoldRelative :: Parser GroupOp
pPositionFoldRelative = do
    varOrPositionFold <- pVarOr pPositionFold
    keyword "relative"
    keyword "to"
    relativeTo <- skipTrailingWhitespace (pVarOr pGroupOp)
    return $ Relative varOrPositionFold relativeTo

pFold :: Parser PositionFold
pFold = D.dbg "pFold" $
        try (keyword "sum" >> return SumOver)
    <|> try (keyword "average" >> return Average)
    <|> try (keyword "minimum" >> return Min)
    <|> try (keyword "maximum" >> return Max)

parens p = do
    void $ Text.Megaparsec.Char.char '('
    res <- p
    void $ Text.Megaparsec.Char.char ')'
    return res

-- A 'DataExpr' will always start with a variable (Source),
--  followed by zero or more 'Filter' and/or 'GroupBy' operations.

pDataExpr = D.dbg "pDataExpr" $ do
    var <- skipTrailingWhitespace pVar
    dataExprList <- many $ pGroupBy <|> pFilter
    return $ foldl' (\input filterOrGroup -> mkDataExpr filterOrGroup input) (Source var) dataExprList
  where
    mkDataExpr (Left filterComp) = Filter filterComp
    mkDataExpr (Right fieldName) = GroupBy fieldName

pFilter :: Parser (Either BoolExpr (VarOr FieldName))
pFilter = D.dbg "pFilter" $ do
    keyword "where"
    Left <$> pComparison

pGroupBy :: Parser (Either BoolExpr (VarOr FieldName))
pGroupBy = D.dbg "pGroupBy" $ do
    keyword "grouped"
    keyword "by"
    groupField <- skipTrailingWhitespace (pVarOr pFieldName)
    return $ Right groupField

pComparison :: Parser BoolExpr
pComparison = D.dbg "pComparison" $ do
    lhs <- skipTrailingWhitespace (pVarOr pValueExpr)
    bComp <- skipTrailingWhitespace pBoolCompare
    rhs <- skipTrailingWhitespace (pVarOr pValueExpr)
    return $ Comparison lhs bComp rhs

pAnd :: Parser BoolExpr
pAnd = D.dbg "pAnd" $ do
    exprA <- skipTrailingNewline (pVarOr pBoolExpr)
    keyword "AND"
    exprB <- skipTrailingWhitespace (pVarOr pBoolExpr)
    return $ And exprA exprB

pOr :: Parser BoolExpr
pOr = D.dbg "pAnd" $ do
    exprA <- skipTrailingNewline (pVarOr pBoolExpr)
    keyword "OR"
    exprB <- skipTrailingWhitespace (pVarOr pBoolExpr)
    return $ Or exprA exprB

-- TODO: correct and/or/not precedence?
pBoolExpr :: Parser BoolExpr
pBoolExpr =
    try pComparison <|> try pNot <|> try pAnd <|> try pOr

pNot :: Parser BoolExpr
pNot = keyword "NOT" >> skipTrailingWhitespace pBoolExpr


-- | Variables start with a lower case letter
pVar :: Parser Text
pVar = D.dbg "pVar" $ do
    word@(firstChar : remainingChars) <- pWord
    let lowerFirstChar = C.toLower firstChar
    if firstChar == lowerFirstChar
        then return (toS word)
        else failParse "Variable name must start with lower case letter"
                [toS $ lowerFirstChar : remainingChars]

pWord :: Parser String
pWord =
    some Text.Megaparsec.Char.letterChar

-- | Field names start with an upper case letter
pFieldName :: Parser Text
pFieldName = D.dbg "pFieldName" $ do
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
