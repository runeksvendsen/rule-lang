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

debug :: Show a => String -> Parser a -> Parser a
debug = const id

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

parens :: (MonadParsec e s m, Token s ~ Char) => m b -> m b
parens p = do
    void $ Text.Megaparsec.Char.char '('
    res <- p
    void $ Text.Megaparsec.Char.char ')'
    return res

braces :: Parser a -> Parser a
braces = Text.Megaparsec.between
    (discardSurroundingWhitespace $ Text.Megaparsec.Char.char '{')
    (discardSurroundingWhitespace $ Text.Megaparsec.Char.char '}')
  where
    -- discard whitespace (including newlines) before and after
    discardSurroundingWhitespace p = scn >> p >> scn

-- #########

documentParser :: Parser RuleExpr
documentParser = skipTrailingNewline pRuleExpr <* eof

pRuleExpr :: Parser RuleExpr
pRuleExpr = debug "pRuleExpr" $
    try pLet <|> try pForEach <|> try pRule

pLet :: Parser RuleExpr
pLet = do
    (varName, groupExpr) <- skipTrailingNewline pLetHeader
    scope <- pRuleExpr
    return $ Let varName groupExpr scope

pLetHeader = debug "pLetHeader" $ do
    keyword "let"
    varName <- skipTrailingWhitespace pVar
    keyword "="
    varExpr <- skipTrailingWhitespace pVarExpr
    return (varName, varExpr)

-- forEachHeader :: Parser (ValueExpr, ValueExpr)
forEachHeader = debug "forEachHeader" $ do
    keyword "for"
    keyword "each"
    fieldName <- skipTrailingWhitespace (pVarOr pFieldName)
    keyword "in"
    -- supports:
    --  (var grouped_by/where x ...)
    --  var grouped_by/where x
    --  var
    dataExpr <- skipTrailingWhitespace (try (NotVar <$> parens pDataExpr) <|> pVarOr pDataExpr)
    return (fieldName, dataExpr)

pForEach :: Parser RuleExpr
pForEach = debug "pForEach" $ do
    (fieldNameExpr, dataExpr) <- forEachHeader
    scope <- braces pRuleExpr
    return $ Foreach fieldNameExpr dataExpr scope

pRule :: Parser RuleExpr
pRule = debug "pRule" $ do
    debug "rule:" $ keyword "rule:"
    Rule <$> (pVarOr $ pBoolExpr sc)

pBoolCompare :: Parser BoolCompare
pBoolCompare = debug "pBoolCompare" $ do
    let bCompareSymbols = foldr1 (<|>) (map Text.Megaparsec.Char.char "=!><")
    string <- some bCompareSymbols
    case Prelude.lookup string stringToValue of
        Just bComp -> return bComp
        Nothing -> failParse "Unknown operator" (map (toS . fst) Comparison.stringToValue)

pVarExpr :: Parser VarExpr
pVarExpr = debug "pVarExpr" $
        try (BoolExpr <$> pBoolExpr sc)
    <|> try (ValueExpr <$> pValueExpr)
    <|> try (DataExpr <$> pDataExpr)

-- TODO: document reserved keywords, e.g. "sum" or "average".
pValueExpr :: Parser ValueExpr
pValueExpr = debug "pValueExpr" $
        try (GroupOp <$> pGroupOp)
    <|> try (Literal <$> pLiteral)

-- 'Var' is last because otherwise e.g. "sum" is parsed as
--   a variable instead of as a 'PositionFold'
pVarOr :: Parser a -> Parser (VarOr a)
pVarOr pA = try (NotVar <$> pA) <|> try (Var <$> pVar)

-- NB: relative "Count" NOT supported
pGroupOp :: Parser GroupOp
pGroupOp = debug "pGroupOp" $
        try (GroupCount <$> (keyword "count" >> pVarOr pDataExpr))
    <|> try pPositionFoldRelative
    <|> try pPositionFold

-- Parser of 'PositionFold' and 'Relative'
pPositionFold :: Parser GroupOp
pPositionFold = debug "pPositionFold" $ do
    fold <- pFold
    fieldName <- skipTrailingWhitespace (pVarOr pFieldName)
    keyword "of"
    input <- skipTrailingWhitespace (pVarOr pDataExpr)
    return $ PositionFold fold fieldName input

-- Parser of 'Relative'
pPositionFoldRelative :: Parser GroupOp
pPositionFoldRelative = do
    varOrPositionFold <- skipTrailingWhitespace (pVarOr pPositionFold)
    keyword "relative"
    keyword "to"
    relativeTo <- skipTrailingWhitespace (pVarOr pFieldName)
    return $ Relative varOrPositionFold relativeTo

pFold :: Parser PositionFold
pFold = debug "pFold" $
        try (keyword "sum" >> return SumOver)
    <|> try (keyword "average" >> return Average)
    <|> try (keyword "minimum" >> return Min)
    <|> try (keyword "maximum" >> return Max)

-- A 'DataExpr' will always start with a variable (Var),
--  followed by zero or more 'Filter' and/or 'GroupBy' operations.
pDataExpr :: Parser DataExpr
pDataExpr = debug "pDataExpr" $ do
    var <- skipTrailingWhitespace pVar
    dataExprList <- some $ pGroupBy <|> pFilter
    let initialDataExpr = mkDataExpr (head dataExprList) (Var var)
    return $ foldl' (\accum filterOrGroup -> mkDataExpr filterOrGroup (NotVar accum)) initialDataExpr (tail dataExprList)
  where
    mkDataExpr :: Either BoolExpr (VarOr FieldName) -> VarOr DataExpr -> DataExpr
    mkDataExpr (Left filterComp) = Filter filterComp
    mkDataExpr (Right fieldName) = GroupBy fieldName

pFilter :: Parser (Either BoolExpr (VarOr FieldName))
pFilter = debug "pFilter" $ do
    keyword "where"
    Left <$> pComparison

pGroupBy :: Parser (Either BoolExpr (VarOr Text))
pGroupBy = debug "pGroupBy" $ do
    keyword "grouped"
    keyword "by"
    groupField <- skipTrailingWhitespace (pVarOr pFieldName)
    return $ Right groupField

pComparison :: Parser BoolExpr
pComparison = debug "pComparison" $ do
    lhs <- skipTrailingWhitespace (pVarOr pValueExpr)
    bComp <- skipTrailingWhitespace pBoolCompare
    rhs <- skipTrailingWhitespace (pVarOr pValueExpr)
    return $ Comparison lhs bComp rhs

-- TODO: correct and/or/not precedence?
pBoolExpr :: Parser () -> Parser BoolExpr
pBoolExpr whitespaceConsumer = debug "pBoolExpr" $ do
    res <- pBoolExpr'
    case res of
        -- refuse to parse a naked Var
        Var _ -> failure Nothing (Set.fromList []) -- TODO: message here?
        NotVar notVar -> return notVar
  where
    pBoolExpr' = try (NotVar <$> pNot) <|> try pComparisonAndOr
    pComparisonAndOr = do
        comparisonOrVar <- skipTrailingWhitespace (pVarOr pComparison)
        extraM <- optional $ try pAnd <|> try pOr
        case extraM of
            Nothing -> return comparisonOrVar
            Just mkAndOr -> return $ mkAndOr comparisonOrVar
    pAnd = do
        keyword "AND"
        exprB <- pBoolExpr'
        whitespaceConsumer
        return $ \exprOrVar -> NotVar $ And exprOrVar exprB
    pOr = do
        keyword "OR"
        exprB <- pBoolExpr'
        whitespaceConsumer
        return $ \exprOrVar -> NotVar $ Or exprOrVar exprB
    pNot :: Parser BoolExpr
    pNot = debug "pNot" $ do
        keyword "NOT"
        boolExpr <- pBoolExpr'
        whitespaceConsumer
        return (Not boolExpr)

-- | Variables start with a lower case letter
pVar :: Parser Text
pVar = do
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
pFieldName = debug "pFieldName" $ do
    word@(firstChar : remainingChars) <- pWord
    let upperFirstChar = C.toUpper firstChar
    if firstChar == upperFirstChar
        then return (toS word)
        else failParse "Field name must start with upper case letter" [toS $ upperFirstChar : remainingChars]

pLiteral :: Parser Literal
pLiteral = debug "pLiteral" $
        try pPercentage
    <|> try (FieldName <$> pFieldName)
    -- TODO: 'Number' (FieldValue) will also match integers
    <|> try (FieldValue <$> pFieldValue)

pPercentage :: Parser Literal
pPercentage = do
    num <- pNumber
    void $ Text.Megaparsec.Char.char '%'
    return $ Percent num

pFieldValue :: Parser FieldValue
pFieldValue = debug "pFieldValue" $
        try (String . toS <$> pStringLiteral)
    <|> try (Bool <$> pBool)
    <|> try (Number <$> pNumber)
  where
    pBool :: Parser Bool
    pBool = try (keyword "true" >> return True) <|> (keyword "false" >> return False)
    pStringLiteral :: Parser String
    pStringLiteral =
        Text.Megaparsec.Char.char '\"' *> manyTill L.charLiteral
            (Text.Megaparsec.Char.char '\"')

pNumber :: Parser Number
pNumber = debug "pNumber" $
    try (fromReal @Double <$> L.float) <|> try (fromIntegral @Integer <$> L.decimal)

failParse
    :: Text     -- ^ Error message for user
    -> [Text]   -- ^ Expected items
    -> Parser a
failParse msg expected =
    failure (Just $ Label $ neText msg) (Set.fromList es)
  where
    es = map (Tokens . NE.fromList . T.unpack) expected
