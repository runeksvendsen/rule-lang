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

documentParser :: Parser (NonEmpty RuleExpr)
documentParser = some' (skipTrailingNewline pRuleExpr) <* eof

some' :: Parser a -> Parser (NonEmpty a)
some' p = NE.fromList <$> some p

pRuleExpr :: Parser RuleExpr
pRuleExpr = debug "pRuleExpr" $
    pLet <|> pForEach <|> pIf <|> pRule

pLet :: Parser RuleExpr
pLet = do
    (varName, groupExpr) <- skipTrailingNewline pLetHeader
    return $ Let varName groupExpr

pLetHeader :: Parser (Text, VarExpr)
pLetHeader = debug "pLetHeader" $ do
    keyword "let"
    varName <- skipTrailingWhitespace pDefineVar
    keyword "="
    varExpr <- skipTrailingWhitespace pVarExpr
    return (varName, varExpr)

pForEach :: Parser RuleExpr
pForEach = debug "pForEach" $ do
    dataExpr <- forEachHeader
    scope <- braces (some' (skipTrailingNewline pRuleExpr))
    return $ Foreach dataExpr scope

forEachHeader :: Parser (VarOr DataExpr)
forEachHeader = debug "forEachHeader" $ do
    keyword "for"
    keyword "all"
    -- supports:
    --  (var grouped_by/where x ...)
    --  var grouped_by/where x
    --  var
    skipTrailingWhitespace (try (NotVar <$> parens pDataExpr) <|> pVarOr pDataExpr)

pIf :: Parser RuleExpr
pIf = debug "pIf" $ do
    keyword "if"
    varOrBoolExpr <- skipTrailingWhitespace (pVarOr pBoolExpr)
    scope <- braces (some' (skipTrailingNewline pRuleExpr))
    return $ If varOrBoolExpr scope

pRule :: Parser RuleExpr
pRule = debug "pRule" $ do
    keyword "require"
    Rule <$> (pVarOr $ pBoolExpr)

pBoolCompare :: Parser BoolCompare
pBoolCompare = debug "pBoolCompare" $ do
    let bCompareSymbols = foldr1 (<|>) (map Text.Megaparsec.Char.char "=!><")
    string <- some bCompareSymbols
    case Prelude.lookup string stringToValue of
        Just bComp -> return bComp
        Nothing -> failParse "Unknown operator" (map (toS . fst) Comparison.stringToValue)

pVarExpr :: Parser VarExpr
pVarExpr = debug "pVarExpr" $
        try (DataExpr <$> pDataExpr)
    <|> try (BoolExpr <$> pBoolExpr)
    <|> try (ValueExpr <$> pValueExpr)

-- TODO: document reserved keywords, e.g. "sum" or "average".
pValueExpr :: Parser ValueExpr
pValueExpr = debug "pValueExpr" $
        try (GroupOp <$> pGroupOp)
    <|> try (Literal <$> pLiteral)

-- 'Var' is last because otherwise e.g. "sum" is parsed as
--   a variable instead of as a 'PositionFold'
pVarOr :: Parser a -> Parser (VarOr a)
pVarOr pA = try (NotVar <$> pA) <|> try (Var <$> pVarReferece)

pGroupOp :: Parser GroupOp
pGroupOp = debug "pCountOrFold" $
    (GroupCount <$> (keyword "count" >> pVarOr pDataExpr))
    <|> pPositionFold

pPositionFold :: Parser GroupOp
pPositionFold = debug "pPositionFold" $ do
    fold <- pFold
    fieldName <- skipTrailingWhitespace (pVarOr pFieldName)
    keyword "of"
    input <- skipTrailingWhitespace (pVarOr pDataExpr)
    relativeM <- optional $ do
        keyword "relative"
        keyword "to"
        skipTrailingWhitespace (pVarOr pDataExpr)
    return $ PositionFold fold fieldName input relativeM

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
    var <- skipTrailingWhitespace pVarReferece
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
    Left <$> skipTrailingWhitespace (parens pBoolExpr)

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
pBoolExpr :: Parser BoolExpr
pBoolExpr = debug "pBoolExpr" $ do
    pNot <|> try pAnd <|> try pOr <|> try pComparison
  where
    pNot :: Parser BoolExpr
    pNot = debug "pNot" $ do
        keyword "NOT"
        boolExpr <- skipTrailingWhitespace $ pVarOr pBoolExpr
        return (Not boolExpr)
    infixParens word mk = parens $ do
        exprA <- skipTrailingWhitespace (pVarOr pBoolExpr)
        keyword word
        exprB <- skipTrailingWhitespace (pVarOr pBoolExpr)
        return $ mk exprA exprB
    pAnd = debug "pAnd" $ infixParens "AND" And
    pOr = debug "pOr" $ infixParens "OR" Or

-- | Variable name defined through a let-binding (must start with lowercase letter)
pDefineVar :: Parser Text
pDefineVar = do
    firstChar : remainingChars <- toS <$> pVarReferece
    let lowerFirstChar = C.toLower firstChar
        word = firstChar : remainingChars
    if firstChar == lowerFirstChar
        then return (toS word)
        else failParse "Variable name must start with lower case letter"
                [toS $ lowerFirstChar : remainingChars]

-- | A reference to a variable name (no restriction on start letter case)
pVarReferece :: Parser Text
pVarReferece = do
    firstChar <- Text.Megaparsec.Char.letterChar
    remainingChars <- many Text.Megaparsec.Char.alphaNumChar
    return (toS $ firstChar : remainingChars)

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
