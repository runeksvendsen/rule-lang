{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parse
( documentParser
, module Absyn
, M.parse
)
where

import LangPrelude
import qualified Comparison
import Types
import Absyn as Absyn

import Control.Applicative (some, many, (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char
import qualified Text.Megaparsec.Debug as D

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.List (foldl')


-- [NOTE] Keywords:
--   let,forall,if,require,count,of,relative,to,
--   sum,average,minimum,maximum,where,grouped,by,
--   NOT,AND,OR,true,false,

type Parser = M.Parsec Void Text

documentParser :: Parser (NonEmpty RuleExpr)
documentParser = some' (skipTrailingNewline pRuleExpr) <* M.eof

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
    debug "keyword" $ keyword "let"
    varName <- skipTrailingWhitespace pDefineVar
    keyword "="
    varExpr <- skipTrailingWhitespace pVarExpr
    return (varName, varExpr)

pForEach :: Parser RuleExpr
pForEach = debug "pForEach" $ do
    dataExpr <- forEachHeader
    scope <- braces (many (skipTrailingNewline pRuleExpr))
    return $ Foreach dataExpr scope

forEachHeader :: Parser (VarOr DataExpr)
forEachHeader = debug "forEachHeader" $ do
    keyword "forall"
    -- supports:
    --  (var grouped_by/where x ...)
    --  var grouped_by/where x
    --  var
    skipTrailingWhitespace (M.try (NotVar <$> parens pDataExpr) <|> pVarOr pDataExpr)

pIf :: Parser RuleExpr
pIf = debug "pIf" $ do
    keyword "if"
    varOrBoolExpr <- skipTrailingWhitespace (pVarOr pBoolExpr)
    scope <- braces (many (skipTrailingNewline pRuleExpr))
    return $ If varOrBoolExpr scope

pRule :: Parser RuleExpr
pRule = debug "pRule" $ do
    keyword "require"
    Rule <$> (pVarOr pBoolExpr)

pBoolCompare :: Parser BoolCompare
pBoolCompare = debug "pBoolCompare" $
    oneOfPair Comparison.stringToValue

pVarExpr :: Parser VarExpr
pVarExpr = debug "pVarExpr" $
        M.try (DataExpr <$> pDataExpr)
    <|> M.try (BoolExpr <$> pBoolExpr)
    <|> M.try (ValueExpr <$> pValueExpr)

-- TODO: document reserved keywords, e.g. "sum" or "average".
pValueExpr :: Parser ValueExpr
pValueExpr = debug "pValueExpr" $
        M.try (Literal <$> pLiteral)
    <|> M.try (GroupOp <$> pGroupOp)

-- 'Var' is last because otherwise e.g. "sum" is parsed as
--   a variable instead of as a 'PositionFold'
pVarOr :: Parser a -> Parser (VarOr a)
pVarOr pA = M.try (NotVar <$> pA) <|> M.try (Var <$> pVarReferece)

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
    relativeM <- M.optional $ do
        keyword "relative"
        keyword "to"
        skipTrailingWhitespace (pVarOr pDataExpr)
    return $ PositionFold fold fieldName input relativeM

pFold :: Parser PositionFold
pFold = debug "pFold" $
        M.try (keyword "sum" >> return SumOver)
    <|> M.try (keyword "average" >> return Average)
    <|> M.try (keyword "minimum" >> return Min)
    <|> M.try (keyword "maximum" >> return Max)

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

-- | Form: "a comp b" where "a" and "b" are either
--    a variable or a 'ValueExpr', and "comp" is a
pComparison :: Parser BoolExpr
pComparison = debug "pComparison" $ do
    lhs <- skipTrailingWhitespace (pVarOr pValueExpr)
    bComp <- skipTrailingWhitespace pBoolCompare
    rhs <- skipTrailingWhitespace (pVarOr pValueExpr)
    return $ Comparison lhs bComp rhs

-- TODO: enable parsing of non-parenthesized combination of AND/OR
pBoolExpr :: Parser BoolExpr
pBoolExpr = debug "pBoolExpr" $ do
    pNot <|> M.try pAnd <|> M.try pOr <|> M.try pComparison
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

-- | Field names start with a "." followed by an upper case character
--    followed by zero or more alphanumeric characters
pFieldName :: Parser Text
pFieldName = debug "pFieldName" $ do
    void $ Text.Megaparsec.Char.char '.'
    word@(firstChar : remainingChars) <- toS <$> pVarReferece
    if C.isUpper firstChar
        then return (toS word)
        else failParse "Field name must start with upper case letter"
                [toS $ C.toUpper firstChar : remainingChars]

-- | Variable name defined through a let-binding (must start with lowercase letter)
pDefineVar :: Parser Text
pDefineVar = do
    word@(firstChar : remainingChars) <- toS <$> pVarReferece
    if C.isLower firstChar
        then return (toS word)
        else failParse "Variable name must start with lower case letter"
                [toS $ C.toLower firstChar : remainingChars]

-- | A reference to a variable name (no restriction on start letter case)
pVarReferece :: Parser Text
pVarReferece = do
    firstChar <- Text.Megaparsec.Char.letterChar
    remainingChars <- many Text.Megaparsec.Char.alphaNumChar
    return (toS $ firstChar : remainingChars)

pLiteral :: Parser Literal
pLiteral = debug "pLiteral" $
        M.try pPercentage
    <|> M.try (FieldName <$> pFieldName)
    -- TODO: 'Number' (FieldValue) will also match integers
    <|> M.try (FieldValue <$> pFieldValue)

pPercentage :: Parser Literal
pPercentage = do
    num <- pNumber
    void $ Text.Megaparsec.Char.char '%'
    return $ Percent num

pFieldValue :: Parser FieldValue
pFieldValue = debug "pFieldValue" $
        M.try (String . toS <$> pStringLiteral)
    <|> M.try (Bool <$> pBool)
    <|> M.try (Number <$> pNumber)
  where
    pBool :: Parser Bool
    -- FIXME: variables that start with "true" or "false" (e.g. "falsePositions") are incorrectly parsed as
    --  constant+variable
    pBool = M.try (pConstant "true" >> return True) <|> (pConstant "false" >> return False)
    pConstant c = debug "pConstant" $ do
        void $ skipTrailingNewline $ M.chunk c
    pStringLiteral :: Parser String
    pStringLiteral =
        Text.Megaparsec.Char.char '\"' *> M.manyTill L.charLiteral
            (Text.Megaparsec.Char.char '\"')

pNumber :: Parser Number
pNumber = debug "pNumber" $
    M.try (fromReal @Double <$> L.float) <|> M.try (fromIntegral @Integer <$> L.decimal)


-- #### Helper functions ####

parse :: Show a => Parser a -> Text -> IO ()
parse p input =
    case M.parse p "" input of
        Left  e -> putStr (M.errorBundlePretty (e :: M.ParseErrorBundle T.Text Void))
        Right x -> print x

debug :: Show a => String -> Parser a -> Parser a
debug =
    off
  where
    off = const id
    on :: Show a => String -> Parser a -> Parser a
    on = D.dbg

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
sc = L.space (void $ M.oneOf [' ', '\t']) lineComment blockComment

skipTrailingWhitespace :: Parser a -> Parser a
skipTrailingWhitespace = L.lexeme sc

skipTrailingNewline :: Parser a -> Parser a
skipTrailingNewline = L.lexeme scn

keyword :: Text -> Parser ()
keyword input = do
    void $ M.chunk input
    void $ skipTrailingWhitespace $ some (M.oneOf [' ', '\t'])
    -- The above ensures the variable "counterparty"
    --  is not parsed as the GroupOp "count erparty"

parens :: (M.MonadParsec e s m, M.Token s ~ Char) => m b -> m b
parens p = do
    void $ Text.Megaparsec.Char.char '('
    res <- p
    void $ Text.Megaparsec.Char.char ')'
    return res

braces :: Parser a -> Parser a
braces = M.between
    (discardSurroundingWhitespace $ Text.Megaparsec.Char.char '{')
    (discardSurroundingWhitespace $ Text.Megaparsec.Char.char '}')
  where
    -- discard whitespace (including newlines) before and after
    discardSurroundingWhitespace p = scn >> p >> scn

-- | Parse using one of the parsers in the supplied list.
--   Starts with the first parser in the list.
oneOf :: [Parser a] -> Parser a
oneOf = foldl' (\state parser -> state <|> M.try parser) M.empty

-- | Given a list of: a pair of text to parse and the resulting abstract syntax,
--    return a parser that parses one of these
oneOfPair :: [(Text, a)] -> Parser a
oneOfPair textAbsynPair = do
    let pairParser (text, absyn) = keyword text >> return absyn
    oneOf (map pairParser textAbsynPair)

-- | Report a parser error to the user,
--    containing zero or more suggestions
failParse
    :: Text     -- ^ Error message for user
    -> [Text]   -- ^ Expected items
    -> Parser a
failParse msg expected =
    M.failure (Just $ M.Label $ neText msg) (Set.fromList es)
  where
    es = map (M.Tokens . NE.fromList . T.unpack) expected
