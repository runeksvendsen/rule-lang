{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parse
( ruleParserDoc
, pExpr
, module Absyn
, M.parse
, Parser
)
where

import Prelude
import LangPrelude
import Types
import Absyn as Absyn

import Control.Applicative (many, (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as Char
import Control.Monad.Combinators.Expr (Operator(Prefix, InfixL, InfixN), makeExprParser)

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set


type Parser = M.Parsec Void Text

keywords :: [Text]
keywords =
    [ "let"
    , "forall"
    , "if"
    , "require"
    , "sum"
    , "average"
    , "minimum"
    , "maximum"
    , "of"
    , "count"
    , "relative"
    , "to"
    , "where"
    , "grouped"
    , "by"
    , "NOT"
    , "AND"
    , "OR"
    ]

-- ########################
-- ######    rule    ######
-- ########################

-- 1) Optional initial whitespace (including newlines)
-- 2) zero or more newline-separated rules
-- 3) <end of file>
ruleParserDoc :: Parser [RuleExpr]
ruleParserDoc = scn *> rules <* M.eof

-- Zero or more RuleExpr separated by one or more newlines
rules :: Parser [RuleExpr]
rules =
    many (skipTrailingNewline $ lexeme pRuleExpr <* Char.eol)
  where
    skipTrailingNewline = L.lexeme scn


-- ########################
-- ######  RuleExpr  ######
-- ########################

pRuleExpr :: Parser RuleExpr
pRuleExpr =
    pLet <|> pForEach <|> pIf <|> pRule

pLet :: Parser RuleExpr
pLet = do
    keyword "let"
    varName <- lexeme pDefineVar
    keyword "="
    expr <- lexeme pExpr
    return $ Let varName expr

pForEach :: Parser RuleExpr
pForEach = do
    keyword "forall"
    dataExpr <- lexeme pExpr
    scope <- braces rules
    return $ Forall dataExpr scope

pIf :: Parser RuleExpr
pIf = do
    keyword "if"
    varOrBoolExpr <- lexeme pExpr
    scope <- braces rules
    return $ If varOrBoolExpr scope

pRule :: Parser RuleExpr
pRule = do
    keyword "require"
    Rule <$> pExpr

-- ###################
-- ###### Expr ######
-- ###################

pExpr :: Parser Expr
pExpr =
    expr
  where
    expr = makeExprParser term table
    term = lexeme $ parens expr <|> pTerm
    pTerm = Literal <$> pLiteral <|> Var <$> pVarReferece
    table =
        [ [ InfixL $ keyword "where"                   >> return (\a -> DataExpr . Filter a)
          , InfixL $ keyword "grouped" >> keyword "by" >> return (\a -> DataExpr . GroupBy a) ]
        , [ InfixN $ keyword "of" >> return Map ]
        , [ Prefix $ keyword "count"   >> return (ValueExpr . GroupCount)
          , Prefix $ keyword "sum"     >> return (ValueExpr . FoldMap Sum)
          , Prefix $ keyword "average" >> return (ValueExpr . FoldMap Avg)
          , Prefix $ keyword "minimum" >> return (ValueExpr . FoldMap Min)
          , Prefix $ keyword "maximum" >> return (ValueExpr . FoldMap Max) ]
        , [ InfixL $ keyword "relative" >> keyword "to" >> return (\a -> ValueExpr . Relative a)]
        , [ InfixN $ keyword "=="                                   >> return (mkComp Eq)
          , InfixN $ keyword "!="                                   >> return (mkComp NEq)
          , InfixN $ keyword ">="                                   >> return (mkComp GtEq)
          , InfixN $ keyword "<="                                   >> return (mkComp LtEq)
          , InfixN $ keyword ">" <* M.notFollowedBy (Char.char '=') >> return (mkComp Gt)
          , InfixN $ keyword "<" <* M.notFollowedBy (Char.char '=') >> return (mkComp Lt) ]
        , [ Prefix $ keyword "NOT" >> return (BoolExpr . Not) ]
        , [ InfixL $ keyword "AND" >> return (\a -> BoolExpr . And a) ]
        , [ InfixL $ keyword "OR"  >> return (\a -> BoolExpr . Or a) ] ]
    mkComp numComp a b = BoolExpr $ Comparison a numComp b

-- ########################
-- ##### Identifiers ######
-- ########################

-- | Variable name defined through a let-binding (must begin with lowercase letter)
pDefineVar :: Parser Text
pDefineVar = do
    word@(firstChar : remainingChars) <- toS <$> pVarReferece
    if C.isLower firstChar
        then return (toS word)
        else failParse "Variable name must begin with lower case letter"
                [toS $ C.toLower firstChar : remainingChars]

-- | A reference to a variable name (no restriction on start letter case)
pVarReferece :: Parser Text
pVarReferece = do
    firstChar <- Char.letterChar
    remainingChars <- many Char.alphaNumChar
    let identifier = toS $ firstChar : remainingChars
    -- HACK: prevent keywords from being parsed as
    --  identifiers/variable references
    if not $ identifier `elem` keywords
        then return identifier
        else M.failure Nothing (Set.fromList [])


-- ########################
-- ######  Literals   ######
-- ########################

pLiteral :: Parser Literal
pLiteral =
        M.try pPercentage
    <|> FieldValue <$> pFieldValue
    <|> FieldName <$> pFieldName

-- | Field names begin with a "." followed by an upper case character
--    followed by zero or more alphanumeric characters
pFieldName :: Parser FieldName
pFieldName = do
    varRef <- Char.char '.' *> pVarReferece
    checkBeginChar (toS varRef)
  where
    checkBeginChar [] = error "BUG: pVarReferece returned empty string"
    checkBeginChar word@(firstChar : remainingChars) =
        if C.isUpper firstChar
            then return (fromString $ toS word)
            else failParse "Field name must being with upper case letter"
                    [toS $ C.toUpper firstChar : remainingChars]

pPercentage :: Parser Literal
pPercentage = Percent <$>
    pNumber <* Char.char '%'

pFieldValue :: Parser FieldValue
pFieldValue =
        (String . toS <$> pStringLiteral)
    <|> Bool <$> pBool
    <|> Number <$> pNumber
  where
    pBool = (pConstant "true" >> return True) <|> (pConstant "false" >> return False)
    pConstant c = M.chunk c *> M.notFollowedBy Char.alphaNumChar
    pStringLiteral =
        Char.char '\"' *> M.manyTill L.charLiteral
            (Char.char '\"')

pNumber :: Parser Number
pNumber =
    M.try (fromReal @Double <$> L.float) <|> fromIntegral @Integer <$> L.decimal


-- ########################
-- ######  Helpers   ######
-- ########################

keyword :: Text -> Parser ()
keyword input =
    lexeme $
        M.chunk input *> M.notFollowedBy Char.alphaNumChar
    -- "notFollowedBy" makes sure keyword-prefixed strings are not parsed as a keyword.
    -- E.g. "counterParty" shouldn't be parsed as "count" on a variable named "erParty",
    --   but as a variable named "counterParty".

-- Parse an expression enclosed in parentheses.
parens :: Parser a -> Parser a
parens = M.between
    (symbol "(")
    (symbol ")")

-- Run "inputParser" between:
--   <newline/whitespace>{<newline/whitespace>inputParser<newline/whitespace>}<whitespace>
-- NB: Notice that no newline(s) are parsed after closing brace.
braces :: Parser a -> Parser a
braces = M.between
    (scn *> L.symbol scn "{")
    (scn *> L.symbol sc "}")

-- Line comments start with //
lineComment :: Parser ()
lineComment = L.skipLineComment "//"

-- Block comments are inside /* */
blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

-- A space-consumer that consumes newlines
scn :: Parser ()
scn = L.space (void Char.spaceChar) lineComment blockComment

-- A space-consumer that does NOT consume newlines
sc :: Parser ()
sc = L.space (void $ M.oneOf [' ', '\t']) lineComment blockComment

-- Parse a literal text string, ignoring trailing spaces/tabs
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Parse something and remove optional trailing tabs/spaces
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

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
