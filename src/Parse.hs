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

import LangPrelude
import Types
import Absyn as Absyn

import Control.Applicative (many, (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char
import qualified Text.Megaparsec.Debug as D
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
    many (skipTrailingNewline pRuleExpr)
  where
    skipTrailingNewline = L.lexeme scn


-- ########################
-- ######  RuleExpr  ######
-- ########################

pRuleExpr :: Parser RuleExpr
pRuleExpr = debug "pRuleExpr" $
    pLet <|> pForEach <|> pIf <|> pRule

pLet :: Parser RuleExpr
pLet = do
    keyword "let"
    varName <- lexeme pDefineVar
    keyword "="
    expr <- lexeme pExpr
    return $ Let varName expr

pForEach :: Parser RuleExpr
pForEach = debug "pForEach" $ do
    keyword "forall"
    dataExpr <- lexeme pExpr
    scope <- braces rules
    return $ Forall dataExpr scope

pIf :: Parser RuleExpr
pIf = debug "pIf" $ do
    keyword "if"
    varOrBoolExpr <- lexeme pExpr
    scope <- braces rules
    return $ If varOrBoolExpr scope

pRule :: Parser RuleExpr
pRule = debug "pRule" $ do
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
        [ [ InfixL $ keyword "where" >> return (\a -> DataExpr . Filter a)
          , InfixL $ keyword "grouped" >> keyword "by" >> return (\a -> DataExpr . GroupBy a)
          ]
        , [ InfixN $ keyword "of" >> return Map ]
        , [ Prefix $ keyword "count" >> return (ValueExpr . GroupCount)
          , Prefix $ keyword "sum" >> return (ValueExpr . FoldMap Sum)
          , Prefix $ keyword "average" >> return (ValueExpr . FoldMap Average)
          , Prefix $ keyword "minimum" >> return (ValueExpr . FoldMap Min)
          , Prefix $ keyword "maximum" >> return (ValueExpr . FoldMap Max)
          ]
        , [ InfixL $ keyword "relative" >> keyword "to" >> return (\a -> ValueExpr . Relative a)]
        , [ InfixN $ op "==" >> return (mkComp Eq)
          , InfixN $ op "!=" >> return (mkComp NEq)
          , InfixN $ op ">" >> return (mkComp Gt)
          , InfixN $ op "<" >> return (mkComp Lt)
          , InfixN $ op ">=" >> return (mkComp GtEq)
          , InfixN $ op "<=" >> return (mkComp LtEq)
          ]
        , [ Prefix $ keyword "NOT" >> return (BoolExpr . Not) ]
        , [ InfixL $ keyword "AND" >> return (\a -> BoolExpr . And a) ]
        , [ InfixL $ keyword "OR" >> return (\a -> BoolExpr . Or a) ]
        ]
    op n = L.lexeme sc $ M.try (Text.Megaparsec.Char.string n <* M.notFollowedBy (Text.Megaparsec.Char.char '='))
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
    firstChar <- Text.Megaparsec.Char.letterChar
    remainingChars <- many Text.Megaparsec.Char.alphaNumChar
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
pLiteral = debug "pLiteral" $
        M.try pPercentage
    <|> FieldValue <$> pFieldValue
    <|> FieldName <$> pFieldName

-- | Field names begin with a "." followed by an upper case character
--    followed by zero or more alphanumeric characters
pFieldName :: Parser FieldName
pFieldName = debug "FieldName" $ do
    varRef <- Text.Megaparsec.Char.char '.' *> pVarReferece
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
    pNumber <* Text.Megaparsec.Char.char '%'

pFieldValue :: Parser FieldValue
pFieldValue = debug "pFieldValue" $
        (String . toS <$> pStringLiteral)
    <|> Bool <$> pBool
    <|> Number <$> pNumber
  where
    pBool = (pConstant "true" >> return True) <|> (pConstant "false" >> return False)
    pConstant c = debug "pConstant" $
        M.chunk c *> M.notFollowedBy Text.Megaparsec.Char.alphaNumChar
    pStringLiteral =
        Text.Megaparsec.Char.char '\"' *> M.manyTill L.charLiteral
            (Text.Megaparsec.Char.char '\"')

pNumber :: Parser Number
pNumber = debug "pNumber" $
    M.try (fromReal @Double <$> L.float) <|> fromIntegral @Integer <$> L.decimal


-- ########################
-- ######  Helpers   ######
-- ########################

keyword :: Text -> Parser ()
keyword input =
    lexeme $
        M.chunk input *> M.notFollowedBy Text.Megaparsec.Char.alphaNumChar
    -- "notFollowedBy" makes sure keyword-prefixed strings are not parsed as a keyword.
    -- E.g. "counterParty" shouldn't be parsed as "count" on a variable named "erParty",
    --   but as a variable named "counterParty".

-- Parse an expression enclosed in parentheses.
parens :: Parser a -> Parser a
parens = M.between
    (symbol "(")
    (symbol ")")

-- Parse an expression enclosed in braces, discarding all whitespace
--   before and after both the opening and closing brace.
braces :: Parser a -> Parser a
braces = M.between
    (scn *> L.symbol scn "{")
    (scn *> L.symbol scn "}")

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

debug :: Show a => String -> Parser a -> Parser a
debug =
    off
  where
    off = const id
    on :: Show a => String -> Parser a -> Parser a
    on = D.dbg
