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
, pIntExpr
)
where

import Prelude
import LangPrelude (Text, Void, toS, fromString, void, neText)

import Types
import Absyn as Absyn

import Control.Applicative (many, (<|>))
import Data.Functor (($>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char.Lexer as M
import qualified Text.Megaparsec.Char as M hiding (space)
import qualified Control.Monad.Combinators.Expr as Expr
import Control.Monad.Combinators.Expr (Operator(Prefix, InfixL, InfixN))
import qualified Text.Megaparsec.Debug as D

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
ruleParserDoc = spaceTabNewline *> pRules <* M.eof

-- Zero or more RuleExpr separated by one or more newlines
pRules :: Parser [RuleExpr]
pRules =
    many (skipTrailingNewline $ lexeme pRuleExpr <* M.eol)
  where
    skipTrailingNewline = M.lexeme spaceTabNewline


-- ########################
-- ######  RuleExpr  ######
-- ########################

pRuleExpr :: Parser RuleExpr
pRuleExpr =
    pLet <|> pForEach <|> pIf <|> pRule

pLet :: Parser RuleExpr
pLet = do
    varName <- kw "let" *> lexeme pDefineVar
    expr <- kw "=" *> lexeme pExpr
    return $ Let varName expr

pForEach :: Parser RuleExpr
pForEach = do
    dataExpr <- kw "forall" *> lexeme pExpr
    scope <- braces pRules
    return $ Forall dataExpr scope

pIf :: Parser RuleExpr
pIf = do
    varOrBoolExpr <- kw "if" *> lexeme pExpr
    scope <- braces pRules
    return $ If varOrBoolExpr scope

pRule :: Parser RuleExpr
pRule = kw "require" *> (Rule <$> pExpr)


-- ###################
-- ###### Expr ######
-- ###################

pIntExpr :: Parser Int
pIntExpr =
    Expr.makeExprParser (lexeme $ parens pIntExpr <|> pInt) table
  where
    pInt = M.decimal
    table = [ [ Prefix $ kw "increment" *> return (+1) ]
            , [ InfixL $ kw "+" *> return (+) ]
            , [ InfixL $ kw "-" *> return (-) ]
            ]

pExpr :: Parser Expr
pExpr =
    Expr.makeExprParser term exprOperatorTable
  where
    term = lexeme $ parens pExpr <|> pTerm
    pTerm = Literal <$> pLiteral <|> Var <$> pVarReference

exprOperatorTable :: [[Operator Parser Expr]]
exprOperatorTable =
    [ [ InfixL $ kw "where" *> return (\a -> DataExpr . Filter a)
      , InfixL $ kw "grouped" *> kw "by" *> return (\a -> DataExpr . GroupBy a)
      ]
    , [ InfixN $ kw "of" *> return Map ]
    , [ Prefix $ kw "count"   *> return (ValueExpr . GroupCount)
      , Prefix $ kw "sum"     *> return (ValueExpr . FoldMap Sum)
      , Prefix $ kw "average" *> return (ValueExpr . FoldMap Avg)
      , Prefix $ kw "minimum" *> return (ValueExpr . FoldMap Min)
      , Prefix $ kw "maximum" *> return (ValueExpr . FoldMap Max)
      ]
    , [ InfixL $ kw "relative" *> kw "to" *> return (\a -> ValueExpr . Relative a) ]
    , [ InfixN $ kw "==" *> return (mkComparison Eq)
      , InfixN $ kw "!=" *> return (mkComparison NEq)
      , InfixN $ kw ">=" *> return (mkComparison GtEq)
      , InfixN $ kw "<=" *> return (mkComparison LtEq)
      , InfixN $ kw ">" <* M.notFollowedBy (M.char '=') *> return (mkComparison Gt)
      , InfixN $ kw "<" <* M.notFollowedBy (M.char '=') *> return (mkComparison Lt)
      ]
    , [ Prefix $ kw "NOT" *> return (BoolExpr . Not) ]
    , [ InfixL $ kw "AND" *> return (\a -> BoolExpr . And a) ]
    , [ InfixL $ kw "OR"  *> return (\a -> BoolExpr . Or a) ] ]
  where
    mkComparison numComp a b = BoolExpr $ Comparison a numComp b


-- ########################
-- ##### Identifiers ######
-- ########################

-- | Variable name defined through a let-binding (must begin with lowercase letter)
pDefineVar :: Parser Text
pDefineVar = do
    word@(firstChar : remainingChars) <- toS <$> pVarReference
    if C.isLower firstChar
        then return (toS word)
        else failParse "Variable name must begin with lower case letter"
                [toS $ C.toLower firstChar : remainingChars]

-- | A reference to a variable name (no restriction on start letter case)
pVarReference :: Parser Text
pVarReference = do
    firstChar <- M.letterChar
    remainingChars <- many M.alphaNumChar
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
        Percent <$> M.try pPercentage
    <|> FieldValue <$> pFieldValue
    <|> FieldName <$> pFieldName

-- | Field names begin with a "." followed by an upper case character
--    followed by zero or more alphanumeric characters
pFieldName :: Parser FieldName
pFieldName = do
    varRef <- M.char '.' *> pVarReference
    let word@(firstChar : remainingChars) = toS varRef
    if C.isUpper firstChar
        then return (fromString word)
        else failParse "Field name must begin with upper case letter"
                [toS $ C.toUpper firstChar : remainingChars]

pPercentage :: Parser Number
pPercentage = pNumber <* M.char '%'

pFieldValue :: Parser FieldValue
pFieldValue =
        Bool <$> pBool
    <|> String <$> pStringLiteral
    <|> Number <$> pNumber

pBool :: Parser Bool
pBool =
        pConstant "true" *> return True
    <|> pConstant "false" *> return False
  where
    pConstant str = M.chunk str *> M.notFollowedBy M.alphaNumChar

pStringLiteral :: Parser Text
pStringLiteral = fmap toS $
    M.char '"' *> M.manyTill M.charLiteral (M.char '"')

-- Parse a signed integer or floating point number
pNumber :: Parser Number
pNumber = signed $
    M.try (fromReal <$> M.float) <|> fromIntegral <$> M.decimal
  where
    signed = M.signed (return ())


-- ########################
-- ######  Helpers   ######
-- ########################

-- Parse the given keyword (discarding trailing spaces/tabs)
kw :: Text -> Parser ()
kw input =
    lexeme $
        M.chunk input *> M.notFollowedBy M.alphaNumChar
    -- "notFollowedBy" makes sure kw-prefixed strings are not parsed as a kw.
    -- E.g. "counterParty" shouldn't be parsed as "count" on a variable named "erParty",
    --   but as a variable named "counterParty".

-- Parse an expression enclosed in parentheses.
-- Discards trailing spaces/tabs after both opening and closing parens.
parens :: Parser a -> Parser a
parens = M.between
    (lexeme $ M.chunk "(")
    (lexeme $ M.chunk ")")

-- Run "inputParser" between:
--   <space/tab/newline>{<space/tab/newline>inputParser<space/tab/newline>}<space/tab>
-- NB: Notice that no newline(s) are parsed after closing brace.
braces :: Parser a -> Parser a
braces = M.between
    (spaceTabNewline *> M.chunk "{" *> spaceTabNewline)
    (spaceTabNewline *> M.chunk "}" *> spaceTab)

-- Line comments start with //
lineComment :: Parser ()
lineComment = M.skipLineComment "//"

-- Block comments are inside /* */
blockComment :: Parser ()
blockComment = M.skipBlockComment "/*" "*/"

-- A space-consumer that consumes newlines
spaceTabNewline :: Parser ()
spaceTabNewline = M.space (void M.spaceChar) lineComment blockComment

-- A space-consumer that does NOT consume newlines
spaceTab :: Parser ()
spaceTab = M.space (void $ M.oneOf [' ', '\t']) lineComment blockComment

-- Parse something and remove optional trailing tabs/spaces
lexeme :: Parser a -> Parser a
lexeme = M.lexeme spaceTab

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
