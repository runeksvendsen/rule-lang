{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parse
( ruleParser
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
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char
import qualified Text.Megaparsec.Debug as D
import qualified Control.Monad.Combinators.Expr as C
import qualified Control.Applicative.Combinators.NonEmpty as CN

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

ruleParser :: Parser (NonEmpty RuleExpr)
ruleParser = scn *> CN.some (debug "pRuleExpr-NEWLINE" $ skipTrailingNewline pRuleExpr) <* M.eof


-- ########################
-- ######  RuleExpr  ######
-- ########################

pRuleExpr :: Parser RuleExpr
pRuleExpr = debug "pRuleExpr" $
    pLet <|> pForEach <|> pIf <|> pRule

pLet :: Parser RuleExpr
pLet = do
    keyword "let"
    varName <- skipTrailingWhitespace pDefineVar
    keyword "="
    expr <- skipTrailingWhitespace pExpr
    return $ Let varName expr

pForEach :: Parser RuleExpr
pForEach = debug "pForEach" $ do
    keyword "forall"
    dataExpr <- skipTrailingWhitespace pExpr
    scope <- braces $ many (skipTrailingNewline pRuleExpr)
    return $ Forall dataExpr scope

pIf :: Parser RuleExpr
pIf = debug "pIf" $ do
    keyword "if"
    varOrBoolExpr <- skipTrailingWhitespace pExpr
    scope <- braces $ many (skipTrailingNewline pRuleExpr)
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
    expr = C.makeExprParser term table <?> "everythingExpr"
    term = skipTrailingWhitespace (parens expr) <|> pTerm <?> "everythingTerm"
    pTerm = skipTrailingWhitespace $ Literal <$> pLiteral <|> Var <$> pVarReferece
    table =
        [ [ C.InfixL $ keyword "where" >> return (\a -> DataExpr . Filter a)
          , C.InfixL $ keyword "grouped" >> keyword "by" >> return (\a -> DataExpr . GroupBy a)
          ]
        , [ C.InfixN $ keyword "of" >> return Map ]
        , [ prefix "count" (ValueExpr . GroupCount)
          , prefix "sum" (ValueExpr . FoldMap Sum)
          , prefix "average" (ValueExpr . FoldMap Average)
          , prefix "minimum" (ValueExpr . FoldMap Min)
          , prefix "maximum" (ValueExpr . FoldMap Max)
          ]
        , [ C.InfixL $ keyword "relative" >> keyword "to" >> return (\a -> ValueExpr . Relative a)]
        , [ binary' "==" (mkComp Eq)
          , binary' "!=" (mkComp NEq)
          , binary' ">" (mkComp Gt)
          , binary' "<" (mkComp Lt)
          , binary' ">=" (mkComp GtEq)
          , binary' "<=" (mkComp LtEq)
          ]
        , [ prefix "NOT" (BoolExpr . Not) ]
        , [ binary "AND" (\a -> BoolExpr . And a) ]
        , [ binary "OR" (\a -> BoolExpr . Or a) ]
        ]
    binary  name f = C.InfixL  (f <$ L.symbol sc name)
    prefix  name f = C.Prefix  (f <$ L.symbol sc name)
    binary' name f = C.InfixN (f <$ op name)
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
    <|> (FieldValue <$> pFieldValue)
    <|> FieldName <$> pFieldName

-- | Field names begin with a "." followed by an upper case character
--    followed by zero or more alphanumeric characters
pFieldName :: Parser FieldName
pFieldName = debug "FieldName" $ do
    void $ Text.Megaparsec.Char.char '.'
    word@(firstChar : remainingChars) <- toS <$> pVarReferece
    if C.isUpper firstChar
        then return (fromString $ toS word)
        else failParse "Field name must being with upper case letter"
                [toS $ C.toUpper firstChar : remainingChars]

pPercentage :: Parser Literal
pPercentage = do
    num <- pNumber
    void $ Text.Megaparsec.Char.char '%'
    return $ Percent num

pFieldValue :: Parser FieldValue
pFieldValue = debug "pFieldValue" $
        (String . toS <$> pStringLiteral)
    <|> Bool <$> pBool
    <|> Number <$> pNumber
  where
    pBool = (pConstant "true" >> return True) <|> (pConstant "false" >> return False)
    pConstant c = debug "pConstant" $
        skipTrailingNewline $
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
    skipTrailingWhitespace $
        M.chunk input *> M.notFollowedBy Text.Megaparsec.Char.alphaNumChar
    -- "notFollowedBy" makes sure keyword-prefixed strings are not parsed as a keyword.
    -- E.g. "counterParty" shouldn't be parsed as "count" on a variable named "erParty",
    --   but as a variable named "counterParty".

-- Parse an expression enclosed in parentheses.
parens :: (M.MonadParsec e s m, M.Token s ~ Char) => m b -> m b
parens p =
    Text.Megaparsec.Char.char '(' *> p <* Text.Megaparsec.Char.char ')'

-- Parse an expression enclosed in braces, discarding all whitespace
--   before and after both the opening and closing brace.
braces :: Parser a -> Parser a
braces p = M.between
    (discardSurroundingWhitespace $ Text.Megaparsec.Char.char '{')
    (discardSurroundingWhitespace $ Text.Megaparsec.Char.char '}')
    p
  where
    -- discard whitespace (including newlines) before and after
    discardSurroundingWhitespace p' = scn >> p' >> scn

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
