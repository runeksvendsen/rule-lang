{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
module Orphans
()
where

import Prelude
import Absyn
import qualified Comparison

import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NE
import Test.SmallCheck.Series
import qualified Test.SmallCheck.Series as SS
import qualified Data.Text as T
-- DEBUG
import Debug.Trace
import Data.Proxy (Proxy(Proxy))


traceM' :: (Monad m, Show b) => m b -> m b
traceM' mb = do
    a <- mb
    traceShowM a
    return a

-- ##### Inviariants #####
--   * Variable names:
--        * non-empty (no "")
--        * start with a lower case letter
--   * Field names start with a upper case letter
--   * Numbers are non-negative

varName :: Monad m => Series m T.Text
varName = return "a"

fieldName :: Monad m => Series m Expr
fieldName = return $ Literal . FieldName $ "A"

someString :: Monad m => Series m T.Text
someString = return "hello"

nonEmptyList :: Serial m a => Series m [a]
nonEmptyList = do
   NonEmpty lst <- SS.series
   return lst

pascalCaseText :: Monad m => Series m T.Text
pascalCaseText = do
    NonEmpty nonEmptyString <- SS.series
    let (firstChar : remainingChars) = nonEmptyString
    return $ T.pack (Char.toUpper firstChar : remainingChars :: String)

camelCaseText :: Monad m => Series m T.Text
camelCaseText = do
    NonEmpty nonEmptyString <- SS.series
    let (firstChar : remainingChars) = nonEmptyString
    return $ T.pack (Char.toLower firstChar : remainingChars :: String)

varOr :: Monad m => Series m Expr -> Series m Expr
varOr ss =
    (Var <$> varName)
        \/ ss

instance Monad m => Serial m RuleExpr where
    series =
        let ruleExprSeries = SS.series
            nonEmptyRule = lengthTwoList ruleExprSeries
        in
        (Let <$> varName <*> SS.series)
            -- self-recursive
            \/ (Forall <$> SS.series <*> nonEmptyRule)
            -- self-recursive
            \/ (If <$> SS.series <*> nonEmptyRule)
            \/ (Rule <$> SS.series)

instance Monad m => Serial m Expr where
    series =
        (ValueExpr <$> SS.series)
            \/ (DataExpr <$> SS.series)
            \/ (BoolExpr <$> SS.series)
            \/ (Literal <$> SS.series)
            \/ (Var <$> varName)

-- Part of 'ValueExpr', so further 'ValueExpr'
--  must be constructed with decreased depth
instance Monad m => Serial m DataExpr where
    series =
        let dataExpr = decDepth $ expr (Proxy @DataExpr)
        in (GroupBy <$> varOr dataExpr <*> decDepth (varOr fieldName))
            \/ (Filter <$> varOr dataExpr <*> decDepth (expr (Proxy @BoolExpr)))

instance Monad m => Serial m ValueExpr where
    series =
        let dataExpr = decDepth (exprOrVar (Proxy @DataExpr))
            map' = Map <$> exprOrVar (Proxy @FieldName) <*> dataExpr
        in (GroupCount <$> dataExpr)
            \/ (FoldMap <$> SS.series <*> map')
            \/ (Relative <$> decDepth SS.series <*> decDepth (exprOrVar (Proxy @ValueExpr)))

-- Part of 'ValueExpr' via 'DataExpr', so further 'ValueExpr'
--  must be constructed with decreased depth
instance Monad m => Serial m BoolExpr where
    series =
        let comparable = decDepth $ expr (Proxy @ValueExpr) \/ expr (Proxy @Literal)
            boolExpr = decDepth $ expr (Proxy @BoolExpr)
        in (Comparison <$> varOr comparable <*> decDepth SS.series <*> varOr comparable)
            \/ (And <$> boolExpr <*> boolExpr)
            \/ (Or <$> boolExpr <*> boolExpr)
            \/ (Not <$> boolExpr)

instance Monad m => Serial m Literal where
    series =
        (Percent <$> SS.series)
            \/ (FieldName <$> SS.series)
            \/ (FieldValue <$> SS.series)

instance Monad m => Serial m Fold
instance Monad m => Serial m Comparison.BoolCompare

instance Monad m => Serial m FieldName where
    series = return "A"

instance Monad m => Serial m FieldValue where
    series =
        (Number <$> SS.series)
            \/ (String <$> someString)
            \/ (Bool <$> return True)
            \/ (Bool <$> return False)

instance Monad m => Serial m Number where
    series = return $ fromReal (1.0 :: Double)

instance Monad m => Serial m (NE.NonEmpty RuleExpr) where
    series = NE.fromList <$> nonEmptyList

lengthTwoList :: Monad m => Series m a -> Series m [a]
lengthTwoList series' = do
    first <- decDepth series'
    next <- decDepth $ decDepth series'
    return [] \/ return [first] \/ return (first : [next])

-- Generate something that's inside a 'Expr' and toExpr it in a 'Expr'.
-- Use with TypeApplications, e.g.: "expr (Proxy @DataExpr)"
expr :: forall m a. (Serial m a, ToExpr a) => Proxy a -> Series m Expr
expr _ = fmap toExpr (SS.series :: Series m a)

exprOrVar :: (Serial m a, ToExpr a) => Proxy a -> Series m Expr
exprOrVar a = varOr (expr a)

class ToExpr a where
    toExpr :: a -> Expr

instance ToExpr DataExpr where
    toExpr = DataExpr

instance ToExpr ValueExpr where
    toExpr = ValueExpr

instance ToExpr BoolExpr where
    toExpr = BoolExpr

instance ToExpr Literal where
    toExpr = Literal

instance ToExpr FieldValue where
    toExpr = Literal . FieldValue

instance ToExpr FieldName where
    toExpr = Literal . FieldName

