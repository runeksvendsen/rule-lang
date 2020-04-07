{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Orphans where

import Prelude
import qualified Absyn
import qualified Comparison

import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NE
import Test.SmallCheck.Series
import qualified Test.SmallCheck.Series as SS
import qualified Data.Text                        as T
-- DEBUG
import Debug.Trace


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

instance (Monad m, Serial m a) => Serial m (Absyn.VarOr a) where
    series =
        (Absyn.Var <$> camelCaseText)
            \/ (Absyn.NotVar <$> SS.series)

instance Monad m => Serial m Absyn.Literal where
    series =
        (Absyn.Percent <$> SS.series)
            \/ (Absyn.FieldName <$> pascalCaseText)
            \/ (Absyn.FieldValue <$> SS.series)

instance Monad m => Serial m Absyn.RuleExpr where
    series =
        -- self-recursive
        (Absyn.Let <$> SS.series <*> SS.series <*> decDepth SS.series)
            -- self-recursive
            \/ (Absyn.Foreach <$> varOrFieldName <*> SS.series <*> decDepth SS.series)
            \/ (Absyn.Rule <$> SS.series)

varOrFieldName :: Monad m => Series m (Absyn.VarOr T.Text)
varOrFieldName =
    (Absyn.Var <$> camelCaseText)
        \/ (Absyn.NotVar <$> pascalCaseText)

instance Monad m => Serial m Absyn.VarExpr where
    series =
        (Absyn.ValueExpr <$> SS.series)
            \/ (Absyn.DataExpr <$> SS.series)
            \/ (Absyn.BoolExpr <$> SS.series)

instance Monad m => Serial m Absyn.ValueExpr where
    series =
        (Absyn.Literal <$> SS.series)
            \/ (Absyn.GroupOp <$> SS.series)

-- Part of 'ValueExpr', so further 'ValueExpr'
--  must be constructed with decreased depth
instance Monad m => Serial m Absyn.DataExpr where
    series =
        (Absyn.GroupBy <$> SS.series <*> decDepth SS.series)
            \/ (Absyn.Filter <$> SS.series <*> decDepth SS.series)

-- Part of 'ValueExpr', so further 'ValueExpr'
--  must be constructed with decreased depth
instance Monad m => Serial m Absyn.GroupOp where
    series =
        (Absyn.GroupCount <$> decDepth SS.series)
            \/ (Absyn.PositionFold <$> SS.series <*> varOrFieldName <*> decDepth SS.series)
            \/ (Absyn.Relative <$> decDepth SS.series <*> varOrFieldName)

-- Part of 'ValueExpr' via 'DataExpr', so further 'ValueExpr'
--  must be constructed with decreased depth
instance Monad m => Serial m Absyn.BoolExpr where
    series =
        (Absyn.Comparison <$> SS.series <*> SS.series <*> SS.series)
            \/ (Absyn.And <$> decDepth SS.series <*> decDepth SS.series)
            \/ (Absyn.Or <$> decDepth SS.series <*> decDepth SS.series)
            \/ (Absyn.Not <$> decDepth SS.series)

instance Monad m => Serial m Absyn.PositionFold
instance Monad m => Serial m Comparison.BoolCompare
instance Monad m => Serial m Absyn.FieldValue

instance Monad m => Serial m Absyn.Number where
    series = (Absyn.fromReal :: Double -> Absyn.Number) . getNonNegative <$> SS.series

instance Monad m => Serial m T.Text where
    series = T.pack <$> nonEmptyList

instance Monad m => Serial m (NE.NonEmpty Absyn.RuleExpr) where
    series = NE.fromList <$> nonEmptyList
