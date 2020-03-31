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

instance Monad m => Serial m Absyn.Literal where
    series =
        (Absyn.Integer <$> SS.series)
            \/ (Absyn.Percent <$> SS.series)
            \/ (Absyn.FieldName <$> pascalCaseText)
            \/ (Absyn.FieldValue <$> SS.series)

instance Monad m => Serial m Absyn.RuleExpr where
    series =
        -- self-recursive
        (Absyn.Let <$> SS.series <*> SS.series <*> decDepth SS.series)
            -- self-recursive
            \/ (Absyn.Foreach <$> SS.series <*> SS.series <*> decDepth SS.series)
            \/ (Absyn.Rule <$> SS.series)
            -- self-recursive
            \/ (Absyn.And <$> decDepth SS.series <*> decDepth SS.series)

instance Monad m => Serial m Absyn.GroupValueExpr where
    series =
        (Absyn.Literal <$> SS.series)
            \/ (Absyn.GroupOp <$> SS.series)
            \/ (Absyn.DataExpr <$> SS.series)
            \/ (Absyn.Var <$> camelCaseText)

-- Part of 'GroupValueExpr', so further 'GroupValueExpr'
--  must be constructed with decreased depth
instance Monad m => Serial m Absyn.DataExpr where
    series =
        (Absyn.GroupBy <$> decDepth SS.series <*> decDepth SS.series)
            \/ (Absyn.Filter <$> SS.series <*> decDepth SS.series)

-- Part of 'GroupValueExpr', so further 'GroupValueExpr'
--  must be constructed with decreased depth
instance Monad m => Serial m Absyn.GroupOp where
    series =
        (Absyn.GroupCount <$> decDepth SS.series)
            \/ (Absyn.PositionFold <$> SS.series <*> decDepth SS.series <*> decDepth SS.series)
            \/ (Absyn.Relative <$> decDepth SS.series <*> decDepth SS.series)

-- Part of 'GroupValueExpr' via 'DataExpr', so further 'GroupValueExpr'
--  must be constructed with decreased depth
instance Monad m => Serial m Absyn.Comparison where
    series =
        Absyn.Comparison <$> decDepth SS.series <*> SS.series <*> decDepth SS.series

instance Monad m => Serial m Absyn.PositionFold
instance Monad m => Serial m Comparison.BoolCompare
instance Monad m => Serial m Absyn.FieldValue

instance Monad m => Serial m Absyn.Number where
    series = (Absyn.fromReal :: Double -> Absyn.Number) <$> SS.series

instance Monad m => Serial m T.Text where
    series = T.pack <$> nonEmptyList

instance Monad m => Serial m (NE.NonEmpty Absyn.RuleExpr) where
    series = NE.fromList <$> nonEmptyList
