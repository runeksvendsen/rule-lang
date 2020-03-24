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

instance Monad m => Serial m Absyn.GroupValueExpr where
    series =
        (Absyn.Literal <$> SS.series)
            \/ (Absyn.GroupOp <$> SS.series)
            \/ (Absyn.DataExpr <$> SS.series)
            \/ (Absyn.Var <$> camelCaseText)

instance Monad m => Serial m Absyn.DataExpr
instance Monad m => Serial m Absyn.GroupOp
instance Monad m => Serial m Absyn.PositionFold
instance Monad m => Serial m Absyn.Comparison
instance Monad m => Serial m Absyn.RuleExpr
instance Monad m => Serial m Comparison.BoolCompare
instance Monad m => Serial m Absyn.FieldValue

instance Monad m => Serial m Absyn.Number where
    series = (Absyn.fromReal :: Double -> Absyn.Number) <$> SS.series

instance Monad m => Serial m T.Text where
    series = T.pack <$> nonEmptyList

instance Monad m => Serial m (NE.NonEmpty Absyn.RuleExpr) where
    series = NE.fromList <$> nonEmptyList
