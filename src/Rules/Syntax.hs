{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Rules.Syntax where

import LangPrelude
import Absyn
import qualified Data.List.NonEmpty               as NE


($:) :: (a -> b) -> a -> b
f $: x =  f x
infixr 6 $:

(|:) :: (a -> b) -> a -> b
f |: x =  f x
infixr 8 |:

(+++) :: RuleExpr -> RuleExpr -> RuleExpr
a +++ b = And a b
infixr 7 +++

let' name dataExprList = Let name (NE.fromList dataExprList)
forEach = Foreach
where' groupValueExpr boolCompare inputValue =
    GroupComparison groupValueExpr boolCompare inputValue


group = GroupComparison
pos = PosComparison
--(Comparison valExpr fComp value)
rule = Rule
sumOver = SumOver
sumOverRelative fieldName relative = sumOver fieldName (Just relative)
-- of' fieldName = sumOver fieldName (Just "Portfolio")
numberOfRelativeTo = GroupComparison . CountDistinct
numberOf field comp val = GroupComparison (CountDistinct field) comp val
forall = PosComparison
sumOf field rel comp val = GroupComparison (SumOver field rel) comp val
relativeTo field relative = SumOver field (Just relative)
