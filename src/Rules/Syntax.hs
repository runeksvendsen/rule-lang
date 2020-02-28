{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Rules.Syntax where

import Prelude
import Absyn


($:) :: (a -> b) -> a -> b
f $: x =  f x
infixr 6 $:

(|:) :: (a -> b) -> a -> b
f |: x =  f x
infixr 8 |:

(+++) :: RuleExpr -> RuleExpr -> RuleExpr
a +++ b = And a b
infixr 7 +++

forEach = GroupBy
where' valExpr fComp value = Filter (Comparison valExpr fComp value)
rule valExpr fComp value = Rule (Comparison valExpr fComp value)
sumOver = SumOver
sumOverRelative fieldName relative = sumOver fieldName (Just relative)
of' fieldName = sumOver fieldName (Just "Portfolio")
numberOfRelativeTo = GroupValueExpr . CountDistinct
numberOf = GroupValueExpr . CountDistinct
forall = PosValueExpr . Get
sumOf field rel = GroupValueExpr (SumOver field rel)
relativeTo group = Just group
