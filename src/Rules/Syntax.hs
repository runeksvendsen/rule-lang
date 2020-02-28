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
where' = Filter
group = GroupComparison
pos = PosComparison
--(Comparison valExpr fComp value)
rule = Rule
sumOver = SumOver
sumOverRelative fieldName relative = sumOver fieldName (Just relative)
of' fieldName = sumOver fieldName (Just "Portfolio")
numberOfRelativeTo = GroupComparison . CountDistinct
numberOf field comp val = GroupComparison (CountDistinct field) comp val
forall = PosComparison
sumOf field rel comp val = GroupComparison (SumOver field rel) comp val
relativeTo = Just

-- sumOf' field rel = GroupComparison (SumOver field rel)
