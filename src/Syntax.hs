module Syntax where

import Prelude
import Absyn


($:) :: (a -> b) -> a -> b
f $: x =  f x
infixr 6 $:

(|:) :: (a -> b) -> a -> b
f |: x =  f x
infixr 8 |:

(+++) :: DataExpr a -> DataExpr a -> DataExpr a
a +++ b = Both a b
infixr 7 +++

forEach = GroupBy
where' valExpr fComp value scope = Filter (Comparison valExpr fComp value) (Just scope)
whereEnd valExpr fComp value = Filter (Comparison valExpr fComp value) Nothing
rule valExpr fComp value = Rule (Comparison valExpr fComp value)
sumOver = SumOver
sumOverRelative fieldName groupName relative = sumOver fieldName groupName (Just relative)
of' fieldName groupName = sumOver fieldName groupName (Just "Portfolio")
numberOfRelativeTo = CountDistinct
numberOf fieldName = CountDistinct fieldName "Portfolio"
forall = Forall
