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

let' = Let
forEach = Foreach
where'' groupValueExpr boolCompare inputValue =
    FilterGroup $ GroupComparison groupValueExpr boolCompare inputValue
groupedBy input field = GroupBy field input
percent = Literal . Percent
where' input filterComp = Filter filterComp input


group = GroupComparison
pos = PosComparison
rule = Rule
sumOver = SumOver

-- numberOf field comp val = GroupComparison (CountDistinct field) comp val
-- forall = PosComparison
-- sumOf field rel comp val = GroupComparison (SumOver field rel) comp val
relativeTo = RelativeComparison
