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
a +++ b = And a (nonEmpty b)
infixr 7 +++

let' = Let
forEach = Foreach
where'' groupValueExpr boolCompare inputValue =
    Comparison groupValueExpr boolCompare inputValue
groupedBy input field = GroupBy field input
percent = Literal . Percent
where' input filterComp = Filter filterComp input

relativeFold foldType field varIn varRel = GroupOp $
                 GroupOp (PositionFold foldType field varIn)
    `relativeTo` varRel

rule a b c = Rule (Comparison a b c)
sumOver = SumOver

relativeTo = Relative
