module Eval.Bool where

import LangPrelude
import Absyn
import Eval.Types
import Eval.Common
import qualified Comparison
import qualified Eval.GroupFold
import qualified Eval.DataExpr
import qualified Eval.Grouping


-- The result of evaluating a 'VarExpr'
data LiteralOrTree
    = EvalLiteral Literal               -- ValueExpr/BoolExpr
    | EvalTree Eval.DataExpr.EvalTree   -- DataExpr
        deriving (Eq, Show)

eval :: Env LiteralOrTree   -- Evaluated variable environment
     -> NonEmpty RuleExpr   -- Rule
     -> Bool                -- Success/failure (True/False)
eval initialEnv ruleExprs' =
    snd $ foldl' go (initialEnv, True) ruleExprs'
  where
    go (env, success) (Let varName varExpr) =
        let newEnv = insert env varName (evalVarExpr env varExpr)
        in (newEnv, success)
    go (env, success) (Foreach varOrDataExpr ruleExprs) =
        let envTree = accumMap (\env' tree (fieldName, _) -> insert env' fieldName (EvalTree tree))
                               (\_ -> id)
                               env
                               (getTree env varOrDataExpr)
            results = map (\env' -> eval env' ruleExprs) (termNodes envTree)
        in (env, all (== True) (success : results))
    go (env, success) (If varOrBoolExpr ruleExprs) =
        if getBool env varOrBoolExpr
            then (env, eval env ruleExprs && success)
            else (env, success)
    go (env, success) (Rule varOrBoolExpr) =
        (env, getBool env varOrBoolExpr && success)

evalDataExpr
    :: Env LiteralOrTree
    -> DataExpr
    -> Tree [Position]
evalDataExpr env dataExpr =
    case dataExpr of
        GroupBy varOrFieldName varOrDataExpr ->
            let tree = getTree env varOrDataExpr
                fieldName = getFieldName env varOrFieldName
            in Eval.Grouping.addGrouping fieldName tree
        Filter boolExpr varOrDataExpr ->
            undefined
  where
    evalPosComparison :: FieldName -> BoolCompare -> FieldValue -> Position -> Bool
    evalPosComparison fieldName bCompare fieldValue pos =
        let fCompare = comparator bCompare
        in lookup' fieldName pos `fCompare` fieldValue

evalVarExpr
    :: Env LiteralOrTree
    -> VarExpr
    -> LiteralOrTree
evalVarExpr env varExpr =
    case varExpr of
        ValueExpr valueExpr -> EvalLiteral $ evalValueExpr env valueExpr
        BoolExpr boolExpr -> EvalLiteral $ FieldValue $ Bool $ evalBoolExpr env boolExpr
        DataExpr dataExpr -> EvalTree $ evalDataExpr env dataExpr

evalValueExpr
    :: Env LiteralOrTree
    -> ValueExpr
    -> Literal
evalValueExpr env valueExpr =
    case valueExpr of
        Literal literal -> literal
        GroupOp groupOp -> evalGroupOp env groupOp

evalGroupOp :: Env LiteralOrTree -> GroupOp -> Literal
evalGroupOp env groupOp =
    case groupOp of
        GroupCount varOrDataExpr ->
            -- TODO: incorrect count of trees with zero positions in a 'TermNode' (empty groups)
            let positions = termNodes (getTree env varOrDataExpr)
            in FieldValue . Number . fromIntegral $ length positions
        PositionFold positionFold varOrFieldName varOrDataExpr maybeVarOrDataExpr ->
            let fieldName = getFieldName env varOrFieldName
                doFold = evalPositionFold env positionFold fieldName
                baseNumber = doFold varOrDataExpr
                -- NB: the below will return 'Infinity' if the relative 'DataExpr'
                --  is the empty tree, and an exception will be thrown if this is
                --  compared to another number (in a 'Comparison')
                mkRelativeNumber relativeVarOrDataExpr = Percent $ baseNumber / doFold relativeVarOrDataExpr
            in maybe (FieldValue $ Number baseNumber) mkRelativeNumber maybeVarOrDataExpr

evalPositionFold :: Env LiteralOrTree -> PositionFold -> FieldName -> VarOr DataExpr -> Number
evalPositionFold env positionFold fieldName varOrDataExpr =
    let foldFunction = Eval.GroupFold.getFold positionFold fieldName
        tree = getTree env varOrDataExpr
    in foldFunction (concat $ termNodes tree)

evalBoolExpr
    :: Env LiteralOrTree
    -> BoolExpr
    -> Bool
evalBoolExpr env boolExpr =
    case boolExpr of
        Comparison varOrValueExpr1 boolCompare varOrValueExpr2 ->
            let compareFun = Comparison.comparator boolCompare
            -- NB: will throw an exception if two incompatible literals are compared
            --  for order (e.g. 'Percent' and 'Number').
            -- TODO: if two incompatible literals are compared for equality then it will
            --  just return 'False' (don't use 'Eq' for equality comparison)
            in getLiteral env varOrValueExpr1 `compareFun` getLiteral env varOrValueExpr2
        And varOrBoolExpr1 varOrBoolExpr2 ->
            getBool env varOrBoolExpr1 && getBool env varOrBoolExpr2
        Or varOrBoolExpr1 varOrBoolExpr2 ->
            getBool env varOrBoolExpr1 || getBool env varOrBoolExpr2
        Not varOrBoolExpr ->
            not $ getBool env varOrBoolExpr

-- #####################
-- ###### Helpers ######
-- #####################

getTree :: Env LiteralOrTree -> VarOr DataExpr -> Tree [Position]
getTree env (NotVar dataExpr) = evalDataExpr env dataExpr
getTree env (Var varName) = fromLitOrTree $ lookup' varName env
  where
    fromLitOrTree (EvalTree tree) = tree
    fromLitOrTree other = typeError "Grouping" other

getBool :: Env LiteralOrTree -> VarOr BoolExpr -> Bool
getBool env (NotVar boolExpr) = evalBoolExpr env boolExpr
getBool env (Var varName) = fromLitOrTree $ lookup' varName env
  where
    fromLitOrTree (EvalLiteral (FieldValue (Bool boolExpr))) = boolExpr
    fromLitOrTree other = typeError "Boolean" other

getFieldName :: Env LiteralOrTree -> VarOr FieldName -> FieldName
getFieldName _ (NotVar fieldName) = fieldName
getFieldName env (Var varName) = fromLitOrTree $ lookup' varName env
  where
    fromLitOrTree (EvalLiteral (FieldName fieldName)) = fieldName
    fromLitOrTree other = typeError "FieldName" other

getLiteral :: Env LiteralOrTree -> VarOr ValueExpr -> Literal
getLiteral env (NotVar valueExpr) = evalValueExpr env valueExpr
getLiteral env (Var varName) = fromLitOrTree $ lookup' varName env
  where
    fromLitOrTree (EvalLiteral literal) = literal
    fromLitOrTree other = typeError "FieldName" other

typeError :: String -> LiteralOrTree -> a
typeError expectedType foundValue =
    error $ "Type error. Expected '" ++ expectedType ++ "' found " ++ show foundValue
