module Eval
( eval
)
where

import LangPrelude
import Absyn
import Types
import Tree
import qualified Comparison
import qualified Data.List
import qualified Data.List.NonEmpty as NE


-- | Variable environment.
-- The first item is the innermost scope.
type Env a = NE.NonEmpty (Text, a)

-- The result of evaluating a 'VarExpr'
data LiteralOrTree
    = EvalValue Literal             -- ValueExpr/BoolExpr
    | EvalTree (Tree [Position])    -- DataExpr
        deriving (Eq, Show)

-- | Returns 'False' if a rule has been violated,
--    otherwise 'True'
eval :: Env LiteralOrTree   -- Evaluated variable environment
     -> [RuleExpr]          -- Rule
     -> Bool                -- Success/failure (True/False)
eval initialEnv ruleExprs' =
    snd $ foldl' go (initialEnv, True) ruleExprs'
  where
    go (env, success) (Let varName varExpr) =
        let newEnv = insert' env varName (evalVarExpr env varExpr)
        in (newEnv, success)
    go (env, success) (Forall varOrDataExpr ruleExprs) =
        let envTree = accumMap (\env' tree (fieldName, _) -> insert' env' fieldName (EvalTree tree))
                               (\_ env' -> eval env' ruleExprs)
                               env
                               (getTree env varOrDataExpr)
        in (env, all (== True) (success : termNodes envTree))
    go (env, success) (If varOrBoolExpr ruleExprs) =
        if getGroupBool env varOrBoolExpr
            then (env, eval env ruleExprs && success)
            else (env, success)
    go (env, success) (Rule varOrBoolExpr) =
        (env, getGroupBool env varOrBoolExpr && success)

evalDataExpr
    :: Env LiteralOrTree
    -> DataExpr
    -> Tree [Position]
evalDataExpr env dataExpr =
    case dataExpr of
        GroupBy varOrFieldName varOrDataExpr ->
            let tree = getTree env varOrDataExpr
                fieldName = getFieldName env varOrFieldName
            in addGrouping posLookup fieldName tree
        Filter boolExpr varOrDataExpr ->
            let filterPos env' =
                    case evalBoolExpr env' boolExpr of
                        -- Group comparison: either remove all or no positions
                        Left b -> const b
                        -- Position comparison: remove/keep on a per-position basis
                        Right posFun -> posFun
            in accumMap (\env' tree' (fieldName, _) -> insert' env' fieldName (EvalTree tree'))
                     (\positions env' -> filter (filterPos env') positions)
                     env
                     (getTree env varOrDataExpr)

evalVarExpr
    :: Env LiteralOrTree
    -> VarExpr
    -> LiteralOrTree
evalVarExpr env varExpr =
    case varExpr of
        ValueExpr valueExpr -> EvalValue $ evalValueExpr env valueExpr
        BoolExpr boolExpr -> EvalValue $ FieldValue $ Bool $ getGroupBool env (NotVar boolExpr)
        DataExpr dataExpr -> EvalTree $ evalDataExpr env dataExpr

evalValueExpr
    :: Env LiteralOrTree
    -> ValueExpr
    -> Literal
evalValueExpr env valueExpr =
    case valueExpr of
        Literal value -> value
        GroupOp groupOp -> evalGroupOp env groupOp

evalGroupOp :: Env LiteralOrTree -> GroupOp -> Literal
evalGroupOp env groupOp =
    case groupOp of
        GroupCount varOrDataExpr ->
            let positions = termNodes (getTree env varOrDataExpr)
            -- NB: we filter off TermNodes with an empty position list,
            --  since an empty group is not a group
            in FieldValue . Number . fromIntegral $ length $ filter (not . null) positions
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
    let tree = getTree env varOrDataExpr
        foldFunc = foldFunction positionFold
    in foldFunc $ map getNumber (concat $ termNodes tree)
  where
    getNumber pos = do
        case posLookup fieldName pos of
            Number num -> num
            fieldValue -> typeError "Number" (EvalValue $ FieldValue fieldValue)
    foldFunction
        :: (Fractional a, Ord a, Foldable t)
        => PositionFold
        -> t a
        -> a
    foldFunction SumOver = sum
    foldFunction Average = \numList ->
            let len = length numList
            in if len == 0 then 0 else sum numList / realToFrac len
    foldFunction Max = foldl' max 0
    foldFunction Min = foldl' min 0


evalBoolExpr
    :: Env LiteralOrTree
    -> BoolExpr
    -> Either Bool (Position -> Bool)
evalBoolExpr env boolExpr =
    case boolExpr of
        -- sum Literal of Issuer relative to Country      >=      25%
        Comparison varOrValueExpr1 boolCompare varOrValueExpr2 ->
            let compareFun :: Ord a => a -> a -> Bool
                compareFun = Comparison.comparator boolCompare
            -- NB: will throw an exception if two incompatible literals are compared
            --  for order (e.g. 'Percent' and 'Number').
            -- TODO: if two incompatible literals are compared for *equality* then
            --  'False' is returned (don't use 'Eq' for equality comparison)
                lit1 = getValue env varOrValueExpr1
                lit2 = getValue env varOrValueExpr2
            -- Position comparison: FieldName & FieldValue
            -- Group comparison: everything else
            in case (lit1, lit2) of
                (FieldName fieldName, FieldValue fieldValue) -> Right $
                    \pos -> posLookup fieldName pos `compareFun` fieldValue
                _ -> Left $
                    lit1 `compareFun` lit2
        And varOrBoolExpr1 varOrBoolExpr2 ->
            binopCompose (&&) (getBool env varOrBoolExpr1) (getBool env varOrBoolExpr2)
        Or varOrBoolExpr1 varOrBoolExpr2 ->
            binopCompose (||) (getBool env varOrBoolExpr1) (getBool env varOrBoolExpr2)
        Not varOrBoolExpr ->
            unopCompose not (getBool env varOrBoolExpr)
  where
    -- compose two booleans returned by 'evalBoolExpr'
    binopCompose
        :: (Bool -> Bool -> Bool)
        -> Either Bool (a -> Bool)
        -> Either Bool (a -> Bool)
        -> Either Bool (a -> Bool)
    binopCompose f (Left b1) (Left b2) = Left $ f b1 b2
    binopCompose f (Right b1Fun) (Left b2) = Right $ \pos -> f (b1Fun pos) b2
    binopCompose f (Left b1) (Right b2Fun) = Right $ \pos -> f b1 (b2Fun pos)
    binopCompose f (Right b1Fun) (Right b2Fun) = Right $ \pos -> f (b1Fun pos) (b2Fun pos)
    unopCompose
        :: (Bool -> Bool)
        -> Either Bool (a -> Bool)
        -> Either Bool (a -> Bool)
    unopCompose f (Right b1Fun) = Right $ \pos -> f $ b1Fun pos
    unopCompose f (Left b1) = Left $ f b1


-- #####################
-- ###### Helpers ######
-- #####################

insert' :: Env b -> Text -> b -> Env b
insert' env key value = (key,value) `NE.cons` env

-- Look up a variable and throw exception if not found
lookup' :: Text -> Env b -> b
lookup' varName items =
    fromMaybe (error $ "Variable '" ++ toS varName ++ "' not found") $
        Data.List.lookup varName (NE.toList items)

posLookup :: FieldName -> Position -> FieldValue
posLookup fieldName pos =
    let errorMsg = unlines ["Field name '" ++ toS fieldName ++ "' not found in position:", show pos]
    in maybe (error errorMsg)
        id
        (lookup fieldName pos)

getTree :: Env LiteralOrTree -> VarOr DataExpr -> Tree [Position]
getTree env (NotVar dataExpr) = evalDataExpr env dataExpr
getTree env (Var varName) = fromLitOrTree $ lookup' varName env
  where
    fromLitOrTree (EvalTree tree) = tree
    fromLitOrTree other = typeError "Grouping" other

getBool :: Env LiteralOrTree -> VarOr BoolExpr -> Either Bool (Position -> Bool)
getBool env (NotVar boolExpr) = evalBoolExpr env boolExpr
getBool env (Var varName) = Left $ fromLitOrTree $ lookup' varName env
  where
    fromLitOrTree (EvalValue (FieldValue (Bool boolExpr))) = boolExpr
    fromLitOrTree other = typeError "Boolean" other

getGroupBool :: Env LiteralOrTree -> VarOr BoolExpr -> Bool
getGroupBool env (NotVar boolExpr) =
    case evalBoolExpr env boolExpr of
        Left b -> b
        Right _ -> error $ "Expected " ++  "group comparison" ++ ", found: " ++ "position comparison"
getGroupBool env (Var varName) = fromLitOrTree $ lookup' varName env
  where
    fromLitOrTree (EvalValue (FieldValue (Bool boolExpr))) = boolExpr
    fromLitOrTree other = typeError "Boolean" other

getFieldName :: Env LiteralOrTree -> VarOr FieldName -> FieldName
getFieldName _ (NotVar fieldName) = fieldName
getFieldName env (Var varName) = fromLitOrTree $ lookup' varName env
  where
    fromLitOrTree (EvalValue (FieldName fieldName)) = fieldName
    fromLitOrTree other = typeError "FieldName" other

getFieldValue :: Env LiteralOrTree -> VarOr FieldValue -> FieldValue
getFieldValue _ (NotVar fieldValue) = fieldValue
getFieldValue env (Var varName) = fromLitOrTree $ lookup' varName env
  where
    fromLitOrTree (EvalValue (FieldValue fieldValue)) = fieldValue
    fromLitOrTree other = typeError "FieldValue" other

getValue :: Env LiteralOrTree -> VarOr ValueExpr -> Literal
getValue env (NotVar valueExpr) = evalValueExpr env valueExpr
getValue env (Var varName) = fromLitOrTree $ lookup' varName env
  where
    fromLitOrTree (EvalValue value) = value
    fromLitOrTree other = typeError "FieldName" other

typeError :: String -> LiteralOrTree -> a
typeError expectedType foundValue =
    error $ "Type error. Expected " ++ expectedType ++ ", but found " ++ show foundValue
