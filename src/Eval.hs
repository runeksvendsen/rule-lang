{-# LANGUAGE BangPatterns #-}
module Eval
( eval
, mkInitialEnv
, Env
, RuntimeValue
  -- * Re-exports
, Position
)
where

import LangPrelude hiding ((&&))
import Absyn
import Types
import Tree
import qualified Comparison
import qualified Data.List
import qualified Data.List.NonEmpty as NE


-- TEMPORARY
infixr 3  &&
(&&) :: Bool -> Bool -> Bool
(&&) True  x  =  x
(&&) False !_ =  False


-- | Variable environment.
-- The first item is the innermost scope.
type Env a = NE.NonEmpty (Text, a)

-- | Create the initial variable environment from
--     a list of the positions in the portfolio.
mkInitialEnv :: [Position] -> Env RuntimeValue
mkInitialEnv portfolioPositions =
    nonEmpty ("Portfolio", portfolioTree)
  where
    portfolioTree = Tree $ TermNode $ NodeData ("Portfolio", "") portfolioPositions

-- The result of evaluating an 'Expr'
data RuntimeValue
    = Constant Literal                  -- Result of evaluating: ValueExpr or BoolExpr
    | Tree (Tree [Position])            -- Result of evaluating: DataExpr
    | MappedTree (Tree [FieldValue])    -- Result of evaluating: Map
        deriving (Eq, Show, Generic)

instance NFData RuntimeValue

-- | Returns 'False' if a rule has been violated,
--    otherwise 'True'
eval :: Env RuntimeValue    -- Evaluated variable environment
     -> [RuleExpr]          -- Rule
     -> Bool                -- Success/failure (True/False)
eval initialEnv ruleExprs' =
    snd $ foldl' go (initialEnv, True) ruleExprs'
  where
    go (env, success) (Let varName varExpr) =
        let newEnv = insert' env varName (evalExpr env varExpr)
        in (newEnv, success)
    go (env, success) (Forall varOrDataExpr ruleExprs) =
        -- Do not overwrite the Portfolio-variable
        let insertNonPortfolio env' tree' (fieldName, _) =
                if fieldName /= "Portfolio"
                    then insert' env' (toS fieldName) (Tree tree')
                    else env'
            -- FIXME: empty nodes can cause evaluation of an expression to fail
            --  because of a missing group becoming an undefined variable.
            -- Solution: remove empty nodes from the tree when filtering.
            envTree = rootToLeafFold insertNonPortfolio env (treeOrCrash env varOrDataExpr)
            boolTree = mapTermNode (\(_, env') -> eval env' ruleExprs) envTree
        in (env, all (== True) (success : termNodes boolTree))
    go (env, success) (If varOrBoolExpr ruleExprs) =
        if groupBoolOrCrash env varOrBoolExpr
            then (env, success && eval env ruleExprs)
            else (env, success)
    go (env, success) (Rule varOrBoolExpr) =
        (env, success && groupBoolOrCrash env varOrBoolExpr)

evalExpr
    :: Env RuntimeValue
    -> Expr
    -> RuntimeValue
evalExpr env varExpr =
    case varExpr of
        Literal literal -> Constant literal
        ValueExpr valueExpr -> Constant $ evalValueExpr env valueExpr
        BoolExpr boolExpr -> Constant $ FieldValue $ Bool $ groupBoolOrCrash env (BoolExpr boolExpr)
        DataExpr dataExpr -> Tree $ evalDataExpr env dataExpr
        Map fieldName dataExpr -> MappedTree $ evalMap env (fieldName, dataExpr)
        Var name -> lookup' name env

evalDataExpr
    :: Env RuntimeValue
    -> DataExpr
    -> Tree [Position]
evalDataExpr env dataExpr =
    case dataExpr of
        GroupBy varOrDataExpr varOrFieldName ->
            let tree = treeOrCrash env varOrDataExpr
                fieldName = fieldNameOrCrash env varOrFieldName
            in addGrouping posLookup fieldName tree
        Filter varOrDataExpr boolExpr ->
            let filterPos env' =
                    case boolOrCrash env' boolExpr of
                        -- Group comparison: either remove all or no positions
                        Left b -> const b
                        -- Position comparison: remove/keep on a per-position basis
                        Right posFun -> posFun
                -- Do not overwrite the Portfolio-variable
                insertNonPortfolio env' tree' (fieldName, _) =
                    if fieldName /= "Portfolio"
                        then insert' env' (toS fieldName) (Tree tree')
                        else env'
                envTree = rootToLeafFold insertNonPortfolio env (treeOrCrash env varOrDataExpr)
            in mapTermNode (\(positions, env') -> filter (filterPos env') positions) envTree

-- Fails on relative comparisons that is not between two 'Number' or two 'Percent'
evalValueExpr :: Env RuntimeValue -> ValueExpr -> Literal
evalValueExpr env valueExpr =
    case valueExpr of
        Relative exprLeft exprRight ->
            let valueLeft = valueOrCrash env exprLeft
                valueRight = valueOrCrash env exprRight
            in Percent $
                let withNumbers (Percent numA, Percent numB) f =
                        f (numA, numB)
                    withNumbers (FieldValue (Number numA), FieldValue (Number numB)) f =
                        f (numA, numB)
                    withNumbers (litA, litB) _ =
                        error $ "Invalid relative comparison: " ++ show (litA, litB)
                in withNumbers (valueLeft, valueRight) $ \(numA, numB) -> (numA * 100) / numB
        GroupCount varOrDataExpr ->
            let tree = treeOrCrash env varOrDataExpr
            in FieldValue . Number . fromIntegral . length . filter (not . null) . termNodes $ tree
        FoldMap fold' mapDataExpr ->
            let mappedTree = mapOrCrash env mapDataExpr
            in FieldValue . Number . toFoldFunction fold' . map numberOrCrash . concat . termNodes $ mappedTree
  where
    numberOrCrash fieldValue = do
        case fieldValue of
            Number num -> num
            other -> typeError "Number" other
    toFoldFunction Sum = sum
    toFoldFunction Avg = uncurry (/) . foldl' (\(sum', count) value -> (sum' + value, count + 1)) (0,0)
    toFoldFunction Max = foldl' max 0
    toFoldFunction Min = foldl' min 0

evalMap
    :: Env RuntimeValue
    -> (Expr, Expr)
    -> Tree [FieldValue]
evalMap env (fieldNameExpr, dataExprExpr) =
    let tree = treeOrCrash env dataExprExpr
        fieldName = fieldNameOrCrash env fieldNameExpr
    in fmap (fmap $ posLookup fieldName) tree

-- Evaluate a 'BoolExpr' to either
--    * a single 'Bool', that applies to the in-scope group combination defined by the given environment
--    * a 'Bool' for each position (Position -> Bool)
evalBoolExpr
    :: Env RuntimeValue
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
                litA = getVarExpr env varOrValueExpr1
                litB = getVarExpr env varOrValueExpr2
            -- Position comparison: FieldName & FieldValue
            -- Group comparison: everything else
            in case (litA, litB) of
                -- Position comparison: FieldName and FieldValue (or vice versa)
                (Constant (FieldName fieldName), Constant (FieldValue fieldValue)) -> Right $
                    \pos -> posLookup fieldName pos `compareFun` fieldValue
                (Constant (FieldValue fieldValue), Constant (FieldName fieldName)) -> Right $
                    \pos -> posLookup fieldName pos `compareFun` fieldValue
                -- Group comparison: two constants
                (Constant cstA, Constant cstB) -> Left $
                    cstA `compareFun` cstB
                _ ->
                    error $ "Invalid comparison: " ++ show (litA, litB)
        And varOrBoolExpr1 varOrBoolExpr2 ->
            binopCompose (&&) (boolOrCrash env varOrBoolExpr1) (boolOrCrash env varOrBoolExpr2)
        Or varOrBoolExpr1 varOrBoolExpr2 ->
            binopCompose (||) (boolOrCrash env varOrBoolExpr1) (boolOrCrash env varOrBoolExpr2)
        Not varOrBoolExpr ->
            unopCompose not (boolOrCrash env varOrBoolExpr)
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


-- ########################
-- ######  Helpers   ######
-- ########################

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
        (lookup (toS fieldName) pos)

-- Either lookup the runtime value for a variable or evaluate an expression.
-- Throws a type error if the looked-up value is of the wrong type.
lookupOrEval
    :: String                                   -- ^ Expected type
    -> (Env RuntimeValue -> a -> b)             -- ^ Evaluation function for 'a'
    -> (RuntimeValue -> Maybe b)                -- ^ Extract a value from a 'RuntimeValue' ('Nothing' means "unexpected type")
    -> (Expr -> Maybe a)                        -- ^ Extract expression from a 'Expr'
    -> Env RuntimeValue                         -- ^ The variable environment
    -> Expr                                     -- ^ Either a variable (pointing to a 'RuntimeValue' in the variable environment)
                                                --     or an expression that needs to be evaluated
    -> b
lookupOrEval expectedType eval' fromRuntimeValue extract env expr =
    case expr of
        Var varName ->
            let runtimeVal = lookup' varName env in
            case fromRuntimeValue runtimeVal of
                Just val -> val
                Nothing -> typeError expectedType runtimeVal
        notVar ->
            case extract notVar of
                Just expr' -> eval' env expr'
                Nothing -> typeError expectedType notVar

treeOrCrash :: Env RuntimeValue -> Expr -> Tree [Position]
treeOrCrash =
    lookupOrEval "Grouping" evalDataExpr fromLitOrTree fromExpr
  where
    fromExpr (DataExpr e) = Just e
    fromExpr _ = Nothing
    fromLitOrTree (Tree tree) = Just tree
    fromLitOrTree _ = Nothing

boolOrCrash :: Env RuntimeValue -> Expr -> Either Bool (Position -> Bool)
boolOrCrash =
    lookupOrEval "Boolean" evalBoolExpr fromLitOrTree fromExpr
  where
    fromExpr (BoolExpr e) = Just e
    fromExpr _ = Nothing
    fromLitOrTree (Constant (FieldValue (Bool bool))) = Just $ Left bool
    fromLitOrTree _ = Nothing

groupBoolOrCrash :: Env RuntimeValue -> Expr -> Bool
groupBoolOrCrash =
    lookupOrEval "Boolean" evalGroupBoolExpr fromLitOrTree fromExpr
  where
    evalGroupBoolExpr env expr =
        case evalBoolExpr env expr of
            Left b -> b
            Right _ -> typeError "group comparison" "position comparison"
    fromExpr (BoolExpr boolExpr) = Just boolExpr
    fromExpr _ = Nothing
    fromLitOrTree (Constant (FieldValue (Bool boolExpr))) = Just boolExpr
    fromLitOrTree _ = Nothing

fieldNameOrCrash :: Env RuntimeValue -> Expr -> FieldName
fieldNameOrCrash =
    lookupOrEval "FieldName" (\_ -> id) fromLitOrTree fromExpr
  where
    fromExpr (Literal (FieldName fn)) = Just fn
    fromExpr _ = Nothing
    fromLitOrTree (Constant (FieldName fieldName)) = Just fieldName
    fromLitOrTree _ = Nothing

valueOrCrash :: Env RuntimeValue -> Expr -> Literal
valueOrCrash =
    lookupOrEval "Value" evalValueExpr fromLitOrTree fromExpr
  where
    fromExpr (ValueExpr e) = Just e
    fromExpr _ = Nothing
    fromLitOrTree (Constant value) = Just value
    fromLitOrTree _ = Nothing

mapOrCrash :: Env RuntimeValue -> Expr -> Tree [FieldValue]
mapOrCrash =
    lookupOrEval "Map" evalMap fromLitOrTree fromExpr
  where
    fromExpr (Map field dataExpr) = Just (field, dataExpr)
    fromExpr _ = Nothing
    fromLitOrTree (MappedTree tree) = Just tree
    fromLitOrTree _ = Nothing

getVarExpr :: Env RuntimeValue -> Expr -> RuntimeValue
getVarExpr =
    lookupOrEval "Expr" evalExpr (Just . id) (Just . id)

typeError :: Show b => String -> b -> a
typeError expectedType foundValue =
    error $ "Type error. Expected " ++ expectedType ++ ", but found " ++ show foundValue
