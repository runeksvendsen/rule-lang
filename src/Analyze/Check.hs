module Analyze.Check
( checkData
)
where

import LangPrelude
import AbsynFun


checkData :: RuleExpr -> [Text]
checkData expr' = undefined
    -- let aux errors groupEnv varEnv expr =
    --         case expr of
    --             And a b ->
    --                 let eval = aux [] groupEnv varEnv
    --                 in eval a <> eval b <> errors
    --             Let name rhs scope ->
    --                 let (newErrors, _) = foldr checkDataExpr (errors, groupEnv) rhs
    --                 in aux newErrors groupEnv (insert varEnv name rhs) scope

    --             Foreach var expr ->
    --                 let varNotFound = "Variable '" <> var <> "' doesn't exist"
    --                     rhs = maybe (varNotFound : errors) () (lookup var varEnv)
    --                     (_, newGroupEnv) = foldr checkDataExpr (errors, groupEnv) rhs
    --                 in aux errors newGroupEnv varEnv

    --             Rule comparison -> checkComparison groupEnv comparison <> errors
    -- in aux [] ["Portfolio"] emptyMap expr'

-- checkDataExpr
--     :: DataExpr
--     -> ([FieldName], [FieldName])
--     -> ([FieldName], [FieldName])
-- checkDataExpr dataExpr (errors, groupEnv) =
--     case dataExpr of
--         GroupBy field -> (errors, field : groupEnv)
--         Filter comparison ->
--             let newErrors = checkComparison groupEnv comparison <> errors
--             in (newErrors, groupEnv)

-- checkComparison
--     :: [FieldName]
--     -> Comparison
--     -> [FieldName]
-- checkComparison groupEnv comparison =
--     let groupNotFound name = "Grouping '" <> name <> "' doesn't exist"
--         groupExists name = name `elem` groupEnv
--         groupError name = if groupExists name then Nothing else Just (groupNotFound name)
--     in case comparison of
--         (GroupComparison (SumOver _ groupNameOpt) _ _) ->
--             catMaybes [groupNameOpt >>= groupError]
--         _ -> []

