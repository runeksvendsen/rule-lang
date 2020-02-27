module Analyze.Check
( checkData
)
where

import LangPrelude
import Absyn


checkData :: RuleExpr -> [Text]
checkData expr =
    let aux errors groupEnv varEnv expr =
            case expr of
                And a b ->
                    let eval = aux [] groupEnv varEnv
                    in eval a <> eval b <> errors
                Let name rhs scope ->
                    aux errors groupEnv (insert varEnv name rhs) scope
                Var var ->
                    let varNotFound = "Variable '" <> var <> "' doesn't exist"
                    in maybe (varNotFound : errors) (aux errors groupEnv varEnv) (lookup var varEnv)
                GroupBy field scope -> aux errors (field : groupEnv) varEnv scope
                Filter comparison expr ->
                    let newErrors = checkComparison groupEnv comparison <> errors
                    in aux newErrors groupEnv varEnv expr
                Rule comparison -> checkComparison groupEnv comparison <> errors
    in aux [] ["Portfolio"] emptyMap expr

checkComparison
    :: [GroupName]
    -> Comparison
    -> [GroupName]
checkComparison groupEnv (Comparison valueExpr _ _) =
    let groupNotFound name = "Grouping '" <> name <> "' doesn't exist"
        groupExists name = name `elem` groupEnv
        groupError name = if groupExists name then Nothing else Just (groupNotFound name)
    in case valueExpr of
        PosValueExpr (Get _) -> []
        GroupValueExpr (CountDistinct _) -> []
        GroupValueExpr (SumOver _ groupNameOpt) ->
            catMaybes [groupNameOpt >>= groupError]
