module Eval.Bool where

import LangPrelude
import Absyn
import Tree
import Eval.Types
-- TREE
import qualified Data.List.NonEmpty as NE


eval :: Env VarExpr         -- Initial variable environment
     -> NonEmpty RuleExpr   -- Rule expression
     -> Bool  -- Success/failure (True/False)
eval initialEnv ruleExprs' =
    snd $ foldl' go (initialEnv, True) ruleExprs'
  where
    go (env, result) (Let varName varExpr) =
        let newEnv = insert env varName varExpr
        in (newEnv, result)
    go (env, result) (Foreach varOrDataExpr ruleExprs) =
        let dataExpr = either error id (fromVarOr DataExpr env varOrDataExpr)
        -- insert into varEnv for each
        in (env, result && eval env ruleExprs)
    go (env, result) (If varOrBoolExpr ruleExprs) =
        undefined
    go (env, result) (Rule varOrBoolExpr) =
        undefined

fromVarOr :: (a -> VarExpr) -> Env VarExpr -> VarOr a -> Either String VarExpr
fromVarOr _ env (Var varName) =
    maybe (Left $ "Variable '" ++ toS varName ++ "' not found")
          Right
          (lookup varName env)
fromVarOr f _ (NotVar notVar) =
    Right (f notVar)


accumMap
    -- Accumulating function
    -- Applied to the contents of a Node
    :: (state -> Tree a -> (FieldName, FieldValue) -> state)
    -- Mapping function
    -- Applied to the contents of a TermNode
    -> (state -> a -> b)
    -- Initial state
    -> state
    -- Input tree
    -> Tree a
    -- Output tree
    -> Tree b
accumMap f mkRes accum tree@(Node (NodeData label subTree)) =
    let newAccum = f accum tree label
        newSubTree = map (accumMap f mkRes newAccum) subTree
    in Node (NodeData label newSubTree)
accumMap f mkRes accum tree@(TermNode (NodeData label a)) =
    let newAccum = f accum tree label
    in TermNode (NodeData label (mkRes newAccum a))

testResult = accumMap
    (\state tree label -> state + sum (leaves tree))
    (,)
    0

em = ("", "")

testTree =
    Node $ NodeData em                          -- sum: 3+5+9+17+13 = 47
        [ Node $ NodeData em                    -- sum: 3+5         = 8
            [ TermNode $ NodeData em 3
            , TermNode $ NodeData em 5
            ]
        , Node $ NodeData em                    -- sum: 9+17        = 26
            [ TermNode $ NodeData em 9
            , TermNode $ NodeData em 17
            ]
        , Node $ NodeData em                    -- sum: 13          = 13
            [ TermNode $ NodeData em 13
            ]
        ]

lol =
    Node $ NodeData em                          -- sum: 3+5+9+17+13 = 47
        [ Node $ NodeData em                    -- sum: 3+5         = 8
            [ TermNode (NodeData em (58,3))
            , TermNode (NodeData em (60,5))
            ]
        , Node $ NodeData em                    -- sum: 9+17        = 26
            [ TermNode (NodeData em (82,9))
            , TermNode (NodeData em (90,17))
            ]
        , Node $ NodeData em                    -- sum: 13          = 13
            [ TermNode (NodeData em (73,13))
            ]
        ]
