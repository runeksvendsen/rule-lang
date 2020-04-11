module Tree
( Tree(..)
, NodeData(..)
, EvalTree
, accumMap
, termNodes
)
where

import LangPrelude
import Types
import qualified Data.List.NonEmpty as NE


type EvalTree = Tree [Position]

-- data Tree leaf =
--       Node (FieldName, FieldValue) [Tree leaf]
--     | Leaf leaf

data Tree leafLabel =
      Node (NodeData [Tree leafLabel])
    | TermNode (NodeData leafLabel)
        deriving (Eq, Show)

data NodeData a = NodeData (FieldName, FieldValue) a
    deriving (Eq, Show)

-- Collect all term nodes
termNodes :: Tree leafLabel -> [leafLabel]
termNodes tree =
    go [] tree
  where
    go accum (Node (NodeData _ subTree)) = concat $ map (go accum) subTree
    go accum (TermNode (NodeData _ leaf)) = leaf : accum


accumMap
    -- Accumulating function
    -- Applied to the contents of a Node
    :: (state -> Tree a -> (FieldName, FieldValue) -> state)
    -- Mapping function
    -- Applied to the contents of a TermNode
    -> (a -> state -> b)
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
    in TermNode (NodeData label (mkRes a newAccum))

testResult = accumMap
    (\state tree label -> state + sum (termNodes tree))
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
