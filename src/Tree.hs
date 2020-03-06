module Tree
( Tree(..)
, EvalTree
, mapMTermNode
, collectTermNodeTrees
, drawTree
-- * Re-exports
, Data.Tree.unfoldTreeM
)
where

import LangPrelude
-- import AbsynFun
import Eval.Types
-- import Eval.Result

import qualified Data.Tree
import qualified Data.List.NonEmpty               as NE


data Tree leafLabel =
      Node (FieldName, FieldValue) [Tree leafLabel]
    | TermNode (FieldName, FieldValue) [leafLabel]

type EvalTree = Tree Position

mapMTermNode'
    :: Monad m
    => (ScopeData -> m [Position])
    -> Tree Position
    -> m (Tree Position)
mapMTermNode' f tree =
    go [] tree
  where
    mkLevelPos lab leafs = LevelPos (fromTuple lab) (NE.fromList leafs)
    go scopeData node@(Node lab subTree) = do
        let descendandLeafs = concat $ map collectLeafs subTree
        if null descendandLeafs
            then return node  -- There are no leaves below this Node
            else Node lab <$> mapM (go $ mkLevelPos lab descendandLeafs : scopeData) subTree
    go scopeData node@(TermNode lab posList) = do
        if null posList
            then return node  -- There are no leaves below this Node
            else TermNode lab <$> f (NE.fromList $ mkLevelPos lab posList : scopeData)

mapMTermNode
    :: Monad m
    => (NonEmpty Position -> m [Position])
    -> Tree Position
    -> m (Tree Position)
mapMTermNode f tree =
    go tree
  where
    go (Node lab subTree) =
        Node lab <$> mapM go subTree
    go node@(TermNode _ []) = return node
    go (TermNode lab posList) =
        TermNode lab <$> f (NE.fromList posList)


collectTermNodeTrees
    :: Tree leafLabel
    -> [ Tree leafLabel ]
collectTermNodeTrees = map (uncurry TermNode) . collectTermNodes

collectTermNodes
    :: Tree leafLabel
    -> [ ((FieldName, FieldValue), [leafLabel]) ]
collectTermNodes tree =
    go tree
  where
    go (Node _ subTree) =
        concat $ map go subTree
    go (TermNode lab posList) =
        [(lab, posList)]

collectLeafs
    :: Tree leafLabel
    -> [leafLabel]
collectLeafs tree =
    go tree
  where
    go (Node _ subTree) =
        concat $ map go subTree
    go (TermNode _ posList) =
        posList

fromTuple :: (FieldName, FieldValue) -> Level
fromTuple = uncurry Level

drawTree :: Show leafLabel => Tree leafLabel -> String
drawTree = Data.Tree.drawTree . fmap show . toContainers

data NodeLabel leafLabel
    = NodeLab (FieldName, FieldValue)
    | TermNodeLab (FieldName, FieldValue) [leafLabel]
        deriving Show

toContainers
    :: Tree leafLabel
    -> Data.Tree.Tree (NodeLabel leafLabel)
toContainers (Node nodeLabel subTree) = undefined
    Data.Tree.Node (NodeLab nodeLabel) (map toContainers subTree)
toContainers (TermNode nodeLabel leafList) =
    Data.Tree.Node (TermNodeLab nodeLabel leafList) []

fromContainers
    :: Show leafLabel
    => Data.Tree.Tree (NodeLabel leafLabel)
    -> Tree leafLabel
fromContainers (Data.Tree.Node (NodeLab nodeLabel) subTree) = undefined
    Node nodeLabel (map fromContainers subTree)
fromContainers (Data.Tree.Node (TermNodeLab nodeLabel leafList) []) =
    TermNode nodeLabel leafList
fromContainers treeNode@(Data.Tree.Node (TermNodeLab _ _) _) =
    error $ "Invalid tree node: " ++ show treeNode
