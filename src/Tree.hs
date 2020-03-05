-- {-# LANGUAGE ScopedTypeVariables #-}
module Tree
( Tree(..)
, mapTermNodeM
, forTermNodeM
, collectTermNodes
, collectTermNodeLabels
, collectTermNodeTrees
, drawTree
-- * Re-exports
, Data.Tree.unfoldTreeM
)
where

import LangPrelude
-- import Absyn
import Eval.Types
-- import Eval.Result

import qualified Data.Tree

data Tree leafLabel =
      Node (FieldName, FieldValue) [Tree leafLabel]
    | TermNode (FieldName, FieldValue) leafLabel

instance Functor Tree where
    fmap f (Node lab treeList) = Node lab (map (fmap f) treeList)
    fmap f (TermNode lab leaf) = TermNode lab (f leaf)

-- data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Foldable Tree where
   foldMap f (TermNode lab leaf) = f leaf
   foldMap f (Node lab nodeList) =
        foldr (\item result -> foldMap f item `mappend` result) mempty nodeList

forTermNodeM
    :: Monad m
    => Tree a
    -> (Tree a -> m b)
    -> m (Tree b)
forTermNodeM = flip mapTermNodeM

mapTermNodeM
    :: Monad m
    => (Tree a -> m b)  -- ^ A tree from the root node and to a single 'TermNode'
    -> Tree a
    -> m (Tree b)
mapTermNodeM f tree =
    go [] tree
  where
    go pathAccum (Node lab subTree) =
        Node lab <$> mapM (go (lab : pathAccum)) subTree
    go pathAccum (TermNode lab leaf) =
        let treePath = foldr (\nodeLab tree -> Node nodeLab [tree]) (TermNode lab leaf) pathAccum
        in TermNode lab <$> f treePath

collectTermNodeTrees
    :: Tree leafLabel
    -> [ Tree leafLabel ]
collectTermNodeTrees = map (uncurry TermNode) . collectTermNodes

collectTermNodeLabels :: Tree a -> [a]
collectTermNodeLabels = map snd . collectTermNodes

collectTermNodes
    :: Tree leafLabel
    -> [ ((FieldName, FieldValue), leafLabel) ]
collectTermNodes tree =
    go tree
  where
    go (Node _ subTree) =
        concat $ map go subTree
    go (TermNode lab posList) =
        [(lab, posList)]

fromTuple :: (FieldName, FieldValue) -> Level
fromTuple = uncurry Level

drawTree :: Show leafLabel => Tree [leafLabel] -> String
drawTree = Data.Tree.drawTree . fmap show . toContainers

-- Used for converting to/from 'Data.Tree.Tree'
data NodeLabel leafLabel
    = NodeLab (FieldName, FieldValue)
    | TermNodeLab (FieldName, FieldValue) leafLabel
        deriving Show

toContainers
    :: Tree [leafLabel]
    -> Data.Tree.Tree (NodeLabel [leafLabel])
toContainers (Node nodeLabel subTree) =
    Data.Tree.Node (NodeLab nodeLabel) (map toContainers subTree)
toContainers (TermNode nodeLabel leafList) =
    Data.Tree.Node (TermNodeLab nodeLabel leafList) []
