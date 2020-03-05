module Sunburst.Highcharts where

import LangPrelude
import Data.List                (intersperse)
import qualified Tree
import qualified Data.Aeson                         as Json
import qualified Data.Text                        as T
import qualified Data.List.NonEmpty               as NE


convert
    :: (a -> Integer)
    -> (a -> Text)
    -> Tree.Tree [a]
    -> SunburstTree
convert getValue getName tree' =
    go [] 0 tree'
  where
    go parentId selfId tree =
        let selfList = selfId :| parentId
            mkNode fieldVal = Node (NodeInfo parentId selfList (showValue fieldVal))
        in case tree of
            (Tree.Node (_, fieldVal) treeList) ->
                mkNode fieldVal $ map (uncurry $ go (NE.toList selfList)) (zip [0..] treeList)
            Tree.TermNode (_, fieldVal) posList ->
                let mkLeafNode parentId' selfId' pos =
                        Leaf $ LeafNode (NodeInfo parentId' (selfId' :| parentId') (getName pos)) (getValue pos)
                in mkNode fieldVal $ map (uncurry $ mkLeafNode (NE.toList selfList)) (zip [0..] posList)

stringId :: Show a => [a] -> Text
stringId = T.concat . intersperse "." . map (toS . show)

data SunburstTree
    = Node NodeInfo [SunburstTree]
    | Leaf LeafNode

data NodeInfo = NodeInfo
    { parent    :: [Word]
    , id        :: NonEmpty Word
    , name      :: Text
    }

-- |
data LeafNode = LeafNode
    NodeInfo
    Integer     -- Value
