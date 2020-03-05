module Sunburst.D3 where


import LangPrelude
import Eval.Types (Position)
import qualified Tree

import qualified Data.Aeson                         as Json
import qualified Data.Text                        as T


convert :: (Position -> Double) -> Tree.Tree [Position] -> SunburstTree
convert f (Tree.Node (name, val) treeList) =
    Node $ SunburstNode (showValue val) (map (convert f) treeList)
convert f (Tree.TermNode (name, val) posList) =
    Leaf $ SunburstLeaf (showValue val) (sum $ map f posList)

data SunburstTree =
      Node SunburstNode
    | Leaf SunburstLeaf
        deriving Generic

data SunburstNode =
    SunburstNode
        { nodeName      :: Text
        , nodeChildren  :: [SunburstTree]
        } deriving Generic

data SunburstLeaf =
    SunburstLeaf
        { leafName  :: Text
        , leafValue :: Double
        } deriving Generic


recordToField
    :: String
    -> String
recordToField = toS . T.toLower . T.drop 4 . toS

instance Json.ToJSON SunburstLeaf where
    toJSON =
        let customOptions = Json.defaultOptions
                { Json.fieldLabelModifier = recordToField }
        in  Json.genericToJSON customOptions

instance Json.ToJSON SunburstNode where
    toJSON =
        let customOptions = Json.defaultOptions
                { Json.fieldLabelModifier = recordToField }
        in  Json.genericToJSON customOptions

instance Json.ToJSON SunburstTree where
    toJSON =
        let customOptions = Json.defaultOptions
                { Json.sumEncoding = Json.UntaggedValue }
        in  Json.genericToJSON customOptions
