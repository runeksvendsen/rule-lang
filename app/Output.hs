{-# OPTIONS_GHC -fno-warn-orphans #-}
module Output where

import LangPrelude
import Eval.Types
import Eval.Result

import           Data.List                        (intersperse)
import qualified Data.HashMap.Strict              as M
import           Data.Hashable                    (Hashable)
import qualified Data.Aeson                       as Json
import qualified Data.List.NonEmpty               as NE
import qualified Data.Text                        as T


instance Hashable ResultStatus
instance Json.ToJSON ResultStatus where
    toJSON = Json.String . prettyShow

prettyShow :: ResultStatus -> Text
prettyShow (MissingField fieldName) = "MissingField: " <> fieldName
prettyShow a = toS $ show a

toObjectSecId
    :: NonEmpty Result
    -> Map Text (Map Text (NonEmpty Json.Value))
toObjectSecId =
    fmap (fmap (NE.map getSecurityIdOrFail)) . toObject

getSecurityIdOrFail :: Position -> Json.Value
getSecurityIdOrFail pos =
    fromMaybe (notFoundError pos) $ M.lookup "SecurityID" pos
  where
    notFoundError pos =
        error $ "ERROR: 'SecurityID' key not found for: " ++ show pos

toObject
    :: NonEmpty Result
    -> Map Text (Map Text (NonEmpty Position))
toObject =
    fmap (fmap (neConcat . NE.map rPosition)) . mapKey prettyShow . toMap
  where
    neConcat :: NonEmpty (NonEmpty a) -> NonEmpty a
    neConcat = NE.fromList . concat . fmap NE.toList

toMap
    :: NonEmpty Result
    -> Map ResultStatus (Map Text (NonEmpty Result))
toMap =
    fmap (mapByGroup (showLevels . rScope)) . mapByGroup rStatus
  where
    showLevels = T.concat . intersperse " > " . ("Portfolio" :) . map showLevel . reverse . NE.init
    showLevel (Level name val) = name <> "=" <> showValue val

mapByGroup
    :: (Ord k, Hashable k)
    => (a -> k)
    -> NonEmpty a
    -> Map k (NonEmpty a)
mapByGroup f =
    mapFromList . groupAllWith' f
  where
    mapFromList = M.fromList . NE.toList

groupAllWith' :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty (b, NonEmpty a)
groupAllWith' f = NE.map (\ne -> (f $ NE.head ne, ne)) . NE.fromList . NE.groupAllWith f . NE.toList

mapKey :: (Eq k2, Hashable k2) => (k1 -> k2) -> Map k1 v -> Map k2 v
mapKey f =
    M.fromList . map applyFst . M.toList
  where
    applyFst (k, v) = (f k, v)
