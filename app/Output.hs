{-# OPTIONS_GHC -fno-warn-orphans #-}
module Output where

import LangPrelude
import Eval.Types
import Eval.Result

import qualified Data.HashMap.Strict              as M
import           Data.Hashable                    (Hashable)
import qualified Data.Aeson                       as Json
import qualified Data.List.NonEmpty               as NE


instance Hashable ResultStatus
instance Json.ToJSON ResultStatus where
    toJSON (MissingField fieldName) = Json.String $ "MissingField: " <> fieldName
    toJSON a = Json.String . toS $ show a


toObjectSecId :: [Result] -> Map Text (NonEmpty Level, NonEmpty Json.Value)
toObjectSecId =
    M.map (fmap (NE.map getSecurityIdOrFail)) . toObject
  where
    notFoundError pos =
        error $ "ERROR: 'SecurityID' key not found for: " ++ show pos
    getSecurityIdOrFail pos =
        fromMaybe (notFoundError pos) $ M.lookup "SecurityID" pos

toObject :: [Result] -> Map Text (NonEmpty Level, NonEmpty Position)
toObject = mapKey (toS . show) . toMap

toMap :: [Result] -> Map ResultStatus (NonEmpty Level, NonEmpty Position)
toMap =
    foldr insertPositions M.empty
  where
    insertPositions (Result pos lvl status) =
        M.insertWith (<>) status (lvl, pos)

mapKey :: (Eq k2, Hashable k2) => (k1 -> k2) -> Map k1 v -> Map k2 v
mapKey f =
    M.fromList . map applyFst . M.toList
  where
    applyFst (k, v) = (f k, v)
