{-# OPTIONS_GHC -fno-warn-orphans #-}
module Output where

import LangPrelude
import Eval.Types
import Eval.Result
import qualified Data.HashMap.Strict                as Map
import           Data.Hashable                      (Hashable)
import qualified Data.Aeson                         as Json
import qualified Data.List.NonEmpty                 as NE


instance Hashable ResultStatus
instance Json.ToJSON ResultStatus where
    toJSON (MissingField fieldName) = Json.String $ "MissingField: " <> fieldName
    toJSON a = Json.String . toS $ show a

toObjectSecId :: [Result] -> HashMap Text ([Level], NonEmpty Json.Value)
toObjectSecId =
    Map.map (fmap (NE.map getSecurityIdOrFail)) . toObject
  where
    notFoundError pos =
        error $ "ERROR: 'SecurityID' key not found for: " ++ show pos
    getSecurityIdOrFail pos =
        fromMaybe (notFoundError pos) $ Map.lookup "SecurityID" pos

toObject :: [Result] -> HashMap Text ([Level], NonEmpty Position)
toObject = mapKey (toS . show) . toMap

toMap :: [Result] -> HashMap ResultStatus ([Level], NonEmpty Position)
toMap =
    foldr insertPositions Map.empty
  where
    insertPositions (Result pos lvl status) =
        Map.insertWith (<>) status (lvl, pos)

mapKey :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
mapKey f =
    Map.fromList . map applyFst . Map.toList
  where
    applyFst (k, v) = (f k, v)
