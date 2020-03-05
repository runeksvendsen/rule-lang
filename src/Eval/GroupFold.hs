module Eval.GroupFold
( getFold
)
where

import           LangPrelude
import           Absyn
import           Eval.Result
import           Eval.Monad
import           Eval.Common (getField)
import           Eval.Types

import           Data.List                          (sort, group)
import qualified Data.Aeson                         as Json


getFold :: GroupFold -> FieldName -> [Position] -> EvalM GroupValue
getFold groupFold fieldName = case groupFold of
    CountDistinct ->
        fmap (Count . fromIntegral . length . group . sort) . getFields
    SumOver ->
        fmap (Sum . realToFrac . sum) . getFields
    Average -> undefined
    Max -> undefined
    Min -> undefined
  where
    getFields = fmap catMaybes . mapM getNumber
    getNumber pos = do
        resM <- getField fieldName pos
        case resM of
            Just (Json.Number num) ->
                return (Just num)
            Just fieldValue -> do
                logResult (FieldTypeError groupFold fieldName fieldValue) (nonEmpty pos)
                return Nothing
            Nothing -> return Nothing
