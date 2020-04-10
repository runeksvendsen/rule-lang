module Eval.GroupFold
( getFold
)
where

import           LangPrelude
import           Absyn
import           Types
import           Eval.Result
-- import           Eval.Monad
-- import           Eval.Common                        (getField)

import           Data.List                          (foldl')
import qualified Data.Aeson                         as Json


getFold :: PositionFold -> FieldName -> [Position] -> EvalM Number
getFold positionFold fieldName =
    fmap (fromReal . foldFun) . getFields
  where
    foldFun = foldFunction positionFold
    getFields = fmap catMaybes . mapM getNumber
    getNumber pos = do
        resM <- getField fieldName pos
        case resM of
            Just (Number num) ->
                return (Just num)
            Just fieldValue -> do
                logResult (FieldTypeError positionFold fieldName fieldValue) (nonEmpty pos)
                return Nothing
            Nothing -> return Nothing

foldFunction
    :: (Fractional a, Ord a, Foldable t)
    => PositionFold
    -> t a
    -> a
foldFunction SumOver = sum
foldFunction Average = \numList ->
        let len = length numList
        in if len == 0 then 0 else sum numList / realToFrac len
foldFunction Max = foldl' max 0
foldFunction Min = foldl' min 0
