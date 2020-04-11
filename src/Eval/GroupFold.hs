module Eval.GroupFold
( getFold
)
where

import           LangPrelude
import           Absyn
import           Types


getFold :: PositionFold -> FieldName -> [Position] -> Number
getFold positionFold fieldName =
    fromReal . foldFunction positionFold . map getNumber
  where
    getNumber pos = do
        let resM = lookup fieldName pos
        case resM of
            Just (Number num) -> num
            Just fieldValue -> do
                error $ "Type mismatch. Expected Number found " ++ show fieldValue
            Nothing ->
                error $ "Field " ++ show fieldName ++ " not found for position " ++ show pos

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
