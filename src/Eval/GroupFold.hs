module Eval.GroupFold
( getFold
)
where

import           LangPrelude
import           AbsynFun
import           Eval.Common (getField)
import           Eval.Types
import qualified Eval.DataExpr

import           Data.List                          (sort, group)
import qualified Data.Aeson                         as Json


getFold :: GroupFold -> [Position] -> GroupValue
getFold (CountDistinct fieldName) =
    Count . fromIntegral . length . group . sort . map (getField fieldName)
getFold (SumOver fieldName) =
    Sum . realToFrac . sum . map getNumber . map (getField fieldName)
  where
    getNumber (Json.Number num) = num
    getNumber other = error $ "SumOver non-number: " ++ show other

