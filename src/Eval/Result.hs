{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval.Result
( Result
, addPassed
, addFailed
, addIgnored
, addDataMiss
, joinResults
  -- * Re-exports
, Position
)
where

import LangPrelude
import Eval.Types


data Result a = Result
    { rPassed   :: [a]  -- ^ A rule was applied and it passed
    , rFailed   :: [a]  -- ^ A rule was applied and it failed
    , rIgnored  :: [a]  -- ^ No rule was applied (filtered off)
    , rDataMiss :: [(Text, [a])]  -- ^ The specified field is missing ("DataException")
    }

instance Monoid (Result a) where
    mempty = Result [] [] [] []

instance Semigroup (Result a) where
    Result a1 b1 c1 d1 <> Result a2 b2 c2 d2 =
        Result (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

addPassed :: Result a -> [a] -> Result a
addPassed res a = res { rPassed = a ++ rPassed res }

addFailed :: Result a -> [a] -> Result a
addFailed res a = res { rFailed = a ++ rFailed res }

addIgnored :: Result a -> [a] -> Result a
addIgnored res a = res { rIgnored = a ++ rIgnored res }

addDataMiss :: Result a -> (Text, [a]) -> Result a
addDataMiss res a = res { rDataMiss = a : rDataMiss res }

joinResults :: Foldable f => f (Result a) -> Result a
joinResults = foldr (<>) mempty
