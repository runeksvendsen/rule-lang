import Prelude
import qualified Unit
import qualified Spec.Parse
import Orphans ()
import Test.Tasty
import Test.Tasty.SmallCheck  as SC
import Test.Hspec.Runner
import qualified Hedgehog


main :: IO ()
main = do
    Unit.test
    return ()
    -- hspecWith defaultConfig { configSmallCheckDepth = scDepth } Spec.Parse.spec
  -- defaultMain Spec.Parse.scProps
    -- defaultMain $ localOption (SC.SmallCheckDepth scDepth) Spec.Parse.scProps
  where
   scDepth = 2
