import Prelude
import qualified Spec.Parse
import Orphans ()
import Test.Tasty
import Test.Tasty.SmallCheck  as SC
import Test.Hspec.Runner


main :: IO ()
main = do
    hspecWith defaultConfig { configSmallCheckDepth = scDepth } Spec.Parse.spec
    defaultMain $ localOption (SC.SmallCheckDepth scDepth) Spec.Parse.scProps
  where
   scDepth = 3
