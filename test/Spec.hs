import Prelude
import qualified Unit
import qualified Spec.Parse
import Orphans ()
import Test.Tasty
import Test.Tasty.SmallCheck  as SC
import Test.Hspec.Runner


main :: IO ()
main = do
    Unit.test
    defaultMain $ localOption (SC.SmallCheckDepth scDepth) Spec.Parse.scProps
  where
   scDepth = 2
