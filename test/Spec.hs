import Prelude
import qualified Unit.Parse
import qualified Unit.Eval
import qualified Spec.Parse
import Orphans ()
import Test.Tasty
import Test.Tasty.SmallCheck  as SC


main :: IO ()
main = do
    Unit.Eval.main
    Unit.Parse.main
    defaultMain $ localOption (SC.SmallCheckDepth scDepth) Spec.Parse.scProps
  where
   scDepth = 2
