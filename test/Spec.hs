import Prelude
import qualified Unit.Parse
import qualified Unit.PrintParse
import qualified Unit.Eval
import qualified Spec.Parse
import Orphans ()
import Test.Tasty
import Test.Tasty.SmallCheck  as SC
import Test.Hspec.Runner


main :: IO ()
main = do
    Unit.Eval.main
    Unit.Parse.main
    Unit.PrintParse.main
    hspecWith defaultConfig { configSmallCheckDepth = scDepth }  Spec.Parse.spec
    -- defaultMain $ localOption (SC.SmallCheckDepth scDepth) Spec.Parse.scProps
  where
   scDepth = 2
