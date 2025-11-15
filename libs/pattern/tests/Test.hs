-- | Test runner for Pattern library.
--
-- This module uses HSpec to run all test suites.
import Test.Hspec
import qualified Spec.Pattern.CoreSpec as CoreSpec
import qualified Spec.Pattern.GraphSpec as GraphSpec
import qualified Spec.Pattern.ViewsSpec as ViewsSpec
import qualified Spec.Pattern.Properties as Properties

main :: IO ()
main = hspec testSpec

testSpec :: Spec
testSpec = do
  describe "Pattern library" $ do
    CoreSpec.spec
    GraphSpec.spec
    ViewsSpec.spec
    Properties.spec

