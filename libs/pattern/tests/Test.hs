-- | Test runner for Pattern library.
--
-- This module uses HSpec to run all test suites.
import Test.Hspec
import qualified Spec.Pattern.CoreSpec as CoreSpec
import qualified Spec.Pattern.GraphSpec as GraphSpec
import qualified Spec.Pattern.Graph.GraphClassifierSpec as GraphClassifierSpec
import qualified Spec.Pattern.PatternGraphProperties as PatternGraphProperties
import qualified Spec.Pattern.PatternGraphSpec as PatternGraphSpec
import qualified Spec.Pattern.Properties as Properties
import qualified Spec.Pattern.ReconcileSpec as ReconcileSpec
import qualified Spec.Pattern.ReconcileProperties as ReconcileProperties

main :: IO ()
main = hspec testSpec

testSpec :: Spec
testSpec = do
  describe "Pattern library" $ do
    CoreSpec.spec
    GraphSpec.spec
    GraphClassifierSpec.spec
    PatternGraphSpec.spec
    PatternGraphProperties.spec
    Properties.spec
    ReconcileSpec.spec
    ReconcileProperties.spec

