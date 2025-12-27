-- | Test runner for gram-hs-cli.
--
-- This module uses HSpec to run all test suites.
import Test.Hspec
import qualified Spec.CLI.OutputSpec as OutputSpec
import qualified Spec.CLI.JSONSpec as JSONSpec
import qualified Spec.CLI.GenerateSuiteSpec as GenerateSuiteSpec
import qualified Spec.Properties.DeterministicSpec as DeterministicSpec

main :: IO ()
main = hspec testSpec

testSpec :: Spec
testSpec = do
  describe "gram-hs-cli" $ do
    OutputSpec.spec
    JSONSpec.spec
    GenerateSuiteSpec.spec
    DeterministicSpec.spec

