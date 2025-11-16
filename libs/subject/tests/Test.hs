-- | Test runner for Subject library.
--
-- This module uses HSpec to run all test suites.
import Test.Hspec
import qualified Spec.Subject.CoreSpec as CoreSpec
import qualified Spec.Subject.ValueSpec as ValueSpec

main :: IO ()
main = hspec testSpec

testSpec :: Spec
testSpec = do
  describe "Subject library" $ do
    CoreSpec.spec
    ValueSpec.spec

