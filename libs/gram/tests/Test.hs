import Test.Hspec
import qualified Spec.Gram.SerializeSpec as SerializeSpec
import qualified Spec.Gram.ParseSpec as ParseSpec

main :: IO ()
main = hspec testSpec

testSpec :: Spec
testSpec = do
  describe "Gram library" $ do
    SerializeSpec.spec
    ParseSpec.spec

