import Test.Hspec
import qualified Spec.Gram.SerializeSpec as SerializeSpec
import qualified Spec.Gram.ParseSpec as ParseSpec
import qualified Spec.Gram.ParseMinimalRepro as ParseMinimalRepro
import qualified Spec.Gram.ParseRangeRepro as ParseRangeRepro
import qualified Spec.Gram.CorpusSpec as CorpusSpec
import qualified SemanticsSpec

main :: IO ()
main = hspec testSpec

testSpec :: Spec
testSpec = do
  describe "Gram library" $ do
    -- Run ParseSpec first to see if order matters
    ParseSpec.spec
    SerializeSpec.spec
    ParseMinimalRepro.spec
    ParseRangeRepro.spec
    CorpusSpec.spec
    SemanticsSpec.spec

