module Spec.Pattern.Graph.GraphClassifierSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Pattern.Core (Pattern(..), pattern)
import Pattern.Graph.GraphClassifier (GraphClass(..), GraphClassifier(..), classifyByShape, canonicalClassifier)
import Pattern.PatternGraph ()  -- bring GraphValue Subject instance into scope
import Subject.Core (Subject(..), Symbol(..))

-- Helpers: Pattern Subject (GraphValue instance from Pattern.PatternGraph)
node :: Symbol -> Pattern Subject
node s = Pattern (Subject s Set.empty Map.empty) []

pat :: String -> [Pattern Subject] -> Pattern Subject
pat s els = Pattern (Subject (Symbol s) Set.empty Map.empty) els

spec :: Spec
spec = do
  describe "Pattern.Graph.GraphClassifier" $ do

    describe "classifyByShape" $ do
      it "classifies an atomic pattern as GNode" $ do
        let n = node (Symbol "a")
        classifyByShape n `shouldBe` GNode

      it "classifies a pattern with one element as GAnnotation" $ do
        let n1 = node (Symbol "b")
        let anno = pat "a" [n1]
        classifyByShape anno `shouldBe` GAnnotation

      it "classifies a pattern with two node elements as GRelationship" $ do
        let n1 = node (Symbol "a")
        let n2 = node (Symbol "b")
        let rel = pat "r" [n1, n2]
        classifyByShape rel `shouldBe` GRelationship

      it "classifies a sequence of properly chaining relationships as GWalk" $ do
        let nA = node (Symbol "A")
        let nB = node (Symbol "B")
        let nC = node (Symbol "C")
        let nD = node (Symbol "D")

        let rel1 = pat "r1" [nA, nB]
        let rel2 = pat "r2" [nB, nC]
        let rel3 = pat "r3" [nD, nC]  -- testing undirected chaining

        let w = pat "w" [rel1, rel2, rel3]
        classifyByShape w `shouldBe` GWalk

      it "rejects star patterns (sharing center but not chaining end-to-end) and falls back to GOther" $ do
        let nA = node (Symbol "A")
        let nB = node (Symbol "B")
        let nC = node (Symbol "C")
        let nD = node (Symbol "D")

        -- All share node A, but they don't form a contiguous walk end-to-end
        let rel1 = pat "r1" [nA, nB]
        let rel2 = pat "r2" [nA, nC]
        let rel3 = pat "r3" [nA, nD]

        let star = pat "star" [rel1, rel2, rel3]
        classifyByShape star `shouldBe` GOther ()

      it "rejects relationships containing non-node elements and falls back to GOther" $ do
        let n1 = node (Symbol "a")
        let notNode = pat "b" [node (Symbol "c")]
        let badRel = pat "r" [n1, notNode]
        classifyByShape badRel `shouldBe` GOther ()

      it "rejects walks containing non-relationships and falls back to GOther" $ do
        let n1 = node (Symbol "a")
        let n2 = node (Symbol "b")
        let rel = pat "r" [n1, n2]
        let badWalk = pat "w" [rel, n1]
        classifyByShape badWalk `shouldBe` GOther ()

      it "classifies canonicalClassifier just like classifyByShape" $ do
        let n = node (Symbol "a")
        classify canonicalClassifier n `shouldBe` GNode
