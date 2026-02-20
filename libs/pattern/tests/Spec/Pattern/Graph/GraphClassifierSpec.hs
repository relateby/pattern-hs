module Spec.Pattern.Graph.GraphClassifierSpec (spec) where

import Test.Hspec
import Pattern.Core (Pattern(..), pattern, point)
import Pattern.Graph.GraphClassifier

spec :: Spec
spec = do
  describe "Pattern.Graph.GraphClassifier" $ do
    
    describe "classifyByShape" $ do
      it "classifies an atomic pattern as GNode" $ do
        let n = point "a"
        classifyByShape n `shouldBe` GNode

      it "classifies a pattern with one element as GAnnotation" $ do
        let n1 = point "b"
        let anno = pattern "a" [n1]
        classifyByShape anno `shouldBe` GAnnotation

      it "classifies a pattern with two node elements as GRelationship" $ do
        let n1 = point "a"
        let n2 = point "b"
        let rel = pattern "r" [n1, n2]
        classifyByShape rel `shouldBe` GRelationship

      it "classifies a sequence of properly chaining relationships as GWalk" $ do
        let nA = point "A"
        let nB = point "B"
        let nC = point "C"
        let nD = point "D"
        
        let rel1 = pattern "r1" [nA, nB]
        let rel2 = pattern "r2" [nB, nC]
        let rel3 = pattern "r3" [nD, nC] -- testing undirected chaining
        
        let w = pattern "w" [rel1, rel2, rel3]
        classifyByShape w `shouldBe` GWalk

      it "rejects star patterns (sharing center but not chaining end-to-end) and falls back to GOther" $ do
        let nA = point "A"
        let nB = point "B"
        let nC = point "C"
        let nD = point "D"
        
        -- All share node A, but they don't form a contiguous walk end-to-end
        let rel1 = pattern "r1" [nA, nB]
        let rel2 = pattern "r2" [nA, nC]
        let rel3 = pattern "r3" [nA, nD]
        
        let star = pattern "star" [rel1, rel2, rel3]
        classifyByShape star `shouldBe` GOther ()

      it "rejects relationships containing non-node elements and falls back to GOther" $ do
        let n1 = point "a"
        let notNode = pattern "b" [point "c"]
        let badRel = pattern "r" [n1, notNode]
        classifyByShape badRel `shouldBe` GOther ()

      it "rejects walks containing non-relationships and falls back to GOther" $ do
        let n1 = point "a"
        let n2 = point "b"
        let rel = pattern "r" [n1, n2]
        let badWalk = pattern "w" [rel, n1]
        classifyByShape badWalk `shouldBe` GOther ()

      it "classifies canonicalClassifier just like classifyByShape" $ do
        let n = point "a"
        classify canonicalClassifier n `shouldBe` GNode
