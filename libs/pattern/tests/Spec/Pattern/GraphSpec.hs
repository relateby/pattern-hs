-- | Unit tests for Pattern.Graph module.
module Spec.Pattern.GraphSpec where

import Data.List (isPrefixOf)
import Test.Hspec
import Pattern.Core (Pattern(..), pattern, patternWith)
import Pattern.Graph (GraphLens(..), nodes, isNodeLens)

spec :: Spec
spec = do
  describe "Pattern.Graph" $ do
    
    describe "GraphLens data structure (Phase 2: Foundational)" $ do
      
      it "T009: GraphLens construction with atomic predicate" $ do
        let graphPattern = patternWith "graph"
              [ pattern "a"
              , pattern "b"
              , pattern "c"
              ]
        let isAtomic (Pattern _ els) = null els
        let lens = GraphLens graphPattern isAtomic
        scopePattern lens `shouldBe` graphPattern
        isNodeLens lens (pattern "a") `shouldBe` True
        isNodeLens lens (pattern "b") `shouldBe` True
      
      it "T010: GraphLens with empty scopePattern" $ do
        let emptyPattern = patternWith "empty" []
        let isAtomic (Pattern _ els) = null els
        let lens = GraphLens emptyPattern isAtomic
        nodes lens `shouldBe` []
    
    describe "nodes function (Phase 2: Foundational)" $ do
      
      it "T013: nodes with atomic predicate" $ do
        let graphPattern = patternWith "graph"
              [ pattern "a"
              , pattern "b"
              , patternWith "rel" [pattern "a", pattern "b"]
              ]
        let isAtomic (Pattern _ els) = null els
        let lens = GraphLens graphPattern isAtomic
        let result = nodes lens
        length result `shouldBe` 2
        (pattern "a" `elem` result) `shouldBe` True
        (pattern "b" `elem` result) `shouldBe` True
      
      it "T014: nodes with value-based predicate" $ do
        let graphPattern = patternWith "graph"
              [ pattern "node1"
              , pattern "node2"
              , pattern "notnode"
              ]
        let isNodeValue (Pattern v _) = "node" `isPrefixOf` v
        let lens = GraphLens graphPattern isNodeValue
        let result = nodes lens
        length result `shouldBe` 2
        (pattern "node1" `elem` result) `shouldBe` True
        (pattern "node2" `elem` result) `shouldBe` True
      
      it "T016: nodes with empty Pattern scope" $ do
        let emptyPattern = patternWith "empty" []
        let isAtomic (Pattern _ els) = null els
        let lens = GraphLens emptyPattern isAtomic
        nodes lens `shouldBe` []
      
      it "T017: nodes with Pattern with no nodes (all fail predicate)" $ do
        let graphPattern = patternWith "graph"
              [ patternWith "rel1" [pattern "a", pattern "b"]
              , patternWith "rel2" [pattern "b", pattern "c"]
              ]
        let isAtomic (Pattern _ els) = null els
        let lens = GraphLens graphPattern isAtomic
        nodes lens `shouldBe` []

