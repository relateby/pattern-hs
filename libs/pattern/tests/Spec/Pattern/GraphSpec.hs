-- | Unit tests for Pattern.Graph module.
module Spec.Pattern.GraphSpec where

import Data.List (isPrefixOf)
import Test.Hspec
import Pattern.Core (Pattern(..), pattern, point)
import Pattern.Graph (GraphLens(..), mkGraphLens, nodes, isNode)

spec :: Spec
spec = do
  describe "Pattern.Graph" $ do
    
    describe "GraphLens data structure (Phase 2: Foundational)" $ do
      
      it "T009: GraphLens construction with atomic predicate" $ do
        let graphPattern = pattern "graph"
              [ point "a"
              , point "b"
              , point "c"
              ]
        let isAtomic (Pattern _ els) = null els
        let lens = mkGraphLens graphPattern isAtomic
        scopePattern lens `shouldBe` graphPattern
        isNode lens (point "a") `shouldBe` True
        isNode lens (point "b") `shouldBe` True
      
      it "T010: GraphLens with empty scopePattern" $ do
        let emptyPattern = pattern "empty" []
        let isAtomic (Pattern _ els) = null els
        let lens = mkGraphLens emptyPattern isAtomic
        nodes lens `shouldBe` []
    
    describe "nodes function (Phase 2: Foundational)" $ do
      
      it "T013: nodes with atomic predicate" $ do
        let graphPattern = pattern "graph"
              [ point "a"
              , point "b"
              , pattern "rel" [point "a", point "b"]
              ]
        let isAtomic (Pattern _ els) = null els
        let lens = mkGraphLens graphPattern isAtomic
        let result = nodes lens
        length result `shouldBe` 2
        (point "a" `elem` result) `shouldBe` True
        (point "b" `elem` result) `shouldBe` True
      
      it "T014: nodes with value-based predicate" $ do
        let graphPattern = pattern "graph"
              [ point "node1"
              , point "node2"
              , point "notnode"
              ]
        let isNodeValue (Pattern v _) = "node" `isPrefixOf` v
        let lens = mkGraphLens graphPattern isNodeValue
        let result = nodes lens
        length result `shouldBe` 2
        (point "node1" `elem` result) `shouldBe` True
        (point "node2" `elem` result) `shouldBe` True
      
      it "T016: nodes with empty Pattern scope" $ do
        let emptyPattern = pattern "empty" []
        let isAtomic (Pattern _ els) = null els
        let lens = mkGraphLens emptyPattern isAtomic
        nodes lens `shouldBe` []
      
      it "T017: nodes with Pattern with no nodes (all fail predicate)" $ do
        let graphPattern = pattern "graph"
              [ pattern "rel1" [point "a", point "b"]
              , pattern "rel2" [point "b", point "c"]
              ]
        let isAtomic (Pattern _ els) = null els
        let lens = mkGraphLens graphPattern isAtomic
        nodes lens `shouldBe` []

