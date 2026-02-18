-- | Unit and integration tests for Pattern.PatternGraph.
{-# LANGUAGE OverloadedStrings #-}

module Spec.Pattern.PatternGraphSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Pattern.Core (Pattern(..), pattern, point)
import Pattern.PatternGraph
  ( MergeResult(..),
    PatternClass(..),
    PatternGraph(..),
    classify,
    empty,
    fromPatterns,
    fromPatternsWithPolicy,
    merge,
    mergeWithPolicy,
    toGraphLens,
  )
import Pattern.Graph (nodes, relationships)
import Pattern.Reconcile (ReconciliationPolicy(..))
import Subject.Core (Subject(..), Symbol(..))
import Test.Hspec

-- Helper: atomic node (0 elements) = Node
node :: Symbol -> Pattern Subject
node s = Pattern (Subject s Set.empty Map.empty) []

-- Helper: relationship (2 node elements)
rel :: Symbol -> Symbol -> Symbol -> Pattern Subject
rel r a b = Pattern (Subject r Set.empty Map.empty) [node a, node b]

spec :: Spec
spec = do
  describe "Pattern.PatternGraph" $ do
    describe "empty" $ do
      it "empty graph has no nodes, relationships, walks, or annotations" $ do
        let g = empty :: PatternGraph Subject
        Map.size (pgNodes g) `shouldBe` 0
        Map.size (pgRelationships g) `shouldBe` 0
        Map.size (pgWalks g) `shouldBe` 0
        Map.size (pgAnnotations g) `shouldBe` 0

    describe "merge" $ do
      it "merge adds a node and returns MergeResult with empty unrecognized" $ do
        let g0 = empty :: PatternGraph Subject
        let (MergeResult g1 unk) = merge (node "a") g0
        unk `shouldBe` []
        Map.size (pgNodes g1) `shouldBe` 1
        Map.size (pgRelationships g1) `shouldBe` 0

      it "merge adds a relationship after nodes exist" $ do
        let g0 = empty :: PatternGraph Subject
        let (MergeResult g1 _) = merge (node "a") g0
        let (MergeResult g2 _) = merge (node "b") g1
        let (MergeResult g3 unk) = merge (rel "r" "a" "b") g2
        unk `shouldBe` []
        Map.size (pgNodes g3) `shouldBe` 2
        Map.size (pgRelationships g3) `shouldBe` 1

    describe "fromPatterns" $ do
      it "fromPatterns builds graph from list; unrecognized in result" $ do
        let patterns = [node "a", node "b", rel "r1" "a" "b"]
        let MergeResult graph unrecognized = fromPatterns patterns
        unrecognized `shouldBe` []
        Map.size (pgNodes graph) `shouldBe` 2
        Map.size (pgRelationships graph) `shouldBe` 1

      it "unrecognized patterns appear in result list, not in graph" $ do
        -- Pattern with 3 elements is not Node/Annotation/Relationship/Walk (Walk requires all rels)
        let weird = Pattern (Subject (Symbol "w") Set.empty Map.empty) [node (Symbol "a"), node (Symbol "b"), node (Symbol "c")]
        let MergeResult graph unk = fromPatterns [node (Symbol "a"), weird]
        Map.size (pgNodes graph) `shouldBe` 1
        length unk `shouldBe` 1

    describe "classification" $ do
      it "classify Node (0 elements)" $ do
        classify (node "n") `shouldBe` Node

      it "classify Relationship (2 node elements)" $ do
        classify (rel "r" "a" "b") `shouldBe` Relationship

      it "classify Annotation (1 element)" $ do
        let ann = Pattern (Subject (Symbol "ann") Set.empty Map.empty) [node (Symbol "x")]
        classify ann `shouldBe` Annotation

      it "classify Unrecognized (3 elements not a walk)" $ do
        let three = Pattern (Subject (Symbol "t") Set.empty Map.empty) [node (Symbol "a"), node (Symbol "b"), node (Symbol "c")]
        classify three `shouldBe` Unrecognized

    describe "toGraphLens" $ do
      it "nodes and relationships from lens match container" $ do
        let MergeResult graph _ = fromPatterns [node "a", node "b", rel "r" "a" "b"]
        let lens = toGraphLens graph
        length (nodes lens) `shouldBe` 2
        length (relationships lens) `shouldBe` 1

    describe "mergeWithPolicy / fromPatternsWithPolicy (T011)" $ do
      it "mergeWithPolicy accepts reconciliation policy" $ do
        let MergeResult g1 _ = mergeWithPolicy LastWriteWins (node "n") empty
        Map.size (pgNodes g1) `shouldBe` 1
        let MergeResult g2 _ = mergeWithPolicy FirstWriteWins (node "n") empty
        Map.size (pgNodes g2) `shouldBe` 1

    describe "duplicate identity reconciled per policy (T014)" $ do
      it "insert same node twice yields one entry (LastWriteWins)" $ do
        let g0 = empty :: PatternGraph Subject
        let MergeResult g1 _ = mergeWithPolicy LastWriteWins (node "a") g0
        let MergeResult g2 _ = mergeWithPolicy LastWriteWins (node "a") g1
        Map.size (pgNodes g2) `shouldBe` 1

      it "fromPatternsWithPolicy uses policy for duplicates" $ do
        let patterns = [node "a", node "a", node "a"]
        let MergeResult graph _ = fromPatternsWithPolicy LastWriteWins patterns
        Map.size (pgNodes graph) `shouldBe` 1

    describe "walk decomposition (T012, T014)" $ do
      it "merge walk stores walk and component relationships and nodes in maps" $ do
        -- Walk: path with two relationships (a)-[r1]->(b)-[r2]->(c)
        let r1 = rel (Symbol "r1") (Symbol "a") (Symbol "b")
        let r2 = rel (Symbol "r2") (Symbol "b") (Symbol "c")
        let walkPat = Pattern (Subject (Symbol "path") Set.empty Map.empty) [r1, r2]
        let MergeResult graph _ = merge walkPat empty
        Map.size (pgWalks graph) `shouldBe` 1
        Map.size (pgRelationships graph) `shouldBe` 2
        Map.size (pgNodes graph) `shouldBe` 3
        Map.member (Symbol "path") (pgWalks graph) `shouldBe` True
        Map.member (Symbol "r1") (pgRelationships graph) `shouldBe` True
        Map.member (Symbol "r2") (pgRelationships graph) `shouldBe` True
        Map.member (Symbol "a") (pgNodes graph) `shouldBe` True
        Map.member (Symbol "b") (pgNodes graph) `shouldBe` True
        Map.member (Symbol "c") (pgNodes graph) `shouldBe` True

    describe "annotation inner merge (T013, T014)" $ do
      it "merge annotation stores annotation and merges inner element into category" $ do
        let innerNode = node (Symbol "x")
        let ann = Pattern (Subject (Symbol "ann1") Set.empty Map.empty) [innerNode]
        let MergeResult graph _ = merge ann empty
        Map.size (pgAnnotations graph) `shouldBe` 1
        Map.size (pgNodes graph) `shouldBe` 1
        Map.member (Symbol "ann1") (pgAnnotations graph) `shouldBe` True
        Map.member (Symbol "x") (pgNodes graph) `shouldBe` True

