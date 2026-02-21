-- | Unit and integration tests for Pattern.PatternGraph.
{-# LANGUAGE OverloadedStrings #-}

module Spec.Pattern.PatternGraphSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Pattern.Core (Pattern(..), pattern, point)
import Pattern.PatternGraph
  ( GraphValue(..),
    PatternGraph(..),
    empty,
    fromPatterns,
    fromPatternsWithPolicy,
    fromPatternGraph,
    merge,
    mergeWithPolicy,
  )
import Pattern.Graph.GraphClassifier (GraphClass(..), GraphClassifier(..), canonicalClassifier, classify, classifyByShape)
import Pattern.Graph.GraphQuery (queryNodes, queryRelationships)
import Pattern.Reconcile (ReconciliationPolicy(..))
import Subject.Core (Subject(..), Symbol(..))
import Test.Hspec

-- Helper: atomic node (0 elements) = Node
node :: Symbol -> Pattern Subject
node s = Pattern (Subject s Set.empty Map.empty) []

-- Helper: relationship (2 node elements)
rel :: Symbol -> Symbol -> Symbol -> Pattern Subject
rel r a b = Pattern (Subject r Set.empty Map.empty) [node a, node b]

data MyDomain = DomainHyperedge | DomainOther deriving (Eq, Show)

spec :: Spec
spec = do
  describe "Pattern.PatternGraph" $ do
    describe "empty" $ do
      it "empty graph has no nodes, relationships, walks, or annotations" $ do
        let g = empty :: PatternGraph () Subject
        Map.size (pgNodes g) `shouldBe` 0
        Map.size (pgRelationships g) `shouldBe` 0
        Map.size (pgWalks g) `shouldBe` 0
        Map.size (pgAnnotations g) `shouldBe` 0
        Map.size (pgOther g) `shouldBe` 0

    describe "merge" $ do
      it "merge adds a node" $ do
        let g0 = empty :: PatternGraph () Subject
        let g1 = merge canonicalClassifier (node "a") g0
        Map.size (pgNodes g1) `shouldBe` 1
        Map.size (pgRelationships g1) `shouldBe` 0

      it "merge adds a relationship after nodes exist" $ do
        let g0 = empty :: PatternGraph () Subject
        let g1 = merge canonicalClassifier (node "a") g0
        let g2 = merge canonicalClassifier (node "b") g1
        let g3 = merge canonicalClassifier (rel "r" "a" "b") g2
        Map.size (pgNodes g3) `shouldBe` 2
        Map.size (pgRelationships g3) `shouldBe` 1

    describe "fromPatterns" $ do
      it "fromPatterns builds graph from list" $ do
        let patterns = [node "a", node "b", rel "r1" "a" "b"]
        let graph = fromPatterns canonicalClassifier patterns
        Map.size (pgNodes graph) `shouldBe` 2
        Map.size (pgRelationships graph) `shouldBe` 1

      it "unrecognized patterns appear in pgOther" $ do
        -- Pattern with 3 elements is not Node/Annotation/Relationship/Walk (Walk requires all rels)
        let weird = Pattern (Subject (Symbol "w") Set.empty Map.empty) [node (Symbol "a"), node (Symbol "b"), node (Symbol "c")]
        let graph = fromPatterns canonicalClassifier [node (Symbol "a"), weird]
        Map.size (pgNodes graph) `shouldBe` 1
        Map.size (pgOther graph) `shouldBe` 1

    describe "classification" $ do
      it "classify GNode (0 elements)" $ do
        classify canonicalClassifier (node "n") `shouldBe` GNode

      it "classify GRelationship (2 node elements)" $ do
        classify canonicalClassifier (rel "r" "a" "b") `shouldBe` GRelationship

      it "classify GAnnotation (1 element)" $ do
        let ann = Pattern (Subject (Symbol "ann") Set.empty Map.empty) [node (Symbol "x")]
        classify canonicalClassifier ann `shouldBe` GAnnotation

      it "classify GOther (3 elements not a walk)" $ do
        let three = Pattern (Subject (Symbol "t") Set.empty Map.empty) [node (Symbol "a"), node (Symbol "b"), node (Symbol "c")]
        classify canonicalClassifier three `shouldBe` GOther ()

    describe "fromPatternGraph" $ do
      it "empty graph yields empty queryNodes" $ do
        let gq = fromPatternGraph (empty :: PatternGraph () Subject)
        length (queryNodes gq) `shouldBe` 0
      it "nodes and relationships match container" $ do
        let graph = fromPatterns canonicalClassifier [node "a", node "b", rel "r" "a" "b"]
        let gq = fromPatternGraph graph
        length (queryNodes gq) `shouldBe` 2
        length (queryRelationships gq) `shouldBe` 1

    describe "mergeWithPolicy / fromPatternsWithPolicy (T011)" $ do
      it "mergeWithPolicy accepts reconciliation policy" $ do
        let g1 = mergeWithPolicy canonicalClassifier LastWriteWins (node "n") empty
        Map.size (pgNodes g1) `shouldBe` 1
        let g2 = mergeWithPolicy canonicalClassifier FirstWriteWins (node "n") empty
        Map.size (pgNodes g2) `shouldBe` 1

    describe "duplicate identity reconciled per policy (T014)" $ do
      it "insert same node twice yields one entry (LastWriteWins)" $ do
        let g0 = empty :: PatternGraph () Subject
        let g1 = mergeWithPolicy canonicalClassifier LastWriteWins (node "a") g0
        let g2 = mergeWithPolicy canonicalClassifier LastWriteWins (node "a") g1
        Map.size (pgNodes g2) `shouldBe` 1

      it "fromPatternsWithPolicy uses policy for duplicates" $ do
        let patterns = [node "a", node "a", node "a"]
        let graph = fromPatternsWithPolicy canonicalClassifier LastWriteWins patterns
        Map.size (pgNodes graph) `shouldBe` 1

    describe "walk decomposition (T012, T014)" $ do
      it "merge walk stores walk and component relationships and nodes in maps" $ do
        -- Walk: path with two relationships (a)-[r1]->(b)-[r2]->(c)
        let r1 = rel (Symbol "r1") (Symbol "a") (Symbol "b")
        let r2 = rel (Symbol "r2") (Symbol "b") (Symbol "c")
        let walkPat = Pattern (Subject (Symbol "path") Set.empty Map.empty) [r1, r2]
        let graph = merge canonicalClassifier walkPat empty
        Map.size (pgWalks graph) `shouldBe` 1
        Map.size (pgRelationships graph) `shouldBe` 2
        Map.size (pgNodes graph) `shouldBe` 3
        Map.member (Symbol "path") (pgWalks graph) `shouldBe` True
        Map.member (Symbol "r1") (pgRelationships graph) `shouldBe` True
        Map.member (Symbol "r2") (pgRelationships graph) `shouldBe` True
        Map.member (Symbol "a") (pgNodes graph) `shouldBe` True
        Map.member (Symbol "b") (pgNodes graph) `shouldBe` True
        Map.member (Symbol "c") (pgNodes graph) `shouldBe` True

    describe "custom classifier (US2)" $ do
      it "demonstrates a custom classifier mapping specific patterns to a domain in pgOther" $ do
        -- Create a custom classifier that identifies hyperedges
        let myClassifier = GraphClassifier
              { classify = \pat@(Pattern _ els) ->
                  if length els > 2 && all (\(Pattern _ inner) -> null inner) els
                  then GOther DomainHyperedge
                  else case classifyByShape pat of
                         GNode -> GNode
                         GRelationship -> GRelationship
                         GWalk -> GWalk
                         GAnnotation -> GAnnotation
                         GOther () -> GOther DomainOther
              }
              
        -- Create a hyperedge (3 nodes)
        let n1 = node (Symbol "n1")
        let n2 = node (Symbol "n2")
        let n3 = node (Symbol "n3")
        let hyperedge = Pattern (Subject (Symbol "hyper") Set.empty Map.empty) [n1, n2, n3]
        
        -- Construct the graph using the custom classifier
        let graph = fromPatterns myClassifier [n1, n2, n3, hyperedge]
        
        -- Verify nodes are classified normally
        Map.size (pgNodes graph) `shouldBe` 3
        
        -- Verify the hyperedge is classified into pgOther with the custom tag
        Map.size (pgOther graph) `shouldBe` 1
        case Map.lookup (Symbol "hyper") (pgOther graph) of
          Just (domainTag, p) -> do
            domainTag `shouldBe` DomainHyperedge
            value p `shouldBe` value hyperedge
          Nothing -> expectationFailure "Hyperedge should be in pgOther"

