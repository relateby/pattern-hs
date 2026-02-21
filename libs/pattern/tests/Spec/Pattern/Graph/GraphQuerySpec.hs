-- | Unit and property tests for Pattern.Graph.GraphQuery.
--
-- Covers T015 (construction), T016 (property: fromGraphLens ≡ fromPatternGraph),
-- T017 (TraversalWeight canonical values), and T056 (queryContainers).
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
module Spec.Pattern.Graph.GraphQuerySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort)
import Data.Maybe (isJust, isNothing)
import Test.Hspec
import Test.QuickCheck

import Pattern.Core (Pattern(..), point)
import Pattern.Graph (GraphLens(..), mkGraphLens)
import Pattern.Graph.GraphClassifier (GraphValue(..), canonicalClassifier)
import Pattern.Graph.GraphQuery
import Pattern.PatternGraph (PatternGraph(..), empty, merge, fromPatterns, fromPatternGraph)
import Subject.Core (Subject(..), Symbol(..))

-- ============================================================================
-- Test helpers
-- ============================================================================

-- | Atomic node (0 elements) — classified as GNode by canonicalClassifier
node :: Symbol -> Pattern Subject
node s = Pattern (Subject s Set.empty Map.empty) []

-- | Relationship (2 node elements) — classified as GRelationship
rel :: Symbol -> Symbol -> Symbol -> Pattern Subject
rel r a b = Pattern (Subject r Set.empty Map.empty) [node a, node b]

-- | Annotation (1 element) — classified as GAnnotation
annotation :: Symbol -> Symbol -> Pattern Subject
annotation a n = Pattern (Subject a Set.empty Map.empty) [node n]

-- | A small known graph: nodes A, B, C; relationships A→B, B→C
knownPatternGraph :: PatternGraph () Subject
knownPatternGraph = fromPatterns canonicalClassifier
  [ node "A"
  , node "B"
  , node "C"
  , rel "r1" "A" "B"
  , rel "r2" "B" "C"
  ]

-- | Equivalent GraphLens for the same graph
knownGraphLens :: GraphLens Subject
knownGraphLens =
  let scope = Pattern (Subject "scope" Set.empty Map.empty)
        [ node "A", node "B", node "C"
        , rel "r1" "A" "B", rel "r2" "B" "C"
        ]
      isAtomic (Pattern _ els) = null els
  in mkGraphLens scope isAtomic

-- | Identity extractor shorthand
nodeId :: Pattern Subject -> Symbol
nodeId p = identify (value p)

-- ============================================================================
-- T015: Unit tests for GraphQuery construction
-- ============================================================================

spec :: Spec
spec = do
  describe "Pattern.Graph.GraphQuery" $ do

    -- -----------------------------------------------------------------------
    -- T017: TraversalWeight canonical values
    -- -----------------------------------------------------------------------
    describe "TraversalWeight canonical values (T017)" $ do

      it "undirected returns 1.0 for Forward" $
        undirected (node "x") Forward `shouldBe` 1.0

      it "undirected returns 1.0 for Backward" $
        undirected (node "x") Backward `shouldBe` 1.0

      it "directed returns 1.0 for Forward" $
        directed (node "x") Forward `shouldBe` 1.0

      it "directed returns infinity for Backward" $
        isInfinite (directed (node "x") Backward) `shouldBe` True

      it "directedReverse returns infinity for Forward" $
        isInfinite (directedReverse (node "x") Forward) `shouldBe` True

      it "directedReverse returns 1.0 for Backward" $
        directedReverse (node "x") Backward `shouldBe` 1.0

    -- -----------------------------------------------------------------------
    -- T015: fromPatternGraph — all nine fields
    -- -----------------------------------------------------------------------
    describe "fromPatternGraph — all nine fields (T015)" $ do
      let gq = fromPatternGraph knownPatternGraph

      it "queryNodes returns all three nodes" $
        length (queryNodes gq) `shouldBe` 3

      it "queryRelationships returns both relationships" $
        length (queryRelationships gq) `shouldBe` 2

      it "queryIncidentRels for A returns r1 only" $ do
        let nodeA = head [ n | n <- queryNodes gq, nodeId n == "A" ]
        length (queryIncidentRels gq nodeA) `shouldBe` 1

      it "queryIncidentRels for B returns r1 and r2" $ do
        let nodeB = head [ n | n <- queryNodes gq, nodeId n == "B" ]
        length (queryIncidentRels gq nodeB) `shouldBe` 2

      it "querySource returns source of r1 (A)" $ do
        let r = head [ r' | r' <- queryRelationships gq
                           , identify (value r') == "r1" ]
        fmap nodeId (querySource gq r) `shouldBe` Just "A"

      it "queryTarget returns target of r1 (B)" $ do
        let r = head [ r' | r' <- queryRelationships gq
                           , identify (value r') == "r1" ]
        fmap nodeId (queryTarget gq r) `shouldBe` Just "B"

      it "queryDegree for A is 1" $ do
        let nodeA = head [ n | n <- queryNodes gq, nodeId n == "A" ]
        queryDegree gq nodeA `shouldBe` 1

      it "queryDegree for B is 2" $ do
        let nodeB = head [ n | n <- queryNodes gq, nodeId n == "B" ]
        queryDegree gq nodeB `shouldBe` 2

      it "queryNodeById finds A" $
        fmap nodeId (queryNodeById gq "A") `shouldBe` Just "A"

      it "queryNodeById returns Nothing for unknown id" $
        queryNodeById gq "Z" `shouldBe` Nothing

      it "queryRelationshipById finds r1" $
        fmap (identify . value) (queryRelationshipById gq "r1") `shouldBe` Just "r1"

      it "queryRelationshipById returns Nothing for unknown id" $
        queryRelationshipById gq "rX" `shouldBe` Nothing

      it "queryContainers for node A returns r1 (A is source)" $ do
        let nodeA = head [ n | n <- queryNodes gq, nodeId n == "A" ]
        let containers = queryContainers gq nodeA
        any (\c -> identify (value c) == "r1") containers `shouldBe` True

    -- -----------------------------------------------------------------------
    -- T015: fromGraphLens — all nine fields
    -- -----------------------------------------------------------------------
    describe "fromGraphLens — all nine fields (T015)" $ do
      let gq = fromGraphLens knownGraphLens

      it "queryNodes returns all three nodes" $
        length (queryNodes gq) `shouldBe` 3

      it "queryRelationships returns both relationships" $
        length (queryRelationships gq) `shouldBe` 2

      it "querySource returns source of r1 (A)" $ do
        let r = head [ r' | r' <- queryRelationships gq
                           , identify (value r') == "r1" ]
        fmap nodeId (querySource gq r) `shouldBe` Just "A"

      it "queryTarget returns target of r1 (B)" $ do
        let r = head [ r' | r' <- queryRelationships gq
                           , identify (value r') == "r1" ]
        fmap nodeId (queryTarget gq r) `shouldBe` Just "B"

      it "queryNodeById finds A" $
        fmap nodeId (queryNodeById gq "A") `shouldBe` Just "A"

      it "queryNodeById returns Nothing for unknown id" $
        queryNodeById gq "Z" `shouldBe` Nothing

    -- -----------------------------------------------------------------------
    -- T016: Property — fromGraphLens and fromPatternGraph agree on equivalent graphs
    -- -----------------------------------------------------------------------
    describe "fromGraphLens and fromPatternGraph equivalence (T016)" $ do

      it "queryNodes count matches between fromGraphLens and fromPatternGraph" $ do
        let gqPG = fromPatternGraph knownPatternGraph
        let gqGL = fromGraphLens knownGraphLens
        length (queryNodes gqPG) `shouldBe` length (queryNodes gqGL)

      it "queryRelationships count matches" $ do
        let gqPG = fromPatternGraph knownPatternGraph
        let gqGL = fromGraphLens knownGraphLens
        length (queryRelationships gqPG) `shouldBe` length (queryRelationships gqGL)

      it "querySource agrees for r1" $ do
        let gqPG = fromPatternGraph knownPatternGraph
        let gqGL = fromGraphLens knownGraphLens
        let rPG = head [ r | r <- queryRelationships gqPG, identify (value r) == "r1" ]
        let rGL = head [ r | r <- queryRelationships gqGL, identify (value r) == "r1" ]
        fmap nodeId (querySource gqPG rPG) `shouldBe` fmap nodeId (querySource gqGL rGL)

      it "queryTarget agrees for r1" $ do
        let gqPG = fromPatternGraph knownPatternGraph
        let gqGL = fromGraphLens knownGraphLens
        let rPG = head [ r | r <- queryRelationships gqPG, identify (value r) == "r1" ]
        let rGL = head [ r | r <- queryRelationships gqGL, identify (value r) == "r1" ]
        fmap nodeId (queryTarget gqPG rPG) `shouldBe` fmap nodeId (queryTarget gqGL rGL)

      it "queryIncidentRels count for B agrees" $ do
        let gqPG = fromPatternGraph knownPatternGraph
        let gqGL = fromGraphLens knownGraphLens
        let nodeBpg = head [ n | n <- queryNodes gqPG, nodeId n == "B" ]
        let nodeBgl = head [ n | n <- queryNodes gqGL, nodeId n == "B" ]
        length (queryIncidentRels gqPG nodeBpg) `shouldBe` length (queryIncidentRels gqGL nodeBgl)

    -- -----------------------------------------------------------------------
    -- T056: queryContainers unit tests
    -- -----------------------------------------------------------------------
    describe "queryContainers (T056)" $ do

      it "node with no containers returns empty list" $ do
        let pg = fromPatterns canonicalClassifier [node "solo"]
        let gq = fromPatternGraph pg
        let n  = head (queryNodes gq)
        queryContainers gq n `shouldBe` []

      it "node participating in a relationship is contained by it" $ do
        let pg = fromPatterns canonicalClassifier [node "A", node "B", rel "r" "A" "B"]
        let gq = fromPatternGraph pg
        let nodeA = head [ n | n <- queryNodes gq, nodeId n == "A" ]
        let containers = queryContainers gq nodeA
        length containers `shouldBe` 1
        identify (value (head containers)) `shouldBe` "r"

      it "node with annotation is contained by it" $ do
        let pg = fromPatterns canonicalClassifier [node "A", annotation "ann" "A"]
        let gq = fromPatternGraph pg
        let nodeA = head [ n | n <- queryNodes gq, nodeId n == "A" ]
        let containers = queryContainers gq nodeA
        any (\c -> identify (value c) == "ann") containers `shouldBe` True

    -- -----------------------------------------------------------------------
    -- frameQuery tests (T047, T048)
    -- -----------------------------------------------------------------------
    describe "frameQuery (T047, T048)" $ do

      it "frameQuery restricts queryNodes to matching elements" $ do
        let pg = fromPatterns canonicalClassifier
              [ node "A", node "B", node "C"
              , rel "r1" "A" "B", rel "r2" "B" "C"
              ]
        let gq = fromPatternGraph pg
        let framed = frameQuery (\p -> identify (value p) `elem` ["A", "B", "r1"]) gq
        let ns = queryNodes framed
        length ns `shouldBe` 2
        all (\n -> nodeId n `elem` ["A", "B"]) ns `shouldBe` True

      it "frameQuery excludes cross-frame relationships from queryIncidentRels" $ do
        let pg = fromPatterns canonicalClassifier
              [ node "A", node "B", node "C"
              , rel "r1" "A" "B", rel "r2" "B" "C"
              ]
        let gq = fromPatternGraph pg
        -- Frame includes A and B but not C or r2
        let framed = frameQuery (\p -> identify (value p) `elem` ["A", "B", "r1"]) gq
        let nodeB = head [ n | n <- queryNodes framed, nodeId n == "B" ]
        -- r2 connects B→C; C is outside frame, so r2 should be excluded
        let incRels = queryIncidentRels framed nodeB
        all (\r -> identify (value r) /= "r2") incRels `shouldBe` True

      it "frameQuery producing empty graph — queryNodes returns []" $ do
        let gq = fromPatternGraph knownPatternGraph
        let framed = frameQuery (const False) gq
        queryNodes framed `shouldBe` []

      it "frameQuery empty graph — queryRelationships returns []" $ do
        let gq = fromPatternGraph knownPatternGraph
        let framed = frameQuery (const False) gq
        queryRelationships framed `shouldBe` []

    -- -----------------------------------------------------------------------
    -- memoizeIncidentRels tests (T049, T050)
    -- -----------------------------------------------------------------------
    describe "memoizeIncidentRels (T049, T050)" $ do

      it "memoizeIncidentRels returns same incident rels as unwrapped for all nodes" $ do
        let gq = fromPatternGraph knownPatternGraph
        let memo = memoizeIncidentRels gq
        let ns = queryNodes gq
        all (\n -> length (queryIncidentRels memo n) == length (queryIncidentRels gq n)) ns
          `shouldBe` True

      it "memoizeIncidentRels preserves queryNodes" $ do
        let gq = fromPatternGraph knownPatternGraph
        let memo = memoizeIncidentRels gq
        length (queryNodes memo) `shouldBe` length (queryNodes gq)

      it "memoizeIncidentRels preserves queryRelationships" $ do
        let gq = fromPatternGraph knownPatternGraph
        let memo = memoizeIncidentRels gq
        length (queryRelationships memo) `shouldBe` length (queryRelationships gq)

      it "frameQuery then memoizeIncidentRels composition works" $ do
        let gq = fromPatternGraph knownPatternGraph
        let composed = memoizeIncidentRels . frameQuery (\p -> nodeId p `elem` ["A", "B", "r1"]) $ gq
        length (queryNodes composed) `shouldBe` 2

    -- -----------------------------------------------------------------------
    -- T051: frameQuery preserves GraphQuery invariants
    -- -----------------------------------------------------------------------
    describe "frameQuery invariants (T051)" $ do

      it "querySource r = Just s implies s is in queryNodes of framed result" $ do
        let gq = fromPatternGraph knownPatternGraph
        let framed = frameQuery (\p -> nodeId p `elem` ["A", "B", "r1"]) gq
        let rels = queryRelationships framed
        let ns = queryNodes framed
        let nodeIds = map nodeId ns
        all (\r -> case querySource framed r of
                     Nothing -> True
                     Just s  -> nodeId s `elem` nodeIds) rels
          `shouldBe` True

      it "queryTarget r = Just t implies t is in queryNodes of framed result" $ do
        let gq = fromPatternGraph knownPatternGraph
        let framed = frameQuery (\p -> nodeId p `elem` ["A", "B", "r1"]) gq
        let rels = queryRelationships framed
        let ns = queryNodes framed
        let nodeIds = map nodeId ns
        all (\r -> case queryTarget framed r of
                     Nothing -> True
                     Just t  -> nodeId t `elem` nodeIds) rels
          `shouldBe` True
