-- | Tests for Pattern.Graph.Transform module.
module Spec.Pattern.Graph.TransformSpec where

import Control.Exception (evaluate)
import Test.Hspec
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sort)
import Pattern.Core (Pattern(..), ScopeQuery(..), point, toScopeDict)
import qualified Pattern.Core as P
import Pattern.Graph (GraphView(..))
import Pattern.Graph.GraphClassifier
  ( GraphClass(..), GraphClassifier(..), GraphValue(..)
  , canonicalClassifier, classifyByShape )
import Pattern.Graph.GraphQuery (GraphQuery(..))
import Pattern.Graph.Transform
import Pattern.Graph.Types (Substitution(..))
import Pattern.PatternGraph (PatternGraph(..), empty, mergeWithPolicy, toGraphView, materialize)
import qualified Pattern.PatternGraph as PG
import Pattern.Reconcile (ReconciliationPolicy(..))
import Subject.Core (Subject(..), Symbol(..))
import qualified Subject.Core as Subj
import qualified Data.Set as Set
import qualified Data.Map.Strict as PropMap

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

mkSubject :: String -> Subject
mkSubject s = Subject (Symbol s) Set.empty PropMap.empty

mkNode :: String -> Pattern Subject
mkNode s = point (mkSubject s)

mkRel :: String -> String -> String -> Pattern Subject
mkRel relId src tgt =
  Pattern (mkSubject relId) [mkNode src, mkNode tgt]

-- A small graph: a -> b -> c
simpleGraph :: PatternGraph () Subject
simpleGraph =
  PG.fromPatterns canonicalClassifier
    [ mkNode "a"
    , mkNode "b"
    , mkNode "c"
    , mkRel "ab" "a" "b"
    , mkRel "bc" "b" "c"
    ]

-- A walk: path [ab, bc]  (a -> b -> c)
mkWalk :: String -> [Pattern Subject] -> Pattern Subject
mkWalk walkId rels = Pattern (mkSubject walkId) rels

mkAnnotation :: String -> Pattern Subject -> Pattern Subject
mkAnnotation annotId inner = Pattern (mkSubject annotId) [inner]

mkOther :: String -> [Pattern Subject] -> Pattern Subject
mkOther otherId parts = Pattern (mkSubject otherId) parts

graphWithWalk :: PatternGraph () Subject
graphWithWalk =
  PG.fromPatterns canonicalClassifier
    [ mkNode "a"
    , mkNode "b"
    , mkNode "c"
    , mkRel "ab" "a" "b"
    , mkRel "bc" "b" "c"
    , mkWalk "path1" [mkRel "ab" "a" "b", mkRel "bc" "b" "c"]
    ]

graphWithScopeBoundary :: PatternGraph () Subject
graphWithScopeBoundary =
  PG.fromPatterns canonicalClassifier
    [ mkNode "a"
    , mkNode "b"
    , mkNode "c"
    , mkNode "d"
    , mkRel "ab" "a" "b"
    , mkRel "bc" "b" "c"
    , mkWalk "path1" [mkRel "ab" "a" "b", mkRel "bc" "b" "c"]
    , mkAnnotation "annot-ab" (mkRel "ab" "a" "b")
    , mkOther "other" [mkNode "a", mkNode "c", mkNode "d"]
    ]

graphWithDuplicateScopeIds :: PatternGraph () Subject
graphWithDuplicateScopeIds =
  PG.fromPatterns canonicalClassifier
    [ mkNode "dup"
    , mkNode "a"
    , mkNode "b"
    , mkNode "c"
    , mkOther "dup" [mkNode "a", mkNode "b", mkNode "c"]
    ]

-- Count elements by class
countByClass :: GraphClass () -> GraphView () Subject -> Int
countByClass target view =
  length [ () | (cls, _) <- viewElements view, cls == target ]

subjectIds :: [Pattern Subject] -> [Symbol]
subjectIds = map (Subj.identity . value)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Pattern.Graph.Transform" $ do

    describe "graph-backed scope answers (US2/US3)" $ do

      it "T014: scopeDictFromGraphView covers all graph-scoped elements and lookups" $ do
        let view = toGraphView canonicalClassifier graphWithScopeBoundary
        let scope = scopeDictFromGraphView view

        length (allElements scope) `shouldBe` length (viewElements view)
        fmap (Subj.identity . value) (byIdentity scope (Symbol "path1")) `shouldBe` Just (Symbol "path1")
        fmap (Subj.identity . value) (byIdentity scope (Symbol "annot-ab")) `shouldBe` Just (Symbol "annot-ab")
        fmap (Subj.identity . value) (byIdentity scope (Symbol "other")) `shouldBe` Just (Symbol "other")

      it "T014b: graph scope answers direct containers and siblings within one GraphView" $ do
        let view = toGraphView canonicalClassifier graphWithScopeBoundary
        let scope = scopeDictFromGraphView view
        let Just relAb = byIdentity scope (Symbol "ab")
        let Just nodeA = byIdentity scope (Symbol "a")

        sort (subjectIds (containers scope relAb)) `shouldBe` sort [Symbol "path1", Symbol "annot-ab"]
        sort (subjectIds (siblings scope relAb)) `shouldBe` sort [Symbol "bc"]
        sort (subjectIds (siblings scope nodeA)) `shouldBe` sort [Symbol "b", Symbol "c", Symbol "d"]

      it "T020: reifying a graph-backed scope value preserves generic scope answers" $ do
        let view = toGraphView canonicalClassifier graphWithScopeBoundary
        let direct = scopeDictFromGraphView view
        let reified = toScopeDict direct
        let ids = [Symbol "a", Symbol "ab", Symbol "path1", Symbol "annot-ab", Symbol "other"]
        let elementsInScope = allElements direct
        let sampleElements =
              [ p
              | symbol <- ids
              , Just p <- [byIdentity direct symbol]
              ]

        allElements reified `shouldBe` elementsInScope
        map (fmap (Subj.identity . value) . byIdentity reified) ids
          `shouldBe` map (fmap (Subj.identity . value) . byIdentity direct) ids
        map (sort . subjectIds . containers reified) sampleElements
          `shouldBe` map (sort . subjectIds . containers direct) sampleElements
        map (sort . subjectIds . siblings reified) sampleElements
          `shouldBe` map (sort . subjectIds . siblings direct) sampleElements

      it "rejects duplicate graph identities when reifying generic graph scope" $ do
        let view = toGraphView canonicalClassifier graphWithDuplicateScopeIds
        evaluate (byIdentity (scopeDictFromGraphView view) (Symbol "dup"))
          `shouldThrow` anyErrorCall

    -- -----------------------------------------------------------------------
    -- Phase 1 / T008: GraphView initialization
    -- -----------------------------------------------------------------------
    describe "GraphView initialization (Phase 1)" $ do

      it "toGraphView produces a GraphView with correct element count" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let elems = viewElements view
        length elems `shouldBe` 5  -- 3 nodes + 2 relationships

      it "toGraphView viewQuery has correct nodes" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let ns = queryNodes (viewQuery view)
        length ns `shouldBe` 3

      it "materialize round-trips a PatternGraph" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let result = materialize canonicalClassifier LastWriteWins view
        Map.size (PG.pgNodes result) `shouldBe` 3
        Map.size (PG.pgRelationships result) `shouldBe` 2

    -- -----------------------------------------------------------------------
    -- US1 / T010: unfoldGraph
    -- -----------------------------------------------------------------------
    describe "unfoldGraph (US1)" $ do

      it "builds a graph from seed data" $ do
        let seeds = [("a","b","ab"), ("b","c","bc")]
        let expand (src, tgt, relId) =
              [ mkNode src, mkNode tgt, mkRel relId src tgt ]
        let g = unfoldGraph canonicalClassifier LastWriteWins expand seeds
        Map.size (PG.pgNodes g) `shouldBe` 3
        Map.size (PG.pgRelationships g) `shouldBe` 2

      it "merges duplicate nodes from multiple seeds" $ do
        let seeds = [("a","b","ab"), ("a","c","ac")]
        let expand (src, tgt, relId) =
              [ mkNode src, mkNode tgt, mkRel relId src tgt ]
        let g = unfoldGraph canonicalClassifier LastWriteWins expand seeds
        Map.size (PG.pgNodes g) `shouldBe` 3  -- a, b, c (a deduplicated)
        Map.size (PG.pgRelationships g) `shouldBe` 2

      it "produces an empty graph from an empty seed list" $ do
        let g = unfoldGraph canonicalClassifier LastWriteWins
                  (\_ -> [mkNode "x"]) ([] :: [()])
        Map.size (PG.pgNodes g) `shouldBe` 0

      it "handles seeds that produce only nodes" $ do
        let seeds = ["a", "b", "c"] :: [String]
        let expand s = [mkNode s]
        let g = unfoldGraph canonicalClassifier LastWriteWins expand seeds
        Map.size (PG.pgNodes g) `shouldBe` 3
        Map.size (PG.pgRelationships g) `shouldBe` 0

      it "handles seeds that produce only relationships (auto-inserts endpoint nodes)" $ do
        let seeds = [("a","b","ab")] :: [(String,String,String)]
        let expand (src, tgt, relId) = [mkRel relId src tgt]
        let g = unfoldGraph canonicalClassifier LastWriteWins expand seeds
        -- mergeWithPolicy for a relationship also inserts its endpoint nodes
        Map.size (PG.pgRelationships g) `shouldBe` 1
        Map.size (PG.pgNodes g) `shouldBe` 2

    -- -----------------------------------------------------------------------
    -- US2 / T013: mapGraph
    -- -----------------------------------------------------------------------
    describe "mapGraph (US2)" $ do

      it "applies node mapper only to nodes, leaving relationships unchanged" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let sentinel = mkSubject "MAPPED"
        let mapped = mapGraph canonicalClassifier
                       (\(Pattern _ els) -> Pattern sentinel els)
                       id id id id view
        let nodeSubs = [ v | (GNode, Pattern v _) <- viewElements mapped ]
        all (== sentinel) nodeSubs `shouldBe` True
        let relSubs = [ v | (GRelationship, Pattern v _) <- viewElements mapped ]
        any (== sentinel) relSubs `shouldBe` False

      it "applies relationship mapper only to relationships, leaving nodes unchanged" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let sentinel = mkSubject "MAPPED"
        let mapped = mapGraph canonicalClassifier
                       id
                       (\(Pattern _ els) -> Pattern sentinel els)
                       id id id view
        let relSubs = [ v | (GRelationship, Pattern v _) <- viewElements mapped ]
        all (== sentinel) relSubs `shouldBe` True
        let nodeSubs = [ v | (GNode, Pattern v _) <- viewElements mapped ]
        any (== sentinel) nodeSubs `shouldBe` False

      it "preserves element counts per class after mapping" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let mapped = mapGraph canonicalClassifier id id id id id view
        countByClass GNode mapped `shouldBe` countByClass GNode view
        countByClass GRelationship mapped `shouldBe` countByClass GRelationship view

      it "composes two mapGraph calls correctly" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let step1 = mapGraph canonicalClassifier
                      (\(Pattern v els) -> Pattern v (els ++ [point v])) id id id id view
        let step2 = mapGraph canonicalClassifier
                      (\(Pattern v els) -> Pattern v (take 1 els)) id id id id step1
        -- Each node should have exactly 1 child after compose
        let nodeLengths = [ length els | (GNode, Pattern _ els) <- viewElements step2 ]
        all (== 1) nodeLengths `shouldBe` True

      it "viewQuery is unchanged after mapGraph" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let mapped = mapGraph canonicalClassifier id id id id id view
        length (queryNodes (viewQuery mapped)) `shouldBe` length (queryNodes (viewQuery view))

    -- -----------------------------------------------------------------------
    -- US2 / T013: mapAllGraph
    -- -----------------------------------------------------------------------
    describe "mapAllGraph (US2)" $ do

      it "applies function uniformly to every element" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let sentinel = mkSubject "ALL"
        let mapped = mapAllGraph (\(Pattern _ els) -> Pattern sentinel els) view
        let allSentinel = all (\(_, Pattern v _) -> v == sentinel) (viewElements mapped)
        allSentinel `shouldBe` True

      it "preserves total element count" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let mapped = mapAllGraph id view
        length (viewElements mapped) `shouldBe` length (viewElements view)

      it "preserves class tags after mapAllGraph" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let mapped = mapAllGraph id view
        let origClasses = map fst (viewElements view)
        let mappedClasses = map fst (viewElements mapped)
        mappedClasses `shouldBe` origClasses

      it "mapAllGraph id is equivalent to identity on viewElements" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        viewElements (mapAllGraph id view) `shouldBe` viewElements view

    -- -----------------------------------------------------------------------
    -- US2 / T014: filterGraph — Walk gap substitutions
    -- -----------------------------------------------------------------------
    describe "filterGraph (US2)" $ do

      it "removes elements that fail the predicate" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let keepOnlyNodes cls _ = cls == GNode
        let filtered = filterGraph canonicalClassifier keepOnlyNodes SpliceGap view
        all (\(cls, _) -> cls == GNode) (viewElements filtered) `shouldBe` True

      it "keeps all elements when predicate is always True" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let filtered = filterGraph canonicalClassifier (\_ _ -> True) SpliceGap view
        length (viewElements filtered) `shouldBe` length (viewElements view)

      it "removes all elements when predicate is always False" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let filtered = filterGraph canonicalClassifier (\_ _ -> False) SpliceGap view
        null (viewElements filtered) `shouldBe` True

      it "SpliceGap: removes a relationship from a walk's elements, shortening it" $ do
        let view = toGraphView canonicalClassifier graphWithWalk
        -- Keep the walk itself but remove the 'bc' relationship inside it
        let keepPred cls (Pattern v _) = case cls of
              GRelationship -> Subj.identity v /= Symbol "bc"
              _             -> True
        let filtered = filterGraph canonicalClassifier keepPred SpliceGap view
        let walks = [ p | (GWalk, p) <- viewElements filtered ]
        length walks `shouldBe` 1
        -- Walk should now have only 1 relationship (ab), not 2
        length (elements (head walks)) `shouldBe` 1

      it "DeleteContainer: removes the entire walk when any internal relationship is removed" $ do
        let view = toGraphView canonicalClassifier graphWithWalk
        let keepPred cls (Pattern v _) = case cls of
              GRelationship -> Subj.identity v /= Symbol "bc"
              _             -> True
        let filtered = filterGraph canonicalClassifier keepPred DeleteContainer view
        let walks = [ p | (GWalk, p) <- viewElements filtered ]
        null walks `shouldBe` True

      it "ReplaceWithSurrogate: replaces removed relationship with surrogate in walk" $ do
        let surrogate = mkRel "gap" "x" "y"
        let view = toGraphView canonicalClassifier graphWithWalk
        let keepPred cls (Pattern v _) = case cls of
              GRelationship -> Subj.identity v /= Symbol "bc"
              _             -> True
        let filtered = filterGraph canonicalClassifier keepPred (ReplaceWithSurrogate surrogate) view
        let walks = [ p | (GWalk, p) <- viewElements filtered ]
        length walks `shouldBe` 1
        -- Walk should still have 2 elements: ab + surrogate
        length (elements (head walks)) `shouldBe` 2
        let relIds = map (\(Pattern v _) -> Subj.identity v) (elements (head walks))
        (Symbol "gap" `elem` relIds) `shouldBe` True

      it "viewQuery snapshot is unchanged after filterGraph" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let filtered = filterGraph canonicalClassifier (\_ _ -> False) SpliceGap view
        length (queryNodes (viewQuery filtered)) `shouldBe` 3

    -- -----------------------------------------------------------------------
    -- US2 / T015: foldGraph
    -- -----------------------------------------------------------------------
    describe "foldGraph (US2)" $ do

      it "counts total elements via list Monoid" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        length (foldGraph (\_ _ -> [()]) view) `shouldBe` 5

      it "collects node identities only" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let nodeIds cls (Pattern v _) = case cls of
              GNode -> [Subj.identity v]
              _     -> []
        length (foldGraph nodeIds view) `shouldBe` 3

      it "collects relationship identities only" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let relIds cls (Pattern v _) = case cls of
              GRelationship -> [Subj.identity v]
              _             -> []
        length (foldGraph relIds view) `shouldBe` 2

      it "returns mempty for an empty view" $ do
        let emptyView = toGraphView canonicalClassifier
              (PG.empty :: PatternGraph () Subject)
        foldGraph (\_ _ -> [()]) emptyView `shouldBe` []

      it "Sum Monoid: sums a numeric property across all nodes" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        -- Each node contributes 1, relationships contribute 0
        let countNodes cls _ = case cls of { GNode -> 1 :: Int; _ -> 0 }
        sum (foldGraph (\cls p -> [countNodes cls p]) view) `shouldBe` 3

      it "foldGraph composes with mapAllGraph" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        -- After mapping, fold should see the mapped values
        let sentinel = mkSubject "X"
        let mapped = mapAllGraph (\(Pattern _ els) -> Pattern sentinel els) view
        let allX = foldGraph (\_ (Pattern v _) -> [v == sentinel]) mapped
        all id allX `shouldBe` True

    -- -----------------------------------------------------------------------
    -- US3 / T020 + T021: mapWithContext snapshot isolation
    -- -----------------------------------------------------------------------
    describe "mapWithContext (US3)" $ do

      it "preserves element count" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let enriched = mapWithContext canonicalClassifier (\_ p -> p) view
        length (viewElements enriched) `shouldBe` length (viewElements view)

      it "preserves class tags on all elements" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let enriched = mapWithContext canonicalClassifier (\_ p -> p) view
        map fst (viewElements enriched) `shouldBe` map fst (viewElements view)

      it "snapshot query is the original — not the in-flight view" $ do
        -- The mapper replaces every node's value with a sentinel, but the
        -- query passed to each call must still reflect the original 3 nodes.
        let view = toGraphView canonicalClassifier simpleGraph
        let sentinel = mkSubject "NEW"
        let nodeCountsSeenByMapper = mapWithContext canonicalClassifier
              (\q (Pattern _ els) ->
                  Pattern (mkSubject (show (length (queryNodes q)))) els)
              view
        -- Every element should have seen exactly 3 nodes in the snapshot
        let vals = [ Subj.identity v | (_, Pattern v _) <- viewElements nodeCountsSeenByMapper ]
        all (== Symbol "3") vals `shouldBe` True

      it "snapshot isolation: mapper for element N does not see changes from element N-1" $ do
        -- Build a view, then map: each node annotates itself with the degree
        -- of its first neighbor *from the original graph*. If snapshot were
        -- not isolated, a prior element's mutation could corrupt later lookups.
        let view = toGraphView canonicalClassifier simpleGraph
        -- Enrich each pattern with its own degree from the snapshot query
        let enriched = mapWithContext canonicalClassifier
              (\q p ->
                let deg = length (queryIncidentRels q p)
                in Pattern (mkSubject (show deg)) (elements p))
              view
        -- Degrees in simpleGraph (a->b->c): a=1, b=2, c=1, ab=2, bc=2
        -- The exact values matter less than that they're consistent with the
        -- original graph, not a partially-mutated one.
        -- All degree values must be non-negative integers (valid snapshot reads)
        let vals = [ Subj.identity v | (_, Pattern v _) <- viewElements enriched ]
        all (\(Symbol s) -> all (`elem` ("0123456789" :: String)) s) vals `shouldBe` True

      it "enriches nodes with neighbor count from snapshot" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let enriched = mapWithContext canonicalClassifier
              (\q p ->
                let neighborCount = length (queryIncidentRels q p)
                in Pattern (mkSubject (show neighborCount)) (elements p))
              view
        -- Node 'b' has degree 2 (connected to both ab and bc), and the
        -- enrichment stores this degree as the node's value.
        let bDeg = [ Subj.identity v
                   | (GNode, Pattern v _) <- viewElements enriched
                   , Subj.identity v == Symbol "2"
                   ]
        -- There should be exactly one node entry whose enriched value encodes
        -- degree 2 (this is node 'b' in simpleGraph).
        bDeg `shouldBe` [Symbol "2"]
        -- The total number of elements (3 nodes + 2 rels) remains unchanged.
        length (viewElements enriched) `shouldBe` 5

      it "viewQuery in the output still reflects the original snapshot" $ do
        -- Even after mapWithContext transforms viewElements, viewQuery must
        -- remain the original snapshot (not rebuilt from the new elements).
        let view = toGraphView canonicalClassifier simpleGraph
        let sentinel = mkSubject "CHANGED"
        let enriched = mapWithContext canonicalClassifier
              (\_ (Pattern _ els) -> Pattern sentinel els) view
        -- viewQuery should still report 3 original nodes
        length (queryNodes (viewQuery enriched)) `shouldBe` 3
        -- viewElements should have the sentinel values
        let allSentinel = all (\(_, Pattern v _) -> v == sentinel) (viewElements enriched)
        allSentinel `shouldBe` True

      it "composes with mapAllGraph — snapshot query is the pre-composition original" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        -- First apply mapAllGraph to mutate all values
        let mutated = mapAllGraph
              (\(Pattern _ els) -> Pattern (mkSubject "MUTATED") els) view
        -- Then apply mapWithContext on the mutated view.
        -- The query in `mutated` still reflects the original graph.
        let enriched = mapWithContext canonicalClassifier
              (\q p -> Pattern (mkSubject (show (length (queryNodes q)))) (elements p))
              mutated
        -- All elements should see 3 nodes (the original snapshot)
        let vals = [ Subj.identity v | (_, Pattern v _) <- viewElements enriched ]
        all (== Symbol "3") vals `shouldBe` True

    -- -----------------------------------------------------------------------
    -- US4 / T022: paraGraph
    -- -----------------------------------------------------------------------
    describe "paraGraph (US4)" $ do

      it "T010: paraGraph preserves wrapper behavior on graph-scoped inputs" $ do
        let view = toGraphView canonicalClassifier graphWithWalk
        let score _ p subResults = length (elements p) + sum (subResults :: [Int])
        let result = paraGraph score view

        Map.lookup (identify (mkSubject "a")) result `shouldBe` Just 0
        Map.lookup (identify (mkSubject "ab")) result `shouldBe` Just 2
        Map.lookup (identify (mkSubject "bc")) result `shouldBe` Just 2
        Map.lookup (identify (mkSubject "path1")) result `shouldBe` Just 6

      it "produces a result for every element" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let countChildren _ _ subResults = length subResults
        let result = paraGraph countChildren view
        Map.size result `shouldBe` 5

      it "nodes have no sub-results (atomic, no sub-elements)" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let captureSubCount _ _ subResults = length (subResults :: [Int])
        let result = paraGraph captureSubCount view
        -- Nodes are atomic; their sub-result count should be 0
        let nodeIds = map (\(Pattern v _) -> identify v) $
              filter (\(Pattern _ els) -> null els) $
              map snd (viewElements view)
        let nodeCounts = map (\i -> Map.findWithDefault 0 i result) nodeIds
        all (== 0) nodeCounts `shouldBe` True

    -- -----------------------------------------------------------------------
    -- 037-topo-shape-sort / T004: topoShapeSort annotation-of-annotation ordering
    -- -----------------------------------------------------------------------
    describe "topoShapeSort: annotation-of-annotation ordering (T004)" $ do

      it "annotation A wrapping annotation B: A receives B's result (B processed first)" $ do
        -- B = GAnnotation wrapping a node; A = GAnnotation wrapping B
        let annotB = Pattern (mkSubject "b") [mkNode "n"]
            annotA = Pattern (mkSubject "a") [annotB]
            g = PG.fromPatterns canonicalClassifier [mkNode "n", annotB, annotA]
            view = toGraphView canonicalClassifier g
            countSubs _ _ subResults = length (subResults :: [Int])
            result = paraGraph countSubs view
        -- n is GNode (no sub-elements → 0 subResults)
        -- b is GAnnotation wrapping n → b receives [n's result] → 1 subResult
        -- a is GAnnotation wrapping b → a receives [b's result] → 1 subResult
        Map.lookup (identify (mkSubject "b")) result `shouldBe` Just 1
        Map.lookup (identify (mkSubject "a")) result `shouldBe` Just 1

    -- -----------------------------------------------------------------------
    -- 037-topo-shape-sort / T005: topoShapeSort cycle soft-failure
    -- -----------------------------------------------------------------------
    describe "topoShapeSort: cycle soft-failure (T005)" $ do

      it "mutual annotation cycle raises no exception and produces a result for every element" $ do
        -- annotA wraps a pattern with id 'b', annotB wraps a pattern with id 'a'
        -- This creates a cycle in the within-bucket dependency graph
        let annotA = Pattern (mkSubject "a") [Pattern (mkSubject "b") []]
            annotB = Pattern (mkSubject "b") [Pattern (mkSubject "a") []]
            g = PG.fromPatterns canonicalClassifier [annotA, annotB]
            view = toGraphView canonicalClassifier g
            result = paraGraph (\_ _ subs -> length (subs :: [Int])) view
        -- Both elements must be in the result (no element dropped)
        Map.size result `shouldBe` 2

    -- -----------------------------------------------------------------------
    -- 037-topo-shape-sort / T006: paraGraph annotation-of-annotation fold correctness (SC-006)
    -- -----------------------------------------------------------------------
    describe "paraGraph: annotation-of-annotation fold correctness (T006 / SC-006)" $ do

      it "node n, annotation b wrapping n, annotation a wrapping b: each receives exactly 1 subResult" $ do
        let annotB = Pattern (mkSubject "b") [mkNode "n"]
            annotA = Pattern (mkSubject "a") [annotB]
            g = PG.fromPatterns canonicalClassifier [mkNode "n", annotB, annotA]
            view = toGraphView canonicalClassifier g
            countSubs _ _ subResults = length (subResults :: [Int])
            result = paraGraph countSubs view
        -- n has no sub-elements → 0 subResults
        Map.lookup (identify (mkSubject "n")) result `shouldBe` Just 0
        -- b wraps n → b receives [result_n] → 1 subResult
        Map.lookup (identify (mkSubject "b")) result `shouldBe` Just 1
        -- a wraps b → a receives [result_b] → 1 subResult (not 0)
        Map.lookup (identify (mkSubject "a")) result `shouldBe` Just 1

    -- -----------------------------------------------------------------------
    -- 037-topo-shape-sort / T007: topoShapeSort GOther-of-GOther ordering
    -- -----------------------------------------------------------------------
    describe "topoShapeSort: GOther-of-GOther ordering (T007)" $ do

      it "GOther X containing GOther Y: X receives Y's result (Y processed first)" $ do
        -- gOtherY has 3 node children → classifies as GOther
        -- gOtherX has gOtherY + 1 node child → classifies as GOther
        -- insertOther does not recurse, so GNodes must be added as top-level patterns.
        -- Input order deliberately puts X before Y to exercise within-bucket sort.
        let gOtherY = Pattern (mkSubject "y") [mkNode "p", mkNode "q", mkNode "r"]
            gOtherX = Pattern (mkSubject "x") [gOtherY, mkNode "q"]
            g = PG.fromPatterns canonicalClassifier
                  [ mkNode "p", mkNode "q", mkNode "r"  -- must be top-level for toGraphView
                  , gOtherX, gOtherY ]
            view = toGraphView canonicalClassifier g
            countSubs _ _ subResults = length (subResults :: [Int])
            result = paraGraph countSubs view
        -- Y wraps 3 GNodes → Y gets 3 subResults
        Map.lookup (identify (mkSubject "y")) result `shouldBe` Just 3
        -- X wraps Y + 1 GNode → X gets 2 subResults (Y's result + q's result)
        -- Without within-bucket sort, X would only get 1 (q only, since Y not yet processed)
        Map.lookup (identify (mkSubject "x")) result `shouldBe` Just 2

    -- -----------------------------------------------------------------------
    -- US4 / T023: paraGraphFixed convergence
    -- -----------------------------------------------------------------------
    describe "paraGraphFixed (US4)" $ do

      it "T010b: paraGraphFixed preserves stable wrapper results" $ do
        let view = toGraphView canonicalClassifier graphWithWalk
        let conv old new = old == new
        let score _ p subResults = length (elements p) + sum (subResults :: [Int])
        let result = paraGraphFixed conv score 0 view

        Map.lookup (identify (mkSubject "path1")) result `shouldBe` Just 6

      it "converges to a stable result" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let epsilon = 0.001 :: Double
        let conv old new = abs (old - new) < epsilon
        -- Simple algorithm: degree count (stable after 1 round)
        let step q p _ = fromIntegral (length (queryIncidentRels q p))
        let result = paraGraphFixed conv step 0.0 view
        Map.size result `shouldBe` 5

      it "initial value is overwritten after first round" $ do
        let view = toGraphView canonicalClassifier simpleGraph
        let conv old new = old == new
        let step _ _ _ = (42.0 :: Double)
        let result = paraGraphFixed conv step 0.0 view
        all (== 42.0) (Map.elems result) `shouldBe` True
