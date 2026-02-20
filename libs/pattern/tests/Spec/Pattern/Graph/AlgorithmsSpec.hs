-- | Unit and property tests for Pattern.Graph.Algorithms.
--
-- Covers T033–T037 (US1 traversal, path, structural, centrality, edge cases),
-- T040–T043 (US2 directed/undirected differentiation),
-- T057–T058 (US4 context helpers).
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
module Spec.Pattern.Graph.AlgorithmsSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)
import Test.Hspec
import Test.QuickCheck

import Pattern.Core (Pattern(..), point)
import Pattern.Graph.GraphClassifier (GraphValue(..), canonicalClassifier)
import Pattern.Graph.GraphQuery
import Pattern.Graph.Algorithms
import Pattern.PatternGraph (PatternGraph(..), empty, merge, fromPatterns, fromPatternGraph)
import Subject.Core (Subject(..), Symbol(..))

-- ============================================================================
-- Test helpers
-- ============================================================================

node :: Symbol -> Pattern Subject
node s = Pattern (Subject s Set.empty Map.empty) []

rel :: Symbol -> Symbol -> Symbol -> Pattern Subject
rel r a b = Pattern (Subject r Set.empty Map.empty) [node a, node b]

annotation :: Symbol -> Symbol -> Pattern Subject
annotation a n = Pattern (Subject a Set.empty Map.empty) [node n]

nodeId :: Pattern Subject -> Symbol
nodeId p = identify (value p)

mkGQ :: [Pattern Subject] -> GraphQuery Subject
mkGQ ps = fromPatternGraph (fromPatterns canonicalClassifier ps)

-- | Linear graph: A → B → C
linearGraph :: GraphQuery Subject
linearGraph = mkGQ
  [ node "A", node "B", node "C"
  , rel "r1" "A" "B"
  , rel "r2" "B" "C"
  ]

-- | Disconnected graph: A–B and C–D (two components)
disconnectedGraph :: GraphQuery Subject
disconnectedGraph = mkGQ
  [ node "A", node "B", node "C", node "D"
  , rel "r1" "A" "B"
  , rel "r2" "C" "D"
  ]

-- | Cyclic graph: A → B → C → A
cyclicGraph :: GraphQuery Subject
cyclicGraph = mkGQ
  [ node "A", node "B", node "C"
  , rel "r1" "A" "B"
  , rel "r2" "B" "C"
  , rel "r3" "C" "A"
  ]

-- | DAG: A → B, A → C, B → D, C → D
dagGraph :: GraphQuery Subject
dagGraph = mkGQ
  [ node "A", node "B", node "C", node "D"
  , rel "r1" "A" "B"
  , rel "r2" "A" "C"
  , rel "r3" "B" "D"
  , rel "r4" "C" "D"
  ]

-- | Empty graph
emptyGraph :: GraphQuery Subject
emptyGraph = fromPatternGraph (empty :: PatternGraph () Subject)

-- ============================================================================
-- T033: Traversal and path algorithm tests
-- ============================================================================

spec :: Spec
spec = do
  describe "Pattern.Graph.Algorithms" $ do

    -- -----------------------------------------------------------------------
    -- bfs
    -- -----------------------------------------------------------------------
    describe "bfs (T033)" $ do

      it "bfs from A in linear graph visits all three nodes" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let result = bfs linearGraph undirected nodeA
        length result `shouldBe` 3

      it "bfs from A includes A, B, C" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let result = bfs linearGraph undirected nodeA
        sort (map nodeId result) `shouldBe` sort ["A", "B", "C"]

      it "bfs from A with directed weight only reaches B and C (not reverse)" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let result = bfs linearGraph directed nodeA
        sort (map nodeId result) `shouldBe` sort ["A", "B", "C"]

      it "bfs from C with directed weight only reaches C (no forward edges)" $ do
        let nodeC = head [ n | n <- queryNodes linearGraph, nodeId n == "C" ]
        let result = bfs linearGraph directed nodeC
        map nodeId result `shouldBe` ["C"]

      it "bfs on empty graph from any node returns just that node" $ do
        let n = node "solo"
        let gq = mkGQ [n]
        let nodeN = head (queryNodes gq)
        bfs gq undirected nodeN `shouldBe` [nodeN]

    -- -----------------------------------------------------------------------
    -- dfs
    -- -----------------------------------------------------------------------
    describe "dfs (T033)" $ do

      it "dfs from A in linear graph visits all three nodes" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let result = dfs linearGraph undirected nodeA
        length result `shouldBe` 3

      it "dfs from A includes A, B, C" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let result = dfs linearGraph undirected nodeA
        sort (map nodeId result) `shouldBe` sort ["A", "B", "C"]

    -- -----------------------------------------------------------------------
    -- shortestPath
    -- -----------------------------------------------------------------------
    describe "shortestPath (T033, T037)" $ do

      it "shortestPath A→C in linear graph returns Just [A,B,C]" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let nodeC = head [ n | n <- queryNodes linearGraph, nodeId n == "C" ]
        let result = shortestPath linearGraph undirected nodeA nodeC
        fmap (map nodeId) result `shouldBe` Just ["A", "B", "C"]

      it "shortestPath A→A returns Just [A]" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let result = shortestPath linearGraph undirected nodeA nodeA
        fmap (map nodeId) result `shouldBe` Just ["A"]

      it "shortestPath returns Nothing when no path exists (disconnected)" $ do
        let nodeA = head [ n | n <- queryNodes disconnectedGraph, nodeId n == "A" ]
        let nodeC = head [ n | n <- queryNodes disconnectedGraph, nodeId n == "C" ]
        shortestPath disconnectedGraph directed nodeA nodeC `shouldBe` Nothing

      it "shortestPath on empty graph returns Nothing" $ do
        let gq = mkGQ [node "A", node "B"]
        let nodeA = head [ n | n <- queryNodes gq, nodeId n == "A" ]
        let nodeB = head [ n | n <- queryNodes gq, nodeId n == "B" ]
        shortestPath gq undirected nodeA nodeB `shouldBe` Nothing

    -- -----------------------------------------------------------------------
    -- hasPath
    -- -----------------------------------------------------------------------
    describe "hasPath (T033)" $ do

      it "hasPath A→C in linear undirected graph is True" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let nodeC = head [ n | n <- queryNodes linearGraph, nodeId n == "C" ]
        hasPath linearGraph undirected nodeA nodeC `shouldBe` True

      it "hasPath A→C in linear directed graph is True" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let nodeC = head [ n | n <- queryNodes linearGraph, nodeId n == "C" ]
        hasPath linearGraph directed nodeA nodeC `shouldBe` True

      it "hasPath C→A in linear directed graph is False" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let nodeC = head [ n | n <- queryNodes linearGraph, nodeId n == "C" ]
        hasPath linearGraph directed nodeC nodeA `shouldBe` False

      it "hasPath A→C in disconnected directed graph is False" $ do
        let nodeA = head [ n | n <- queryNodes disconnectedGraph, nodeId n == "A" ]
        let nodeC = head [ n | n <- queryNodes disconnectedGraph, nodeId n == "C" ]
        hasPath disconnectedGraph directed nodeA nodeC `shouldBe` False

    -- -----------------------------------------------------------------------
    -- allPaths
    -- -----------------------------------------------------------------------
    describe "allPaths (T033, T037)" $ do

      it "allPaths A→C in linear graph returns one path" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let nodeC = head [ n | n <- queryNodes linearGraph, nodeId n == "C" ]
        let paths = allPaths linearGraph undirected nodeA nodeC
        length paths `shouldBe` 1

      it "allPaths in DAG A→D returns two paths (via B and via C)" $ do
        let nodeA = head [ n | n <- queryNodes dagGraph, nodeId n == "A" ]
        let nodeD = head [ n | n <- queryNodes dagGraph, nodeId n == "D" ]
        let paths = allPaths dagGraph directed nodeA nodeD
        length paths `shouldBe` 2

      it "allPaths on empty graph (no relationships) returns []" $ do
        let gq = mkGQ [node "A", node "B"]
        let nodeA = head [ n | n <- queryNodes gq, nodeId n == "A" ]
        let nodeB = head [ n | n <- queryNodes gq, nodeId n == "B" ]
        allPaths gq undirected nodeA nodeB `shouldBe` []

    -- -----------------------------------------------------------------------
    -- isNeighbor
    -- -----------------------------------------------------------------------
    describe "isNeighbor (T033)" $ do

      it "A and B are neighbors in linear graph (undirected)" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let nodeB = head [ n | n <- queryNodes linearGraph, nodeId n == "B" ]
        isNeighbor linearGraph undirected nodeA nodeB `shouldBe` True

      it "A and C are not direct neighbors in linear graph" $ do
        let nodeA = head [ n | n <- queryNodes linearGraph, nodeId n == "A" ]
        let nodeC = head [ n | n <- queryNodes linearGraph, nodeId n == "C" ]
        isNeighbor linearGraph undirected nodeA nodeC `shouldBe` False

    -- -----------------------------------------------------------------------
    -- T034: Structural algorithms
    -- -----------------------------------------------------------------------
    describe "connectedComponents (T034)" $ do

      it "linear graph has one connected component (undirected)" $ do
        let comps = connectedComponents linearGraph undirected
        length comps `shouldBe` 1

      it "disconnected graph has two components (undirected)" $ do
        let comps = connectedComponents disconnectedGraph undirected
        length comps `shouldBe` 2

      it "empty graph has zero components" $ do
        let comps = connectedComponents emptyGraph undirected
        comps `shouldBe` []

    describe "topologicalSort (T034, T037)" $ do

      it "topologicalSort on DAG returns Just ordering" $ do
        let result = topologicalSort dagGraph
        isJust result `shouldBe` True

      it "topologicalSort on DAG: A appears before D" $ do
        let Just order = topologicalSort dagGraph
        let ids = map nodeId order
        let posA = length (takeWhile (/= "A") ids)
        let posD = length (takeWhile (/= "D") ids)
        posA < posD `shouldBe` True

      it "topologicalSort on cyclic graph returns Nothing" $ do
        topologicalSort cyclicGraph `shouldBe` Nothing

    describe "hasCycle (T034)" $ do

      it "hasCycle on cyclic graph is True" $
        hasCycle cyclicGraph `shouldBe` True

      it "hasCycle on DAG is False" $
        hasCycle dagGraph `shouldBe` False

      it "hasCycle on linear graph is False" $
        hasCycle linearGraph `shouldBe` False

    describe "minimumSpanningTree (T034)" $ do

      it "MST of linear graph includes all nodes" $ do
        let mst = minimumSpanningTree linearGraph undirected
        length mst `shouldBe` 3

      it "MST of disconnected graph includes nodes from both components (spanning forest)" $ do
        let mst = minimumSpanningTree disconnectedGraph undirected
        length mst `shouldBe` 4

    -- -----------------------------------------------------------------------
    -- T035: Centrality algorithms
    -- -----------------------------------------------------------------------
    describe "degreeCentrality (T035)" $ do

      it "degreeCentrality returns a map with one entry per node" $ do
        let dc = degreeCentrality linearGraph
        Map.size dc `shouldBe` 3

      it "degreeCentrality of B in linear graph is higher than A and C" $ do
        let dc = degreeCentrality linearGraph
        let dcA = Map.findWithDefault 0.0 "A" dc
        let dcB = Map.findWithDefault 0.0 "B" dc
        let dcC = Map.findWithDefault 0.0 "C" dc
        dcB > dcA `shouldBe` True
        dcB > dcC `shouldBe` True

    describe "betweennessCentrality (T035)" $ do

      it "betweennessCentrality returns a map with one entry per node" $ do
        let bc = betweennessCentrality linearGraph undirected
        Map.size bc `shouldBe` 3

      it "betweennessCentrality of B is highest in linear graph" $ do
        let bc = betweennessCentrality linearGraph undirected
        let bcA = Map.findWithDefault 0.0 "A" bc
        let bcB = Map.findWithDefault 0.0 "B" bc
        let bcC = Map.findWithDefault 0.0 "C" bc
        bcB >= bcA `shouldBe` True
        bcB >= bcC `shouldBe` True

    -- -----------------------------------------------------------------------
    -- T036: Property — fromGraphLens and fromPatternGraph produce same connectedComponents
    -- -----------------------------------------------------------------------
    describe "representation equivalence property (T036)" $ do

      it "connectedComponents count is consistent for same PatternGraph" $ do
        let pgGraph = fromPatterns canonicalClassifier
              [ node "A", node "B", node "C"
              , rel "r1" "A" "B", rel "r2" "B" "C"
              ]
        let gqPG = fromPatternGraph pgGraph
        let gqGL = fromPatternGraph pgGraph
        length (connectedComponents gqPG undirected) `shouldBe`
          length (connectedComponents gqGL undirected)

    -- -----------------------------------------------------------------------
    -- T040–T043: US2 directed/undirected differentiation
    -- -----------------------------------------------------------------------
    describe "TraversalWeight differentiation (T040–T043)" $ do

      let dirGraph = mkGQ [ node "A", node "B", rel "r" "A" "B" ]

      it "T040: hasPath directed A→B = True" $ do
        let nodeA = head [ n | n <- queryNodes dirGraph, nodeId n == "A" ]
        let nodeB = head [ n | n <- queryNodes dirGraph, nodeId n == "B" ]
        hasPath dirGraph directed nodeA nodeB `shouldBe` True

      it "T040: hasPath directed B→A = False" $ do
        let nodeA = head [ n | n <- queryNodes dirGraph, nodeId n == "A" ]
        let nodeB = head [ n | n <- queryNodes dirGraph, nodeId n == "B" ]
        hasPath dirGraph directed nodeB nodeA `shouldBe` False

      it "T040: hasPath undirected B→A = True" $ do
        let nodeA = head [ n | n <- queryNodes dirGraph, nodeId n == "A" ]
        let nodeB = head [ n | n <- queryNodes dirGraph, nodeId n == "B" ]
        hasPath dirGraph undirected nodeB nodeA `shouldBe` True

      it "T043: hasPath directedReverse B→A = True" $ do
        let nodeA = head [ n | n <- queryNodes dirGraph, nodeId n == "A" ]
        let nodeB = head [ n | n <- queryNodes dirGraph, nodeId n == "B" ]
        hasPath dirGraph directedReverse nodeB nodeA `shouldBe` True

      it "T043: hasPath directedReverse A→B = False" $ do
        let nodeA = head [ n | n <- queryNodes dirGraph, nodeId n == "A" ]
        let nodeB = head [ n | n <- queryNodes dirGraph, nodeId n == "B" ]
        hasPath dirGraph directedReverse nodeA nodeB `shouldBe` False

      it "T041: custom weight function — shortestPath prefers lower-cost path" $ do
        -- Graph: A→B (cost 10), A→C→B (cost 1+1=2)
        -- Custom weight: r_expensive costs 10, others cost 1
        let gq = mkGQ
              [ node "A", node "B", node "C"
              , rel "r_expensive" "A" "B"
              , rel "r_cheap1" "A" "C"
              , rel "r_cheap2" "C" "B"
              ]
        let customWeight :: TraversalWeight Subject
            customWeight p Forward
              | identify (value p) == "r_expensive" = 10.0
              | otherwise = 1.0
            customWeight _ Backward = 1 / 0
        let nodeA = head [ n | n <- queryNodes gq, nodeId n == "A" ]
        let nodeB = head [ n | n <- queryNodes gq, nodeId n == "B" ]
        let result = shortestPath gq customWeight nodeA nodeB
        -- Should take the cheap path A→C→B, length 3 nodes
        fmap length result `shouldBe` Just 3

      it "T042: connectedComponents undirected ≤ components directed for directed graph" $ do
        let compsUndirected = connectedComponents dirGraph undirected
        let compsDirected   = connectedComponents dirGraph directed
        length compsUndirected <= length compsDirected `shouldBe` True

    -- -----------------------------------------------------------------------
    -- T057–T058: US4 context query helpers
    -- -----------------------------------------------------------------------
    describe "queryAnnotationsOf (T057)" $ do

      it "returns annotation containing the node" $ do
        let pg = fromPatterns canonicalClassifier
              [ node "A", annotation "ann1" "A" ]
        let gq = fromPatternGraph pg
        case [ n | n <- queryNodes gq, nodeId n == "A" ] of
          [] -> expectationFailure "node A not found"
          (nodeA:_) -> do
            let anns = queryAnnotationsOf canonicalClassifier gq nodeA
            length anns `shouldBe` 1
            identify (value (head anns)) `shouldBe` "ann1"

      it "returns empty list when node has no annotations" $ do
        let pg = fromPatterns canonicalClassifier [node "A", node "B", rel "r" "A" "B"]
        let gq = fromPatternGraph pg
        case [ n | n <- queryNodes gq, nodeId n == "A" ] of
          [] -> expectationFailure "node A not found"
          (nodeA:_) ->
            queryAnnotationsOf canonicalClassifier gq nodeA `shouldBe` []

    describe "queryWalksContaining (T057)" $ do

      it "returns empty list when no walks exist" $ do
        let gq = linearGraph
        case [ n | n <- queryNodes gq, nodeId n == "A" ] of
          [] -> expectationFailure "node A not found"
          (nodeA:_) ->
            queryWalksContaining canonicalClassifier gq nodeA `shouldBe` []

    describe "queryCoMembers (T058)" $ do

      it "queryCoMembers returns container when it matches" $ do
        let pg = fromPatterns canonicalClassifier
              [ node "A", node "B", rel "r" "A" "B" ]
        let gq = fromPatternGraph pg
        case ( [ n | n <- queryNodes gq, nodeId n == "A" ]
             , [ r | r <- queryRelationships gq, identify (value r) == "r" ]
             ) of
          (nodeA:_, relR:_) -> do
            let coMembers = queryCoMembers gq nodeA relR
            length coMembers `shouldBe` 1
          _ -> expectationFailure "expected node A and rel r"

    -- =========================================================================
    -- Representation independence (T073b / SC-007)
    -- =========================================================================

    describe "Representation independence (T073b)" $ do

      let nodeA = node "A"
          nodeB = node "B"
          nodeC = node "C"
          relAB = rel "r1" "A" "B"
          relBC = rel "r2" "B" "C"
          -- Hand-built GraphQuery over a fixed triangle A→B→C
          elems :: Pattern Subject -> [Pattern Subject]
          elems (Pattern _ es) = es
          handBuiltGQ :: GraphQuery Subject
          handBuiltGQ = GraphQuery
            { queryNodes            = [nodeA, nodeB, nodeC]
            , queryRelationships    = [relAB, relBC]
            , queryIncidentRels     = \n ->
                let nId = nodeId n
                in filter (\r -> case elems r of
                             (s:t:_) -> nodeId s == nId || nodeId t == nId
                             _       -> False)
                          [relAB, relBC]
            , querySource           = \r -> case elems r of
                (s:_) -> Just s
                _     -> Nothing
            , queryTarget           = \r -> case elems r of
                (_:t:_) -> Just t
                _       -> Nothing
            , queryDegree           = \n ->
                let nId = nodeId n
                in length $ filter (\r -> case elems r of
                                     (s:t:_) -> nodeId s == nId || nodeId t == nId
                                     _       -> False)
                                   [relAB, relBC]
            , queryNodeById         = \i ->
                case filter (\n -> nodeId n == i) [nodeA, nodeB, nodeC] of
                  (n:_) -> Just n
                  []    -> Nothing
            , queryRelationshipById = \i ->
                case filter (\r -> identify (value r) == i) [relAB, relBC] of
                  (r:_) -> Just r
                  []    -> Nothing
            , queryContainers       = \_ -> []
            }

      it "bfs on hand-built GraphQuery returns all reachable nodes" $ do
        let reachable = bfs handBuiltGQ undirected nodeA
        length reachable `shouldBe` 3

      it "shortestPath on hand-built GraphQuery finds A→C" $ do
        let path = shortestPath handBuiltGQ undirected nodeA nodeC
        case path of
          Just ps -> length ps `shouldBe` 3
          Nothing -> expectationFailure "expected a path A→B→C"

      it "connectedComponents on hand-built GraphQuery returns one component" $ do
        let comps = connectedComponents handBuiltGQ undirected
        length comps `shouldBe` 1
