-- | Contract: Pattern.Graph.Algorithms
--
-- This file defines the complete public API for the Algorithms module.
-- It is a specification artifact — not compiled code.
--
-- Module: Pattern.Graph.Algorithms
-- All functions accept 'GraphQuery v' as their first argument.
-- Traversal algorithms also accept 'TraversalWeight v'.

module Pattern.Graph.Algorithms
  ( -- * Traversal
    bfs
  , dfs

    -- * Paths
  , shortestPath
  , hasPath
  , allPaths

    -- * Boolean queries
  , isNeighbor
  , isConnected

    -- * Structural
  , connectedComponents
  , topologicalSort
  , hasCycle

    -- * Spanning
  , minimumSpanningTree

    -- * Centrality
  , degreeCentrality
  , betweennessCentrality

    -- * Context query helpers
  , queryAnnotationsOf
  , queryWalksContaining
  , queryCoMembers
  ) where

import Pattern.Core              (Pattern(..))
import Pattern.Graph.GraphClassifier (GraphValue(..), GraphClassifier)
import Pattern.Graph.GraphQuery  (GraphQuery, TraversalWeight)
import Data.Map.Strict           (Map)

-- ---------------------------------------------------------------------------
-- Traversal
-- ---------------------------------------------------------------------------

-- | Breadth-first search from a starting node.
--
-- Returns all nodes reachable from the starting node, in BFS order,
-- respecting the supplied 'TraversalWeight' (infinite-cost edges are not traversed).
--
-- == Complexity
-- O((n + r) log n) where n = nodes, r = relationships
bfs :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]

-- | Depth-first search from a starting node.
--
-- Returns all nodes reachable from the starting node, in DFS order,
-- respecting the supplied 'TraversalWeight'.
--
-- == Complexity
-- O((n + r) log n)
dfs :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]

-- ---------------------------------------------------------------------------
-- Paths
-- ---------------------------------------------------------------------------

-- | Find the shortest (minimum total weight) path between two nodes.
--
-- Returns 'Just' a sequence of nodes if a path exists, 'Nothing' otherwise.
-- Uses Dijkstra's algorithm; assumes non-negative weights.
--
-- == Complexity
-- O((n + r) log n)
shortestPath :: Ord (Id v) => GraphQuery v -> TraversalWeight v
             -> Pattern v -> Pattern v -> Maybe [Pattern v]

-- | Determine whether any path exists between two nodes.
--
-- Returns 'True' if there is at least one traversable path from start to end.
--
-- == Complexity
-- O((n + r) log n)
hasPath :: Ord (Id v) => GraphQuery v -> TraversalWeight v
        -> Pattern v -> Pattern v -> Bool

-- | Enumerate all simple paths between two nodes.
--
-- Returns all paths as sequences of nodes. May be exponential in the number
-- of paths; use with care on dense graphs.
--
-- == Note
-- A simple path visits each node at most once.
allPaths :: Ord (Id v) => GraphQuery v -> TraversalWeight v
         -> Pattern v -> Pattern v -> [[Pattern v]]

-- ---------------------------------------------------------------------------
-- Boolean queries
-- ---------------------------------------------------------------------------

-- | Determine whether two nodes are direct neighbors.
--
-- Returns 'True' if there exists a relationship between the two nodes
-- that is traversable in the given direction (i.e. finite cost).
isNeighbor :: Eq (Id v) => GraphQuery v -> TraversalWeight v
           -> Pattern v -> Pattern v -> Bool

-- | Determine whether the entire graph is connected.
--
-- Returns 'True' if every node is reachable from every other node
-- under the supplied 'TraversalWeight'.
isConnected :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Bool

-- ---------------------------------------------------------------------------
-- Structural
-- ---------------------------------------------------------------------------

-- | Find all connected components.
--
-- Returns a list of node groups where each group contains all nodes
-- mutually reachable under the supplied 'TraversalWeight'.
--
-- == Complexity
-- O((n + r) log n)
connectedComponents :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> [[Pattern v]]

-- | Topological sort of the graph.
--
-- Returns 'Just' a linear ordering of nodes such that for every directed
-- relationship u→v, u appears before v. Returns 'Nothing' if the graph
-- contains a cycle.
--
-- Note: does not take 'TraversalWeight' — operates on the directed structure
-- implied by relationship endpoint order.
--
-- == Complexity
-- O(n + r)
topologicalSort :: Ord (Id v) => GraphQuery v -> Maybe [Pattern v]

-- | Determine whether the graph contains a cycle.
--
-- Returns 'True' if any directed cycle exists in the graph.
-- Does not take 'TraversalWeight' — operates on directed endpoint order.
--
-- == Complexity
-- O(n + r)
hasCycle :: Ord (Id v) => GraphQuery v -> Bool

-- ---------------------------------------------------------------------------
-- Spanning
-- ---------------------------------------------------------------------------

-- | Compute a minimum spanning tree (or forest).
--
-- Returns the set of relationships forming a spanning tree with minimum
-- total weight. Uses Kruskal's or Prim's algorithm.
--
-- == Complexity
-- O(r log r)
minimumSpanningTree :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> [Pattern v]

-- ---------------------------------------------------------------------------
-- Centrality
-- ---------------------------------------------------------------------------

-- | Compute degree centrality for all nodes.
--
-- Returns a map from node identity to normalized degree centrality score.
-- Degree centrality = degree(n) / (|nodes| - 1).
--
-- == Complexity
-- O(n + r)
degreeCentrality :: Ord (Id v) => GraphQuery v -> Map (Id v) Double

-- | Compute betweenness centrality for all nodes.
--
-- Returns a map from node identity to betweenness centrality score.
-- Betweenness centrality = fraction of shortest paths passing through each node.
--
-- == Complexity
-- O(n * (n + r) log n) — Brandes algorithm
betweennessCentrality :: Ord (Id v) => GraphQuery v -> TraversalWeight v
                      -> Map (Id v) Double

-- ---------------------------------------------------------------------------
-- Context query helpers
-- ---------------------------------------------------------------------------

-- | All annotations attached to a given element.
--
-- Calls 'queryContainers' and filters results classified as 'GAnnotation'
-- by the supplied 'GraphClassifier'. No new 'GraphQuery' fields required.
queryAnnotationsOf :: GraphClassifier extra v -> GraphQuery v
                   -> Pattern v -> [Pattern v]

-- | All walks that contain a given element (directly or via its relationships).
--
-- Calls 'queryContainers' and filters results classified as 'GWalk'.
queryWalksContaining :: GraphClassifier extra v -> GraphQuery v
                     -> Pattern v -> [Pattern v]

-- | All elements sharing a specific container with a given element.
--
-- Returns elements that are co-members of the given container pattern.
queryCoMembers :: GraphQuery v -> Pattern v -> Pattern v -> [Pattern v]
