-- | Contract: Pattern.Graph.GraphQuery
--
-- This file defines the complete public API for the GraphQuery module.
-- It is a specification artifact — not compiled code.
--
-- Module: Pattern.Graph.GraphQuery
-- Exposes: GraphQuery, TraversalDirection, TraversalWeight,
--          fromGraphLens, fromPatternGraph,
--          undirected, directed, directedReverse,
--          frameQuery, memoizeIncidentRels

module Pattern.Graph.GraphQuery
  ( -- * Core types
    GraphQuery(..)
  , TraversalDirection(..)
  , TraversalWeight

    -- * Canonical traversal weights
  , undirected
  , directed
  , directedReverse

    -- * Constructors
  , fromGraphLens
  , fromPatternGraph

    -- * Combinators
  , frameQuery
  , memoizeIncidentRels
  ) where

import Pattern.Core              (Pattern(..))
import Pattern.Graph             (GraphLens)
import Pattern.Graph.GraphClassifier (GraphValue(..))
import Pattern.PatternGraph      (PatternGraph)
import Data.Map.Strict           (Map)

-- ---------------------------------------------------------------------------
-- TraversalDirection
-- ---------------------------------------------------------------------------

-- | The two orientations along a directed relationship.
--
-- 'Forward' follows the relationship from source to target.
-- 'Backward' follows it from target to source.
--
-- == Categorical interpretation
-- An element of a two-element set. Used as a parameter to 'TraversalWeight'
-- to encode directionality as a cost rather than a structural property.
data TraversalDirection = Forward | Backward
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- TraversalWeight
-- ---------------------------------------------------------------------------

-- | A function assigning a traversal cost to each (relationship, direction) pair.
--
-- Encodes both directionality and edge weight as a single concept.
-- Infinity (@1\/0 :: Double@) encodes impassability — traversal is blocked
-- in that direction.
--
-- == Invariants
-- * Must return a non-negative 'Double' (including @+Infinity@).
-- * Negative weights are not supported by the standard Dijkstra-based algorithms.
--
-- == Canonical values
-- * 'undirected'      — uniform cost 1.0, direction ignored
-- * 'directed'        — 1.0 forward, infinity backward
-- * 'directedReverse' — infinity forward, 1.0 backward
type TraversalWeight v = Pattern v -> TraversalDirection -> Double

-- | Undirected, unweighted traversal. Direction is ignored; cost is uniform.
undirected :: TraversalWeight v
undirected _ _ = 1.0

-- | Directed forward-only traversal. Reverse direction is impassable.
directed :: TraversalWeight v
directed _ Forward  = 1.0
directed _ Backward = 1/0

-- | Directed reverse-only traversal. Forward direction is impassable.
directedReverse :: TraversalWeight v
directedReverse _ Forward  = 1/0
directedReverse _ Backward = 1.0

-- ---------------------------------------------------------------------------
-- GraphQuery
-- ---------------------------------------------------------------------------

-- | A record-of-functions abstracting graph traversal and lookup over any
-- graph representation.
--
-- == Categorical interpretation
-- A coalgebra-like structure: given a graph element, the fields produce the
-- elements reachable from it (downward traversal) or the structures containing
-- it (upward traversal via 'queryContainers').
--
-- == Design principles
-- * Representation-independent: works with 'GraphLens', 'PatternGraph',
--   database-backed graphs, or any custom source.
-- * Composable: 'GraphQuery v -> GraphQuery v' transformations (framing,
--   caching, logging) are the extension pattern.
-- * 'queryNeighbors' is intentionally absent: neighbors are direction-dependent.
--   Algorithms derive reachability from 'queryIncidentRels' + 'querySource' +
--   'queryTarget' + a supplied 'TraversalWeight'.
--
-- == Invariants
-- * @querySource r = Just s@ implies @s ∈ queryNodes@
-- * @queryTarget r = Just t@ implies @t ∈ queryNodes@
-- * @r ∈ queryIncidentRels n@ implies @querySource r = Just n ∨ queryTarget r = Just n@
-- * @queryDegree n = length (queryIncidentRels n)@ (default; implementations may be faster)
-- * @queryNodeById (identify (value n)) = Just n@ for all @n ∈ queryNodes@
-- * @queryRelationshipById (identify (value r)) = Just r@ for all @r ∈ queryRelationships@
-- * 'queryContainers' returns only direct containers — not transitive containment
data GraphQuery v = GraphQuery
  { queryNodes            :: [Pattern v]
    -- ^ All node-classified elements in the graph.

  , queryRelationships    :: [Pattern v]
    -- ^ All relationship-classified elements in the graph.

  , queryIncidentRels     :: Pattern v -> [Pattern v]
    -- ^ All relationships where the given node is source or target.
    -- Hot-path function; implementations should apply @{-# INLINE #-}@.

  , querySource           :: Pattern v -> Maybe (Pattern v)
    -- ^ The source (first endpoint) of a relationship.
    -- Returns 'Nothing' if the pattern is not a relationship.
    -- Hot-path function; implementations should apply @{-# INLINE #-}@.

  , queryTarget           :: Pattern v -> Maybe (Pattern v)
    -- ^ The target (second endpoint) of a relationship.
    -- Returns 'Nothing' if the pattern is not a relationship.
    -- Hot-path function; implementations should apply @{-# INLINE #-}@.

  , queryDegree           :: Pattern v -> Int
    -- ^ Count of incident relationships for a node.
    -- Derivable from 'queryIncidentRels' but included explicitly because
    -- implementations may provide O(1) versions (e.g. a degree index).
    -- Hot-path function; implementations should apply @{-# INLINE #-}@.

  , queryNodeById         :: Id v -> Maybe (Pattern v)
    -- ^ Node lookup by identity. O(log n) from 'PatternGraph'; O(n) from 'GraphLens'.

  , queryRelationshipById :: Id v -> Maybe (Pattern v)
    -- ^ Relationship lookup by identity. O(log r) from 'PatternGraph'; O(r) from 'GraphLens'.

  , queryContainers       :: Pattern v -> [Pattern v]
    -- ^ All higher-order structures (relationships, walks, annotations) that
    -- directly contain the given element. The upward traversal dual to downward
    -- decomposition. Required by GraphMutation for coherent deletion; independently
    -- useful for impact analysis and pattern matching.
  }

-- ---------------------------------------------------------------------------
-- Constructors
-- ---------------------------------------------------------------------------

-- | Construct a 'GraphQuery v' from a 'GraphLens v'.
--
-- All fields are derived from existing 'Pattern.Graph' functions.
-- 'queryNodeById' and 'queryRelationshipById' perform O(n) / O(r) scans
-- (no index available from 'GraphLens').
--
-- This is the bridge that allows existing 'GraphLens'-based code to use
-- 'Pattern.Graph.Algorithms' without changes.
fromGraphLens :: (GraphValue v, Eq v) => GraphLens v -> GraphQuery v

-- | Construct a 'GraphQuery v' directly from a 'PatternGraph extra v'.
--
-- Reads from the typed maps (@pgNodes@, @pgRelationships@, @pgWalks@,
-- @pgAnnotations@) without going through 'GraphLens'. Provides O(log n)
-- lookups for 'queryNodeById' and 'queryRelationshipById'.
--
-- Supersedes 'Pattern.PatternGraph.toGraphLens' for algorithm access.
fromPatternGraph :: (GraphValue v, Eq v) => PatternGraph extra v -> GraphQuery v

-- ---------------------------------------------------------------------------
-- Combinators
-- ---------------------------------------------------------------------------

-- | Produce a filtered subgraph view as a 'GraphQuery v'.
--
-- Nodes and relationships not matching the predicate are excluded.
-- 'queryIncidentRels' on the result excludes relationships whose endpoints
-- fall outside the frame.
--
-- All 'GraphQuery' invariants hold on the result.
--
-- == Example
-- @
-- let personFrame = frameQuery isPerson (fromPatternGraph pg)
-- @
frameQuery :: (Pattern v -> Bool) -> GraphQuery v -> GraphQuery v

-- | Wrap 'queryIncidentRels' with a memoization layer keyed by node identity.
--
-- Useful for algorithms that call 'queryIncidentRels' repeatedly on the same
-- node (e.g. betweenness centrality). All other fields are passed through unchanged.
--
-- The memoization is per-'GraphQuery' value, not global.
memoizeIncidentRels :: Ord (Id v) => GraphQuery v -> GraphQuery v
