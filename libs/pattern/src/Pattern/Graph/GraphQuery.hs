-- | GraphQuery: Portable, composable graph query interface.
--
-- This module provides 'GraphQuery v', a record-of-functions that abstracts
-- over any graph representation. Algorithms in "Pattern.Graph.Algorithms"
-- accept 'GraphQuery v' and are therefore independent of whether the
-- underlying graph is a 'GraphLens' or a 'PatternGraph'.
--
-- == Categorical Interpretation
--
-- 'GraphQuery v' is a coalgebra: given a graph element, it produces the
-- elements reachable from it. 'queryContainers' is the upward dual — given
-- an element, it produces the structures that contain it. Together they form
-- a bidirectional traversal interface.
--
-- == Design Principles
--
-- 1. Record-of-functions (not a typeclass) — consistent with 'GraphClassifier'.
-- 2. 'TraversalWeight' is a call-site parameter, not part of the interface.
-- 3. Combinators ('frameQuery', 'memoizeIncidentRels') are plain functions.
--
-- == Example
--
-- > import Pattern.Graph.GraphQuery (fromGraphLens, directed)
-- > import Pattern.PatternGraph (fromPatternGraph)
-- > import qualified Pattern.Graph.Algorithms as Alg
-- >
-- > -- From a PatternGraph (O(log n) lookups):
-- > let gq = fromPatternGraph myPatternGraph
-- > let path = Alg.shortestPath gq directed nodeA nodeB
-- >
-- > -- From a GraphLens (O(n) lookups):
-- > let gq2 = fromGraphLens myLens
-- > let comps = Alg.connectedComponents gq2 undirected
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Pattern.Graph.GraphQuery
  ( -- * Traversal types
    TraversalDirection(..)
  , TraversalWeight
  , undirected
  , directed
  , directedReverse
    -- * GraphQuery interface
  , GraphQuery(..)
    -- * Constructors
  , fromGraphLens
    -- Note: 'fromPatternGraph' is defined in "Pattern.PatternGraph" to avoid
    -- a circular import (GraphQuery → PatternGraph → Graph → GraphQuery).
    -- Import it from "Pattern.PatternGraph" directly.
    -- * Combinators
  , frameQuery
  , memoizeIncidentRels
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Pattern.Core (Pattern(..))
import Pattern.Graph (GraphLens(..))
import qualified Pattern.Graph as G
import Pattern.Graph.GraphClassifier (GraphValue(..))

-- Note: 'fromPatternGraph' is defined in "Pattern.PatternGraph" to avoid
-- a circular import cycle. The Map imports are kept for memoizeIncidentRels.

-- ============================================================================
-- TraversalDirection
-- ============================================================================

-- | The two orientations along a directed relationship.
--
-- 'Forward' follows the relationship from source to target;
-- 'Backward' follows it from target to source.
data TraversalDirection = Forward | Backward
  deriving (Eq, Show)

-- ============================================================================
-- TraversalWeight
-- ============================================================================

-- | A function assigning a traversal cost to each (relationship, direction) pair.
--
-- Infinity (@1\/0 :: Double@) encodes impassability — traversal is blocked in
-- that direction. Non-negative values encode traversal cost. Negative weights
-- are not supported by the standard Dijkstra-based algorithms.
--
-- Canonical values: 'undirected', 'directed', 'directedReverse'.
type TraversalWeight v = Pattern v -> TraversalDirection -> Double

-- | Uniform cost in both directions. Direction is ignored.
undirected :: TraversalWeight v
undirected _ _ = 1.0

-- | Forward traversal only. Backward traversal is impassable.
directed :: TraversalWeight v
directed _ Forward  = 1.0
directed _ Backward = 1 / 0

-- | Backward traversal only. Forward traversal is impassable.
directedReverse :: TraversalWeight v
directedReverse _ Forward  = 1 / 0
directedReverse _ Backward = 1.0

-- ============================================================================
-- GraphQuery
-- ============================================================================

-- | A record-of-functions representing a graph query interface.
--
-- Construct via 'fromGraphLens' or 'fromPatternGraph'. Compose with
-- 'frameQuery' and 'memoizeIncidentRels'.
--
-- == Performance note (Haskell-specific)
--
-- The hot-path fields ('queryIncidentRels', 'querySource', 'queryTarget',
-- 'queryDegree') are function-typed. GHC will inline their applications at
-- call sites when the 'GraphQuery' value is known statically; use
-- @{-# INLINE #-}@ on algorithms that receive 'GraphQuery' as a parameter
-- to encourage this. @{-# UNPACK #-}@ does not apply here because all fields
-- are either function types or boxed list types — neither can be unboxed.
--
-- == Invariants
--
-- * @querySource r = Just s@ implies @s ∈ queryNodes@.
-- * @queryTarget r = Just t@ implies @t ∈ queryNodes@.
-- * @r ∈ queryIncidentRels n@ implies @querySource r = Just n ∨ queryTarget r = Just n@.
-- * @queryDegree n = length (queryIncidentRels n)@ (default; implementations may be faster).
-- * @queryNodeById (identify (value n)) = Just n@ for all @n ∈ queryNodes@.
-- * @queryRelationshipById (identify (value r)) = Just r@ for all @r ∈ queryRelationships@.
-- * @queryContainers@ returns only direct containers — does not recurse transitively.
data GraphQuery v = GraphQuery
  { queryNodes            :: [Pattern v]
    -- ^ All node-classified elements in the graph. O(n).
  , queryRelationships    :: [Pattern v]
    -- ^ All relationship-classified elements. O(r).
  , queryIncidentRels     :: Pattern v -> [Pattern v]
    -- ^ All relationships where the given node is source or target. O(r).
  , querySource           :: Pattern v -> Maybe (Pattern v)
    -- ^ The source (first endpoint) of a relationship; 'Nothing' if not a relationship. O(1).
  , queryTarget           :: Pattern v -> Maybe (Pattern v)
    -- ^ The target (second endpoint) of a relationship; 'Nothing' if not a relationship. O(1).
  , queryDegree           :: Pattern v -> Int
    -- ^ Count of incident relationships for a node. O(r) default; O(1) if indexed.
  , queryNodeById         :: Id v -> Maybe (Pattern v)
    -- ^ Node lookup by identity. O(log n) from PatternGraph; O(n) from GraphLens.
  , queryRelationshipById :: Id v -> Maybe (Pattern v)
    -- ^ Relationship lookup by identity. O(log r) from PatternGraph; O(r) from GraphLens.
  , queryContainers       :: Pattern v -> [Pattern v]
    -- ^ All higher-order structures (relationships, walks, annotations) that directly
    -- contain the given element. O(r + w + a).
  }

-- ============================================================================
-- Constructors
-- ============================================================================

-- | Construct a 'GraphQuery' from a 'GraphLens'.
--
-- All fields are derived from existing 'Pattern.Graph' functions.
-- 'queryNodeById' and 'queryRelationshipById' perform O(n) \/ O(r) scans
-- (no index available from 'GraphLens'). 'queryContainers' scans relationships
-- and walks.
fromGraphLens :: (GraphValue v, Eq v) => GraphLens v -> GraphQuery v
fromGraphLens lens = GraphQuery
  { queryNodes            = G.nodes lens
  , queryRelationships    = G.relationships lens
  , queryIncidentRels     = G.incidentRels lens
  , querySource           = G.source lens
  , queryTarget           = G.target lens
  , queryDegree           = G.degree lens
  , queryNodeById         = \i -> let ns = G.nodes lens
                                  in foldr (\n acc -> if identify (value n) == i then Just n else acc) Nothing ns
  , queryRelationshipById = \i -> let rs = G.relationships lens
                                  in foldr (\r acc -> if identify (value r) == i then Just r else acc) Nothing rs
  , queryContainers       = \p ->
      let nodeId = identify (value p)
          inRel r = case (G.source lens r, G.target lens r) of
            (Just s, _) | identify (value s) == nodeId -> True
            (_, Just t) | identify (value t) == nodeId -> True
            _ -> False
          containingRels = filter inRel (G.relationships lens)
          containingWalks = filter (\w -> any (\r -> identify (value r) == nodeId) (elements w)) (G.walks lens)
      in containingRels ++ containingWalks
  }

-- ============================================================================
-- Combinators
-- ============================================================================

-- | Produce a 'GraphQuery' restricted to elements satisfying a predicate.
--
-- 'queryIncidentRels' on the framed query excludes relationships whose
-- endpoints fall outside the frame. All 'GraphQuery' invariants are preserved.
--
-- == Example
--
-- > let subgraph = frameQuery (\(Pattern v _) -> v == "Person") gq
frameQuery :: (Pattern v -> Bool) -> GraphQuery v -> GraphQuery v
frameQuery include base = GraphQuery
  { queryNodes            = filter include (queryNodes base)
  , queryRelationships    = filter include (queryRelationships base)
  , queryIncidentRels     = \n ->
      filter (\r -> maybe False include (querySource base r)
                 && maybe False include (queryTarget base r))
             (queryIncidentRels base n)
  , querySource           = querySource base
  , queryTarget           = queryTarget base
  , queryDegree           = \n ->
      length $ filter (\r -> maybe False include (querySource base r)
                          && maybe False include (queryTarget base r))
                      (queryIncidentRels base n)
  , queryNodeById         = \i -> case queryNodeById base i of
      Just n | include n -> Just n
      _                  -> Nothing
  , queryRelationshipById = \i -> case queryRelationshipById base i of
      Just r | include r -> Just r
      _                  -> Nothing
  , queryContainers       = \p ->
      filter include (queryContainers base p)
  }

-- | Wrap 'queryIncidentRels' with a pure memoization layer.
--
-- Builds a complete cache from 'queryNodes' eagerly, then serves all
-- subsequent 'queryIncidentRels' calls from the cache. All other fields
-- are passed through unchanged.
--
-- Useful for algorithms (e.g. betweenness centrality) that call
-- 'queryIncidentRels' repeatedly on the same node.
--
-- Note: The cache is per-'GraphQuery' value, not global.
memoizeIncidentRels :: (GraphValue v, Ord (Id v)) => GraphQuery v -> GraphQuery v
memoizeIncidentRels base =
  let cache = Map.fromList
        [ (identify (value n), queryIncidentRels base n)
        | n <- queryNodes base
        ]
      cachedIncident n = Map.findWithDefault [] (identify (value n)) cache
  in base { queryIncidentRels = cachedIncident
          , queryDegree       = \n -> length (cachedIncident n)
          }
