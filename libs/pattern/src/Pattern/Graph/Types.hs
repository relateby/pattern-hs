{-# LANGUAGE TypeFamilies #-}

-- | Types for graph transformation operations.
--
-- This module defines supporting types used by "Pattern.Graph.Transform":
--
-- * 'GraphView' — the universal interface bridging graph representations
--   to a single queryable and transformable state.
-- * 'Substitution' — governs how container gaps are handled when elements
--   are removed by filtering.
module Pattern.Graph.Types
  ( GraphView(..)
  , Substitution(..)
  ) where

import Pattern.Core (Pattern(..))
import Pattern.Graph.GraphClassifier (GraphClass)
import Pattern.Graph.GraphQuery (GraphQuery)

-- ============================================================================
-- GraphView
-- ============================================================================

-- | A universal interface bridging graph representations to a single
-- queryable and transformable state.
--
-- 'GraphView' pairs a 'GraphQuery' (for snapshot traversal) with an
-- explicitly categorized, traversable list of graph elements. It acts as
-- a lazy transformation target: transformations operate over 'viewElements'
-- while algorithms use 'viewQuery' for context lookups.
--
-- Construct via 'Pattern.PatternGraph.toGraphView' (from a 'PatternGraph')
-- or 'Pattern.Graph.toGraphView' (from a 'GraphLens'). Finalize via
-- 'Pattern.PatternGraph.materialize'.
--
-- == Design Principles
--
-- * 'viewQuery' is the unmodified snapshot — it never reflects in-flight
--   transformations, ensuring deterministic context-aware operations.
-- * 'viewElements' is the mutable list — transformations update this list.
-- * The pairing is shallow: no deep nesting, just a list and a query record.
data GraphView extra v = GraphView
  { viewQuery    :: GraphQuery v
    -- ^ Snapshot query interface for context-aware lookups.
  , viewElements :: [(GraphClass extra, Pattern v)]
    -- ^ Categorized, traversable list of graph elements.
  }

-- | Defines how to handle gaps in container structures (e.g., walks) when
-- an internal element is removed by 'filterGraph'.
--
-- When a walk's internal relationship is filtered out, the walk becomes
-- structurally invalid. 'Substitution' dictates the repair strategy.
--
-- == Design Rationale
--
-- There is no one-size-fits-all fallback for container gaps. Sometimes a
-- gap means the entire container is invalid; other times a surrogate
-- placeholder should be sutured in. An explicit parameter forces callers
-- to reason about filtering consequences directly.
data Substitution v
  = DeleteContainer
    -- ^ Remove the entire container (walk, annotation) when any of its
    -- required elements are absent after filtering.
  | SpliceGap
    -- ^ Remove the missing element and re-stitch the remaining elements
    -- into a shorter container. For a walk, this collapses the gap.
  | ReplaceWithSurrogate (Pattern v)
    -- ^ Substitute the missing element with a fixed surrogate pattern.
    -- The surrogate is inserted in place of the removed element.
