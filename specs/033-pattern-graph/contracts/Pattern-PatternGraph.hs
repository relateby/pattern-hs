{-# LANGUAGE TypeFamilies #-}

-- | API Contract: Pattern.PatternGraph Module
--
-- This file defines the public API contract for the Pattern.PatternGraph module.
-- It specifies type signatures and module structure but omits implementations.
-- This contract serves as:
--   1. Documentation of the public API
--   2. Contract for testing (tests verify these signatures exist)
--   3. Planning artifact for implementation phase
--
-- NOTE: This is a CONTRACT file, not executable code.
-- The actual implementation will live in libs/pattern/src/Pattern/PatternGraph.hs
--
-- PatternGraph is a container for nodes, relationships, walks, and annotations
-- backed by Pattern v, with merge-on-insert semantics and conversion to GraphLens.

module Pattern.PatternGraph
  ( -- * Graph container
    PatternGraph(..)

    -- * Classification
  , PatternClass(..)
  , GraphValue(..)

    -- * Merge and construction
  , merge
  , mergeWithPolicy
  , fromPatterns
  , fromPatternsWithPolicy
  , empty

    -- * Result type for unrecognized patterns
  , MergeResult(..)

    -- * Conversion to GraphLens
  , toGraphLens
  ) where

import Data.Map.Strict (Map)
import Pattern.Core (Pattern(..))
import Pattern.Graph (GraphLens(..))
import Pattern.Reconcile (Mergeable(..), ReconciliationPolicy)

-- ============================================================================
-- Types
-- ============================================================================

-- | Container holding four categories of graph elements, each keyed by identity.
-- All stored elements are 'Pattern v'; classification is via 'GraphValue'.
data PatternGraph v = PatternGraph
  { pgNodes         :: Map (Id v) (Pattern v)
  , pgRelationships :: Map (Id v) (Pattern v)
  , pgWalks         :: Map (Id v) (Pattern v)
  , pgAnnotations   :: Map (Id v) (Pattern v)
  }

-- | Classification of a pattern for dispatch into PatternGraph categories.
data PatternClass
  = Node
  | Annotation
  | Relationship
  | Walk
  | Unrecognized
  deriving (Eq, Show)

-- | Typeclass providing identity and classification for the value type @v@.
-- Used to key maps and to classify patterns as Node/Annotation/Relationship/Walk/Unrecognized.
class Ord (Id v) => GraphValue v where
  type Id v
  identify :: v -> Id v
  classify :: Pattern v -> PatternClass

-- | Result of a merge or batch load: updated graph and any unrecognized patterns.
data MergeResult v = MergeResult
  { mergedGraph   :: PatternGraph v
  , unrecognized :: [Pattern v]
  }

-- ============================================================================
-- Merge and construction
-- ============================================================================

-- | Empty graph (all four maps empty).
empty :: GraphValue v => PatternGraph v

-- | Merge a single pattern into the graph. Returns updated graph and list of
-- unrecognized patterns (total API; see research.md). Uses a default policy.
merge :: GraphValue v => Pattern v -> PatternGraph v -> MergeResult v

-- | Merge a single pattern using the given reconciliation policy for duplicate identities.
mergeWithPolicy
  :: (GraphValue v, Mergeable v)
  => ReconciliationPolicy (MergeStrategy v)
  -> Pattern v
  -> PatternGraph v
  -> MergeResult v

-- | Build a graph from a list of patterns (fold of merge). Returns graph and unrecognized.
fromPatterns :: GraphValue v => [Pattern v] -> MergeResult v

-- | Build a graph from a list of patterns using the given reconciliation policy.
fromPatternsWithPolicy
  :: (GraphValue v, Mergeable v)
  => ReconciliationPolicy (MergeStrategy v)
  -> [Pattern v]
  -> MergeResult v

-- ============================================================================
-- Conversion
-- ============================================================================

-- | Convert a PatternGraph to a GraphLens (scope pattern + atomic predicate)
-- so existing graph algorithms can be used on the same data.
toGraphLens :: GraphValue v => PatternGraph v -> GraphLens v

-- Policy: ReconciliationPolicy (MergeStrategy v) is from Pattern.Reconcile;
-- MergeStrategy v is the associated type of Mergeable v.
