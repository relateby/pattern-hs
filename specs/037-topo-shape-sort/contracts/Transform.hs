-- | Contract: Pattern.Graph.Transform — updated signatures for 037-topo-shape-sort
--
-- This file describes the intended type signatures and documentation contracts
-- for the functions affected by this feature. It is NOT a compilable module.
-- See libs/pattern/src/Pattern/Graph/Transform.hs for the implementation.
--
-- Changes from current state:
--   - sortByArity → topoShapeSort (rename + GraphValue v constraint + algorithm)
--   - paraGraph: updated documentation (no signature change)
--   - paraGraphWithSeed: updated call site (no signature change)
--   - Module header: updated vocabulary

-- ============================================================================
-- topoShapeSort (was: sortByArity)
-- ============================================================================

-- | Order graph elements for correct bottom-up processing in 'paraGraph'.
--
-- Elements are sorted in two passes:
--
-- __Pass 1 — Inter-bucket ordering__ (shape class priority):
--
-- > GNode < GRelationship < GWalk < GAnnotation < GOther
--
-- This ensures cross-class containment dependencies are satisfied:
-- nodes (atomic) before relationships (contain nodes), relationships before
-- walks (contain relationships), walks before annotations (annotations can
-- reference any class below them), and annotations before other (GOther is
-- unconstrained).
--
-- __Pass 2 — Within-bucket ordering__ (Kahn's algorithm):
--
-- Applied to the 'GAnnotation' and 'GOther' buckets only.
-- For each element @p@, its direct sub-elements (@elements p@) that belong
-- to the same bucket are treated as dependencies — they must appear before @p@.
--
-- 'GNode', 'GRelationship', and 'GWalk' require no within-bucket sort:
-- by the definition of 'classifyByShape', their sub-elements always belong
-- to a lower-priority bucket.
--
-- __Cycle handling:__
--
-- If a dependency cycle is detected within a bucket (e.g., annotation A
-- references annotation B which references A), the cycle members are appended
-- after all non-cycle elements in the bucket, in the order they appeared in
-- the input. No error is raised. See 'paraGraph' for the consequence of this
-- in fold results.
--
-- Requires 'GraphValue v' to extract element identities for dependency tracking.
topoShapeSort :: GraphValue v
              => [(GraphClass extra, Pattern v)]
              -> [(GraphClass extra, Pattern v)]

-- ============================================================================
-- paraGraph (signature unchanged; documentation updated)
-- ============================================================================

-- | Single-pass structure-aware fold over a 'GraphView'.
--
-- Elements are processed in containment order via 'topoShapeSort':
-- each element receives already-computed results for its direct sub-elements.
--
-- == Processing Order
--
-- Determined by 'topoShapeSort':
--
-- 1. Nodes (atomic — no sub-elements)
-- 2. Relationships (contain nodes)
-- 3. Walks (contain relationships)
-- 4. Annotations (contain any element type, including other annotations)
-- 5. Other (GOther — unconstrained sub-elements)
--
-- Within the Annotation and Other buckets, elements are additionally sorted
-- so that referenced elements appear before the elements that reference them.
--
-- == The @subResults@ Contract
--
-- The @[r]@ argument received by @f@ contains the results of all direct
-- sub-elements of the current element that have already been processed.
--
-- If a sub-element has not yet been processed — which can occur when a
-- dependency cycle exists within a bucket — its result is /omitted/ from
-- @subResults@. The list will be shorter than @elements p@. Callers
-- should treat @subResults@ as a best-effort list, not a guaranteed
-- complete list.
--
-- == Example
--
-- > -- Count the number of sub-results (i.e., direct dependencies) for each element
-- > countDeps :: GraphValue v => GraphView extra v -> Map (Id v) Int
-- > countDeps = paraGraph (\_ _ subResults -> length subResults)
paraGraph
  :: GraphValue v
  => (GraphQuery v -> Pattern v -> [r] -> r)
  -> GraphView extra v
  -> Map (Id v) r

-- ============================================================================
-- paraGraphFixed (signature unchanged; documentation note)
-- ============================================================================

-- | Iterate 'paraGraph' rounds until the convergence predicate is satisfied.
--
-- Each round applies 'topoShapeSort' to ensure correct within-bucket
-- dependency ordering. The ordering is recomputed identically every round
-- (the 'GraphView' is immutable).
--
-- See 'paraGraph' for the @subResults@ contract, including cycle behaviour.
paraGraphFixed
  :: (GraphValue v, Ord (Id v))
  => (r -> r -> Bool)
  -> (GraphQuery v -> Pattern v -> [r] -> r)
  -> r
  -> GraphView extra v
  -> Map (Id v) r

-- ============================================================================
-- Module export list (unchanged)
-- ============================================================================
--
-- topoShapeSort is NOT exported — it is an internal helper.
-- The public API surface of Pattern.Graph.Transform is unchanged.
