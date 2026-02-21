-- | PatternGraph: container for nodes, relationships, walks, and annotations
-- backed by Pattern v, with merge-on-insert semantics.
--
-- Unrecognized patterns are routed to 'pgOther' by the 'GraphClassifier'.
-- Patterns that fail reconciliation are preserved in 'pgConflicts'.
-- See specs/033-pattern-graph/ for design and data model.
--
-- Round-trip with gram: parse (e.g. 'Gram.Parse.fromGram') → 'fromPatterns' → modify
-- (e.g. 'merge') → serialize by flattening 'pgNodes', 'pgRelationships', 'pgWalks',
-- 'pgAnnotations' and calling 'Gram.Serialize.toGram'. See quickstart.md and
-- libs/gram/tests/Spec/Gram/RoundtripSpec.hs (PatternGraph round-trip test).
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pattern.PatternGraph
  ( -- * Graph container
    PatternGraph(..)

    -- * Classification
  , GraphValue(..)

    -- * Merge and construction
  , merge
  , mergeWithPolicy
  , fromPatterns
  , fromPatternsWithPolicy
  , empty

    -- * Conversion to GraphQuery
  , fromPatternGraph

    -- * GraphView construction and materialization
  , toGraphView
  , materialize
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Pattern.Core (Pattern(..))
import Pattern.Graph.GraphClassifier (GraphClass(..), GraphClassifier(..), GraphValue(..))
import Pattern.Graph.GraphQuery (GraphQuery(..))
import Pattern.Graph.Types (GraphView(..))
import qualified Pattern.Reconcile as Reconcile
import Subject.Core (Subject(..), Symbol)
import qualified Subject.Core as Subj

-- ============================================================================
-- Types
-- ============================================================================

-- | Container holding four categories of graph elements, each keyed by identity.
-- All stored elements are 'Pattern v'; classification is via 'GraphValue'.
data PatternGraph extra v = PatternGraph
  { pgNodes         :: Map (Id v) (Pattern v)
  , pgRelationships :: Map (Id v) (Pattern v)
  , pgWalks         :: Map (Id v) (Pattern v)
  , pgAnnotations   :: Map (Id v) (Pattern v)
  , pgOther         :: Map (Id v) (extra, Pattern v)
  , pgConflicts     :: Map (Id v) [Pattern v]
  }

deriving instance (Eq (Id v), Eq v, Eq extra) => Eq (PatternGraph extra v)
deriving instance (Show (Id v), Show v, Show extra) => Show (PatternGraph extra v)

-- ============================================================================
-- GraphValue Subject
-- ============================================================================

instance GraphValue Subject where
  type Id Subject = Symbol
  identify = Subj.identity

-- ============================================================================
-- Empty and merge (default policy: LastWriteWins)
-- ============================================================================

-- | Empty graph (all five maps empty).
empty :: GraphValue v => PatternGraph extra v
empty = PatternGraph Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

-- | Merge a single pattern into the graph. Uses LastWriteWins for duplicate identities.
merge
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => GraphClassifier extra v
  -> Pattern v
  -> PatternGraph extra v
  -> PatternGraph extra v
merge classifier p g = mergeWithPolicy classifier Reconcile.LastWriteWins p g

-- | Merge a single pattern using the given reconciliation policy for duplicate identities.
mergeWithPolicy
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => GraphClassifier extra v
  -> Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph extra v
  -> PatternGraph extra v
mergeWithPolicy classifier policy p g =
  case classify classifier p of
    GNode -> insertNode policy p g
    GRelationship -> insertRelationship classifier policy p g
    GWalk -> insertWalk classifier policy p g
    GAnnotation -> insertAnnotation classifier policy p g
    GOther extra -> insertOther policy extra p g

-- | Two occurrences of the same identity: root = existing, single child = p.
-- Used so Reconcile.reconcile sees exactly two occurrences (no extra duplicate root).
twoOccurrences :: Pattern v -> Pattern v -> Pattern v
twoOccurrences existing p = Pattern (value existing) [p]

insertNode
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph extra v
  -> PatternGraph extra v
insertNode policy p g =
  let i = identify (value p)
  in case Map.lookup i (pgNodes g) of
    Nothing -> g { pgNodes = Map.insert i p (pgNodes g) }
    Just existing ->
      case Reconcile.reconcile policy (twoOccurrences existing p) of
        Left _ -> g { pgConflicts = Map.insertWith (++) i [p] (pgConflicts g) }
        Right merged -> g { pgNodes = Map.insert i merged (pgNodes g) }

insertRelationship
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => GraphClassifier extra v
  -> Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph extra v
  -> PatternGraph extra v
insertRelationship classifier policy p g =
  let -- Merge endpoint nodes first (relationship has 2 node elements).
      g1 = case elements p of
        [n1, n2] ->
          let g1' = mergeWithPolicy classifier policy n1 g
          in mergeWithPolicy classifier policy n2 g1'
        _        -> g
      i = identify (value p)
  in case Map.lookup i (pgRelationships g1) of
    Nothing -> g1 { pgRelationships = Map.insert i p (pgRelationships g1) }
    Just existing ->
      case Reconcile.reconcile policy (twoOccurrences existing p) of
        Left _ -> g1 { pgConflicts = Map.insertWith (++) i [p] (pgConflicts g1) }
        Right mergedRel -> g1 { pgRelationships = Map.insert i mergedRel (pgRelationships g1) }

insertWalk
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => GraphClassifier extra v
  -> Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph extra v
  -> PatternGraph extra v
insertWalk classifier policy p g =
  let g1 = foldl' (flip (mergeWithPolicy classifier policy)) g (elements p)
      i = identify (value p)
  in case Map.lookup i (pgWalks g1) of
    Nothing -> g1 { pgWalks = Map.insert i p (pgWalks g1) }
    Just existing ->
      case Reconcile.reconcile policy (twoOccurrences existing p) of
        Left _ -> g1 { pgConflicts = Map.insertWith (++) i [p] (pgConflicts g1) }
        Right walkPat -> g1 { pgWalks = Map.insert i walkPat (pgWalks g1) }

insertAnnotation
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => GraphClassifier extra v
  -> Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph extra v
  -> PatternGraph extra v
insertAnnotation classifier policy p g =
  let g1 = case elements p of
        [inner] -> mergeWithPolicy classifier policy inner g
        _ -> g
      i = identify (value p)
  in case Map.lookup i (pgAnnotations g1) of
    Nothing -> g1 { pgAnnotations = Map.insert i p (pgAnnotations g1) }
    Just existing ->
      case Reconcile.reconcile policy (twoOccurrences existing p) of
        Left _ -> g1 { pgConflicts = Map.insertWith (++) i [p] (pgConflicts g1) }
        Right mergedP -> g1 { pgAnnotations = Map.insert i mergedP (pgAnnotations g1) }

insertOther
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> extra
  -> Pattern v
  -> PatternGraph extra v
  -> PatternGraph extra v
insertOther policy extra p g =
  let i = identify (value p)
  in case Map.lookup i (pgOther g) of
    Nothing -> g { pgOther = Map.insert i (extra, p) (pgOther g) }
    Just (_, existing) ->
      case Reconcile.reconcile policy (twoOccurrences existing p) of
        Left _ -> g { pgConflicts = Map.insertWith (++) i [p] (pgConflicts g) }
        Right mergedP -> g { pgOther = Map.insert i (extra, mergedP) (pgOther g) }

-- ============================================================================
-- fromPatterns
-- ============================================================================

-- | Build a graph from a list of patterns (fold of merge).
fromPatterns
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => GraphClassifier extra v
  -> [Pattern v]
  -> PatternGraph extra v
fromPatterns classifier ps = fromPatternsWithPolicy classifier Reconcile.LastWriteWins ps

-- | Build a graph from a list of patterns using the given reconciliation policy.
fromPatternsWithPolicy
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => GraphClassifier extra v
  -> Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> [Pattern v]
  -> PatternGraph extra v
fromPatternsWithPolicy classifier policy ps =
  foldl' (\g p -> mergeWithPolicy classifier policy p g) empty ps

-- ============================================================================
-- fromPatternGraph
-- ============================================================================

-- | Construct a 'GraphQuery v' directly from a 'PatternGraph extra v'.
--
-- Reads from the typed maps (@pgNodes@, @pgRelationships@, @pgWalks@,
-- @pgAnnotations@) without going through 'GraphLens'. Provides O(log n)
-- lookups for 'queryNodeById' and 'queryRelationshipById'.
--
-- Preferred over constructing a 'GraphLens' for algorithm access.
fromPatternGraph :: (GraphValue v, Eq v) => PatternGraph extra v -> GraphQuery v
fromPatternGraph pg = GraphQuery
  { queryNodes            = Map.elems (pgNodes pg)
  , queryRelationships    = Map.elems (pgRelationships pg)
  , queryIncidentRels     = \n ->
      let nodeId = identify (value n)
      in filter (\r -> case (srcOf r, tgtOf r) of
                   (Just s, _) | identify (value s) == nodeId -> True
                   (_, Just t) | identify (value t) == nodeId -> True
                   _ -> False)
               (Map.elems (pgRelationships pg))
  , querySource           = srcOf
  , queryTarget           = tgtOf
  , queryDegree           = \n ->
      let nodeId = identify (value n)
      in length $ filter (\r -> case (srcOf r, tgtOf r) of
                   (Just s, _) | identify (value s) == nodeId -> True
                   (_, Just t) | identify (value t) == nodeId -> True
                   _ -> False)
               (Map.elems (pgRelationships pg))
  , queryNodeById         = \i -> Map.lookup i (pgNodes pg)
  , queryRelationshipById = \i -> Map.lookup i (pgRelationships pg)
  , queryContainers       = \p ->
      let nodeId = identify (value p)
          inRel r = case (srcOf r, tgtOf r) of
            (Just s, _) | identify (value s) == nodeId -> True
            (_, Just t) | identify (value t) == nodeId -> True
            _ -> False
          containingRels  = filter inRel (Map.elems (pgRelationships pg))
          containingWalks = filter (\w -> any (\r -> identify (value r) == nodeId) (elements w))
                                   (Map.elems (pgWalks pg))
          containingAnnotations = filter (\a -> case elements a of
                                            [inner] -> identify (value inner) == nodeId
                                            _ -> False)
                                         (Map.elems (pgAnnotations pg))
      in containingRels ++ containingWalks ++ containingAnnotations
  }
  where
    srcOf (Pattern _ (s:_)) = Just s
    srcOf _                  = Nothing
    tgtOf (Pattern _ [_, t]) = Just t
    tgtOf _                  = Nothing

-- ============================================================================
-- toGraphView
-- ============================================================================

-- | Construct a 'GraphView' from a 'PatternGraph'.
--
-- The 'GraphClassifier' determines the 'GraphClass' tag for each element.
-- 'viewQuery' is the snapshot query built from the same graph; it is never
-- updated by subsequent transformations, ensuring deterministic context-aware
-- operations.
--
-- Note: defined here (not in "Pattern.Graph") to avoid a circular import —
-- 'Pattern.Graph' cannot import 'PatternGraph'.
toGraphView
  :: (GraphValue v, Eq v)
  => GraphClassifier extra v
  -> PatternGraph extra v
  -> GraphView extra v
toGraphView classifier pg =
  GraphView
    { viewQuery    = fromPatternGraph pg
    , viewElements = taggedElems
    }
  where
    taggedElems =
      map (\p -> (classify classifier p, p)) $
        Map.elems (pgNodes pg)
        ++ Map.elems (pgRelationships pg)
        ++ Map.elems (pgWalks pg)
        ++ Map.elems (pgAnnotations pg)
        ++ map snd (Map.elems (pgOther pg))

-- ============================================================================
-- materialize
-- ============================================================================

-- | Reconstruct a 'PatternGraph' from a 'GraphView'.
--
-- Folds all elements in 'viewElements' through 'mergeWithPolicy', using the
-- provided classifier and reconciliation policy. This is the finalizer for a
-- lazy transformation pipeline.
--
-- Note: defined here (not in "Pattern.Graph") to avoid a circular import.
materialize
  :: ( GraphValue v, Eq v
     , Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v )
  => GraphClassifier extra v
  -> Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> GraphView extra v
  -> PatternGraph extra v
materialize classifier policy (GraphView _ elems) =
  foldl' (\g (_, p) -> mergeWithPolicy classifier policy p g) empty elems
