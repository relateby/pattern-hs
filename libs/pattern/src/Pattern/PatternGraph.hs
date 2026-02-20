-- | PatternGraph: container for nodes, relationships, walks, and annotations
-- backed by Pattern v, with merge-on-insert semantics and conversion to GraphLens.
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

    -- * Conversion to GraphLens
  , toGraphLens
  , toGraphLensWithScope
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Pattern.Core (Pattern(..))
import Pattern.Graph (GraphLens(..), mkGraphLens)
import Pattern.Graph.GraphClassifier (GraphClass(..), GraphClassifier(..))
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

-- | Typeclass providing identity and classification for the value type @v@.
-- Used to key maps and to classify patterns as Node/Annotation/Relationship/Walk/Unrecognized.
class Ord (Id v) => GraphValue v where
  type Id v
  identify :: v -> Id v

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
-- toGraphLens
-- ============================================================================

-- | Convert a 'PatternGraph' to a 'GraphLens' (scope pattern + atomic predicate)
-- so existing graph algorithms can be used on the same data.
--
-- Returns 'Nothing' when the graph is empty, since there is no pattern value
-- available to use as the scope decoration. Use 'toGraphLensWithScope' if you
-- need a 'GraphLens' for an empty graph by providing the scope value explicitly.
toGraphLens :: (GraphValue v, Eq v) => PatternGraph extra v -> Maybe (GraphLens v)
toGraphLens g =
  let allPats = Map.elems (pgNodes g) ++ Map.elems (pgRelationships g) ++ Map.elems (pgWalks g)
  in case allPats of
    (p : _) -> Just (toGraphLensWithScope (value p) g)
    [] -> Nothing

-- | Convert a 'PatternGraph' to a 'GraphLens' using the given scope value.
-- Total: can be used for empty graphs, in which case the scope pattern has no elements.
toGraphLensWithScope :: (GraphValue v, Eq v) => v -> PatternGraph extra v -> GraphLens v
toGraphLensWithScope scopeVal g =
  let allPats = Map.elems (pgNodes g) ++ Map.elems (pgRelationships g) ++ Map.elems (pgWalks g)
      scope = Pattern scopeVal allPats
      isNodePred (Pattern _ els) = null els
  in mkGraphLens scope isNodePred
