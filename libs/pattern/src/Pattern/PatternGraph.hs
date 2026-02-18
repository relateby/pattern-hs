-- | PatternGraph: container for nodes, relationships, walks, and annotations
-- backed by Pattern v, with merge-on-insert semantics and conversion to GraphLens.
--
-- Unrecognized patterns are never stored; they are returned in 'MergeResult.unrecognized'.
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
  , toGraphLensWithScope
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Pattern.Core (Pattern(..))
import Pattern.Graph (GraphLens(..))
import qualified Pattern.Reconcile as Reconcile
import Subject.Core (Subject(..), Symbol)
import qualified Subject.Core as Subj

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

deriving instance (Eq (Id v), Eq v) => Eq (PatternGraph v)
deriving instance (Show (Id v), Show v) => Show (PatternGraph v)

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
-- Unrecognized patterns are never stored in the graph; they appear only in
-- 'unrecognized'. Merge and fromPatterns always return (graph, unrecognized);
-- callers can log, error, or ignore the unrecognized list.
data MergeResult v = MergeResult
  { mergedGraph   :: PatternGraph v
  , unrecognized :: [Pattern v]
  }

deriving instance (Eq (Id v), Eq v) => Eq (MergeResult v)
deriving instance (Show (Id v), Show v) => Show (MergeResult v)

-- ============================================================================
-- GraphValue Subject
-- ============================================================================

instance GraphValue Subject where
  type Id Subject = Symbol
  identify = Subj.identity
  classify p = classifyByArity p

classifyByArity :: Pattern Subject -> PatternClass
classifyByArity (Pattern _ els)
  | null els = Node
  | length els == 1 = Annotation
  | length els == 2 && all (null . elements) els = Relationship
  | length els >= 1 && all isRelationshipLike els = Walk
  | otherwise = Unrecognized
  where
    isRelationshipLike e = length (elements e) == 2 && all (null . elements) (elements e)

-- ============================================================================
-- Empty and merge (default policy: LastWriteWins)
-- ============================================================================

-- | Empty graph (all four maps empty).
empty :: GraphValue v => PatternGraph v
empty = PatternGraph Map.empty Map.empty Map.empty Map.empty

-- | Merge a single pattern into the graph. Returns updated graph and list of
-- unrecognized patterns (total API). Uses LastWriteWins for duplicate identities.
merge
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Pattern v -> PatternGraph v -> MergeResult v
merge p g = mergeWithPolicy Reconcile.LastWriteWins p g

-- | Merge a single pattern using the given reconciliation policy for duplicate identities.
mergeWithPolicy
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph v
  -> MergeResult v
mergeWithPolicy policy p g =
  case classify p of
    Unrecognized -> MergeResult g [p]
    Node -> insertNode policy p g
    Relationship -> insertRelationship policy p g
    Walk -> insertWalk policy p g
    Annotation -> insertAnnotation policy p g

-- | Two occurrences of the same identity: root = existing, single child = p.
-- Used so Reconcile.reconcile sees exactly two occurrences (no extra duplicate root).
twoOccurrences :: Pattern v -> Pattern v -> Pattern v
twoOccurrences existing p = Pattern (value existing) [p]

insertNode
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph v
  -> MergeResult v
insertNode policy p g =
  let i = identify (value p)
  in case Map.lookup i (pgNodes g) of
    Nothing -> MergeResult (g { pgNodes = Map.insert i p (pgNodes g) }) []
    Just existing ->
      case Reconcile.reconcile policy (twoOccurrences existing p) of
        Left _ -> MergeResult g [p]
        Right merged -> MergeResult (g { pgNodes = Map.insert i merged (pgNodes g) }) []

insertRelationship
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph v
  -> MergeResult v
insertRelationship policy p g =
  let -- Merge endpoint nodes first (relationship has 2 node elements); accumulate unrecognized (FR-006).
      (g1, unkInner) = case elements p of
        [n1, n2] ->
          let MergeResult g1' unk1 = mergeWithPolicy policy n1 g
              MergeResult g2' unk2 = mergeWithPolicy policy n2 g1'
          in (g2', unk1 ++ unk2)
        _        -> (g, [])
      i = identify (value p)
  in case Map.lookup i (pgRelationships g1) of
    Nothing -> MergeResult (g1 { pgRelationships = Map.insert i p (pgRelationships g1) }) unkInner
    Just existing ->
      case Reconcile.reconcile policy (twoOccurrences existing p) of
        Left _ -> MergeResult g1 (unkInner ++ [p])
        Right mergedRel -> MergeResult (g1 { pgRelationships = Map.insert i mergedRel (pgRelationships g1) }) unkInner

insertWalk
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph v
  -> MergeResult v
insertWalk policy p g =
  let (g1, unkInner) = foldl step (g, []) (elements p)
      step (gAcc, unkAcc) rel =
        let MergeResult g' unk' = mergeWithPolicy policy rel gAcc
        in (g', unkAcc ++ unk')
      i = identify (value p)
  in case Map.lookup i (pgWalks g1) of
    Nothing -> MergeResult (g1 { pgWalks = Map.insert i p (pgWalks g1) }) unkInner
    Just existing ->
      case Reconcile.reconcile policy (twoOccurrences existing p) of
        Left _ -> MergeResult g1 (unkInner ++ [p])
        Right walkPat -> MergeResult (g1 { pgWalks = Map.insert i walkPat (pgWalks g1) }) unkInner

insertAnnotation
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph v
  -> MergeResult v
insertAnnotation policy p g =
  let [inner] = elements p
      MergeResult g1 unkInner = mergeWithPolicy policy inner g
      i = identify (value p)
  in case Map.lookup i (pgAnnotations g1) of
    Nothing -> MergeResult (g1 { pgAnnotations = Map.insert i p (pgAnnotations g1) }) unkInner
    Just existing ->
      case Reconcile.reconcile policy (twoOccurrences existing p) of
        Left _ -> MergeResult g1 (unkInner ++ [p])
        Right mergedP -> MergeResult (g1 { pgAnnotations = Map.insert i mergedP (pgAnnotations g1) }) unkInner

-- ============================================================================
-- fromPatterns
-- ============================================================================

-- | Build a graph from a list of patterns (fold of merge). Returns graph and unrecognized.
fromPatterns
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => [Pattern v]
  -> MergeResult v
fromPatterns ps = fromPatternsWithPolicy Reconcile.LastWriteWins ps

-- | Build a graph from a list of patterns using the given reconciliation policy.
-- Uses strict fold and reverse accumulation for unrecognized to avoid quadratic (++) and large thunks.
fromPatternsWithPolicy
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> [Pattern v]
  -> MergeResult v
fromPatternsWithPolicy policy ps =
  let (g, revUnk) = foldl' step (empty, []) ps
      step (gAcc, revUnkAcc) p =
        let MergeResult g' unk' = mergeWithPolicy policy p gAcc
        in (g', foldr (:) revUnkAcc unk')
  in MergeResult g (reverse revUnk)

-- ============================================================================
-- toGraphLens
-- ============================================================================

-- | Convert a 'PatternGraph' to a 'GraphLens' (scope pattern + atomic predicate)
-- so existing graph algorithms can be used on the same data.
--
-- Returns 'Nothing' when the graph is empty, since there is no pattern value
-- available to use as the scope decoration. Use 'toGraphLensWithScope' if you
-- need a 'GraphLens' for an empty graph by providing the scope value explicitly.
toGraphLens :: GraphValue v => PatternGraph v -> Maybe (GraphLens v)
toGraphLens g =
  let allPats = Map.elems (pgNodes g) ++ Map.elems (pgRelationships g) ++ Map.elems (pgWalks g)
  in case allPats of
    (p : _) -> Just (toGraphLensWithScope (value p) g)
    [] -> Nothing

-- | Convert a 'PatternGraph' to a 'GraphLens' using the given scope value.
-- Total: can be used for empty graphs, in which case the scope pattern has no elements.
toGraphLensWithScope :: GraphValue v => v -> PatternGraph v -> GraphLens v
toGraphLensWithScope scopeVal g =
  let allPats = Map.elems (pgNodes g) ++ Map.elems (pgRelationships g) ++ Map.elems (pgWalks g)
      scope = Pattern scopeVal allPats
      isNode (Pattern _ els) = null els
  in GraphLens scope isNode
