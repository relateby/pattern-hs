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
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Pattern.Core (Pattern(..))
import Pattern.Graph (GraphLens(..))
import Data.Either (fromRight)
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
    Node -> MergeResult (insertNode policy p g) []
    Relationship -> MergeResult (insertRelationship policy p g) []
    Walk -> MergeResult (insertWalk policy p g) []
    Annotation -> MergeResult (insertAnnotation policy p g) []

insertNode
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph v
  -> PatternGraph v
insertNode policy p g =
  let i = identify (value p)
      merged = case Map.lookup i (pgNodes g) of
        Nothing -> p
        Just existing -> fromRight (error "reconcile") (Reconcile.reconcile policy (Pattern (value p) [existing, p]))
  in g { pgNodes = Map.insert i merged (pgNodes g) }

insertRelationship
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph v
  -> PatternGraph v
insertRelationship policy p g =
  let -- Merge endpoint nodes first (relationship has 2 node elements)
      g1 = case elements p of
        [n1, n2] -> merged (mergeWithPolicy policy n1 g) |> mergeWithPolicy policy n2
        _        -> g
      merged (MergeResult g' _) = g'
      (|>) acc f = merged (f acc)
      i = identify (value p)
      mergedRel = case Map.lookup i (pgRelationships g1) of
        Nothing -> p
        Just existing -> fromRight (error "reconcile") (Reconcile.reconcile policy (Pattern (value p) [existing, p]))
  in g1 { pgRelationships = Map.insert i mergedRel (pgRelationships g1) }

insertWalk
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph v
  -> PatternGraph v
insertWalk policy p g =
  let g1 = foldl (\acc rel -> graphOf (mergeWithPolicy policy rel acc)) g (elements p)
      i = identify (value p)
      walkPat = case Map.lookup i (pgWalks g1) of
        Nothing -> p
        Just existing -> fromRight (error "reconcile") (Reconcile.reconcile policy (Pattern (value p) [existing, p]))
  in g1 { pgWalks = Map.insert i walkPat (pgWalks g1) }
  where
    graphOf (MergeResult g' _) = g'

insertAnnotation
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> Pattern v
  -> PatternGraph v
  -> PatternGraph v
insertAnnotation policy p g =
  let [inner] = elements p
      g1 = merged (mergeWithPolicy policy inner g)
      i = identify (value p)
      mergedP = case Map.lookup i (pgAnnotations g1) of
        Nothing -> p
        Just existing -> fromRight (error "reconcile") (Reconcile.reconcile policy (Pattern (value p) [existing, p]))
  in g1 { pgAnnotations = Map.insert i mergedP (pgAnnotations g1) }
  where
    merged (MergeResult g' _) = g'

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
fromPatternsWithPolicy
  :: (GraphValue v, Eq v, Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v)
  => Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> [Pattern v]
  -> MergeResult v
fromPatternsWithPolicy policy ps =
  foldl
    ( \(MergeResult g unk) p ->
        let MergeResult g' unk' = mergeWithPolicy policy p g
        in MergeResult g' (unk ++ unk')
    )
    (MergeResult empty [])
    ps

-- ============================================================================
-- toGraphLens
-- ============================================================================

-- | Convert a PatternGraph to a GraphLens (scope pattern + atomic predicate)
-- so existing graph algorithms can be used on the same data.
toGraphLens :: GraphValue v => PatternGraph v -> GraphLens v
toGraphLens g =
  let allPats = Map.elems (pgNodes g) ++ Map.elems (pgRelationships g) ++ Map.elems (pgWalks g)
      scopeVal = case allPats of
        (p : _) -> value p
        [] -> error "toGraphLens: empty graph has no scope value"
      scope = Pattern scopeVal allPats
      isNode (Pattern _ els) = null els
  in GraphLens scope isNode
