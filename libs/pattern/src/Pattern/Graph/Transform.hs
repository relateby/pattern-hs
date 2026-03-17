{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}  -- Reconcile.HasIdentity v (Id v), Ord (Id v) in signatures
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Graph transformation operations over 'GraphView'.
--
-- This module provides bulk transformation, filtering, folding, and
-- iterative shape-aware algorithms for graphs represented as 'GraphView'.
--
-- == Overview
--
-- Transformations operate lazily over 'GraphView' and are composed by
-- function composition. Finalize a pipeline by calling 'Pattern.PatternGraph.materialize'.
--
-- Elements are classified by shape (via 'classifyByShape') into five buckets:
-- @GNode@, @GRelationship@, @GWalk@, @GAnnotation@, and @GOther@. The
-- 'topoShapeSort' function orders elements within a 'GraphView' so that
-- each element is processed after all elements it structurally depends on.
--
-- == Example
--
-- > pipeline :: PatternGraph Subject -> PatternGraph Subject
-- > pipeline graph =
-- >   Pattern.PatternGraph.materialize canonicalClassifier LastWriteWins
-- >   . mapWithContext canonicalClassifier enrich
-- >   . filterGraph canonicalClassifier isRelevant dissolve
-- >   . mapAllGraph updateTimestamp
-- >   . Pattern.PatternGraph.toGraphView canonicalClassifier
-- >   $ graph
module Pattern.Graph.Transform
  ( -- * Graph construction from seeds
    unfoldGraph
    -- * Bulk transformations
  , mapGraph
  , mapAllGraph
  , filterGraph
  , foldGraph
    -- * Context-aware enrichment
  , mapWithContext
    -- * Iterative topology-aware algorithms
  , scopeDictFromGraphView
  , paraGraph
  , paraGraphFixed
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Pattern.Core (Pattern(..), ScopeDict, ScopeQuery(..), toScopeDict)
import Pattern.Graph.Types (GraphView(..))
import Pattern.Graph.GraphClassifier
  ( GraphClass(..), GraphClassifier(..), GraphValue(..) )
import Pattern.Graph.GraphQuery (GraphQuery(..))
import Pattern.Graph.Types (Substitution(..))
import Pattern.PatternGraph (PatternGraph, mergeWithPolicy, empty)
import qualified Pattern.Reconcile as Reconcile

-- ============================================================================
-- unfoldGraph
-- ============================================================================

-- | Build a 'PatternGraph' by expanding seed values into patterns and merging.
--
-- Each seed is expanded into a list of 'Pattern v' via the provided function.
-- All resulting patterns are merged into a single 'PatternGraph' using the
-- given reconciliation policy.
--
-- == Example
--
-- > rowToPatterns :: Row -> [Pattern Subject]
-- > rowToPatterns row = [ personNode row, departmentNode row, worksInRel row ]
-- >
-- > etlGraph = unfoldGraph canonicalClassifier LastWriteWins rowToPatterns rows
unfoldGraph
  :: ( GraphValue v, Eq v
     , Reconcile.Mergeable v, Reconcile.HasIdentity v (Id v), Reconcile.Refinable v )
  => GraphClassifier extra v
  -> Reconcile.ReconciliationPolicy (Reconcile.MergeStrategy v)
  -> (a -> [Pattern v])
  -> [a]
  -> PatternGraph extra v
unfoldGraph classifier policy expand seeds =
  foldl' (\g seed -> foldl' (\g' p -> mergeWithPolicy classifier policy p g') g (expand seed))
         empty
         seeds

-- ============================================================================
-- mapGraph
-- ============================================================================

-- | Map over view elements, applying a different function per 'GraphClass'.
--
-- Each element in 'viewElements' is transformed by the function corresponding
-- to its category. The 'viewQuery' is passed through unchanged.
{-# INLINE mapGraph #-}
mapGraph
  :: GraphClassifier extra v
  -> (Pattern v -> Pattern v)  -- ^ nodes
  -> (Pattern v -> Pattern v)  -- ^ relationships
  -> (Pattern v -> Pattern v)  -- ^ walks
  -> (Pattern v -> Pattern v)  -- ^ annotations
  -> (Pattern v -> Pattern v)  -- ^ other / unrecognized
  -> GraphView extra v
  -> GraphView extra v
mapGraph _classifier fNode fRel fWalk fAnnot fOther (GraphView q elems) =
  GraphView q (map applyF elems)
  where
    applyF (cls, p) = case cls of
      GNode         -> (cls, fNode p)
      GRelationship -> (cls, fRel p)
      GWalk         -> (cls, fWalk p)
      GAnnotation   -> (cls, fAnnot p)
      GOther _      -> (cls, fOther p)

-- ============================================================================
-- mapAllGraph
-- ============================================================================

-- | Apply a single function uniformly to every element in the view.
mapAllGraph :: (Pattern v -> Pattern v) -> GraphView extra v -> GraphView extra v
mapAllGraph f (GraphView q elems) = GraphView q (map (\(cls, p) -> (cls, f p)) elems)

-- ============================================================================
-- filterGraph
-- ============================================================================

-- | Filter elements from a 'GraphView', repairing container gaps via 'Substitution'.
--
-- Elements that do not satisfy the predicate are removed. Container structures
-- (walks, annotations) whose internal elements are removed are repaired
-- according to the 'Substitution' strategy.
filterGraph
  :: GraphClassifier extra v
  -> (GraphClass extra -> Pattern v -> Bool)
  -> Substitution v
  -> GraphView extra v
  -> GraphView extra v
filterGraph classifier keep subst (GraphView q elems) =
  GraphView q (concatMap applyFilter elems)
  where
    applyFilter entry@(cls, p)
      | keep cls p = case cls of
          GWalk -> case subst of
            DeleteContainer ->
              -- Drop the whole walk if any internal relationship is removed.
              let allKept = all (\e -> keep (classify classifier e) e) (elements p)
              in if allKept then [entry] else []
            _ -> [(cls, repairWalk classifier subst keep p)]
          GAnnotation -> case subst of
            DeleteContainer ->
              let allKept = all (\e -> keep (classify classifier e) e) (elements p)
              in if allKept then [entry] else []
            _ -> [(cls, repairAnnotation classifier subst keep p)]
          _ -> [entry]
      | otherwise = []

-- Repair a walk's internal relationships using SpliceGap or ReplaceWithSurrogate.
repairWalk
  :: GraphClassifier extra v
  -> Substitution v
  -> (GraphClass extra -> Pattern v -> Bool)
  -> Pattern v
  -> Pattern v
repairWalk classifier subst keep (Pattern v rels) =
  Pattern v (concatMap (repairRel classifier subst keep) rels)

repairRel
  :: GraphClassifier extra v
  -> Substitution v
  -> (GraphClass extra -> Pattern v -> Bool)
  -> Pattern v
  -> [Pattern v]
repairRel classifier subst keep rel
  | keep (classify classifier rel) rel = [rel]
  | otherwise = case subst of
      DeleteContainer        -> []  -- handled at walk level; shouldn't reach here
      SpliceGap              -> []  -- drop this rel, shorten walk
      ReplaceWithSurrogate s -> [s] -- insert surrogate in place

repairAnnotation
  :: GraphClassifier extra v
  -> Substitution v
  -> (GraphClass extra -> Pattern v -> Bool)
  -> Pattern v
  -> Pattern v
repairAnnotation classifier subst keep (Pattern v els) =
  Pattern v (concatMap (repairSub classifier subst keep) els)
  where
    repairSub c s k e
      | k (classify c e) e = [e]
      | otherwise = case s of
          DeleteContainer -> []
          SpliceGap -> []
          ReplaceWithSurrogate surr -> [surr]

-- ============================================================================
-- foldGraph
-- ============================================================================

-- | Reduce all view elements into a single 'Monoid' accumulation.
foldGraph
  :: Monoid m
  => (GraphClass extra -> Pattern v -> m)
  -> GraphView extra v
  -> m
foldGraph f (GraphView _ elems) = foldMap (\(cls, p) -> f cls p) elems

-- ============================================================================
-- mapWithContext
-- ============================================================================

-- | Map over view elements with access to the original snapshot 'GraphQuery'.
--
-- The mapping function receives the unmodified query from the original view,
-- ensuring deterministic snapshot semantics: later elements do not see
-- mutations applied to earlier elements.
mapWithContext
  :: GraphClassifier extra v
  -> (GraphQuery v -> Pattern v -> Pattern v)
  -> GraphView extra v
  -> GraphView extra v
mapWithContext _classifier f view@(GraphView q elems) =
  view { viewElements = map (\(cls, p) -> (cls, f q p)) elems }

-- | Reify the generic scope answers for a 'GraphView' snapshot.
--
-- This exposes the unified scope model without exporting the internal adapter
-- type used to compute it.
--
-- Invariants:
--
-- * 'allElements' covers every classified element in the 'GraphView'.
-- * 'byIdentity' is bounded to that snapshot.
-- * 'containers' and 'siblings' are derived from direct containment only.
-- * Duplicate graph identities are rejected when reifying generic scope
--   answers, because 'ScopeQuery' lookup is keyed only by 'Id v'.
--
-- Example:
--
-- > let scope = scopeDictFromGraphView view
-- > length (allElements scope)
scopeDictFromGraphView :: GraphValue v => GraphView extra v -> ScopeDict (Id v) v
scopeDictFromGraphView = toScopeDict . graphViewScope

-- ============================================================================
-- paraGraph
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
paraGraph f view = paraGraphWithSeed f view Map.empty

-- Internal adapter that exposes full GraphView scope to the generic scope layer
-- without changing the public GraphQuery record shape.
data GraphViewScope v = GraphViewScope
  { gvsQuery      :: GraphQuery v
  , gvsElements   :: [Pattern v]
  , gvsIndex      :: Map (Id v) (Pattern v)
  , gvsContainers :: Map (Id v) [Pattern v]
  }

instance GraphValue v => ScopeQuery GraphViewScope v where
  type ScopeId GraphViewScope v = Id v

  containers scope p =
    Map.findWithDefault [] (identify (value p)) (gvsContainers scope)

  siblings scope p =
    dedupeById
      [ sibling
      | container <- containers scope p
      , child <- elements container
      , let childId = identify (value child)
      , childId /= identify (value p)
      , Just sibling <- [Map.lookup childId (gvsIndex scope)]
      ]

  byIdentity scope i = Map.lookup i (gvsIndex scope)
  allElements = gvsElements

dedupeById :: GraphValue v => [Pattern v] -> [Pattern v]
dedupeById = reverse . fst . foldl' step ([], Set.empty)
  where
    step (acc, seen) p =
      let pid = identify (value p)
      in if Set.member pid seen
           then (acc, seen)
           else (p : acc, Set.insert pid seen)

graphViewScope :: GraphValue v => GraphView extra v -> GraphViewScope v
graphViewScope (GraphView q taggedElems) = GraphViewScope
  { gvsQuery = q
  , gvsElements = elems
  , gvsIndex = index
  , gvsContainers = containerIndex
  }
  where
    elems = map snd taggedElems
    index = foldl' insertUniqueElement Map.empty elems
    containerIndex = foldl' indexContainer Map.empty elems

    insertUniqueElement acc p =
      let pid = identify (value p)
      in case Map.lookup pid acc of
           Nothing -> Map.insert pid p acc
           Just _ ->
             error "scopeDictFromGraphView: duplicate element identity in GraphView"

    indexContainer acc container =
      foldl' (insertContainer container) acc (elements container)

    insertContainer container acc child =
      let childId = identify (value child)
      in if Map.member childId index
           then Map.insertWith (++) childId [container] acc
           else acc

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
topoShapeSort elems =
  let isNode'  (GNode, _)         = True; isNode'  _ = False
      isRel'   (GRelationship, _) = True; isRel'   _ = False
      isWalk'  (GWalk, _)         = True; isWalk'  _ = False
      isAnnot' (GAnnotation, _)   = True; isAnnot' _ = False
      isOther' (GOther _, _)      = True; isOther' _ = False
  in filter isNode' elems
     ++ filter isRel' elems
     ++ filter isWalk' elems
     ++ withinBucketTopoSort (filter isAnnot' elems)
     ++ withinBucketTopoSort (filter isOther' elems)

-- Topological sort within a single bucket using Kahn's algorithm.
-- Elements are ordered so that each element appears after all elements
-- it structurally depends on (i.e., contains as sub-elements of the same bucket).
-- Cycle members are appended after all non-cycle elements in input order.
withinBucketTopoSort :: GraphValue v
                     => [(GraphClass extra, Pattern v)]
                     -> [(GraphClass extra, Pattern v)]
withinBucketTopoSort [] = []
withinBucketTopoSort elems =
  let idMap = Map.fromList [ (identify (value p), entry) | entry@(_, p) <- elems ]

      inBucketDeps = Map.fromList
        [ ( identify (value p)
          , [ identify (value e) | e <- elements p, Map.member (identify (value e)) idMap ]
          )
        | (_, p) <- elems
        ]

      dependents = foldl'
        (\acc (pid, deps) -> foldl' (\m dep -> Map.insertWith (++) dep [pid] m) acc deps)
        (Map.map (const []) idMap)
        (Map.toList inBucketDeps)

      inDegree0 = Map.map length inBucketDeps

      initQueue = [ identify (value p) | (_, p) <- elems
                  , Map.findWithDefault 0 (identify (value p)) inDegree0 == 0 ]

      go [] _ sortedRev = sortedRev
      go (pid:queue) deg sortedRev =
        let entry   = idMap Map.! pid
            newDeps = Map.findWithDefault [] pid dependents
            (queue', deg') = foldl'
              (\(q, d) dep ->
                let newD = Map.findWithDefault 0 dep d - 1
                in if newD == 0
                   then (dep : q, Map.insert dep newD d)
                   else (q,       Map.insert dep newD d))
              (queue, deg)
              newDeps
        in go queue' deg' (entry : sortedRev)

      sorted       = reverse (go initQueue inDegree0 [])
      sortedSet    = Map.fromList [ (identify (value p), ()) | (_, p) <- sorted ]
      cycleMembers = filter (\(_, p) -> not (Map.member (identify (value p)) sortedSet)) elems
  in sorted ++ cycleMembers

-- ============================================================================
-- paraGraphFixed
-- ============================================================================

-- | Iterate 'paraGraph' rounds until the convergence predicate is satisfied.
--
-- Each round applies 'topoShapeSort' to ensure correct within-bucket
-- dependency ordering. The ordering is recomputed identically every round
-- (the 'GraphView' is immutable).
--
-- See 'paraGraph' for the @subResults@ contract, including cycle behaviour.
--
-- The convergence predicate @conv old new@ should return 'True' when the
-- result is considered stable. A common example for floating-point algorithms:
--
-- > \old new -> abs (old - new) < 0.0001
--
-- The initial value @r0@ is used for all elements in the first round.
paraGraphFixed
  :: (GraphValue v, Ord (Id v))
  => (r -> r -> Bool)
  -> (GraphQuery v -> Pattern v -> [r] -> r)
  -> r
  -> GraphView extra v
  -> Map (Id v) r
paraGraphFixed conv f r0 view =
  let initial = Map.fromList
        [ (identify (value p), r0)
        | (_, p) <- viewElements view
        ]
  in go initial
  where
    go prev =
      let next = paraGraphWithSeed f view prev
      in if converged prev next
           then next
           else go next

    converged prev next =
      Map.foldrWithKey
        (\k newVal acc ->
          acc && case Map.lookup k prev of
            Nothing  -> False
            Just old -> conv old newVal)
        True
        next

-- Run one paraGraph round, seeding sub-element results from an existing map.
paraGraphWithSeed
  :: GraphValue v
  => (GraphQuery v -> Pattern v -> [r] -> r)
  -> GraphView extra v
  -> Map (Id v) r
  -> Map (Id v) r
paraGraphWithSeed f view seed =
  foldl' processElem seed (topoShapeSort taggedElems)
  where
    q = viewQuery view
    taggedElems = viewElements view

    processElem acc (_, p) =
      let subResults = mapMaybe (\e -> Map.lookup (identify (value e)) acc) (elements p)
          r = f q p subResults
      in Map.insert (identify (value p)) r acc
