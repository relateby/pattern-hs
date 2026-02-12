{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Pattern reconciliation for normalizing duplicate identities.
--
-- This module provides operations to reconcile patterns by resolving duplicate
-- identities and completing partial references. When patterns are parsed, streamed,
-- or merged from multiple sources, the same identity may appear multiple times
-- with different or evolving content. Reconciliation transforms such patterns into
-- coherent ones where each identity appears exactly once.
--
-- == Problem Statement
--
-- Currently nothing prevents the same identity from appearing multiple times with
-- different content in a @Pattern v@. This can arise from:
--
-- * Parsing gram notation with duplicate node definitions
-- * Streaming pattern updates where identities evolve over time
-- * Merging patterns from different sources
-- * References: atomic patterns pointing to fuller definitions elsewhere
--
-- == Solution Overview
--
-- The 'reconcile' operation normalizes patterns using configurable policies:
--
-- 1. **Collect**: Group all occurrences by identity
-- 2. **Reconcile**: Resolve duplicates according to policy
-- 3. **Rebuild**: Construct normalized pattern with each identity appearing once
--
-- == Quick Start
--
-- >>> import Pattern.Core (Pattern(..), pattern, point)
-- >>> import Pattern.Reconcile
-- >>> import Subject.Core (Subject(..), Symbol(..))
-- >>>
-- >>> -- Reconcile with LastWriteWins policy
-- >>> reconcile LastWriteWins patternWithDuplicates
-- >>>
-- >>> -- Get detailed report
-- >>> let (result, report) = reconcileWithReport LastWriteWins pattern
-- >>> reportDuplicatesFound report  -- Number of duplicate identities
--
-- == Reconciliation Policies
--
-- * 'LastWriteWins' - Keep the last occurrence of each identity. Useful for
--   streaming updates where the most recent value is authoritative.
--
-- * 'FirstWriteWins' - Keep the first occurrence of each identity. Useful when
--   initial definitions are authoritative.
--
-- * @'Merge' elementStrategy valueStrategy@ - Combine all occurrences using configurable
--   strategies for structure (elements) and content (value).
--
-- * 'Strict' - Fail with detailed error if any duplicates have different content.
--   Useful for validation and debugging.

module Pattern.Reconcile
  ( -- * Core Abstractions
    HasIdentity(..)
  , Mergeable(..)
  , Refinable(..)

    -- * Reconciliation Policies
  , ReconciliationPolicy(..)
  , ElementMergeStrategy(..)
  , SubjectMergeStrategy(..)
  , LabelMerge(..)
  , PropertyMerge(..)
  , defaultSubjectMergeStrategy

    -- * Error and Report Types
  , Conflict(..)
  , ReconcileError(..)
  , ReconcileReport(..)
  , Path

    -- * Reconciliation Operations
  , reconcile
  , reconcileWithReport

    -- * Inspection
  , needsReconciliation
  , findConflicts
  , collectByIdentity

    -- * Utilities
  , isReference
  ) where

import GHC.Generics (Generic)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Subject.Core (Subject(Subject, labels, properties), Symbol)
import qualified Subject.Core as Subject
import Pattern.Core (Pattern(..))

-- -----------------------------------------------------------------------------
-- Core Abstractions
-- -----------------------------------------------------------------------------

-- | Allows extracting a unique identifier from a value.
class Ord i => HasIdentity v i | v -> i where
  identity :: v -> i

-- | Allows merging two values according to a strategy.
class Mergeable v where
  -- | Configuration for how to merge this specific type
  type MergeStrategy v :: Type
  
  -- | Merge two values. 
  -- The first value is the "accumulated" or "existing" one, 
  -- the second is the "incoming" one.
  merge :: MergeStrategy v -> v -> v -> v

-- | Allows checking if one value is a "partial" version of another.
class Refinable v where
  -- | Returns True if 'sub' contains a subset of the information in 'sup'.
  -- Used to detect if an atomic pattern is a reference to a full definition.
  isRefinementOf :: v {- sup -} -> v {- sub -} -> Bool

-- -----------------------------------------------------------------------------
-- Reconciliation Policy
-- -----------------------------------------------------------------------------

-- | Policy for resolving duplicate identities during reconciliation.
--
-- When a pattern contains the same identity multiple times (determined by
-- comparing 'identity' values), a reconciliation policy determines how to
-- combine or choose between the duplicate occurrences.
--
-- The type parameter @s@ is the value-specific merge strategy (e.g., 'SubjectMergeStrategy').
data ReconciliationPolicy s
  = LastWriteWins
  -- ^ Keep the last occurrence of each identity.
  -- Useful for streaming updates where the most recent value is authoritative.
  | FirstWriteWins
  -- ^ Keep the first occurrence of each identity.
  -- Useful when initial definitions are authoritative and later ones are ignored.
  | Merge ElementMergeStrategy s
  -- ^ Combine all occurrences using specified strategies for elements and values.
  | Strict
  -- ^ Fail if any duplicate identities have different content.
  deriving (Eq, Show, Generic)

-- | Strategy for merging the children (elements) of a pattern.
data ElementMergeStrategy
  = ReplaceElements
  -- ^ Later element list completely replaces earlier ones.
  | AppendElements
  -- ^ Concatenate all element lists in traversal order.
  | UnionElements
  -- ^ Deduplicate elements by identity.
  deriving (Eq, Show, Generic)

-- -----------------------------------------------------------------------------
-- Subject Implementation
-- -----------------------------------------------------------------------------

instance HasIdentity Subject Symbol where
  identity = Subject.identity

-- | Strategy for merging Subject content.
data SubjectMergeStrategy = SubjectMergeStrategy
  { labelMerge    :: LabelMerge
  , propertyMerge :: PropertyMerge
  } deriving (Eq, Show, Generic)

data LabelMerge = UnionLabels | IntersectLabels | ReplaceLabels
  deriving (Eq, Show, Generic)

data PropertyMerge = ReplaceProperties | ShallowMerge | DeepMerge
  deriving (Eq, Show, Generic)

instance Mergeable Subject where
  type MergeStrategy Subject = SubjectMergeStrategy
  merge strategy s1 s2 =
    let mergedLabels = mergeLabels (labelMerge strategy) (Subject.labels s1) (Subject.labels s2)
        mergedProps = mergeProperties (propertyMerge strategy) (Subject.properties s1) (Subject.properties s2)
    in Subject (Subject.identity s1) mergedLabels mergedProps

instance Refinable Subject where
  isRefinementOf full partial =
    Subject.identity full == Subject.identity partial
    && Set.isSubsetOf (Subject.labels partial) (Subject.labels full)
    && Map.isSubmapOf (Subject.properties partial) (Subject.properties full)

-- | Default merge strategy for Subjects.
defaultSubjectMergeStrategy :: SubjectMergeStrategy
defaultSubjectMergeStrategy = SubjectMergeStrategy UnionLabels ShallowMerge

-- Helper functions for Subject merging
mergeLabels :: LabelMerge -> Set String -> Set String -> Set String
mergeLabels UnionLabels l1 l2 = Set.union l1 l2
mergeLabels IntersectLabels l1 l2 = Set.intersection l1 l2
mergeLabels ReplaceLabels _ l2 = l2

mergeProperties :: PropertyMerge -> Map String value -> Map String value -> Map String value
mergeProperties ReplaceProperties _ p2 = p2
mergeProperties ShallowMerge p1 p2 = Map.union p2 p1  -- p2 values win on conflict
mergeProperties DeepMerge p1 p2 = Map.unionWith (\_ v2 -> v2) p1 p2 -- Placeholder for deep merge

-- -----------------------------------------------------------------------------
-- Error and Report Types
-- -----------------------------------------------------------------------------

type Path = [Int]

data Conflict i v = Conflict
  { conflictId        :: i
  , conflictExisting  :: v
  , conflictIncoming  :: v
  , conflictLocations :: [Path]
  } deriving (Eq, Show, Generic)

data ReconcileError i v = ReconcileError
  { errorMessage  :: String
  , errorConflicts :: [Conflict i v]
  } deriving (Eq, Show, Generic)

data ReconcileReport i = ReconcileReport
  { reportDuplicatesFound    :: Int
  , reportReferencesResolved :: Int
  , reportMergesPerformed    :: Int
  , reportSubjectCounts      :: Map i Int
  } deriving (Eq, Show, Generic)

-- -----------------------------------------------------------------------------
-- Core Operations
-- -----------------------------------------------------------------------------

{-# INLINABLE reconcile #-}
reconcile 
  :: (HasIdentity v i, Mergeable v, Refinable v, Eq v)
  => ReconciliationPolicy (MergeStrategy v) 
  -> Pattern v 
  -> Either (ReconcileError i v) (Pattern v)
reconcile policy pattern =
  case policy of
    Strict -> reconcileStrict pattern
    _ -> Right $ reconcileNonStrict policy pattern

{-# INLINABLE reconcileWithReport #-}
reconcileWithReport 
  :: (HasIdentity v i, Mergeable v, Refinable v, Eq v)
  => ReconciliationPolicy (MergeStrategy v) 
  -> Pattern v 
  -> (Either (ReconcileError i v) (Pattern v), ReconcileReport i)
reconcileWithReport policy pattern =
  let occurrenceMap = collectByIdentity pattern
      subjectCounts = Map.map length occurrenceMap
      duplicatesFound = Map.size $ Map.filter (> 1) subjectCounts
      
      -- Count references: atomic patterns that have fuller definitions
      referencesResolved = sum $ map countReferences (Map.elems occurrenceMap)

      result = reconcile policy pattern
      report = ReconcileReport
        { reportDuplicatesFound = duplicatesFound
        , reportReferencesResolved = referencesResolved
        , reportMergesPerformed = if isMergePolicy policy then duplicatesFound else 0
        , reportSubjectCounts = subjectCounts
        }
  in (result, report)
  where
    isMergePolicy (Merge _ _) = True
    isMergePolicy _ = False

    countReferences occurrences =
      let patterns = map fst occurrences
          isActualReference (Pattern val elems) =
            null elems && any (\other -> value other `isRefinementOf` val &&
                                         (not (null (elements other)) || not (val `isRefinementOf` value other)))
                              patterns
      in length $ filter isActualReference patterns

-- -----------------------------------------------------------------------------
-- Internal Implementation
-- -----------------------------------------------------------------------------

reconcileNonStrict 
  :: (HasIdentity v i, Mergeable v, Refinable v)
  => ReconciliationPolicy (MergeStrategy v) 
  -> Pattern v 
  -> Pattern v
reconcileNonStrict policy pattern =
  let occurrenceMap = collectByIdentity pattern
      canonicalMap = Map.map (reconcileOccurrences policy) occurrenceMap
  in fst $ rebuildPattern Set.empty canonicalMap pattern

reconcileOccurrences 
  :: (HasIdentity v i, Mergeable v)
  => ReconciliationPolicy (MergeStrategy v) 
  -> [(Pattern v, Path)] 
  -> Pattern v
reconcileOccurrences LastWriteWins occurrences =
  case map fst occurrences of
    [] -> error "reconcileOccurrences: empty occurrences"
    patterns -> 
      let v = value (last patterns)
          allElements = mergeElements UnionElements (map elements patterns)
      in Pattern v allElements
reconcileOccurrences FirstWriteWins occurrences =
  case map fst occurrences of
    [] -> error "reconcileOccurrences: empty occurrences"
    patterns@(p:_) ->
      let v = value p
          allElements = mergeElements UnionElements (map elements patterns)
      in Pattern v allElements
reconcileOccurrences (Merge elemStrat valStrat) occurrences =
  let patterns = map fst occurrences
      vals = map value patterns
      mergedVal = foldl1 (merge valStrat) vals
      mergedElements = mergeElements elemStrat (map elements patterns)
  in Pattern mergedVal mergedElements
reconcileOccurrences Strict _ = error "Strict policy handled separately"

mergeElements 
  :: (HasIdentity v i, Mergeable v) 
  => ElementMergeStrategy 
  -> [[Pattern v]] 
  -> [Pattern v]
mergeElements ReplaceElements elemLists = 
  case reverse elemLists of
    [] -> []
    (latest:_) -> latest
mergeElements AppendElements elemLists = concat elemLists
mergeElements UnionElements elemLists =
  let allElements = concat elemLists
      elementMap = foldr insertElement Map.empty allElements
  in Map.elems elementMap
  where
    insertElement p m =
      let elemId = identity (value p)
      in Map.insertWith (\_ old -> old) elemId p m -- Keep first encountered

reconcileStrict 
  :: (HasIdentity v i, Eq v) 
  => Pattern v 
  -> Either (ReconcileError i v) (Pattern v)
reconcileStrict pattern =
  let conflicts = findConflicts pattern
  in if null conflicts
       then Right pattern
       else Left $ ReconcileError "Duplicate identities with different content" conflicts

rebuildPattern 
  :: (HasIdentity v i)
  => Set i 
  -> Map i (Pattern v) 
  -> Pattern v 
  -> (Pattern v, Set i)
rebuildPattern visited canonicalMap (Pattern val elems) =
  let vId = identity val
      canonical = case Map.lookup vId canonicalMap of
        Just (Pattern canonVal canonElems) ->
          let visited' = Set.insert vId visited
              (rebuiltCanonElems, finalVisited) = foldl rebuildElem ([], visited') canonElems
          in (Pattern canonVal (reverse rebuiltCanonElems), finalVisited)
        Nothing ->
          let visited' = Set.insert vId visited
              (rebuiltElems, finalVisited) = foldl rebuildElem ([], visited') elems
          in (Pattern val (reverse rebuiltElems), finalVisited)
  in canonical
  where
    rebuildElem (acc, vis) elem =
      let elemId = identity (value elem)
      in if Set.member elemId vis
           then (acc, vis)
           else let (rebuilt, vis') = rebuildPattern vis canonicalMap elem
                in (rebuilt : acc, vis')

collectByIdentity 
  :: (HasIdentity v i) 
  => Pattern v 
  -> Map i [(Pattern v, Path)]
collectByIdentity pattern =
  let occurrences = collectWithPath [] pattern
  in foldr insertOccurrence Map.empty occurrences
  where
    collectWithPath path pat@(Pattern _ elems) =
      (pat, path) : concatMap (\(idx, elem) -> collectWithPath (path ++ [idx]) elem) (zip [0..] elems)
    insertOccurrence (pat, path) =
      Map.insertWith (++) (identity (value pat)) [(pat, path)]

findConflicts 
  :: (HasIdentity v i, Eq v) 
  => Pattern v 
  -> [Conflict i v]
findConflicts pattern =
  let occurrenceMap = collectByIdentity pattern
  in concatMap checkIdentity (Map.toList occurrenceMap)
  where
    checkIdentity (_, [(_, _)]) = []
    checkIdentity (id, occurrences@((Pattern first _, firstPath):rest)) =
      [ Conflict id first (value incoming) [firstPath, incomingPath]
      | (incoming, incomingPath) <- rest
      , value incoming /= first
      ]
    checkIdentity _ = []

needsReconciliation 
  :: (HasIdentity v i) 
  => Pattern v 
  -> Bool
needsReconciliation pattern =
  any (\xs -> length xs > 1) (Map.elems $ collectByIdentity pattern)

isReference 
  :: (HasIdentity v i, Refinable v)
  => Pattern v 
  -> Map i (Pattern v) 
  -> Bool
isReference (Pattern val []) fullerMap =
  case Map.lookup (identity val) fullerMap of
    Nothing -> False
    Just (Pattern fuller _) -> isRefinementOf fuller val
isReference _ _ = False