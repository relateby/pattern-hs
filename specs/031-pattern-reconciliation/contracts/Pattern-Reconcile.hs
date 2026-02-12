{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

-- | API Contract: Pattern.Reconcile Module
--
-- This file defines the public API contract for the Pattern.Reconcile module.
-- It specifies type signatures and module structure but omits implementations.
-- This contract serves as:
--   1. Documentation of the public API
--   2. Contract for testing (tests verify these signatures exist)
--   3. Planning artifact for implementation phase
--
-- NOTE: This is a CONTRACT file, not executable code.
-- The actual implementation is in libs/pattern/src/Pattern/Reconcile.hs
--
-- The implementation is generic over types that have HasIdentity, Mergeable,
-- and Refinable instances. This contract documents the API for Pattern Subject.

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

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Kind (Type)
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import GHC.Generics (Generic)

-- ============================================================================
-- Core Abstractions (generic over value type)
-- ============================================================================

-- | Allows extracting a unique identifier from a value.
class Ord i => HasIdentity v i | v -> i where
  identity :: v -> i

-- | Allows merging two values according to a strategy.
class Mergeable v where
  type MergeStrategy v :: Type
  merge :: MergeStrategy v -> v -> v -> v

-- | Allows checking if one value is a refinement (partial) of another.
class Refinable v where
  isRefinementOf :: v -> v -> Bool

-- ============================================================================
-- Policy Types
-- ============================================================================

-- | Policy for resolving duplicate identities during reconciliation.
--
-- The type parameter @s@ is the value-specific merge strategy
-- (e.g. 'SubjectMergeStrategy' for @Pattern Subject@).
--
-- Four strategies are provided:
--
-- * 'LastWriteWins': Keep the last occurrence (by traversal order)
-- * 'FirstWriteWins': Keep the first occurrence (by traversal order)
-- * 'Merge': Combine all occurrences using element strategy + value strategy
-- * 'Strict': Fail with error if any duplicates have different content
data ReconciliationPolicy s
  = LastWriteWins
  | FirstWriteWins
  | Merge ElementMergeStrategy s
  | Strict
  deriving (Eq, Show, Generic)

-- | Strategy for merging the children (elements) of a pattern.
data ElementMergeStrategy
  = ReplaceElements    -- ^ Later element list completely replaces earlier
  | AppendElements     -- ^ Concatenate all element lists in order
  | UnionElements      -- ^ Deduplicate elements by identity
  deriving (Eq, Show, Generic)

-- | Strategy for merging Subject content (labels and properties).
--
-- Used as the second argument to 'Merge' when reconciling @Pattern Subject@.
-- Element list merging is configured separately via 'ElementMergeStrategy'.
data SubjectMergeStrategy = SubjectMergeStrategy
  { labelMerge    :: LabelMerge
  , propertyMerge :: PropertyMerge
  } deriving (Eq, Show, Generic)

-- | Strategy for merging label sets.
data LabelMerge
  = UnionLabels        -- ^ Combine all labels from all occurrences
  | IntersectLabels    -- ^ Keep only labels present in all occurrences
  | ReplaceLabels      -- ^ Later labels completely replace earlier ones
  deriving (Eq, Show, Generic)

-- | Strategy for merging property maps.
data PropertyMerge
  = ReplaceProperties  -- ^ Later properties completely replace earlier
  | ShallowMerge       -- ^ Merge top-level keys (later wins on conflict)
  | DeepMerge         -- ^ Recursive merge of nested map structures
  deriving (Eq, Show, Generic)

-- | Default merge strategy for Subjects: union labels, shallow merge properties.
defaultSubjectMergeStrategy :: SubjectMergeStrategy
defaultSubjectMergeStrategy = SubjectMergeStrategy UnionLabels ShallowMerge

-- ============================================================================
-- Error and Report Types
-- ============================================================================

-- | Information about a duplicate identity with conflicting content.
-- Polymorphic in identity type @i@ and value type @v@.
data Conflict i v = Conflict
  { conflictId        :: i
  , conflictExisting  :: v
  , conflictIncoming  :: v
  , conflictLocations :: [Path]
  } deriving (Eq, Show, Generic)

-- | Path to a pattern within the nested structure (sequence of indices from root).
type Path = [Int]

-- | Error type returned when reconciliation fails.
data ReconcileError i v = ReconcileError
  { errorMessage   :: String
  , errorConflicts :: [Conflict i v]
  } deriving (Eq, Show, Generic)

-- | Report of reconciliation actions taken.
data ReconcileReport i = ReconcileReport
  { reportDuplicatesFound    :: Int
  , reportReferencesResolved :: Int
  , reportMergesPerformed    :: Int
  , reportSubjectCounts      :: Map i Int
  } deriving (Eq, Show, Generic)

-- ============================================================================
-- Core Reconciliation Operations
-- ============================================================================

-- | Reconcile a pattern, normalizing duplicate identities and resolving references.
reconcile
  :: (HasIdentity v i, Mergeable v, Refinable v, Eq v)
  => ReconciliationPolicy (MergeStrategy v)
  -> Pattern v
  -> Either (ReconcileError i v) (Pattern v)

-- | Reconcile with detailed report of actions taken.
reconcileWithReport
  :: (HasIdentity v i, Mergeable v, Refinable v, Eq v)
  => ReconciliationPolicy (MergeStrategy v)
  -> Pattern v
  -> (Either (ReconcileError i v) (Pattern v), ReconcileReport i)

-- ============================================================================
-- Inspection Operations
-- ============================================================================

-- | Check if a pattern needs reconciliation (has duplicate identities).
needsReconciliation
  :: (HasIdentity v i)
  => Pattern v
  -> Bool

-- | Extract all identity conflicts without reconciling.
findConflicts
  :: (HasIdentity v i, Eq v)
  => Pattern v
  -> [Conflict i v]

-- | Collect all occurrences grouped by identity. Returns map from identity
-- to list of (pattern, path) pairs. For @Pattern Subject@: Map Symbol [(Pattern Subject, Path)].
collectByIdentity
  :: (HasIdentity v i)
  => Pattern v
  -> Map i [(Pattern v, Path)]

-- ============================================================================
-- Utility Operations
-- ============================================================================

-- | Determine if a pattern appears to be a reference (atomic pattern with
-- fuller definition in the map). Second argument is map of known fuller definitions.
isReference
  :: (HasIdentity v i, Refinable v)
  => Pattern v
  -> Map i (Pattern v)
  -> Bool
