{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Pattern reconciliation for normalizing duplicate identities.
--
-- This module provides operations to reconcile patterns by resolving duplicate
-- identities and completing partial references. When patterns are parsed, streamed,
-- or merged from multiple sources, the same identity may appear multiple times
-- with different or evolving content. Reconciliation transforms such patterns into
-- coherent ones where each identity appears exactly once.
--
-- == Quick Start
--
-- >>> import Pattern.Core (Pattern(..), pattern, point)
-- >>> import Pattern.Reconcile
-- >>> import Subject.Core (Subject(..), Symbol(..))
-- >>>
-- >>> -- Reconcile with LastWriteWins policy
-- >>> reconcile LastWriteWins patternWithDuplicates
--
-- == Reconciliation Policies
--
-- * 'LastWriteWins' - Keep the last occurrence of each identity
-- * 'FirstWriteWins' - Keep the first occurrence of each identity
-- * 'Merge' - Combine all occurrences using a configurable strategy
-- * 'Strict' - Fail if any duplicates have different content
--
-- See 'reconcile' for the main entry point.

module Pattern.Reconcile
  ( -- * Reconciliation Policies
    ReconciliationPolicy(..)
  , MergeStrategy(..)
  , LabelMerge(..)
  , PropertyMerge(..)
  , ElementMerge(..)
  , defaultMergeStrategy

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
  , isRefinementOf
  , isReference
  , mergeSubjects
  ) where

import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import Subject.Core (Subject, Symbol)

-- | Policy for resolving duplicate identities during reconciliation.
--
-- When a pattern contains the same identity multiple times (determined by
-- comparing 'Symbol' values), a reconciliation policy determines how to
-- combine or choose between the duplicate occurrences.
--
-- ==== Policies
--
-- * 'LastWriteWins' - Keep the last occurrence encountered during traversal
-- * 'FirstWriteWins' - Keep the first occurrence encountered during traversal
-- * 'Merge' - Combine all occurrences using the provided 'MergeStrategy'
-- * 'Strict' - Fail with 'ReconcileError' if duplicates have different content
--
-- ==== Examples
--
-- >>> reconcile LastWriteWins patternWithDuplicates
-- Right (Pattern ...)  -- contains last occurrence of each identity
--
-- >>> reconcile (Merge defaultMergeStrategy) patternWithDuplicates
-- Right (Pattern ...)  -- contains merged content from all occurrences
--
-- >>> reconcile Strict patternWithConflicts
-- Left (ReconcileError "Duplicate identities" [...])
data ReconciliationPolicy
  = LastWriteWins
  -- ^ Keep the last occurrence of each identity.
  --
  -- Useful for streaming updates where the most recent value is authoritative.
  | FirstWriteWins
  -- ^ Keep the first occurrence of each identity.
  --
  -- Useful when initial definitions are authoritative and later ones are ignored.
  | Merge MergeStrategy
  -- ^ Combine all occurrences using the specified merge strategy.
  --
  -- Useful for data integration where information from multiple sources is combined.
  | Strict
  -- ^ Fail if any duplicate identities have different content.
  --
  -- Useful for validation to ensure pattern coherence before processing.
  deriving (Eq, Show, Generic)

-- | Strategy for merging Subject content when combining duplicate identities.
--
-- A merge strategy is composed of three independent sub-strategies, one for
-- each major component of a Subject:
--
-- * 'labelMerge' - How to combine label sets
-- * 'propertyMerge' - How to combine property maps
-- * 'elementMerge' - How to combine element lists
--
-- Use 'defaultMergeStrategy' for common use cases, or construct a custom
-- strategy for fine-grained control.
--
-- ==== Examples
--
-- >>> let strategy = MergeStrategy UnionLabels ShallowMerge UnionElements
-- >>> reconcile (Merge strategy) pattern
data MergeStrategy = MergeStrategy
  { labelMerge    :: LabelMerge
  -- ^ Strategy for combining label sets
  , propertyMerge :: PropertyMerge
  -- ^ Strategy for combining property maps
  , elementMerge  :: ElementMerge
  -- ^ Strategy for combining element lists
  } deriving (Eq, Show, Generic)

-- | Strategy for merging label sets when combining duplicate identities.
--
-- Labels are tags or classifications applied to a Subject. When the same
-- identity appears with different labels, this strategy determines the result.
--
-- ==== Strategies
--
-- * 'UnionLabels' - Combine all labels (set union)
-- * 'IntersectLabels' - Keep only common labels (set intersection)
-- * 'ReplaceLabels' - Later labels completely replace earlier ones
data LabelMerge
  = UnionLabels
  -- ^ Combine all labels from all occurrences.
  --
  -- Result includes every label that appears in any occurrence.
  -- Recommended for most use cases.
  | IntersectLabels
  -- ^ Keep only labels present in all occurrences.
  --
  -- Result includes only labels that appear in every occurrence.
  -- Useful for finding common classifications.
  | ReplaceLabels
  -- ^ Later labels completely replace earlier ones.
  --
  -- Result contains only the labels from the last occurrence.
  -- Useful when labels represent a single classification that can change.
  deriving (Eq, Show, Generic)

-- | Strategy for merging property maps when combining duplicate identities.
--
-- Properties are key-value pairs storing structured data about a Subject.
-- When the same identity appears with different properties, this strategy
-- determines how to combine the maps.
--
-- ==== Strategies
--
-- * 'ReplaceProperties' - Later properties replace earlier ones entirely
-- * 'ShallowMerge' - Merge top-level keys (later wins on conflicts)
-- * 'DeepMerge' - Recursively merge nested map structures
data PropertyMerge
  = ReplaceProperties
  -- ^ Later property map completely replaces earlier ones.
  --
  -- Result contains only the properties from the last occurrence.
  -- Useful when property maps represent complete snapshots.
  | ShallowMerge
  -- ^ Merge top-level keys, with later values winning on conflicts.
  --
  -- Result combines all keys from all occurrences. If the same key appears
  -- multiple times, the value from the later occurrence is used.
  -- Recommended for most use cases.
  | DeepMerge
  -- ^ Recursively merge nested map structures.
  --
  -- Result deeply merges property maps. For nested maps, recursively combines
  -- keys. For non-map values, later values win. Useful for configuration-like
  -- data with nested structure.
  deriving (Eq, Show, Generic)

-- | Strategy for merging element lists when combining duplicate identities.
--
-- Elements are nested Pattern values contained within a Subject. When the
-- same identity appears with different element lists, this strategy determines
-- how to combine them.
--
-- ==== Strategies
--
-- * 'ReplaceElements' - Later elements replace earlier ones entirely
-- * 'AppendElements' - Concatenate all element lists in order
-- * 'UnionElements' - Deduplicate elements by identity
data ElementMerge
  = ReplaceElements
  -- ^ Later element list completely replaces earlier ones.
  --
  -- Result contains only the elements from the last occurrence.
  -- Useful when element lists represent complete replacements.
  | AppendElements
  -- ^ Concatenate all element lists in traversal order.
  --
  -- Result contains all elements from all occurrences in order, including
  -- duplicates. Useful when order matters or duplicates are meaningful
  -- (e.g., event sequences).
  | UnionElements
  -- ^ Deduplicate elements by identity.
  --
  -- Result contains each distinct identity exactly once. If the same identity
  -- appears in multiple element lists, it is recursively reconciled using the
  -- same policy. Recommended for most use cases.
  deriving (Eq, Show, Generic)

-- | Information about a duplicate identity with conflicting content.
--
-- A conflict occurs when the same identity (determined by 'Symbol' equality)
-- appears multiple times in a pattern with different content (labels,
-- properties, or elements differ).
--
-- Conflicts are reported by 'findConflicts' and in 'ReconcileError' when
-- using the 'Strict' policy.
--
-- ==== Fields
--
-- * 'conflictId' - The identity symbol that appears multiple times
-- * 'conflictExisting' - The first or accumulated Subject encountered
-- * 'conflictIncoming' - The conflicting Subject encountered later
-- * 'conflictLocations' - Paths showing where duplicates appear in the pattern
--
-- ==== Examples
--
-- >>> let conflicts = findConflicts pattern
-- >>> map conflictId conflicts
-- [Symbol "alice", Symbol "bob"]
data Conflict = Conflict
  { conflictId        :: Symbol
  -- ^ The identity with conflicting occurrences
  , conflictExisting  :: Subject
  -- ^ First or accumulated subject for this identity
  , conflictIncoming  :: Subject
  -- ^ Conflicting subject encountered
  , conflictLocations :: [Path]
  -- ^ Locations in the pattern where conflicts occur
  } deriving (Eq, Show, Generic)

-- | Path to a pattern within the nested structure.
--
-- A path is a sequence of indices from the root pattern to a specific location.
-- The empty path @[]@ represents the root, @[0]@ is the first element of the
-- root, @[0, 2]@ is the third element of the first element, and so on.
--
-- Paths are used for conflict reporting and debugging to show exactly where
-- in the pattern structure duplicate identities appear.
--
-- ==== Examples
--
-- >>> [] :: Path
-- []  -- root pattern
--
-- >>> [0] :: Path
-- [0]  -- first element of root
--
-- >>> [1, 2, 0] :: Path
-- [1, 2, 0]  -- first element of third element of second element of root
type Path = [Int]

-- | Error type returned when reconciliation fails.
--
-- Currently used only for 'Strict' policy violations, but designed to support
-- other error cases in future versions.
--
-- ==== Fields
--
-- * 'errorMessage' - Human-readable description of what went wrong
-- * 'errorConflicts' - Detailed information about each conflict found
--
-- ==== Examples
--
-- >>> case reconcile Strict pattern of
-- ...   Left err -> do
-- ...     putStrLn (errorMessage err)
-- ...     mapM_ print (errorConflicts err)
-- ...   Right _ -> putStrLn "No conflicts"
data ReconcileError = ReconcileError
  { errorMessage  :: String
  -- ^ Human-readable error description
  , errorConflicts :: [Conflict]
  -- ^ Details of conflicts found
  } deriving (Eq, Show, Generic)

-- | Report of reconciliation actions taken.
--
-- Provides detailed statistics about what happened during reconciliation.
-- Obtain by using 'reconcileWithReport' instead of 'reconcile'.
--
-- ==== Fields
--
-- * 'reportDuplicatesFound' - Count of identities appearing more than once
-- * 'reportReferencesResolved' - Count of atomic patterns replaced with full definitions
-- * 'reportMergesPerformed' - Count of merge operations performed
-- * 'reportSubjectCounts' - Map from identity to number of occurrences
--
-- ==== Examples
--
-- >>> let (result, report) = reconcileWithReport LastWriteWins pattern
-- >>> reportDuplicatesFound report
-- 5
-- >>> reportReferencesResolved report
-- 2
data ReconcileReport = ReconcileReport
  { reportDuplicatesFound    :: Int
  -- ^ Count of duplicate identities (identities appearing more than once)
  , reportReferencesResolved :: Int
  -- ^ Count of references completed (atomic patterns replaced with full definitions)
  , reportMergesPerformed    :: Int
  -- ^ Count of merge operations performed (only relevant for Merge policy)
  , reportSubjectCounts      :: Map Symbol Int
  -- ^ Occurrence count for each identity in the original pattern
  } deriving (Eq, Show, Generic)

-- | Default merge strategy: union labels, shallow merge properties, union elements.
--
-- This strategy is recommended for most use cases as it:
--
-- * Preserves all labels (no information loss)
-- * Combines properties sensibly (later values override conflicts)
-- * Deduplicates elements by identity (maintains coherent structure)
--
-- ==== Equivalent to
--
-- @
-- MergeStrategy
--   { labelMerge = UnionLabels
--   , propertyMerge = ShallowMerge
--   , elementMerge = UnionElements
--   }
-- @
--
-- ==== Examples
--
-- >>> reconcile (Merge defaultMergeStrategy) pattern
defaultMergeStrategy :: MergeStrategy
defaultMergeStrategy = MergeStrategy
  { labelMerge = UnionLabels
  , propertyMerge = ShallowMerge
  , elementMerge = UnionElements
  }

reconcile :: ReconciliationPolicy -> a -> Either ReconcileError a
reconcile = error "Not yet implemented"

reconcileWithReport :: ReconciliationPolicy -> a -> (Either ReconcileError a, ReconcileReport)
reconcileWithReport = error "Not yet implemented"

needsReconciliation :: a -> Bool
needsReconciliation = error "Not yet implemented"

findConflicts :: a -> [Conflict]
findConflicts = error "Not yet implemented"

collectByIdentity :: a -> b
collectByIdentity = error "Not yet implemented"

isRefinementOf :: a -> a -> Bool
isRefinementOf = error "Not yet implemented"

isReference :: a -> b -> Bool
isReference = error "Not yet implemented"

mergeSubjects :: MergeStrategy -> a -> a -> a
mergeSubjects = error "Not yet implemented"
