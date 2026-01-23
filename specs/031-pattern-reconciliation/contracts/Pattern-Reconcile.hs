{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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
-- The actual implementation will be in libs/pattern/src/Pattern/Reconcile.hs

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

import Data.Map.Strict (Map)
import Data.Set (Set)
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import GHC.Generics (Generic)

-- ============================================================================
-- Policy Types
-- ============================================================================

-- | Policy for resolving duplicate identities during reconciliation.
--
-- Four strategies are provided:
--
-- * 'LastWriteWins': Keep the last occurrence (by traversal order)
-- * 'FirstWriteWins': Keep the first occurrence (by traversal order)
-- * 'Merge': Combine all occurrences using a configurable strategy
-- * 'Strict': Fail with error if any duplicates have different content
--
-- ==== Examples
--
-- >>> reconcile LastWriteWins patternWithDuplicates
-- Right (Pattern ...)  -- last occurrence kept for each identity
--
-- >>> reconcile Strict patternWithConflicts
-- Left (ReconcileError "Duplicate identities" [...])
data ReconciliationPolicy
  = LastWriteWins
  | FirstWriteWins
  | Merge MergeStrategy
  | Strict
  deriving (Eq, Show, Generic)

-- | Strategy for merging Subject content when combining duplicates.
--
-- Composed of three independent strategies for different Subject components:
--
-- * 'labelMerge': How to combine label sets
-- * 'propertyMerge': How to combine property maps
-- * 'elementMerge': How to combine element lists
--
-- Use 'defaultMergeStrategy' for common use cases.
data MergeStrategy = MergeStrategy
  { labelMerge    :: LabelMerge
  , propertyMerge :: PropertyMerge
  , elementMerge  :: ElementMerge
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
  | DeepMerge          -- ^ Recursive merge of nested map structures
  deriving (Eq, Show, Generic)

-- | Strategy for merging element lists.
data ElementMerge
  = ReplaceElements    -- ^ Later element lists completely replace earlier
  | AppendElements     -- ^ Concatenate all element lists in order
  | UnionElements      -- ^ Deduplicate elements by identity
  deriving (Eq, Show, Generic)

-- | Default merge strategy: union labels, shallow merge properties, union elements.
--
-- This strategy is recommended for most use cases:
--
-- * Preserves all labels (no information loss)
-- * Combines properties sensibly (later values override conflicts)
-- * Deduplicates elements by identity (coherent structure)
--
-- ==== Examples
--
-- >>> reconcile (Merge defaultMergeStrategy) pattern
defaultMergeStrategy :: MergeStrategy

-- ============================================================================
-- Error and Report Types
-- ============================================================================

-- | Information about a duplicate identity with conflicting content.
--
-- Used in error reporting (Strict mode) and conflict detection.
data Conflict = Conflict
  { conflictId        :: Symbol        -- ^ The identity with conflict
  , conflictExisting  :: Subject       -- ^ First/accumulated subject
  , conflictIncoming  :: Subject       -- ^ Conflicting subject
  , conflictLocations :: [Path]        -- ^ Where duplicates were found
  } deriving (Eq, Show, Generic)

-- | Path to a pattern within the nested structure.
--
-- A path is a sequence of indices from the root pattern to a specific location.
-- Empty path @[]@ represents the root, path @[0]@ is the first element, etc.
type Path = [Int]

-- | Error type returned when reconciliation fails.
--
-- Currently only used for Strict policy violations, but designed to support
-- other error cases in the future.
data ReconcileError = ReconcileError
  { errorMessage  :: String       -- ^ Human-readable error description
  , errorConflicts :: [Conflict]  -- ^ Details of conflicts found
  } deriving (Eq, Show, Generic)

-- | Report of reconciliation actions taken.
--
-- Provides detailed statistics about what happened during reconciliation:
--
-- * How many duplicate identities were found
-- * How many references were resolved (atomic → full definition)
-- * How many merge operations were performed
-- * Occurrence count for each identity
--
-- Use 'reconcileWithReport' to obtain this information.
data ReconcileReport = ReconcileReport
  { reportDuplicatesFound    :: Int                 -- ^ Count of duplicate identities
  , reportReferencesResolved :: Int                 -- ^ Count of references completed
  , reportMergesPerformed    :: Int                 -- ^ Count of merge operations
  , reportSubjectCounts      :: Map Symbol Int      -- ^ Occurrence count per identity
  } deriving (Eq, Show, Generic)

-- ============================================================================
-- Core Reconciliation Operations
-- ============================================================================

-- | Reconcile a pattern, normalizing duplicate identities and resolving references.
--
-- This is the primary entry point. It walks the pattern, collects all Subject
-- occurrences by identity, reconciles duplicates according to the policy,
-- and rebuilds the pattern with references replaced by full definitions.
--
-- ==== Properties
--
-- * **Idempotent**: @reconcile p (reconcile p x) == reconcile p x@
-- * **Identity Preserving**: Set of unique identities unchanged
-- * **Deterministic**: Same input always produces same output
--
-- ==== Examples
--
-- >>> let alice1 = Subject (Symbol "alice") (Set.singleton "Person") (Map.singleton "v" (VInteger 1))
-- >>> let alice2 = Subject (Symbol "alice") (Set.singleton "Person") (Map.singleton "v" (VInteger 2))
-- >>> let root = Subject (Symbol "root") Set.empty Map.empty
-- >>> let p = pattern root [point alice1, point alice2]
-- >>> reconcile LastWriteWins p
-- Right (Pattern root [Pattern alice2 []])
--
-- >>> reconcile Strict p
-- Left (ReconcileError "Duplicate identity with different content" [...])
reconcile
  :: ReconciliationPolicy
  -> Pattern Subject
  -> Either ReconcileError (Pattern Subject)

-- | Reconcile with detailed report of actions taken.
--
-- Same as 'reconcile', but also returns a 'ReconcileReport' with statistics
-- about what happened during reconciliation.
--
-- ==== Examples
--
-- >>> let (result, report) = reconcileWithReport LastWriteWins pattern
-- >>> reportDuplicatesFound report
-- 5
-- >>> reportReferencesResolved report
-- 2
reconcileWithReport
  :: ReconciliationPolicy
  -> Pattern Subject
  -> (Either ReconcileError (Pattern Subject), ReconcileReport)

-- ============================================================================
-- Inspection Operations
-- ============================================================================

-- | Check if a pattern needs reconciliation (has duplicate identities).
--
-- Returns 'True' if the same identity appears more than once, 'False' otherwise.
-- This is a fast check that doesn't perform full reconciliation.
--
-- ==== Examples
--
-- >>> needsReconciliation (pattern root [point alice1, point alice2])
-- True  -- "alice" appears twice
--
-- >>> needsReconciliation (pattern root [point alice1, point bob1])
-- False  -- each identity appears once
needsReconciliation :: Pattern Subject -> Bool

-- | Extract all identity conflicts without reconciling.
--
-- Returns a list of 'Conflict' values for each duplicate identity that has
-- different content in different occurrences. Useful for validation and
-- diagnostic purposes.
--
-- ==== Examples
--
-- >>> findConflicts pattern
-- [Conflict (Symbol "alice") subject1 subject2 [[0], [2]]]
findConflicts :: Pattern Subject -> [Conflict]

-- | Collect all subject occurrences grouped by identity.
--
-- Internal operation exposed for advanced use cases. Maps each identity to
-- all its occurrences along with their paths in the pattern structure.
--
-- ==== Examples
--
-- >>> collectByIdentity pattern
-- Map.fromList [(Symbol "alice", [(subject1, [0]), (subject2, [2])])]
collectByIdentity :: Pattern Subject -> Map Symbol [(Subject, Path)]

-- ============================================================================
-- Utility Operations
-- ============================================================================

-- | Determine if one Subject is a refinement of another.
--
-- A subject is a refinement if it has the same identity and:
--
-- * All labels from the partial are present in the full
-- * All properties from the partial match in the full
-- * The full may have additional labels/properties
--
-- ==== Examples
--
-- >>> let partial = Subject (Symbol "n") (Set.singleton "Person") Map.empty
-- >>> let full = Subject (Symbol "n") (Set.fromList ["Person", "Employee"]) (Map.singleton "name" (VString "Alice"))
-- >>> isRefinementOf full partial
-- True  -- full refines partial (has all its labels plus more)
isRefinementOf :: Subject -> Subject -> Bool

-- | Determine if a pattern appears to be a reference.
--
-- A pattern is considered a reference if:
--
-- 1. It is atomic (no elements)
-- 2. The same identity appears elsewhere with more content
--
-- The second argument is a map of known fuller definitions.
--
-- ==== Examples
--
-- >>> let atomic = Pattern (Subject (Symbol "alice") Set.empty Map.empty) []
-- >>> let fuller = Pattern (Subject (Symbol "alice") (Set.singleton "Person") Map.empty) [...]
-- >>> isReference atomic (Map.singleton (Symbol "alice") fuller)
-- True  -- atomic pattern, fuller definition exists
isReference :: Pattern Subject -> Map Symbol (Pattern Subject) -> Bool

-- | Merge two subjects according to a merge strategy.
--
-- Combines labels, properties, and structure of two subjects that share the
-- same identity. Used internally during Merge policy reconciliation, exposed
-- for testing and advanced use cases.
--
-- ==== Examples
--
-- >>> let s1 = Subject (Symbol "n") (Set.singleton "A") (Map.singleton "k1" (VInteger 1))
-- >>> let s2 = Subject (Symbol "n") (Set.singleton "B") (Map.singleton "k2" (VInteger 2))
-- >>> mergeSubjects defaultMergeStrategy s1 s2
-- Subject (Symbol "n") (Set.fromList ["A", "B"]) (Map.fromList [("k1", VInteger 1), ("k2", VInteger 2)])
mergeSubjects :: MergeStrategy -> Subject -> Subject -> Subject

-- ============================================================================
-- Notes on Implementation
-- ============================================================================

-- | Implementation approach (for Phase 2):
--
-- 1. Collection Phase: Use 'para' or explicit traversal to collect occurrences
--    - Build OccurrenceMap: Map Symbol [(Subject, Path)]
--    - Track path during traversal
--
-- 2. Reconciliation Phase: Apply policy to each identity
--    - LastWriteWins/FirstWriteWins: Take first/last from list
--    - Merge: Fold mergeSubjects over occurrences
--    - Strict: Check all equal, return conflict if not
--
-- 3. Rebuild Phase: Traverse pattern with visited set
--    - Replace each subject with canonical from map
--    - Skip if identity already visited (emit once)
--    - Recursively rebuild elements
--
-- 4. Reporting Phase: Accumulate stats during phases
--    - Count duplicates: identities with >1 occurrence
--    - Count references: atomic patterns replaced
--    - Count merges: actual merge operations performed
