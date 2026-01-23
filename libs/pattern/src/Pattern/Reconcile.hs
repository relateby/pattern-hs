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

-- Placeholder types - will be defined in Phase 2
data ReconciliationPolicy = LastWriteWins | FirstWriteWins | Merge MergeStrategy | Strict
  deriving (Eq, Show, Generic)

data MergeStrategy = MergeStrategy
  { labelMerge    :: LabelMerge
  , propertyMerge :: PropertyMerge
  , elementMerge  :: ElementMerge
  } deriving (Eq, Show, Generic)

data LabelMerge = UnionLabels | IntersectLabels | ReplaceLabels
  deriving (Eq, Show, Generic)

data PropertyMerge = ReplaceProperties | ShallowMerge | DeepMerge
  deriving (Eq, Show, Generic)

data ElementMerge = ReplaceElements | AppendElements | UnionElements
  deriving (Eq, Show, Generic)

-- Placeholder for other types
data Conflict = Conflict deriving (Eq, Show, Generic)
data ReconcileError = ReconcileError deriving (Eq, Show, Generic)
data ReconcileReport = ReconcileReport deriving (Eq, Show, Generic)
type Path = [Int]

-- Placeholder functions - will be implemented in later phases
defaultMergeStrategy :: MergeStrategy
defaultMergeStrategy = error "Not yet implemented"

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
