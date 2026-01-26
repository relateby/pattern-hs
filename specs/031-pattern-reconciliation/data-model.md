# Data Model: Pattern Subject Reconciliation

**Feature**: Pattern Subject Reconciliation (031)
**Created**: 2026-01-23
**Purpose**: Define the core data types, their relationships, and validation rules for the reconciliation feature

## Overview

The reconciliation data model consists of policy types that control behavior, result types that report outcomes, and internal types used during the reconciliation process. All types are defined in the `Pattern.Reconcile` module.

## Core Entities

### ReconciliationPolicy

**Purpose**: Specifies how duplicate identities should be resolved during reconciliation.

**Definition**:
```haskell
data ReconciliationPolicy s
  = LastWriteWins          -- ^ Keep the last occurrence of each identity
  | FirstWriteWins         -- ^ Keep the first occurrence of each identity
  | Merge ElementMergeStrategy s    -- ^ Merge all occurrences using strategies
  | Strict                 -- ^ Fail if any duplicates have different content
  deriving (Eq, Show)
```

**Type Parameters**:
- `s` is the value-specific merge strategy (e.g., `SubjectMergeStrategy` for `Pattern Subject`)

**Relationships**:
- Used as input to `reconcile` and `reconcileWithReport` functions
- `Merge` variant contains both `ElementMergeStrategy` and a value-specific strategy `s`
- Determines how `Conflict` errors are handled in `Strict` mode

**Validation Rules**:
- All variants are valid (ADT exhaustiveness ensures this)
- No runtime validation needed for policy construction

**State Transitions**: Immutable value type, no state transitions

---

### ElementMergeStrategy

**Purpose**: Configures how the children (elements) of a pattern should be merged when combining duplicate identities.

**Definition**:
```haskell
data ElementMergeStrategy
  = ReplaceElements    -- ^ Later element lists replace earlier ones
  | AppendElements     -- ^ Concatenate all element lists
  | UnionElements      -- ^ Deduplicate elements by identity
  deriving (Eq, Show)
```

**Relationships**:
- First parameter in `Merge` constructor of `ReconciliationPolicy`
- Applied to `Pattern.elements` (list of patterns) during merge operations
- Works independently of value-specific merge strategies

**Validation Rules**:
- All strategy options are valid

**Usage Constraints**:
- `UnionElements` requires identity comparison, so only applicable to patterns with identifiable values

---

### SubjectMergeStrategy

**Purpose**: Configures how Subject content (labels and properties) should be merged when combining duplicate identities.

**Definition**:
```haskell
data SubjectMergeStrategy = SubjectMergeStrategy
  { labelMerge    :: LabelMerge
  , propertyMerge :: PropertyMerge
  } deriving (Eq, Show)

data LabelMerge
  = UnionLabels        -- ^ Combine all labels from all occurrences
  | IntersectLabels    -- ^ Keep only labels present in all occurrences
  | ReplaceLabels      -- ^ Later labels replace earlier ones
  deriving (Eq, Show)

data PropertyMerge
  = ReplaceProperties  -- ^ Later properties completely replace earlier
  | ShallowMerge       -- ^ Merge top-level keys (later wins on conflict)
  | DeepMerge          -- ^ Recursive merge of nested map structures
  deriving (Eq, Show)
```

**Relationships**:
- Second parameter in `Merge` constructor of `ReconciliationPolicy` (when reconciling `Pattern Subject`)
- Applied to `Subject` fields during merge operations:
  - `LabelMerge` → `Subject.labels` (Set of labels)
  - `PropertyMerge` → `Subject.properties` (Map of key-value pairs)

**Validation Rules**:
- All strategy combinations are valid
- Default strategy is `defaultSubjectMergeStrategy = SubjectMergeStrategy UnionLabels ShallowMerge`

**Usage Constraints**:
- `DeepMerge` requires property values to be compatible for merging (maps can merge, scalars use last-write-wins)
- `IntersectLabels` may result in empty label sets if no common labels exist

---

### Conflict

**Purpose**: Captures information about a duplicate identity with different content, used in error reporting and strict mode validation.

**Definition**:
```haskell
data Conflict = Conflict
  { conflictId        :: Symbol         -- ^ The identity with conflict
  , conflictExisting  :: Subject        -- ^ First/accumulated subject
  , conflictIncoming  :: Subject        -- ^ Conflicting subject
  , conflictLocations :: [Path]         -- ^ Where duplicates were found
  } deriving (Eq, Show)

type Path = [Int]  -- ^ Indices from root to location
```

**Relationships**:
- Multiple `Conflict` values contained in `ReconcileError`
- References `Symbol` from `Subject.Core` (identity)
- References `Subject` from `Subject.Core` (the conflicting subjects)
- Uses `Path` to indicate structural location within pattern tree

**Validation Rules**:
- `conflictId` must match `identity conflictExisting` and `identity conflictIncoming`
- `conflictExisting` and `conflictIncoming` must have different content (different labels, properties, or structure)
- `conflictLocations` must be non-empty (at least one location where conflict occurred)
- Path indices must be valid (within bounds of parent pattern's elements)

**Path Semantics**:
- Empty path `[]` indicates root pattern
- Path `[0]` indicates first element of root
- Path `[0, 2, 1]` indicates: first element → third sub-element → second sub-sub-element

---

### ReconcileError

**Purpose**: Error type returned when reconciliation fails in Strict mode or encounters unrecoverable issues.

**Definition**:
```haskell
data ReconcileError = ReconcileError
  { errorMessage  :: String
  , errorConflicts :: [Conflict]
  } deriving (Eq, Show)
```

**Relationships**:
- Contains list of `Conflict` values
- Returned as `Left ReconcileError` from `reconcile` function
- Used only in `Strict` policy mode and unrecoverable error cases

**Validation Rules**:
- `errorConflicts` should be non-empty for conflict-related errors
- `errorMessage` should be descriptive and actionable

**Error Categories**:
1. **Conflict Errors** (Strict mode): Multiple `Conflict` values, one per duplicate identity
2. **Structural Errors** (rare): Invalid pattern structure detected during traversal

---

### ReconcileReport

**Purpose**: Tracks and reports statistics about what reconciliation operations were performed.

**Definition**:
```haskell
data ReconcileReport = ReconcileReport
  { reportDuplicatesFound    :: Int                 -- ^ Count of duplicate identities
  , reportReferencesResolved :: Int                 -- ^ Count of references completed
  , reportMergesPerformed    :: Int                 -- ^ Count of merge operations
  , reportSubjectCounts      :: Map Symbol Int      -- ^ Occurrence count per identity
  } deriving (Eq, Show)
```

**Relationships**:
- Returned alongside result from `reconcileWithReport` function
- `reportSubjectCounts` maps `Symbol` (identity) to occurrence count
- Independent of policy used (reports what happened, not how)

**Validation Rules**:
- All count fields must be non-negative
- `reportDuplicatesFound ≤ sum of (reportSubjectCounts values where count > 1)`
- `reportReferencesResolved ≤ reportDuplicatesFound` (references are a subset of duplicates)
- Empty report: `ReconcileReport 0 0 0 Map.empty`

**Metrics**:
- **Duplicates Found**: Count of identities that appeared more than once
- **References Resolved**: Count of atomic patterns replaced with full definitions
- **Merges Performed**: Count of actual merge operations (when using Merge policy)
- **Subject Counts**: Histogram of how many times each identity appeared

---

### Path

**Purpose**: Represents the location of a pattern within the nested structure, used for conflict reporting and debugging.

**Definition**:
```haskell
type Path = [Int]  -- ^ Sequence of indices from root
```

**Relationships**:
- Used in `Conflict.conflictLocations`
- Derived during pattern traversal
- Maps to `Pattern.elements` indices

**Validation Rules**:
- Each index must be valid for the parent pattern's elements list
- Empty path `[]` is valid (represents root)
- Paths are relative to the root of the pattern being reconciled

**Examples**:
```haskell
-- Path [0] in pattern (root [a, b, c]) refers to element a
-- Path [1, 0] refers to first sub-element of b
-- Path [] refers to the root pattern itself
```

---

## Internal Types (Not Exported)

These types are used internally during reconciliation but not exposed in the public API.

### OccurrenceMap

**Purpose**: Internal map tracking all occurrences of each identity during collection phase.

**Definition**:
```haskell
type OccurrenceMap = Map Symbol [(Subject, Path)]
```

**Usage**: Intermediate structure built by `collectByIdentity`, consumed by `reconcileOccurrences`.

---

### CanonicalMap

**Purpose**: Internal map storing the single canonical Subject for each identity after reconciliation.

**Definition**:
```haskell
type CanonicalMap = Map Symbol Subject
```

**Usage**: Result of `reconcileOccurrences`, used by `rebuildPattern` to replace duplicates.

---

## Relationships Diagram

```text
ReconciliationPolicy s
├─ LastWriteWins
├─ FirstWriteWins
├─ Merge ElementMergeStrategy s
│  ├─ ElementMergeStrategy (ReplaceElements | AppendElements | UnionElements)
│  └─ s (value-specific strategy, e.g., SubjectMergeStrategy)
└─ Strict

SubjectMergeStrategy
├─ labelMerge: LabelMerge (UnionLabels | IntersectLabels | ReplaceLabels)
└─ propertyMerge: PropertyMerge (ReplaceProperties | ShallowMerge | DeepMerge)

Conflict
├─ conflictId: Symbol
├─ conflictExisting: Subject
├─ conflictIncoming: Subject
└─ conflictLocations: [Path]

ReconcileError
├─ errorMessage: String
└─ errorConflicts: [Conflict]

ReconcileReport
├─ reportDuplicatesFound: Int
├─ reportReferencesResolved: Int
├─ reportMergesPerformed: Int
└─ reportSubjectCounts: Map Symbol Int

Path = [Int]
```

---

## Validation Summary

| Entity | Required Validations | When Validated |
|--------|----------------------|----------------|
| ReconciliationPolicy | None (ADT exhaustive) | Construction |
| ElementMergeStrategy | None (ADT exhaustive) | Construction |
| SubjectMergeStrategy | None (all combinations valid) | Construction |
| Conflict | Identity match, different content, non-empty locations | During conflict detection |
| ReconcileError | Non-empty conflicts for conflict errors | During error construction |
| ReconcileReport | Non-negative counts, count consistency | During report accumulation |
| Path | Valid indices for pattern structure | During traversal |

---

## State Transitions

All types are immutable values with no state transitions. The reconciliation process itself has phases:

1. **Collection Phase**: `Pattern Subject` → `OccurrenceMap`
   - Traverse pattern, collect all Subject occurrences by identity
   - Build paths for each occurrence

2. **Reconciliation Phase**: `OccurrenceMap` → `Either ReconcileError CanonicalMap`
   - Apply policy to resolve each identity to single canonical Subject
   - Detect conflicts in Strict mode
   - Perform merges for Merge policy

3. **Rebuild Phase**: `CanonicalMap` → `Pattern Subject`
   - Traverse original pattern
   - Replace duplicates/references with canonical Subjects
   - Track visited identities to emit each once

4. **Reporting Phase** (optional): Throughout → `ReconcileReport`
   - Accumulate statistics during all phases
   - Count duplicates, references, merges

---

## Design Principles

1. **Type Safety**: Use ADTs to make invalid states unrepresentable
2. **Immutability**: All types are pure values, no mutation
3. **Composability**: Strategies are composable (three dimensions combined)
4. **Transparency**: Errors and reports provide full diagnostic information
5. **Efficiency**: Internal maps use strict structures to avoid space leaks

---

## Future Extensions

Potential additions (currently out of scope):

- **Custom Policies**: Allow user-defined reconciliation functions
- **Partial Reports**: Report generation for specific identities
- **Incremental Reconciliation**: Stream-based reconciliation API
- **Conflict Resolution Callbacks**: Interactive conflict resolution
- **Schema-Aware Merging**: Type-specific property merge strategies
