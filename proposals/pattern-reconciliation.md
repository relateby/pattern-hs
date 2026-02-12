# Proposal: Pattern Reconciliation for pattern-hs

## Summary

Add a `reconcile` operation to `Pattern.Core` that normalizes patterns by resolving duplicate identities and completing partial references. This transforms a potentially inconsistent `Pattern Subject` (from parsing, streaming, or merging) into a coherent one where each identity appears exactly once with fully resolved content.

## Motivation

### The Problem

`Pattern Subject` is a recursive s-expression-like structure where `Subject` values carry identity. Currently, nothing prevents the same identity from appearing multiple times with potentially different content:

```haskell
-- Same identity "alice" appears twice with different properties
example :: Pattern Subject
example = pattern rootSubject
  [ point aliceV1           -- alice with name="Alice"
  , point bobSubject
  , point aliceV2           -- alice with name="Alice Smith" (updated?)
  ]
```

This situation arises naturally when:

1. **Parsing gram notation** — Sequential form allows the same ID to appear multiple times
2. **Streaming patterns** — Incremental updates arrive as new patterns with same IDs
3. **Merging patterns** — Combining patterns from different sources
4. **References** — An atomic pattern may reference a fuller definition elsewhere

### Current State

There is no mechanism to:
- Detect duplicate identities
- Resolve which definition is authoritative
- Complete partial references with full content
- Produce a "canonical" pattern from a messy input

### Desired Outcome

A `reconcile` function that takes a `Pattern Subject` and produces a normalized `Pattern Subject` where:
- Each identity appears exactly once
- Partial patterns (references) are replaced with their full definitions
- Conflicts are resolved according to a specified policy
- The result is "coherent" and ready for storage or further processing

## Design

### Core Types

```haskell
-- | Policy for resolving duplicate identities
data ReconciliationPolicy
  = LastWriteWins      -- ^ Later occurrence overwrites earlier
  | FirstWriteWins     -- ^ First occurrence is kept, later ignored
  | Merge MergeStrategy -- ^ Merge content from all occurrences
  | Strict             -- ^ Fail on any duplicate with different content
  deriving (Eq, Show)

-- | Strategy for merging Subject content
data MergeStrategy = MergeStrategy
  { labelMerge    :: LabelMerge
  , propertyMerge :: PropertyMerge
  , elementMerge  :: ElementMerge
  } deriving (Eq, Show)

data LabelMerge
  = UnionLabels        -- ^ Combine all labels
  | IntersectLabels    -- ^ Keep only common labels
  | ReplaceLabels      -- ^ Later labels replace earlier
  deriving (Eq, Show)

data PropertyMerge
  = ReplaceProperties  -- ^ Later properties replace earlier
  | ShallowMerge       -- ^ Merge top-level keys (later wins on conflict)
  | DeepMerge          -- ^ Recursive merge of nested structures
  deriving (Eq, Show)

data ElementMerge
  = ReplaceElements    -- ^ Later elements replace earlier
  | AppendElements     -- ^ Concatenate element lists
  | UnionElements      -- ^ Deduplicate by identity
  deriving (Eq, Show)

-- | Default merge strategy: union labels, shallow merge properties, union elements
defaultMergeStrategy :: MergeStrategy
defaultMergeStrategy = MergeStrategy UnionLabels ShallowMerge UnionElements
```

### Error Types

```haskell
-- | Information about a reconciliation conflict
data Conflict = Conflict
  { conflictId       :: Symbol        -- ^ The identity with conflict
  , conflictExisting :: Subject       -- ^ First/accumulated subject
  , conflictIncoming :: Subject       -- ^ Conflicting subject
  , conflictLocations :: [Path]       -- ^ Where duplicates were found
  } deriving (Eq, Show)

-- | Path to a pattern within the structure
type Path = [Int]  -- indices from root

-- | Reconciliation error (only for Strict policy)
data ReconcileError = ReconcileError
  { errorMessage  :: String
  , errorConflicts :: [Conflict]
  } deriving (Eq, Show)

-- | Report of reconciliation actions taken
data ReconcileReport = ReconcileReport
  { reportDuplicatesFound    :: Int
  , reportReferencesResolved :: Int
  , reportMergesPerformed    :: Int
  , reportSubjectCounts      :: Map Symbol Int  -- ^ id -> occurrence count
  } deriving (Eq, Show)
```

### Core Functions

```haskell
-- | Reconcile a pattern, normalizing duplicate identities and resolving references.
--
-- This is the primary entry point. It walks the pattern, collects all Subject
-- occurrences by identity, reconciles duplicates according to the policy,
-- and rebuilds the pattern with references replaced by full definitions.
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
reconcileWithReport 
  :: ReconciliationPolicy 
  -> Pattern Subject 
  -> (Either ReconcileError (Pattern Subject), ReconcileReport)

-- | Check if a pattern needs reconciliation (has duplicate identities).
needsReconciliation :: Pattern Subject -> Bool

-- | Extract all identity conflicts without reconciling.
findConflicts :: Pattern Subject -> [Conflict]
```

### Reference Detection

A key insight is that "references" are not syntactically marked—they are detected structurally. A pattern is considered a reference if:

1. It is atomic (has no elements)
2. The same identity appears elsewhere with more content (elements, more labels, more properties)

```haskell
-- | Determine if a Subject is a "refinement" of another
-- (has same or more content for same identity)
isRefinementOf :: Subject -> Subject -> Bool
isRefinementOf full partial =
  identity full == identity partial
  && labels partial `Set.isSubsetOf` labels full
  && all (\(k,v) -> Map.lookup k (properties full) == Just v) 
         (Map.toList (properties partial))

-- | Determine if a pattern appears to be a reference
-- (atomic with same identity defined more fully elsewhere)
isReference :: Pattern Subject -> Map Symbol (Pattern Subject) -> Bool
isReference (Pattern subj []) known = 
  case Map.lookup (identity subj) known of
    Just fuller -> not (null (elements fuller)) 
                   || isRefinementOf (value fuller) subj
    Nothing -> False
isReference _ _ = False
```

### Algorithm Sketch

```haskell
reconcile policy pat = do
  -- Phase 1: Collect all subjects by identity
  let occurrences = collectByIdentity pat  -- Map Symbol [(Subject, Path)]
  
  -- Phase 2: Reconcile each identity to a single canonical Subject
  canonical <- reconcileOccurrences policy occurrences  -- Map Symbol Subject
  
  -- Phase 3: Rebuild pattern, replacing duplicates/references with canonical
  rebuildPattern canonical pat

collectByIdentity :: Pattern Subject -> Map Symbol [(Subject, Path)]
collectByIdentity = go []
  where
    go path (Pattern subj elems) =
      let here = Map.singleton (identity subj) [(subj, path)]
          children = zipWith (\i e -> go (path ++ [i]) e) [0..] elems
      in Map.unionsWith (++) (here : children)

reconcileOccurrences 
  :: ReconciliationPolicy 
  -> Map Symbol [(Subject, Path)] 
  -> Either ReconcileError (Map Symbol Subject)
reconcileOccurrences Strict occurrences =
  -- Check all have same content, fail if not
  ...
reconcileOccurrences LastWriteWins occurrences =
  -- Take the last occurrence of each
  Right $ Map.map (fst . last) occurrences
reconcileOccurrences FirstWriteWins occurrences =
  -- Take the first occurrence of each
  Right $ Map.map (fst . head) occurrences
reconcileOccurrences (Merge strategy) occurrences =
  -- Merge all occurrences using strategy
  Right $ Map.map (foldl1 (mergeSubjects strategy) . map fst) occurrences

rebuildPattern 
  :: Map Symbol Subject 
  -> Pattern Subject 
  -> Either ReconcileError (Pattern Subject)
rebuildPattern canonical = go Set.empty
  where
    go seen (Pattern subj elems)
      | identity subj `Set.member` seen = 
          -- Already emitted this identity, skip (or emit as reference?)
          -- Design choice: we eliminate duplicates entirely
          Nothing  -- filtered out by caller
      | otherwise =
          let canonical' = Map.findWithDefault subj (identity subj) canonical
              seen' = Set.insert (identity subj) seen
              elems' = mapMaybe (go seen') elems
          in Just (Pattern canonical' elems')
```

## Module Structure

### Option A: Extend Pattern.Core

Add reconciliation directly to `Pattern.Core`:

```haskell
module Pattern.Core
  ( -- * Pattern Type
    Pattern(..)
    -- * Construction Functions
  , pattern, point, fromList
    -- * Query Functions
  , length, size, depth, values
    -- ... existing exports ...
    
    -- * Reconciliation (NEW)
  , ReconciliationPolicy(..)
  , MergeStrategy(..)
  , LabelMerge(..)
  , PropertyMerge(..)
  , ElementMerge(..)
  , defaultMergeStrategy
  , Conflict(..)
  , ReconcileError(..)
  , ReconcileReport(..)
  , reconcile
  , reconcileWithReport
  , needsReconciliation
  , findConflicts
  ) where
```

**Pros**: Single import for all pattern operations
**Cons**: Core.hs becomes larger, reconciliation is Subject-specific

### Option B: New Pattern.Reconcile Module

Create a separate module:

```haskell
-- Pattern/Reconcile.hs
module Pattern.Reconcile
  ( -- * Policies
    ReconciliationPolicy(..)
  , MergeStrategy(..)
  , LabelMerge(..)
  , PropertyMerge(..)
  , ElementMerge(..)
  , defaultMergeStrategy
    -- * Types
  , Conflict(..)
  , ReconcileError(..)
  , ReconcileReport(..)
    -- * Operations
  , reconcile
  , reconcileWithReport
  , needsReconciliation
  , findConflicts
  ) where

import Pattern.Core
import Subject.Core
```

**Pros**: Separation of concerns, Core stays generic
**Cons**: Users need to import additional module

### Recommendation

**Option B** — Create `Pattern.Reconcile` as a new module.

Rationale:
- `Pattern.Core` is generic over any value type `v`
- Reconciliation is specific to `Pattern Subject` (requires identity)
- Keeps `Pattern.Core` focused and smaller
- Allows reconciliation to depend on both `Pattern.Core` and `Subject.Core`

## Usage Examples

### Basic Reconciliation

```haskell
import Pattern.Core
import Pattern.Reconcile
import Subject.Core

-- After parsing gram (might have duplicates)
parsed :: Pattern Subject
parsed = ...  -- from gram parser

-- Normalize with last-write-wins
case reconcile LastWriteWins parsed of
  Right normalized -> usePattern normalized
  Left err -> handleError err

-- Strict mode for validation
case reconcile Strict parsed of
  Right p -> putStrLn "Pattern is already coherent"
  Left err -> putStrLn $ "Conflicts found: " ++ show (errorConflicts err)
```

### Merging Patterns

```haskell
-- Combine patterns from different sources
combined :: Pattern Subject
combined = pattern rootSubject [patternA, patternB, patternC]

-- Merge with custom strategy
let strategy = MergeStrategy 
      { labelMerge = UnionLabels
      , propertyMerge = DeepMerge
      , elementMerge = UnionElements
      }
    
case reconcile (Merge strategy) combined of
  Right unified -> savePattern unified
  Left err -> handleConflicts err
```

### Streaming Updates

```haskell
-- Process stream of pattern updates
processUpdates :: Pattern Subject -> [Pattern Subject] -> Pattern Subject
processUpdates base updates = 
  let combined = pattern (value base) (elements base ++ updates)
  in case reconcile LastWriteWins combined of
       Right result -> result
       Left _ -> base  -- shouldn't happen with LastWriteWins
```

### With Report

```haskell
-- Get detailed information about what was reconciled
let (result, report) = reconcileWithReport (Merge defaultMergeStrategy) parsed
case result of
  Right normalized -> do
    putStrLn $ "Resolved " ++ show (reportDuplicatesFound report) ++ " duplicates"
    putStrLn $ "Completed " ++ show (reportReferencesResolved report) ++ " references"
    usePattern normalized
  Left err -> 
    handleError err
```

## Relationship to Existing Functions

### Complements, Does Not Replace

| Existing | Purpose | Reconcile Relationship |
|----------|---------|------------------------|
| `fmap` | Transform values | Reconcile transforms structure |
| `traverse` | Effectful traversal | Reconcile is a specific transformation |
| `filterPatterns` | Find matching patterns | `findConflicts` is similar but identity-focused |
| `contains` | Check for subpattern | Reconcile resolves containment of same identity |
| `para` | Structure-aware fold | Reconcile uses similar traversal internally |

### New Capability

Reconciliation provides something none of the existing functions do: **identity-aware normalization**. It treats `Pattern Subject` as a collection of entities that should each appear once, rather than as a pure tree structure.

## Implementation Considerations

### Performance

- **Time Complexity**: O(n) for collection, O(n) for rebuild = O(n) overall
- **Space Complexity**: O(k) for the identity map where k = unique identities
- Should handle patterns with 100+ nesting levels (use explicit stack if needed)
- Should handle patterns with 10,000+ subjects efficiently

### Edge Cases

1. **Self-referential**: Pattern where a subject's elements include itself
   - Resolution: Track seen identities, emit each only once

2. **Orphan references**: Reference to identity not defined elsewhere
   - Resolution: Keep as-is (atomic pattern) or error depending on policy

3. **Empty pattern**: Pattern with no subjects
   - Resolution: Return unchanged

4. **Circular references**: A references B, B references A
   - Resolution: Track visited during rebuild, break cycles

### Testing Requirements

1. **Property tests**: 
   - `reconcile p` is idempotent: `reconcile (reconcile p) == reconcile p`
   - `reconcile` preserves all unique identities
   - `Strict` mode detects all and only actual conflicts
   
2. **Unit tests**:
   - Each policy resolves correctly
   - Each merge strategy combines correctly
   - Reference resolution works
   - Edge cases handled

3. **Equivalence tests**:
   - Round-trip: `reconcile . parse . serialize == reconcile` (for gram)

## Migration / Compatibility

This is a **new feature** with no breaking changes:
- New module `Pattern.Reconcile`
- New types and functions
- Existing code continues to work unchanged
- Users opt-in by importing and calling `reconcile`

## Open Questions

1. **Should orphan references error or pass through?**
   - Current proposal: Pass through (keep atomic pattern)
   - Alternative: Add `OrphanPolicy` option

2. **What about elements order after reconciliation?**
   - Current proposal: Preserve order of first occurrence, dedupe later
   - Alternative: Sort by identity, or by original position

3. **Should there be a `Reconcilable` typeclass?**
   - Current proposal: No, keep it `Subject`-specific
   - Alternative: `class HasIdentity v where identity :: v -> Symbol`

4. **Streaming API?**
   - Current proposal: Batch reconciliation only
   - Future: `reconcileIncremental :: Pattern Subject -> Pattern Subject -> Pattern Subject`

## Summary

Adding `reconcile` to pattern-hs provides a fundamental operation for working with `Pattern Subject`:

- **Makes patterns coherent** — resolves duplicates and references
- **Configurable policies** — different use cases need different resolution strategies  
- **Complements existing API** — does not change or break anything
- **Enables downstream use** — storage, query, and transmission all benefit from coherent patterns

The implementation is straightforward given the existing infrastructure (`para`, `foldMap`, etc.) and follows established patterns in the codebase.

## Appendix: Full Type Signatures

```haskell
-- Pattern/Reconcile.hs

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

import Data.Map (Map)
import Data.Set (Set)
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))

-- Types
data ReconciliationPolicy = LastWriteWins | FirstWriteWins | Merge MergeStrategy | Strict
data MergeStrategy = MergeStrategy LabelMerge PropertyMerge ElementMerge
data LabelMerge = UnionLabels | IntersectLabels | ReplaceLabels
data PropertyMerge = ReplaceProperties | ShallowMerge | DeepMerge
data ElementMerge = ReplaceElements | AppendElements | UnionElements

type Path = [Int]
data Conflict = Conflict Symbol Subject Subject [Path]
data ReconcileError = ReconcileError String [Conflict]
data ReconcileReport = ReconcileReport Int Int Int (Map Symbol Int)

defaultMergeStrategy :: MergeStrategy

-- Core operations
reconcile :: ReconciliationPolicy -> Pattern Subject -> Either ReconcileError (Pattern Subject)
reconcileWithReport :: ReconciliationPolicy -> Pattern Subject -> (Either ReconcileError (Pattern Subject), ReconcileReport)

-- Inspection
needsReconciliation :: Pattern Subject -> Bool
findConflicts :: Pattern Subject -> [Conflict]
collectByIdentity :: Pattern Subject -> Map Symbol [(Subject, Path)]

-- Utilities
isRefinementOf :: Subject -> Subject -> Bool
isReference :: Pattern Subject -> Map Symbol (Pattern Subject) -> Bool
mergeSubjects :: MergeStrategy -> Subject -> Subject -> Subject
```
