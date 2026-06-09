# RFC-010: Pattern Reconciliation

**Status:** accepted
**Date:** 2026-01-15
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Supersedes:** [`proposals/pattern-reconciliation.md`](../pattern-reconciliation.md)
**Related modules:** `Pattern.Core`, `Pattern.Reconcile`, `Subject.Core`

## Summary

`Pattern.Reconcile` provides a `reconcile` operation that normalizes `Pattern Subject`
values by resolving duplicate identities and completing partial references. This
transforms a potentially inconsistent pattern (from parsing, streaming, or merging) into
a coherent one where each identity appears exactly once with fully resolved content.
The operation is identity-aware normalization: it treats `Pattern Subject` as a
collection of entities that should each appear once, not as a pure tree structure.

## Motivation

`Pattern Subject` is a recursive container where `Subject` values carry identity. Nothing
prevents the same identity from appearing multiple times with potentially different content:

```haskell
example :: Pattern Subject
example = pattern rootSubject
  [ point aliceV1           -- alice with name="Alice"
  , point bobSubject
  , point aliceV2           -- alice with name="Alice Smith" (updated?)
  ]
```

This situation arises naturally when:
1. **Parsing gram notation** — sequential form allows the same ID to appear multiple times
2. **Streaming patterns** — incremental updates arrive as new patterns with same IDs
3. **Merging patterns** — combining patterns from different sources
4. **References** — an atomic pattern may reference a fuller definition elsewhere

Without reconciliation, there is no mechanism to detect duplicate identities, resolve
which definition is authoritative, or complete partial references.

## Design

### Module Placement

`Pattern.Reconcile` — a separate module from `Pattern.Core`. Rationale: `Pattern.Core`
is generic over any value type `v`; reconciliation is specific to `Pattern Subject`
(requires identity). Keeping them separate allows reconciliation to depend on both
`Pattern.Core` and `Subject.Core` without pulling Subject-specific logic into the
generic core.

### Core Types

```haskell
data ReconciliationPolicy
  = LastWriteWins       -- later occurrence overwrites earlier
  | FirstWriteWins      -- first occurrence is kept, later ignored
  | Merge MergeStrategy -- merge content from all occurrences
  | Strict              -- fail on any duplicate with different content

data MergeStrategy = MergeStrategy
  { labelMerge    :: LabelMerge
  , propertyMerge :: PropertyMerge
  , elementMerge  :: ElementMerge
  }

data LabelMerge    = UnionLabels | IntersectLabels | ReplaceLabels
data PropertyMerge = ReplaceProperties | ShallowMerge | DeepMerge
data ElementMerge  = ReplaceElements | AppendElements | UnionElements

defaultMergeStrategy :: MergeStrategy
defaultMergeStrategy = MergeStrategy UnionLabels ShallowMerge UnionElements
```

### Error and Report Types

```haskell
data Conflict = Conflict
  { conflictId        :: Symbol
  , conflictExisting  :: Subject
  , conflictIncoming  :: Subject
  , conflictLocations :: [Path]
  }

type Path = [Int]  -- indices from root

data ReconcileError = ReconcileError
  { errorMessage   :: String
  , errorConflicts :: [Conflict]
  }

data ReconcileReport = ReconcileReport
  { reportDuplicatesFound    :: Int
  , reportReferencesResolved :: Int
  , reportMergesPerformed    :: Int
  , reportSubjectCounts      :: Map Symbol Int
  }
```

### Core Functions

```haskell
reconcile
  :: ReconciliationPolicy
  -> Pattern Subject
  -> Either ReconcileError (Pattern Subject)

reconcileWithReport
  :: ReconciliationPolicy
  -> Pattern Subject
  -> (Either ReconcileError (Pattern Subject), ReconcileReport)

needsReconciliation :: Pattern Subject -> Bool
findConflicts       :: Pattern Subject -> [Conflict]
```

### Reference Detection

References are detected structurally, not syntactically. A pattern is considered a
reference if it is atomic (no elements) and the same identity appears elsewhere with
more content (elements, more labels, more properties):

```haskell
isRefinementOf :: Subject -> Subject -> Bool
isRefinementOf full partial =
  identity full == identity partial
  && labels partial `Set.isSubsetOf` labels full
  && all (\(k,v) -> Map.lookup k (properties full) == Just v)
         (Map.toList (properties partial))

isReference :: Pattern Subject -> Map Symbol (Pattern Subject) -> Bool
isReference (Pattern subj []) known =
  case Map.lookup (identity subj) known of
    Just fuller -> not (null (elements fuller))
                   || isRefinementOf (value fuller) subj
    Nothing -> False
isReference _ _ = False
```

### Algorithm

```haskell
reconcile policy pat = do
  -- Phase 1: collect all subjects by identity
  let occurrences = collectByIdentity pat  -- Map Symbol [(Subject, Path)]

  -- Phase 2: reconcile each identity to a single canonical Subject
  canonical <- reconcileOccurrences policy occurrences  -- Map Symbol Subject

  -- Phase 3: rebuild pattern, replacing duplicates/references with canonical
  rebuildPattern canonical pat
```

`collectByIdentity` is a recursive traversal producing a map from identity to all
(subject, path) pairs. `reconcileOccurrences` applies the chosen policy. `rebuildPattern`
walks the pattern with a `Set` of already-emitted identities, emitting each identity
exactly once.

**Complexity**: O(n) time and O(k) space, where n is total pattern nodes and k is unique
identities.

### Usage Examples

```haskell
-- Normalize after parsing (duplicates expected)
case reconcile LastWriteWins parsed of
  Right normalized -> usePattern normalized
  Left err         -> handleError err

-- Validate coherence
case reconcile Strict parsed of
  Right _ -> putStrLn "Pattern is already coherent"
  Left err -> putStrLn $ "Conflicts: " ++ show (errorConflicts err)

-- Merge patterns from different sources
let combined = pattern rootSubject [patternA, patternB, patternC]
case reconcile (Merge defaultMergeStrategy) combined of
  Right unified -> savePattern unified
  Left err      -> handleConflicts err

-- Get diagnostic report
let (result, report) = reconcileWithReport (Merge defaultMergeStrategy) parsed
putStrLn $ "Resolved " ++ show (reportDuplicatesFound report) ++ " duplicates"
```

### Relationship to Existing Functions

| Existing | Purpose | Reconcile Relationship |
|---|---|---|
| `fmap` | Transform values | Reconcile transforms structure |
| `para` | Structure-aware fold | Reconcile uses similar traversal internally |
| `filterPatterns` | Find matching patterns | `findConflicts` is identity-focused |

Reconciliation provides what none of the existing functions do: **identity-aware
normalization**. It treats `Pattern Subject` as a collection of entities that should each
appear once, rather than as a pure tree structure.

### Edge Cases

- **Self-referential**: track seen identities during rebuild; emit each only once
- **Orphan references**: references to undefined identities pass through as-is
  (keep atomic pattern)
- **Circular references**: track visited during rebuild; break cycles
- **Empty pattern**: return unchanged

### Testing Requirements

1. Property tests:
   - `reconcile p` is idempotent: `reconcile (reconcile p) == reconcile p`
   - `reconcile` preserves all unique identities
   - `Strict` mode detects all and only actual conflicts
2. Unit tests: each policy, each merge strategy, reference resolution, edge cases
3. Equivalence tests: `reconcile . parse . serialize == reconcile` for gram round-trips

### Compatibility

No breaking changes. New module `Pattern.Reconcile`. Existing code continues to work
unchanged. Users opt-in by importing and calling `reconcile`.

## Open Questions

1. **Orphan references**: should orphan references (references to undefined identities)
   pass through silently or be configurable via an `OrphanPolicy`?

2. **Element order after reconciliation**: preserve order of first occurrence and dedupe
   later occurrences, or offer an ordering option?

3. **Reconcilable typeclass**: a `class HasIdentity v where identity :: v -> Symbol`
   would generalize reconciliation beyond `Subject`. Current proposal keeps it
   `Subject`-specific; the case for generalization is weak until a second value type
   with identity emerges.

4. **Streaming API**: an incremental form
   `reconcileIncremental :: Pattern Subject -> Pattern Subject -> Pattern Subject`
   would support append-only update patterns without full reprocessing. Deferred.

## Alternatives

**Eager reconciliation during parsing** — reconciling at parse time rather than as a
post-processing step. Rejected because it couples parsing to reconciliation policy,
making it harder to test them independently and preventing use of the same parser for
round-trip validation (where preserving duplicates may be intentional).

**Reserved-property approach** — encoding reconciliation metadata as reserved `Subject`
properties. Rejected because it conflates user-meaningful properties with library
bookkeeping and leaks into serialization.
