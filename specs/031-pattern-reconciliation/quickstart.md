# Quickstart Guide: Pattern Subject Reconciliation

**Feature**: Pattern Subject Reconciliation (031)
**Audience**: Developers using the pattern library
**Purpose**: Get started quickly with reconciliation operations

## Overview

Pattern Subject Reconciliation normalizes patterns by resolving duplicate identities and completing references. After parsing gram notation or merging patterns from multiple sources, you may have the same identity appearing multiple times with different or evolving content. Reconciliation transforms this into a coherent pattern where each identity appears exactly once.

## Installation

Add the `pattern` library to your project dependencies (already includes reconciliation):

```yaml
# cabal
build-depends:
  pattern ^>=0.4

# stack
dependencies:
  - pattern >= 0.4.0
```

## Basic Import

```haskell
import Pattern.Core (Pattern(..), pattern, point)
import Pattern.Reconcile (reconcile, ReconciliationPolicy(..))
import Subject.Core (Subject(..), Symbol(..))
```

## Quick Start Examples

### Example 1: LastWriteWins Policy (Most Common)

Use this when you want the latest occurrence of each identity to win.

```haskell
import qualified Data.Set as Set
import qualified Data.Map as Map
import Subject.Value (VInteger(..))

-- Create subjects with same identity but different properties
let alice1 = Subject (Symbol "alice") (Set.singleton "Person")
                     (Map.singleton "age" (VInteger 30))
    alice2 = Subject (Symbol "alice") (Set.singleton "Person")
                     (Map.singleton "age" (VInteger 31))  -- updated age
    root = Subject (Symbol "root") Set.empty Map.empty

-- Pattern with duplicate identity
let pattern = pattern root [point alice1, point alice2]

-- Reconcile: keep the last occurrence
case reconcile LastWriteWins pattern of
  Right normalized ->
    -- Success! normalized has only alice2 (age 31)
    print normalized
  Left err ->
    -- Error (shouldn't happen with LastWriteWins)
    print err
```

**Result**: Pattern with single alice subject having age 31.

### Example 2: Merge Policy with Default Strategy

Use this when you want to combine information from all occurrences.

```haskell
import Pattern.Reconcile (ElementMergeStrategy(..), SubjectMergeStrategy(..), defaultSubjectMergeStrategy, Merge)

-- Create subjects with complementary information
let alice1 = Subject (Symbol "alice") (Set.singleton "Person")
                     (Map.singleton "name" (VString "Alice"))
    alice2 = Subject (Symbol "alice") (Set.singleton "Employee")
                     (Map.singleton "department" (VString "Engineering"))
    root = Subject (Symbol "root") Set.empty Map.empty

-- Pattern with duplicate identity having different info
let pattern = pattern root [point alice1, point alice2]

-- Reconcile: merge all information
case reconcile (Merge UnionElements defaultSubjectMergeStrategy) pattern of
  Right normalized -> do
    -- Success! normalized has alice with:
    --   labels: ["Person", "Employee"]
    --   properties: {name: "Alice", department: "Engineering"}
    print normalized
  Left err ->
    print err
```

**Result**: Pattern with single alice subject combining all labels and properties.

### Example 3: Strict Policy for Validation

Use this when you want to detect and report conflicts.

```haskell
import Pattern.Reconcile (Strict, findConflicts)

-- Create subjects with conflicting information
let alice1 = Subject (Symbol "alice") (Set.singleton "Person")
                     (Map.singleton "status" (VString "active"))
    alice2 = Subject (Symbol "alice") (Set.singleton "Person")
                     (Map.singleton "status" (VString "inactive"))  -- conflict!
    root = Subject (Symbol "root") Set.empty Map.empty

-- Pattern with conflicting duplicate
let pattern = pattern root [point alice1, point alice2]

-- Reconcile: fail on conflict
case reconcile Strict pattern of
  Right normalized ->
    -- Won't reach here - pattern has conflicts
    print "No conflicts"
  Left (ReconcileError msg conflicts) -> do
    -- Error with detailed conflict information
    putStrLn $ "Reconciliation failed: " ++ msg
    putStrLn $ "Found " ++ show (length conflicts) ++ " conflicts"
    mapM_ print conflicts
```

**Result**: Error with detailed information about the status conflict.

### Example 4: Custom Merge Strategy

Use this when you need fine-grained control over merging.

```haskell
import Pattern.Reconcile (SubjectMergeStrategy(..), ElementMergeStrategy(..), LabelMerge(..), PropertyMerge(..))

-- Create custom strategy
let customStrategy = SubjectMergeStrategy
      { labelMerge = IntersectLabels       -- Keep only common labels
      , propertyMerge = ReplaceProperties  -- Last properties win completely
      }

-- Use custom strategy
case reconcile (Merge AppendElements customStrategy) pattern of
  Right normalized -> print normalized
  Left err -> print err
```

### Example 5: Reconciliation with Reporting

Use this when you need statistics about what was reconciled.

```haskell
import Pattern.Reconcile (reconcileWithReport)

-- Reconcile and get report
let (result, report) = reconcileWithReport LastWriteWins pattern

case result of
  Right normalized -> do
    putStrLn "Reconciliation successful!"
    putStrLn $ "Duplicates found: " ++ show (reportDuplicatesFound report)
    putStrLn $ "References resolved: " ++ show (reportReferencesResolved report)
    putStrLn $ "Merges performed: " ++ show (reportMergesPerformed report)

    -- Show occurrence counts
    putStrLn "Identity occurrence counts:"
    mapM_ (\(id, count) -> putStrLn $ "  " ++ show id ++ ": " ++ show count)
          (Map.toList $ reportSubjectCounts report)
  Left err ->
    putStrLn $ "Reconciliation failed: " ++ show err
```

### Example 6: Check Before Reconciling

Use this to detect if reconciliation is needed.

```haskell
import Pattern.Reconcile (needsReconciliation, findConflicts)

-- Check if pattern needs reconciliation
if needsReconciliation pattern
  then do
    putStrLn "Pattern has duplicates, reconciling..."

    -- Optionally inspect conflicts first
    let conflicts = findConflicts pattern
    unless (null conflicts) $
      putStrLn $ "Found " ++ show (length conflicts) ++ " conflicts"

    -- Reconcile
    case reconcile LastWriteWins pattern of
      Right normalized -> usePattern normalized
      Left err -> handleError err
  else
    putStrLn "Pattern is already coherent"
```

## Common Patterns

### After Parsing Gram Notation

```haskell
import Gram.Parser (parseGram)

-- Parse gram text (may have duplicate IDs)
case parseGram gramText of
  Right parsed ->
    -- Reconcile duplicates from parsing
    case reconcile LastWriteWins parsed of
      Right coherent -> processPattern coherent
      Left err -> handleParseError err
  Left parseErr ->
    handleParseError parseErr
```

### Merging Patterns from Multiple Sources

```haskell
-- Load patterns from different sources
pattern1 <- loadFromFile "source1.gram"
pattern2 <- loadFromAPI "http://example.com/data"
pattern3 <- loadFromDatabase conn

-- Combine patterns (may have overlapping identities)
let root = Subject (Symbol "combined") Set.empty Map.empty
    combined = pattern root [pattern1, pattern2, pattern3]

-- Merge with union strategy (combine all information)
case reconcile (Merge UnionElements defaultSubjectMergeStrategy) combined of
  Right unified -> savePattern unified
  Left err -> handleMergeError err
```

### Streaming Pattern Updates

```haskell
-- Process stream of pattern updates
foldM reconcileUpdate initialPattern updates
  where
    reconcileUpdate base update =
      let combined = pattern (value base) (elements base ++ [update])
      in case reconcile LastWriteWins combined of
           Right result -> return result
           Left err -> throwError err
```

## Policy Selection Guide

Choose the right reconciliation policy for your use case:

| Policy | When to Use | Example Use Case |
|--------|-------------|------------------|
| **LastWriteWins** | Updates/streaming data where latest is most accurate | Real-time data ingestion, live updates |
| **FirstWriteWins** | Initial definitions are authoritative | Schema definitions, template patterns |
| **Merge** | Combining complementary information from sources | Data integration, ETL pipelines |
| **Strict** | Validation and conflict detection | Quality checks, pre-commit validation |

## Merge Strategy Guide

When using `Merge` policy, you need to specify two strategies:
1. `ElementMergeStrategy` - How to merge the element lists
2. `SubjectMergeStrategy` - How to merge Subject content (labels and properties)

Example: `Merge UnionElements defaultSubjectMergeStrategy`

### Element Merge Strategies

- **UnionElements**: Deduplicate elements by identity
  - Use when: Elements represent related entities
  - Example: Merging relationship patterns

- **AppendElements**: Concatenate all element lists
  - Use when: Order matters and duplicates are meaningful
  - Example: Event sequences, history logs

- **ReplaceElements**: Later elements completely replace earlier
  - Use when: Element list is complete replacement
  - Example: Overriding relationship structure

### Subject Merge Strategies

The `SubjectMergeStrategy` type has two fields: `labelMerge` and `propertyMerge`.

Default: `defaultSubjectMergeStrategy = SubjectMergeStrategy UnionLabels ShallowMerge`

#### Label Merge Strategies

- **UnionLabels** (default): Combine all labels from all occurrences
  - Use when: Labels are additive classifications
  - Example: Subject labeled both "Person" and "Employee"

- **IntersectLabels**: Keep only labels present in all occurrences
  - Use when: Looking for common classifications
  - Example: Finding shared categories across sources

- **ReplaceLabels**: Later labels completely replace earlier ones
  - Use when: Labels represent single classification that can change
  - Example: Status labels that transition

#### Property Merge Strategies

- **ShallowMerge** (default): Merge top-level keys, later wins on conflict
  - Use when: Properties are mostly independent
  - Example: Combining user profile fields from different sources

- **DeepMerge**: Recursively merge nested maps
  - Use when: Properties have nested structure
  - Example: Configuration objects, nested preferences

- **ReplaceProperties**: Later properties completely replace earlier
  - Use when: Property sets are complete replacements
  - Example: Full document updates

## Error Handling

All reconciliation operations return `Either ReconcileError result`:

```haskell
case reconcile policy pattern of
  Right result ->
    -- Success path
    usePattern result

  Left (ReconcileError message conflicts) -> do
    -- Error path - handle conflicts
    logError message
    forM_ conflicts $ \conflict -> do
      putStrLn $ "Conflict for identity: " ++ show (conflictId conflict)
      putStrLn $ "  Existing: " ++ show (conflictExisting conflict)
      putStrLn $ "  Incoming: " ++ show (conflictIncoming conflict)
      putStrLn $ "  Locations: " ++ show (conflictLocations conflict)
```

## Performance Considerations

- **Time Complexity**: O(n) where n = total number of subjects in pattern
- **Space Complexity**: O(k) where k = number of unique identities
- **Recommended Limits**: Tested with patterns containing 10,000+ subjects

For very large patterns (>100,000 subjects), consider:
1. Reconciling incrementally in chunks
2. Using LastWriteWins/FirstWriteWins (faster than Merge)
3. Pre-filtering patterns to reduce size before reconciliation

## Next Steps

- Read [data-model.md](./data-model.md) for detailed type documentation
- See [contracts/Pattern-Reconcile.hs](./contracts/Pattern-Reconcile.hs) for full API reference
- Check [research.md](./research.md) for implementation patterns and best practices
- Run property-based tests to verify invariants hold for your use cases

## FAQ

**Q: What happens to orphan references (atomic patterns with no full definition)?**
A: They are preserved as-is. Reconciliation only completes references when a fuller definition exists.

**Q: How are circular references handled?**
A: The rebuild phase tracks visited identities and emits each identity exactly once, breaking cycles.

**Q: Is reconciliation idempotent?**
A: Yes. Reconciling an already-reconciled pattern produces the same result.

**Q: What if my pattern has no duplicates?**
A: Use `needsReconciliation` to check first. If false, reconciliation returns the pattern unchanged (very fast).

**Q: Can I reconcile patterns with non-Subject values?**
A: No. Reconciliation is specific to `Pattern Subject` because it relies on identity from Subject.

**Q: How do I choose between reconcile and reconcileWithReport?**
A: Use `reconcile` for normal operation. Use `reconcileWithReport` when you need statistics for logging, debugging, or monitoring.
