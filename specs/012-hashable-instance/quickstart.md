# Quickstart: Hashable Instance for Pattern

**Feature**: 012-hashable-instance  
**Date**: 2025-01-27  
**Status**: Design Complete

## Overview

The `Hashable` instance for `Pattern` enables using patterns as keys in `HashMap` and elements in `HashSet` for efficient hash-based lookups and deduplication. The instance provides O(1) average-case performance compared to O(log n) for ordered containers.

## Basic Usage

### Hashing Patterns

```haskell
import Pattern.Core
import Data.Hashable (hash)

-- Hash atomic patterns
hash (pattern "a")  -- Int
hash (pattern "b")  -- Int (different from "a")

-- Hash patterns with elements
hash (patternWith "root" [pattern "a", pattern "b"])  -- Int

-- Hash nested patterns
hash (patternWith "outer" [patternWith "inner" [pattern "value"]])  -- Int
```

### Hash Consistency with Eq

Equal patterns produce the same hash:

```haskell
let p1 = pattern "a"
let p2 = pattern "a"

p1 == p2  -- True
hash p1 == hash p2  -- True (hash consistency)
```

## Hash-Based Containers

### HashMap

Use patterns as keys in `HashMap` for O(1) average-case lookups:

```haskell
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

-- Create HashMap with patterns as keys
let m = HashMap.fromList 
  [ (pattern "a", 1)
  , (pattern "b", 2)
  , (patternWith "root" [pattern "c"], 3)
  ]

-- Lookup patterns
HashMap.lookup (pattern "a") m  -- Just 1
HashMap.lookup (pattern "b") m  -- Just 2
HashMap.lookup (pattern "x") m  -- Nothing

-- Insert patterns
let m2 = HashMap.insert (pattern "d") 4 m
HashMap.lookup (pattern "d") m2  -- Just 4
```

### HashSet

Use patterns as elements in `HashSet` for O(1) average-case membership testing:

```haskell
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

-- Create HashSet with patterns as elements
let s = HashSet.fromList 
  [ pattern "a"
  , pattern "b"
  , patternWith "root" [pattern "c"]
  ]

-- Test membership
HashSet.member (pattern "a") s  -- True
HashSet.member (pattern "b") s  -- True
HashSet.member (pattern "x") s  -- False

-- Insert patterns
let s2 = HashSet.insert (pattern "d") s
HashSet.member (pattern "d") s2  -- True

-- Deduplication
let s3 = HashSet.fromList [pattern "a", pattern "a", pattern "b"]
HashSet.size s3  -- 2 (duplicates removed)
```

## Use Cases

### Use Case 1: High-Performance Pattern Lookups

Fast pattern lookups in large collections:

```haskell
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

-- Build pattern index
let patternIndex = HashMap.fromList
  [ (pattern "pattern1", "result1")
  , (pattern "pattern2", "result2")
  , (patternWith "root" [pattern "a", pattern "b"], "result3")
  ]

-- Fast lookup (O(1) average-case)
HashMap.lookup (pattern "pattern1") patternIndex  -- Just "result1"
```

### Use Case 2: Pattern Deduplication

Efficient pattern deduplication in large collections:

```haskell
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

-- Collect unique patterns from stream
let patterns = [pattern "a", pattern "b", pattern "a", pattern "c", pattern "b"]
let uniquePatterns = HashSet.fromList patterns
HashSet.size uniquePatterns  -- 3 (duplicates removed)

-- Fast membership testing (O(1) average-case)
HashSet.member (pattern "a") uniquePatterns  -- True
HashSet.member (pattern "x") uniquePatterns  -- False
```

### Use Case 3: Pattern Caching

Use patterns as keys in caches:

```haskell
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

-- Pattern-based cache
type PatternCache a = HashMap (Pattern String) a

let cache = HashMap.empty :: PatternCache Int

-- Cache pattern results
let cache1 = HashMap.insert (pattern "expensive-computation") 42 cache

-- Fast cache lookup
HashMap.lookup (pattern "expensive-computation") cache1  -- Just 42
```

## Hash Distribution

### Different Structures Produce Different Hashes

Patterns with different structures produce different hashes:

```haskell
-- Pattern 1: flat structure
let p1 = patternWith "a" [pattern "b", pattern "c"]

-- Pattern 2: nested structure with same flattened values
let p2 = patternWith "a" [patternWith "b" [pattern "c"]]

hash p1 /= hash p2  -- True (different structures, different hashes)
```

### Hash Collisions

Hash collisions are possible but rare:

```haskell
-- Different patterns may have same hash (collision)
let p1 = pattern "a"
let p2 = pattern "b"

hash p1 == hash p2  -- May be True (collision) or False (no collision)
```

Hash-based containers handle collisions correctly through equality comparison:

```haskell
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

-- Even if two patterns have the same hash, they're handled correctly
let s = HashSet.fromList [p1, p2]
HashSet.size s  -- 2 (both patterns retained, collision handled through Eq)
```

## Edge Cases

### Atomic Patterns

```haskell
hash (pattern "a")  -- Valid hash value
hash (pattern "")  -- Valid hash value (empty string)
```

### Patterns with Many Elements

```haskell
let largePattern = patternWith "root" [pattern "a", pattern "b", ..., pattern "z"]
hash largePattern  -- Valid hash value (all elements contribute)
```

### Deeply Nested Patterns

```haskell
let deepPattern = patternWith "level1" 
  [patternWith "level2" 
    [patternWith "level3" 
      [pattern "value"]]]
hash deepPattern  -- Valid hash value (nested structures contribute)
```

### Patterns with Same Flattened Values but Different Structures

```haskell
-- Pattern 1: flat
let p1 = patternWith "a" [pattern "b", pattern "c"]

-- Pattern 2: nested (same flattened values)
let p2 = patternWith "a" [patternWith "b" [pattern "c"]]

hash p1 /= hash p2  -- True (different structures, different hashes)
```

## Type Constraints

The `Hashable` instance requires the value type to have a `Hashable` instance:

```haskell
-- This works (String has Hashable instance)
hash (pattern "a") :: Int

-- This works (Int has Hashable instance)
hash (pattern (42 :: Int)) :: Int

-- This doesn't work if CustomType doesn't have Hashable instance
hash (pattern customValue) :: Int
-- Error: No instance for (Hashable CustomType)
```

## Comparison with Ordered Containers

### When to Use HashMap/HashSet

- **Performance-critical lookups**: O(1) average-case vs O(log n) for `Data.Map`/`Data.Set`
- **No ordering needed**: When patterns don't need to be sorted
- **Large collections**: When collection size is large (1000+ patterns), performance difference becomes significant
- **Library integration**: When libraries require `Hashable` instances

### When to Use Data.Map/Data.Set

- **Ordering needed**: When patterns need to be sorted or accessed in order
- **Range queries**: When range-based lookups are required
- **Small collections**: When collection size is small (<100 patterns), overhead difference is negligible
- **Deterministic iteration**: When iteration order must be deterministic and sorted

## Performance

### Hash Computation

Hash computation is O(n) where n is the number of nodes in the pattern structure:

```haskell
-- Atomic pattern: O(1)
hash (pattern "a")

-- Pattern with n elements: O(n)
hash (patternWith "root" [pattern "a", ..., pattern "n"])

-- Nested pattern: O(n) where n is total nodes
hash (patternWith "outer" [patternWith "inner" [pattern "value"]])
```

### HashMap Lookups

HashMap lookups are O(1) average-case:

```haskell
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

let m = HashMap.fromList [(pattern "a", 1), ..., (pattern "z", 26)]
HashMap.lookup (pattern "m") m  -- O(1) average-case
```

### HashSet Membership

HashSet membership testing is O(1) average-case:

```haskell
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

let s = HashSet.fromList [pattern "a", ..., pattern "z"]
HashSet.member (pattern "m") s  -- O(1) average-case
```

## Notes

- Hash consistency with `Eq` is guaranteed: equal patterns have the same hash
- Hash collisions are possible but rare, and are handled correctly through equality comparison
- Structure-preserving hashing distinguishes patterns with different structures
- Performance is O(n) for hash computation, O(1) average-case for lookups
- The instance enables efficient hash-based lookups and deduplication

