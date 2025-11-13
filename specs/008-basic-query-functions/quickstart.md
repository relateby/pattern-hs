# Quickstart: Basic Query Functions

**Feature**: 008-basic-query-functions  
**Date**: 2025-01-27

## Overview

Basic query functions enable pattern introspection by providing structural information and value extraction. These functions are pure (no side effects) and work with patterns of any value type.

## Import

```haskell
import Pattern.Core (Pattern(..), pattern, patternWith, length, size, depth, values, value)
-- Or import from main module:
import Pattern (length, size, depth, values, value)
```

## Basic Usage

### Query Pattern Length

Get the number of direct elements in a pattern's sequence:

```haskell
-- Atomic pattern (no elements)
>>> let p1 = pattern "root"
>>> length p1
0

-- Pattern with elements
>>> let p2 = patternWith "root" [pattern "a", pattern "b", pattern "c"]
>>> length p2
3

-- Nested pattern (only counts direct children)
>>> let p3 = patternWith "root" [patternWith "inner" [pattern "a"]]
>>> length p3
1  -- Only the "inner" element, not nested "a"
```

### Query Pattern Size

Get the total number of nodes in a pattern structure:

```haskell
-- Atomic pattern
>>> let p1 = pattern "root"
>>> size p1
1

-- Pattern with elements
>>> let p2 = patternWith "root" [pattern "a", pattern "b"]
>>> size p2
3  -- root + a + b

-- Nested pattern
>>> let p3 = patternWith "root" [patternWith "inner" [pattern "a"]]
>>> size p3
3  -- root + inner + a
```

### Query Pattern Depth

Get the maximum nesting depth of a pattern structure:

```haskell
-- Atomic pattern (depth 0)
>>> let p1 = pattern "root"
>>> depth p1
0

-- One level of nesting
>>> let p2 = patternWith "root" [pattern "a"]
>>> depth p2
1

-- Multiple levels of nesting
>>> let p3 = patternWith "root" [patternWith "inner" [patternWith "deep" [pattern "a"]]]
>>> depth p3
3

-- Multiple branches with different depths (returns maximum)
>>> let p4 = patternWith "root" 
>>>           [pattern "a", 
>>>            patternWith "b" [patternWith "c" [pattern "d"]]]
>>> depth p4
3  -- Maximum depth across branches
```

### Extract All Values

Get all values from a pattern as a flat list:

```haskell
-- Atomic pattern
>>> let p1 = pattern "root"
>>> values p1
["root"]

-- Pattern with elements
>>> let p2 = patternWith "root" [pattern "a", pattern "b"]
>>> values p2
["root", "a", "b"]

-- Nested pattern
>>> let p3 = patternWith "root" [patternWith "inner" [pattern "a"]]
>>> values p3
["root", "inner", "a"]
```

### Access Pattern Value

Get the decoration value of a pattern:

```haskell
-- String value
>>> let p1 = pattern "test"
>>> value p1
"test"

-- Integer value
>>> let p2 = pattern 42
>>> value p2
42

-- Custom type value
>>> data Person = Person String Int deriving (Show)
>>> let p3 = pattern (Person "Alice" 30)
>>> value p3
Person "Alice" 30
```

## Common Patterns

### Validate Pattern Structure

```haskell
-- Check if pattern is atomic
isAtomic :: Pattern v -> Bool
isAtomic p = length p == 0

-- Check if pattern has exactly one element
isSingular :: Pattern v -> Bool
isSingular p = length p == 1

-- Check if pattern is a pair (two elements)
isPair :: Pattern v -> Bool
isPair p = length p == 2

-- Check if pattern is deeply nested
isDeeplyNested :: Pattern v -> Bool
isDeeplyNested p = depth p > 5
```

### Analyze Pattern Complexity

```haskell
-- Get pattern complexity metrics
patternMetrics :: Pattern v -> (Int, Int, Int)
patternMetrics p = (length p, size p, depth p)

-- Example usage
>>> let p = patternWith "root" [pattern "a", patternWith "b" [pattern "c"]]
>>> patternMetrics p
(2, 4, 2)  -- (length, size, depth)
```

### Aggregate Pattern Values

```haskell
-- Sum integer values in a pattern
sumValues :: Pattern Int -> Int
sumValues p = sum (values p)

-- Example
>>> let p = patternWith 10 [pattern 5, pattern 3]
>>> sumValues p
18

-- Concatenate string values
concatValues :: Pattern String -> String
concatValues p = concat (values p)

-- Example
>>> let p = patternWith "hello" [pattern " ", pattern "world"]
>>> concatValues p
"hello world"
```

### Filter Patterns by Structure

```haskell
-- Find all atomic patterns in a list
atomicPatterns :: [Pattern v] -> [Pattern v]
atomicPatterns = filter (\p -> length p == 0)

-- Find patterns with specific depth
patternsAtDepth :: Int -> [Pattern v] -> [Pattern v]
patternsAtDepth d = filter (\p -> depth p == d)

-- Find large patterns (many nodes)
largePatterns :: Int -> [Pattern v] -> [Pattern v]
largePatterns threshold = filter (\p -> size p > threshold)
```

## Integration with Existing Functions

### With Construction Functions

```haskell
-- Query patterns created with pattern
>>> let p1 = pattern "test"
>>> length p1
0
>>> size p1
1

-- Query patterns created with patternWith
>>> let p2 = patternWith "root" [pattern "a", pattern "b"]
>>> length p2
2
>>> size p2
3

-- Query patterns created with fromList
>>> let p3 = fromList "graph" ["node1", "node2", "node3"]
>>> length p3
3
>>> size p3
4
```

### With Functor Operations

```haskell
-- Query functions work with transformed patterns
>>> let p1 = patternWith "hello" [pattern "world"]
>>> let p2 = fmap (map toUpper) p1
>>> length p2
1  -- Structure preserved
>>> size p2
2  -- Structure preserved
>>> values p2
["HELLO", "WORLD"]
```

### With Foldable Operations

```haskell
-- values is equivalent to toList
>>> let p = patternWith "root" [pattern "a", pattern "b"]
>>> values p
["root", "a", "b"]
>>> toList p
["root", "a", "b"]
>>> values p == toList p
True

-- Use with fold operations
>>> let p = patternWith 10 [pattern 5, pattern 3]
>>> foldr (+) 0 p
18
>>> sum (values p)
18
```

## Edge Cases

### Atomic Patterns

```haskell
>>> let p = pattern "atom"
>>> length p
0
>>> size p
1
>>> depth p
0
>>> values p
["atom"]
>>> value p
"atom"
```

### Single Element Patterns

```haskell
>>> let p = patternWith "root" [pattern "elem"]
>>> length p
1
>>> size p
2
>>> depth p
1
>>> values p
["root", "elem"]
```

### Empty Elements List

```haskell
>>> let p = Pattern { value = "test", elements = [] }
>>> length p
0
>>> size p
1
>>> depth p
0
```

### Deep Nesting

```haskell
-- Create deeply nested pattern
>>> let deep = foldl (\acc _ -> patternWith "level" [acc]) (pattern "base") [1..10]
>>> depth deep
10
>>> size deep
11
```

## Performance Tips

1. **Use `length` for direct element count**: O(1) operation, very fast
2. **Use `size` when you need total node count**: O(n) but necessary for accurate counts
3. **Use `depth` sparingly**: O(n) operation, only when depth information is needed
4. **Use `values` for value extraction**: O(n) but provides all values in order
5. **Use `value` for single value access**: O(1) direct field access

## Next Steps

- See [data-model.md](data-model.md) for detailed data model information
- See [contracts/type-signatures.md](contracts/type-signatures.md) for complete API documentation
- See [spec.md](spec.md) for feature requirements and acceptance criteria

