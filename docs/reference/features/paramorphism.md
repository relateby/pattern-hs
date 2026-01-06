# Paramorphism Reference

**Audience**: Library users and implementers  
**Purpose**: Complete reference documentation for paramorphism operations

## Overview

Paramorphism enables structure-aware folding over patterns. Unlike `Foldable` (which only provides values), paramorphism gives your folding function access to the full pattern structure at each position, enabling sophisticated aggregations that consider depth, element count, and nesting level.

## Function Reference

### `para`

```haskell
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
```

**Description**: Paramorphism function that enables structure-aware folding over patterns. The folding function receives both the current pattern subtree and recursively computed results from children.

**Parameters**:
- `f :: Pattern v -> [r] -> r`: Folding function that receives:
  - `Pattern v`: Current pattern subtree (full pattern structure)
  - `[r]`: List of recursively computed results from children
  - Returns: Aggregated result of type `r`
- `pattern :: Pattern v`: Pattern to fold over

**Returns**: Aggregated result of type `r`

**Example**:
```haskell
import Pattern.Core (pattern, point, para)

-- Depth-weighted sum
let p = pattern 10 [point 5, point 3]
depthWeightedSum = para (\pat rs -> value pat * depth pat + sum rs) p
-- Result: 10 (10*1 + 5*0 + 3*0)
```

**Performance**: O(n) where n is the total number of nodes in the pattern structure.

## Type Signatures

### Core Function

```haskell
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
```

### Common Patterns

```haskell
-- Depth-weighted sum
depthWeightedSum :: Pattern Int -> Int
depthWeightedSum = para (\p rs -> value p * depth p + sum rs)

-- Element-count-aware aggregation
elementCountSum :: Pattern Int -> Int
elementCountSum = para (\p rs -> value p * length (elements p) + sum rs)

-- Nesting-level statistics
nestingLevelStats :: Pattern Int -> (Int, Int, Int)  -- (sum, count, maxDepth)
nestingLevelStats = para (\p rs -> 
  let (s, c, d) = foldr (\(s', c', d') (s'', c'', d'') -> 
    (s' + s'', c' + c'', max d' d'')) (0, 0, 0) rs
  in (value p + s, 1 + c, max (depth p) d))
```

## Laws and Properties

### Structure Access Property

```haskell
para (\p _ -> p) pattern == pattern
```

**Description**: Paramorphism can extract the full pattern structure.

### Value Access Property

```haskell
para (\p rs -> value p : concat rs) pattern == toList pattern
```

**Description**: Paramorphism can simulate `Foldable` when ignoring structure (but includes pattern's own value).

### Recursive Structure Property

```haskell
para f (Pattern v els) == f (Pattern v els) (map (para f) els)
```

**Description**: Standard paramorphism pattern: process children first, then combine with current pattern.

### Order Preservation Property

```haskell
para (\p rs -> value p : concat rs) pattern == toList pattern
```

**Description**: Paramorphism preserves element order when aggregating results.

### Relationship to Foldable

```haskell
para (\p rs -> value p + sum rs) pattern == foldr (+) 0 pattern
```

**Description**: Paramorphism can simulate `Foldable` by including pattern's value and summing child results.

## Relationship to Other Operations

### Comparison with Foldable

**Foldable** (value-only folding):
- Operations: `foldr`, `foldl`, `foldMap`, `toList`
- Access: Only values, no structure
- Use when: You only need values, not structural information

**Example**:
```haskell
foldr (+) 0 pattern  -- Sums all values, ignores structure
```

### Comparison with Comonad

**Comonad** (structure-aware transformation):
- Operations: `extend`, `duplicate`, `extract`
- Access: Structure for transformation
- Use when: You need structure-aware transformation (not aggregation)

**Example**:
```haskell
extend (\p -> value p * depth p) pattern  -- Transforms, returns Pattern
```

**Paramorphism** (structure-aware folding):
- Operations: `para`
- Access: Structure for folding/aggregation
- Use when: You need structure-aware aggregations

**Example**:
```haskell
para (\p rs -> value p * depth p + sum rs) pattern  -- Aggregates, returns result
```

## Implementation Details

### Standard Pattern

```haskell
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
para f (Pattern v els) = 
  f (Pattern v els) (map (para f) els)
```

**Breakdown**:
- `para f (Pattern v els)`: Applies paramorphism function `f` to pattern
- `f (Pattern v els) (map (para f) els)`: Provides folding function with:
  - Current pattern subtree: `Pattern v els` (full pattern structure)
  - Recursively computed results: `map (para f) els` (results from all children)
- Returns aggregated result of type `r`

### Value Processing Order

For a `Pattern v`:
1. Recursively compute results for all children using `para f`
2. Apply folding function `f` to: (1) current pattern subtree, (2) list of child results
3. Return aggregated result

This follows standard paramorphism pattern: process children first, then combine with current pattern.

## Examples

### Depth-Weighted Sum

```haskell
import Pattern.Core (pattern, point, para)

let p = pattern 10 [point 5, point 3]
depthWeightedSum = para (\pat rs -> value pat * depth pat + sum rs) p
-- Calculation:
-- para f (point 5) = 5 * 0 + 0 = 0
-- para f (point 3) = 3 * 0 + 0 = 0
-- para f p = 10 * 1 + (0 + 0) = 10
```

### Element-Count-Aware Aggregation

```haskell
let p = pattern 10 [point 5, point 3]
elementCountSum = para (\pat rs -> value pat * length (elements pat) + sum rs) p
-- Calculation:
-- para f (point 5) = 5 * 0 + 0 = 0
-- para f (point 3) = 3 * 0 + 0 = 0
-- para f p = 10 * 2 + (0 + 0) = 20
-- But wait, we need to include child values: 10 * 2 + (5 + 3) = 28
```

### Nesting-Level Statistics

```haskell
nestingLevelStats :: Pattern Int -> (Int, Int, Int)  -- (sum, count, maxDepth)
nestingLevelStats = para (\p rs -> 
  let (s, c, d) = foldr (\(s', c', d') (s'', c'', d'') -> 
    (s' + s'', c' + c'', max d' d'')) (0, 0, 0) rs
  in (value p + s, 1 + c, max (depth p) d))

let p = pattern 10 [point 5, point 3]
nestingLevelStats p  -- (18, 3, 1)  -- (sum, count, maxDepth)
```

## Performance Characteristics

- **Time Complexity**: O(n) where n is the total number of nodes in the pattern structure
- **Space Complexity**: O(d) where d is the maximum nesting depth (for recursion stack)
- **Order Preservation**: Element order is preserved when aggregating results

## Edge Cases

### Atomic Pattern (No Elements)

```haskell
let atom = point 5
para (\p rs -> value p + sum rs) atom
-- = 5 + 0 = 5
```

### Pattern with Single Element

```haskell
let p = pattern 10 [point 5]
para (\p rs -> value p + sum rs) p
-- = 10 + (5 + 0) = 15
```

### Nested Pattern

```haskell
let nested = pattern 1 [pattern 2 [point 3]]
para (\p rs -> value p + sum rs) nested
-- = 1 + (2 + (3 + 0)) = 6
```

## See Also

- [User Guide: Advanced Morphisms](../../guide/06-advanced-morphisms.md) - Intuitive explanation and examples
- [Porting Guide: Paramorphism Implementation](../../PORTING-GUIDE.md#paramorphism-implementation) - How to implement in other languages
- [Typeclass Instances](typeclass-instances.md) - Relationship to `Foldable` and `Comonad`

