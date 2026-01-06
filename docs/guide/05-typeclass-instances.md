# Typeclass Instances: Advanced Pattern Operations

## Introduction

Patterns support many typeclass instances that enable powerful operations. This section covers:

- **Functor**: Transform values while preserving structure
- **Foldable**: Aggregate values over pattern structures
- **Traversable**: Apply effects while preserving structure
- **Applicative**: Combine patterns with functions
- **Comonad**: Context-aware computations with full structural context
- **Semigroup, Monoid**: Combine patterns
- **Ord, Hashable**: Ordering and hashing

Each instance provides different capabilities. Understanding when to use each one helps you leverage the full power of Patterns.

## Functor: Transforming Values

The **Functor** instance enables transforming pattern values while preserving structure.

### Using `fmap`

```haskell
import Pattern.Core (point, pattern, Pattern(..))
import Data.Functor (fmap)

-- Create a pattern with integer values
pattern = pattern 10 [point 20, point 30]

-- Transform values: add 5 to each value
transformed = fmap (+5) pattern
-- Pattern 15 [Pattern 25 [], Pattern 35 []]
```

**Gram notation:**
```gram
[10 | 20, 30]        -- Original
[15 | 25, 35]        -- Transformed
```

### Structure Preservation

Functor preserves pattern structure (element count, nesting, order):

```haskell
-- Nested pattern
nested = pattern "outer" [pattern "inner" [point "leaf"]]

-- Transform values to uppercase
transformed = fmap (map toUpper) nested
-- Pattern "OUTER" [Pattern "INNER" [Pattern "LEAF" []]]

-- Structure preserved: same nesting, same element count
length nested == length transformed  -- True
depth nested == depth transformed    -- True
```

**Gram notation:**
```gram
["outer" | ["inner" | leaf]]        -- Original
["OUTER" | ["INNER" | LEAF]]        -- Transformed
```

### When to Use Functor

Use Functor when:
- ✅ You need to transform values while preserving structure
- ✅ You want to change value types (e.g., `Pattern Int -> Pattern String`)
- ✅ You're applying pure functions to pattern values

## Foldable: Aggregating Values

The **Foldable** instance enables aggregating values over pattern structures.

### Using `foldMap`

```haskell
import Pattern.Core (point, pattern)
import Data.Foldable (foldMap)
import Data.Monoid (Sum(..))

-- Pattern with integer values
pattern = pattern 10 [point 20, point 30]

-- Sum all values
total = getSum (foldMap Sum pattern)
-- 60 (10 + 20 + 30)
```

**Gram notation:**
```gram
[10 | 20, 30]  -- Sum: 60
```

### Using `toList`

Extract all values as a flat list:

```haskell
import Data.Foldable (toList)

pattern = pattern "root" [pattern "branch" [point "leaf"]]
values = toList pattern
-- ["root", "branch", "leaf"]
```

**Gram notation:**
```gram
["root" | ["branch" | leaf]]  -- Values: ["root", "branch", "leaf"]
```

### When to Use Foldable

Use Foldable when:
- ✅ You need to aggregate values (sum, product, concatenate)
- ✅ You want to extract all values as a list
- ✅ You're computing statistics over pattern values

## Traversable: Effectful Operations

The **Traversable** instance enables applying effects while preserving structure.

### Validation Example

```haskell
import Pattern.Core (point, pattern)
import Data.Traversable (traverse)
import Control.Applicative (Applicative)

-- Validate: values must be positive
validatePositive :: Int -> Maybe Int
validatePositive x = if x > 0 then Just x else Nothing

-- Pattern with values
pattern = pattern 10 [point (-5), point 20]

-- Validate all values
result = traverse validatePositive pattern
-- Nothing (because -5 fails validation)
```

**Gram notation:**
```gram
[10 | -5, 20]  -- Validation fails: Nothing
```

### Success Case

```haskell
pattern = pattern 10 [point 5, point 20]
result = traverse validatePositive pattern
-- Just (Pattern 10 [Pattern 5 [], Pattern 20 []])
```

**Gram notation:**
```gram
[10 | 5, 20]  -- Validation succeeds: Just [10 | 5, 20]
```

### When to Use Traversable

Use Traversable when:
- ✅ You need to apply effects (validation, error handling, IO)
- ✅ You want to preserve structure while handling effects
- ✅ You're working with `Maybe`, `Either`, `IO`, or other applicative effects

## Applicative: Combining Patterns

The **Applicative** instance enables applying functions stored in patterns to values stored in patterns.

### Basic Usage

```haskell
import Pattern.Core (point, pattern, Pattern(..))
import Control.Applicative ((<*>))

-- Function pattern
funcPattern = pure (+1)

-- Value pattern
valuePattern = pure 5

-- Apply function to value
result = funcPattern <*> valuePattern
-- Pattern 6 []
```

**Gram notation:**
```gram
[(+1)] <*> [5]  -- Result: [6]
```

### Pattern Combination

```haskell
-- Function pattern with elements
funcPattern = pattern (+1) [point (*2), point (+10)]

-- Value pattern
valuePattern = pattern 5 [point 3, point 7]

-- Apply functions to values
result = funcPattern <*> valuePattern
-- Pattern 6 [Pattern 6 [], Pattern 12 [], Pattern 15 [], Pattern 17 []]
```

**Gram notation:**
```gram
[(+1) | (*2), (+10)] <*> [5 | 3, 7]
-- Result: [6 | 6, 12, 15, 17]
```

### When to Use Applicative

Use Applicative when:
- ✅ You need to combine patterns with functions
- ✅ You want to apply multiple functions to multiple values
- ✅ You're working with pattern-based computations

## Comonad: Context-Aware Computations

The **Comonad** instance enables context-aware computations where functions have access to full structural context.

### Using `extract`

Extract the decoration value:

```haskell
import Pattern.Core (extract, point, pattern)

pattern = pattern "root" [point "elem1", point "elem2"]
value = extract pattern
-- "root"
```

**Gram notation:**
```gram
["root" | elem1, elem2]  -- extract: "root"
```

### Using `extend` for Context-Aware Operations

Compute depth at each position. The `extend depth` function computes the depth of the subtree rooted at each position:

```haskell
import Pattern.Core (extend, depth)

-- Pattern structure
pattern = pattern "root" [pattern "branch" [point "leaf"]]

-- Compute depth at each position
-- depth computes: atomic = 0, pattern with elements = 1 + max depth of elements
depthPattern = extend depth pattern
-- Pattern 2 [Pattern 1 [Pattern 0 []]]
```

**Gram notation:**
```gram
["root" | ["branch" | leaf]]        -- Original
[2 | [1 | [0]]]                    -- Depth at each position
```

- `leaf` (atomic) has depth 0
- `branch` has depth 1 (one level of nesting: branch → leaf)
- `root` has depth 2 (two levels of nesting: root → branch → leaf)

### Using `duplicate`

Create a pattern of contexts:

```haskell
import Pattern.Core (duplicate, point, pattern)

pattern = pattern "root" [point "elem"]
contexts = duplicate pattern
-- Pattern (Pattern "root" [...]) [Pattern (Pattern "elem" []) []]
```

**Gram notation:**
```gram
["root" | elem]  -- duplicate creates contexts at each position
```

### When to Use Comonad

Use Comonad when:
- ✅ You need context-aware computations (depth, size, indices)
- ✅ Functions need access to full structural context
- ✅ You're computing metadata about pattern structure

## Semigroup and Monoid: Combining Patterns

The **Semigroup** and **Monoid** instances enable combining patterns.

### Semigroup: Combining Two Patterns

```haskell
import Pattern.Core (point, pattern)
import Data.Semigroup ((<>))

p1 = pattern "a" [point "x"]
p2 = pattern "b" [point "y"]

-- Combine patterns
combined = p1 <> p2
-- Pattern "ab" [Pattern "x" [], Pattern "y" []]
```

**Gram notation:**
```gram
["a" | x] <> ["b" | y]  -- Result: ["ab" | x, y]
```

### Monoid: Combining Multiple Patterns

```haskell
import Data.Monoid (mconcat)

patterns = [pattern "a" [], pattern "b" [], pattern "c" []]
combined = mconcat patterns
-- Pattern "abc" []
```

**Gram notation:**
```gram
mconcat [["a"], ["b"], ["c"]]  -- Result: ["abc"]
```

### When to Use Semigroup/Monoid

Use Semigroup/Monoid when:
- ✅ You need to combine multiple patterns
- ✅ Values support Semigroup/Monoid operations
- ✅ You're aggregating patterns

## Ord: Ordering Patterns

The **Ord** instance enables ordering patterns for use in sets, maps, and sorting.

### Comparing Patterns

```haskell
import Pattern.Core (point, pattern)

p1 = point "a"
p2 = point "b"

compare p1 p2  -- LT
p1 < p2        -- True
```

### Using in Data Structures

```haskell
import Data.Set (Set, fromList)
import Pattern.Core (point, pattern)

-- Patterns can be used as set elements
patterns = fromList [point "a", point "b", point "c"]
```

### When to Use Ord

Use Ord when:
- ✅ You need to order patterns
- ✅ You're using patterns as keys in maps or elements in sets
- ✅ You're sorting patterns

## Hashable: Hashing Patterns

The **Hashable** instance enables hashing patterns for use in hash-based data structures.

### Hashing Patterns

```haskell
import Data.Hashable (hash)
import Pattern.Core (point, pattern)

pattern = pattern "root" [point "elem"]
hashValue = hash pattern
```

### When to Use Hashable

Use Hashable when:
- ✅ You're using patterns as keys in hash maps
- ✅ You need fast pattern lookup
- ✅ You're building hash-based data structures

## When to Use Each Instance

### Decision Guide

| Need | Instance | Example |
|------|----------|---------|
| Transform values | **Functor** | `fmap (+1) pattern` |
| Aggregate values | **Foldable** | `foldMap Sum pattern` |
| Apply effects | **Traversable** | `traverse validate pattern` |
| Combine patterns | **Applicative** | `funcPattern <*> valuePattern` |
| Context-aware ops | **Comonad** | `extend depth pattern` |
| Combine patterns | **Semigroup/Monoid** | `p1 <> p2` |
| Order patterns | **Ord** | `p1 < p2` |
| Hash patterns | **Hashable** | `hash pattern` |

## How Instances Compose

Typeclass instances compose naturally:

### Functor + Foldable

```haskell
-- Transform then aggregate
pattern = pattern 10 [point 20, point 30]
result = foldMap Sum (fmap (*2) pattern)
-- Sum 120 (all values doubled, then summed)
```

### Traversable + Functor

```haskell
-- Validate then transform
pattern = pattern 10 [point 5, point 20]
validated = traverse validatePositive pattern
transformed = fmap (*2) <$> validated
-- Just (Pattern 20 [Pattern 10 [], Pattern 40 []])
```

### Comonad + Functor

```haskell
-- Context-aware transformation
pattern = pattern "root" [pattern "branch" [point "leaf"]]
result = fmap (++ "!") <$> extend depth pattern
-- Pattern "0!" [Pattern "1!" [Pattern "2!" []]]
```

## Summary

- **Functor**: Transform values while preserving structure
- **Foldable**: Aggregate values over pattern structures
- **Traversable**: Apply effects while preserving structure
- **Applicative**: Combine patterns with functions
- **Comonad**: Context-aware computations with full structural context
- **Semigroup/Monoid**: Combine patterns
- **Ord/Hashable**: Ordering and hashing for data structures

Each instance provides different capabilities. Choose the right instance based on your needs.

## Next Steps

Explore the mathematical foundations behind these instances in [Advanced Morphisms](06-advanced-morphisms.md).

