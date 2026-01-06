# Basic Operations: Accessing and Querying Patterns

## Introduction

Once you've created Patterns, you'll need to access their components and query their properties. This section covers:

- Accessing pattern components (`value` and `elements`)
- Query operations (`length`, `size`, `depth`)
- Working with nested structures

## Accessing Pattern Components

Patterns provide direct access to their `value` and `elements` fields.

### Accessing the Value

The `value` field contains the decoration about the pattern:

```haskell
import Pattern.Core (point, pattern, Pattern(..))

-- Atomic pattern
atomicPattern = point "test"
value atomicPattern  -- "test"

-- Pattern with elements
patternWithElements = pattern "pattern" [point "elem1"]
value patternWithElements  -- "pattern"
```

**Gram notation:**
```gram
["test"]
["pattern" | elem1]
```

### Accessing the Elements

The `elements` field contains the pattern itself (the sequence):

```haskell
-- Atomic pattern (empty sequence)
atomicPattern = point "atom"
elements atomicPattern  -- []

-- Pattern with elements
patternWithElements = pattern "pattern" [point "elem1", point "elem2"]
elements patternWithElements  -- [Pattern {value = "elem1", elements = []}, ...]
length (elements patternWithElements)  -- 2
```

**Gram notation:**
```gram
["atom"]
["pattern" | elem1, elem2]
```

### Accessing Individual Elements

You can access individual elements by position:

```haskell
pattern = pattern "sequence" [point "A", point "B", point "C"]

-- Access first element
head (elements pattern)  -- Pattern {value = "A", elements = []}

-- Access element by index
elements pattern !! 0  -- Pattern {value = "A", elements = []}
elements pattern !! 1  -- Pattern {value = "B", elements = []}
elements pattern !! 2  -- Pattern {value = "C", elements = []}

-- Iterate over elements
map value (elements pattern)  -- ["A", "B", "C"]
```

**Gram notation:**
```gram
["sequence" | A, B, C]
```

## Query Operations

The Pattern library provides several query functions to inspect pattern structure.

### Length

The `length` function returns the number of elements in a pattern:

```haskell
import Pattern.Core (length, point, pattern)

-- Atomic pattern (no elements)
atomic = point "atom"
length atomic  -- 0

-- Pattern with elements
pattern = pattern "sequence" [point "A", point "B", point "C"]
length pattern  -- 3
```

**Gram notation:**
```gram
["atom"]        -- length: 0
["sequence" | A, B, C]  -- length: 3
```

### Size

The `size` function returns the total number of pattern nodes (including nested patterns):

```haskell
import Pattern.Core (size, point, pattern)

-- Atomic pattern
atomic = point "atom"
size atomic  -- 1 (just the atomic pattern itself)

-- Pattern with elements
pattern = pattern "sequence" [point "A", point "B"]
size pattern  -- 3 (pattern + 2 elements)

-- Nested pattern
nested = pattern "outer" [pattern "inner" [point "A"]]
size nested  -- 3 (outer + inner + A)
```

**Gram notation:**
```gram
["atom"]                    -- size: 1
["sequence" | A, B]         -- size: 3
["outer" | ["inner" | A]]   -- size: 3
```

### Depth

The `depth` function returns the maximum nesting depth:

```haskell
import Pattern.Core (depth, point, pattern)

-- Atomic pattern
atomic = point "atom"
depth atomic  -- 0 (no nesting)

-- Pattern with elements (flat)
pattern = pattern "sequence" [point "A", point "B"]
depth pattern  -- 1 (one level of elements)

-- Nested pattern
nested = pattern "outer" [pattern "inner" [point "A"]]
depth nested  -- 2 (outer -> inner -> A)
```

**Gram notation:**
```gram
["atom"]                    -- depth: 0
["sequence" | A, B]         -- depth: 1
["outer" | ["inner" | A]]   -- depth: 2
```

## Working with Nested Structures

When working with nested patterns, you can traverse the structure:

```haskell
-- Create nested pattern
outer = pattern "outer" [pattern "middle" [point "inner"]]

-- Access outer value
value outer  -- "outer"

-- Access middle pattern
middle = head (elements outer)
value middle  -- "middle"

-- Access inner pattern
inner = head (elements middle)
value inner  -- "inner"

-- Query nested structure
length outer   -- 1 (one element at outer level)
size outer     -- 3 (outer + middle + inner)
depth outer    -- 2 (outer -> middle -> inner)
```

**Gram notation:**
```gram
["outer" | ["middle" | ["inner"]]]
```

## Practical Examples

### Checking Pattern Structure

```haskell
-- Check if pattern is atomic
isAtomic p = length p == 0

-- Check if pattern has elements
hasElements p = length p > 0

-- Get all values in a pattern
allValues p = value p : map value (elements p)
```

### Traversing Patterns

```haskell
-- Extract all values from a pattern and its elements
extractValues :: Pattern String -> [String]
extractValues p = value p : concatMap extractValues (elements p)

-- Example
pattern = pattern "root" [pattern "branch1" [point "leaf1"], point "branch2"]
extractValues pattern  -- ["root", "branch1", "leaf1", "branch2"]
```

**Gram notation:**
```gram
["root" | ["branch1" | leaf1], branch2]
```

## Summary

- **Access components**: Use `value` and `elements` to access pattern decoration and sequence
- **Query structure**: Use `length`, `size`, and `depth` to inspect pattern properties
- **Work with nesting**: Traverse nested structures by accessing elements recursively
- **Practical operations**: Combine accessors and queries to build useful operations

## Next Steps

Now that you can create and query Patterns, explore advanced operations through [Typeclass Instances](05-typeclass-instances.md) like Functor, Foldable, and Traversable.

