# Data Model: Comonad Instance for Pattern

**Feature**: 014-comonad-instance  
**Date**: 2025-01-28

## Overview

The Comonad instance for `Pattern` enables context-aware computations where functions have access to the full structural context (parent, siblings, depth, indices) around each value, not just the value itself. This extends beyond `Foldable` (which only provides values) to enable computations that consider structural context, depth, position, and relationships between pattern elements.

## Core Entity: Comonad Instance

### Definition

A **Comonad** is a typeclass that represents a structure with a focus point and context. For `Pattern`, the Comonad instance enables context-aware computations where functions receive the full pattern structure at each position, not just the value. This is the dual of Monad: while Monad builds up structure, Comonad breaks down structure to access context.

### Typeclass Instance

```haskell
instance Comonad Pattern where
  extract :: Pattern v -> v
  extract (Pattern v _) = v
  
  duplicate :: Pattern v -> Pattern (Pattern v)
  duplicate p@(Pattern _ els) = 
    Pattern p (map duplicate els)
  
  extend :: (Pattern v -> w) -> Pattern v -> Pattern w
  extend f = fmap f . duplicate
```

### Categorical Interpretation

From a category theory perspective, `Pattern` is a comonad that provides a way to perform context-aware computations. The Comonad instance provides:

1. **Context Access**: Functions have access to the full pattern structure at each position, not just the value
2. **Structure Preservation**: The pattern structure (element count, nesting depth, element order) is preserved during context-aware transformations
3. **Recursive Context**: Context is created recursively at all positions in the pattern structure
4. **Dual to Monad**: Comonad is the dual of Monad - while Monad builds structure, Comonad breaks it down to access context

### Comonad Laws

The Comonad instance must satisfy three mathematical laws:

#### Extract-Extend Law

```haskell
extract . extend f = f
```

**Meaning**: Extracting from an extended computation gives the original result.

**Example**:
```haskell
let f p = depth p
    p = patternWith "root" [pattern "a", pattern "b"]
in extract (extend f p) == f p  -- True (both equal depth of p)
```

#### Extend-Extract Law

```haskell
extend extract = id
```

**Meaning**: Extending with extract is identity.

**Example**:
```haskell
let p = patternWith "root" [pattern "a", pattern "b"]
in extend extract p == p  -- True
```

#### Extend Composition Law

```haskell
extend f . extend g = extend (f . extend g)
```

**Meaning**: Extend is associative.

**Example**:
```haskell
let f p = depth p
    g p = size p
    p = patternWith "root" [pattern "a", pattern "b"]
in (extend f . extend g) p == extend (f . extend g) p  -- True
```

## Context Model

### Context Definition

Context for Pattern means the full pattern structure focused at a particular position, including:

1. **Pattern Structure**: The full pattern structure (value and elements) at the focused position
2. **Depth**: Nesting level from root (0 at root, increases with nesting)
3. **Indices**: List of indices from root to current position (e.g., [0, 1] for second element of first element)
4. **Size**: Total number of nodes in subtree at current position
5. **Parent Context**: Context of parent position (if not at root)
6. **Sibling Context**: Contexts of sibling positions (elements at same level)

### Context-Aware Functions

A context-aware function is a function that takes a Pattern and returns a result based on the full structural context at that position, not just the value. Examples include:

- **Depth computation**: `\p -> depth p` - computes depth of pattern structure
- **Size computation**: `\p -> size p` - computes size of pattern structure
- **Indices computation**: `\p -> indicesFromRoot p` - computes indices from root to position
- **Custom computations**: Any function that uses pattern structure information

### Example: Context at Different Positions

```haskell
-- Pattern structure
p = patternWith "root" 
    [ patternWith "a" [pattern "x"]
    , pattern "b"
    ]

-- Context at root position: full pattern p
--   - Pattern structure: p itself
--   - Depth: 0
--   - Indices: []
--   - Size: 4 (root + "a" + "x" + "b")

-- Context at first element: patternWith "a" [pattern "x"]
--   - Pattern structure: patternWith "a" [pattern "x"]
--   - Depth: 1
--   - Indices: [0]
--   - Size: 2 ("a" + "x")

-- Context at nested element: pattern "x"
--   - Pattern structure: pattern "x"
--   - Depth: 2
--   - Indices: [0, 0]
--   - Size: 1 ("x")
```

## Relationship to Other Typeclasses

### Comonad vs Foldable

- **Foldable**: Provides values in sequence - `toList :: Pattern v -> [v]` extracts all values
- **Comonad**: Provides context-aware computation - `extend :: (Pattern v -> w) -> Pattern v -> Pattern w` applies functions with full context

**Use Cases**:
- **Foldable**: Sum values, concatenate strings, count elements
- **Comonad**: Compute depth at each position, compute indices from root, compute size of subtree

### Comonad vs Traversable

- **Traversable**: Provides effectful traversal - `traverse :: (a -> f b) -> Pattern a -> f (Pattern b)` applies effects to values
- **Comonad**: Provides context-aware computation - `extend :: (Pattern v -> w) -> Pattern v -> Pattern w` applies functions with full context

**Use Cases**:
- **Traversable**: Validate values, handle errors, perform IO operations
- **Comonad**: Compute structural metadata (depth, size, indices) at each position

### Comonad vs Functor/Applicative

- **Functor**: Transforms values while preserving structure - `fmap :: (a -> b) -> Pattern a -> Pattern b`
- **Applicative**: Applies functions to values at corresponding positions - `<*> :: Pattern (a -> b) -> Pattern a -> Pattern b`
- **Comonad**: Applies context-aware functions with full structural context - `extend :: (Pattern v -> w) -> Pattern v -> Pattern w`

**Relationship**: Comonad extends Functor/Applicative by providing context-aware computations, not just value transformations.

## Optional Helper Functions

### Depth at Each Position

```haskell
depthAt :: Pattern v -> Pattern Int
depthAt = extend (\p -> depth p)
```

**Purpose**: Compute depth (nesting level) at each position in the pattern structure.

**Example**:
```haskell
let p = patternWith "root" [pattern "a", pattern "b"]
in depthAt p == pattern 0 [pattern 1, pattern 1]  -- True
```

### Size at Each Position

```haskell
sizeAt :: Pattern v -> Pattern Int
sizeAt = extend (\p -> size p)
```

**Purpose**: Compute size (total nodes) of subtree at each position.

**Example**:
```haskell
let p = patternWith "root" [pattern "a", pattern "b"]
in sizeAt p == pattern 3 [pattern 1, pattern 1]  -- True (root=3, a=1, b=1)
```

### Indices at Each Position

```haskell
indicesAt :: Pattern v -> Pattern [Int]
indicesAt = extend (\p -> indicesFromRoot p)
```

**Purpose**: Compute indices from root to each position (list of indices).

**Example**:
```haskell
let p = patternWith "root" [pattern "a", pattern "b"]
in indicesAt p == pattern [] [[0], [1]]  -- True (root=[], a=[0], b=[1])
```

**Note**: Helper functions are optional convenience functions that demonstrate the power of the Comonad instance but are not required for core Comonad functionality.

## Relationship to Zippers

Comonads are closely related to zippers. A zipper is a data structure that represents a data structure with a focus point and its context. The Comonad instance for Pattern provides zipper-like capabilities:

- **Focus**: `extract` gets the value at the current focus (root)
- **Context**: `duplicate` creates contexts at every position (like having a zipper at each position)
- **Navigation**: `extend` lets you compute based on full context (like zipper navigation)

The Comonad instance enables zipper-like operations without requiring an explicit `Zipper` type, providing context-aware computations that need the full structural context at each position.

