# Type Signatures: Comonad Instance for Pattern

**Feature**: 014-comonad-instance  
**Date**: 2025-01-28

## Comonad Instance

### Instance Declaration

```haskell
instance Comonad Pattern where
  extract :: Pattern v -> v
  duplicate :: Pattern v -> Pattern (Pattern v)
  extend :: (Pattern v -> w) -> Pattern v -> Pattern w
```

**Type Constraint**: None (Comonad is a typeclass, not a constraint)

**Module**: `Pattern.Core`

**Dependencies**: Requires `comonad` package for Comonad typeclass

---

## Extract Function

### Type Signature

```haskell
extract :: Pattern v -> v
```

**Purpose**: Extract the decoration value from a pattern, providing the value at the focus point (root).

**Behavior**:
- Takes a pattern of any type `Pattern v`
- Returns the decoration value of type `v`
- Extracts the root value (pattern's decoration value), not element values

**Examples**:

```haskell
-- Atomic pattern
extract (pattern 5) :: Int
-- Result: 5

-- Pattern with elements
extract (patternWith "test" [pattern "a", pattern "b"]) :: String
-- Result: "test" (not "a" or "b")

-- Nested pattern
extract (patternWith "root" [patternWith "a" [pattern "x"]]) :: String
-- Result: "root" (root decoration value)
```

**Haddock Documentation**:

```haskell
-- | Extract the decoration value from a pattern.
--
-- Returns the pattern's decoration value (root value), which is the value
-- at the focus point in the comonadic context. This is the fundamental
-- operation of the Comonad instance, providing access to the value at
-- the current focus.
--
-- === Examples
--
-- >>> extract (pattern 5)
-- 5
--
-- >>> extract (patternWith "test" [pattern "a", pattern "b"])
-- "test"
--
-- >>> extract (patternWith "root" [patternWith "a" [pattern "x"]])
-- "root"
extract :: Pattern v -> v
```

---

## Duplicate Function

### Type Signature

```haskell
duplicate :: Pattern v -> Pattern (Pattern v)
```

**Purpose**: Create a pattern where each position contains the full pattern structure focused at that position, enabling context-aware computations.

**Behavior**:
- Takes a pattern of type `Pattern v`
- Returns a pattern of patterns `Pattern (Pattern v)`
- Root contains the full original pattern
- Each element contains the pattern structure focused at that element's position (recursively)

**Examples**:

```haskell
-- Atomic pattern
duplicate (pattern 5) :: Pattern (Pattern Int)
-- Result: Pattern {value = Pattern {value = 5, elements = []}, elements = []}

-- Pattern with elements
let p = patternWith "root" [pattern "a", pattern "b"]
in duplicate p
-- Result: Pattern {
--   value = Pattern {value = "root", elements = [pattern "a", pattern "b"]},
--   elements = [
--     Pattern {value = Pattern {value = "a", elements = []}, elements = []},
--     Pattern {value = Pattern {value = "b", elements = []}, elements = []}
--   ]
-- }

-- Nested pattern
let p = patternWith "root" [patternWith "a" [pattern "x"]]
in duplicate p
-- Result: Pattern {
--   value = Pattern {value = "root", elements = [patternWith "a" [pattern "x"]]},
--   elements = [
--     Pattern {
--       value = Pattern {value = "a", elements = [pattern "x"]},
--       elements = [Pattern {value = Pattern {value = "x", elements = []}, elements = []}]
--     }
--   ]
-- }
```

**Haddock Documentation**:

```haskell
-- | Create a pattern where each position contains the full pattern structure focused at that position.
--
-- This operation creates a pattern of contexts, where each position contains
-- the full pattern structure focused at that position. This enables context-aware
-- computations by providing the full structural context at each position.
--
-- === Context Creation
--
-- The duplicate operation creates contexts recursively:
--
-- * Root position contains the full original pattern
-- * Each element position contains the pattern structure focused at that element
-- * Contexts are created recursively for all nested positions
--
-- === Examples
--
-- Atomic pattern:
--
-- >>> duplicate (pattern 5)
-- Pattern {value = Pattern {value = 5, elements = []}, elements = []}
--
-- Pattern with elements:
--
-- >>> let p = patternWith "root" [pattern "a", pattern "b"]
-- >>> duplicate p
-- Pattern {
--   value = Pattern {value = "root", elements = [pattern "a", pattern "b"]},
--   elements = [
--     Pattern {value = Pattern {value = "a", elements = []}, elements = []},
--     Pattern {value = Pattern {value = "b", elements = []}, elements = []}
--   ]
-- }
duplicate :: Pattern v -> Pattern (Pattern v)
```

---

## Extend Function

### Type Signature

```haskell
extend :: (Pattern v -> w) -> Pattern v -> Pattern w
```

**Purpose**: Apply a context-aware function to each position in a pattern, creating a new pattern where each position contains the result of applying the function to the pattern structure at that position.

**Behavior**:
- Takes a context-aware function `Pattern v -> w` and a pattern `Pattern v`
- Returns a pattern `Pattern w` where each position contains the result of applying the function
- Function receives the full pattern structure at each position (not just the value)
- Applied recursively to all positions in the pattern structure

**Examples**:

```haskell
-- Compute depth at each position
let depthFunc p = depth p
    p = patternWith "root" [pattern "a", pattern "b"]
in extend depthFunc p
-- Result: Pattern {value = 1, elements = [Pattern {value = 0, elements = []}, Pattern {value = 0, elements = []}]}

-- Compute size at each position
let sizeFunc p = size p
    p = patternWith "root" [pattern "a", pattern "b"]
in extend sizeFunc p
-- Result: Pattern {value = 3, elements = [Pattern {value = 1, elements = []}, Pattern {value = 1, elements = []}]}

-- Custom context-aware function
let customFunc p = length (values p)
    p = patternWith "root" [pattern "a", pattern "b"]
in extend customFunc p
-- Result: Pattern {value = 3, elements = [Pattern {value = 1, elements = []}, Pattern {value = 1, elements = []}]}
```

**Haddock Documentation**:

```haskell
-- | Apply a context-aware function to each position in a pattern.
--
-- This operation applies a function that receives the full pattern structure
-- at each position, not just the value. The function is applied recursively
-- to all positions in the pattern structure, creating a new pattern where
-- each position contains the result of applying the function to the pattern
-- structure at that position.
--
-- === Context-Aware Computation
--
-- The extend operation enables context-aware computations where functions have
-- access to the full structural context (parent, siblings, depth, indices) around
-- each value, not just the value itself. This extends beyond Foldable (which
-- only provides values) to enable computations based on structural context.
--
-- === Examples
--
-- Compute depth at each position:
--
-- >>> let depthFunc p = depth p
-- >>> let p = patternWith "root" [pattern "a", pattern "b"]
-- >>> extend depthFunc p
-- Pattern {value = 1, elements = [Pattern {value = 0, elements = []}, Pattern {value = 0, elements = []}]}
--
-- Compute size at each position:
--
-- >>> let sizeFunc p = size p
-- >>> let p = patternWith "root" [pattern "a", pattern "b"]
-- >>> extend sizeFunc p
-- Pattern {value = 3, elements = [Pattern {value = 1, elements = []}, Pattern {value = 1, elements = []}]}
extend :: (Pattern v -> w) -> Pattern v -> Pattern w
```

---

## Optional Helper Functions

### Depth at Each Position

```haskell
depthAt :: Pattern v -> Pattern Int
depthAt = extend (\p -> depth p)
```

**Purpose**: Compute depth (nesting level) at each position in the pattern structure.

**Type Constraint**: None

**Module**: `Pattern.Core`

**Examples**:

```haskell
-- Simple pattern
depthAt (patternWith "root" [pattern "a", pattern "b"])
-- Result: Pattern {value = 1, elements = [Pattern {value = 0, elements = []}, Pattern {value = 0, elements = []}]}

-- Nested pattern
depthAt (patternWith "root" [patternWith "a" [pattern "x"]])
-- Result: Pattern {value = 2, elements = [Pattern {value = 1, elements = [Pattern {value = 0, elements = []}]}]}
```

---

### Size at Each Position

```haskell
sizeAt :: Pattern v -> Pattern Int
sizeAt = extend (\p -> size p)
```

**Purpose**: Compute size (total nodes) of subtree at each position.

**Type Constraint**: None

**Module**: `Pattern.Core`

**Examples**:

```haskell
-- Simple pattern
sizeAt (patternWith "root" [pattern "a", pattern "b"])
-- Result: Pattern {value = 3, elements = [Pattern {value = 1, elements = []}, Pattern {value = 1, elements = []}]}

-- Nested pattern
sizeAt (patternWith "root" [patternWith "a" [pattern "x"]])
-- Result: Pattern {value = 3, elements = [Pattern {value = 2, elements = [Pattern {value = 1, elements = []}]}]}
```

---

### Indices at Each Position

```haskell
indicesAt :: Pattern v -> Pattern [Int]
indicesAt = extend (\p -> indicesFromRoot p)
```

**Purpose**: Compute indices from root to each position (list of indices).

**Type Constraint**: None

**Module**: `Pattern.Core`

**Note**: This function requires an `indicesFromRoot` helper function that computes the indices from root to a position. This may need to be implemented as part of the feature or may require additional context tracking.

**Examples**:

```haskell
-- Simple pattern
indicesAt (patternWith "root" [pattern "a", pattern "b"])
-- Result: Pattern {value = [], elements = [Pattern {value = [0], elements = []}, Pattern {value = [1], elements = []}]}

-- Nested pattern
indicesAt (patternWith "root" [patternWith "a" [pattern "x"]])
-- Result: Pattern {value = [], elements = [Pattern {value = [0], elements = [Pattern {value = [0, 0], elements = []}]}]}
```

**Note**: Helper functions are optional convenience functions that demonstrate the power of the Comonad instance but are not required for core Comonad functionality.

