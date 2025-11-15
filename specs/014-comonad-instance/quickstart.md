# Quickstart: Comonad Instance for Pattern

**Feature**: 014-comonad-instance  
**Date**: 2025-01-28

## Overview

This quickstart guide demonstrates how to use the Comonad instance for `Pattern` to perform context-aware computations where functions have access to the full structural context (parent, siblings, depth, indices) around each value, not just the value itself. The Comonad instance enables computations based on position and structure, extending beyond `Foldable` which only provides values.

---

## Installation

The Comonad instance is part of the `pattern` package. Once the project is built:

```bash
# Using Cabal
cabal install pattern

# Or add to your project's dependencies
# In your .cabal file:
build-depends: pattern >= 0.1.0.0, comonad >= 5.0.8
```

---

## Basic Usage

### Importing

```haskell
import Pattern
import Control.Comonad  -- For Comonad typeclass
-- Comonad instance is automatically available via the Pattern module
```

### Extracting Values with `extract`

```haskell
-- Extract decoration value from atomic pattern
let p = pattern 5
    value = extract p
-- value = 5

-- Extract decoration value from pattern with elements
let p = patternWith "test" [pattern "a", pattern "b"]
    value = extract p
-- value = "test" (not "a" or "b")

-- Extract decoration value from nested pattern
let p = patternWith "root" [patternWith "a" [pattern "x"]]
    value = extract p
-- value = "root" (root decoration value)
```

### Creating Contexts with `duplicate`

```haskell
-- Duplicate atomic pattern
let p = pattern 5
    contexts = duplicate p
-- contexts = Pattern {value = Pattern {value = 5, elements = []}, elements = []}

-- Duplicate pattern with elements
let p = patternWith "root" [pattern "a", pattern "b"]
    contexts = duplicate p
-- contexts = Pattern {
--   value = Pattern {value = "root", elements = [pattern "a", pattern "b"]},
--   elements = [
--     Pattern {value = Pattern {value = "a", elements = []}, elements = []},
--     Pattern {value = Pattern {value = "b", elements = []}, elements = []}
--   ]
-- }

-- Verify: extract . duplicate = id
let p = patternWith "root" [pattern "a", pattern "b"]
    original = extract (duplicate p)
-- original == p  -- True
```

### Context-Aware Computations with `extend`

```haskell
-- Compute depth at each position
let depthFunc p = depth p
    p = patternWith "root" [pattern "a", pattern "b"]
    depths = extend depthFunc p
-- depths = Pattern {value = 1, elements = [Pattern {value = 0, elements = []}, Pattern {value = 0, elements = []}]}

-- Compute size at each position
let sizeFunc p = size p
    p = patternWith "root" [pattern "a", pattern "b"]
    sizes = extend sizeFunc p
-- sizes = Pattern {value = 3, elements = [Pattern {value = 1, elements = []}, Pattern {value = 1, elements = []}]}

-- Custom context-aware function
let customFunc p = length (values p)
    p = patternWith "root" [pattern "a", pattern "b"]
    counts = extend customFunc p
-- counts = Pattern {value = 3, elements = [Pattern {value = 1, elements = []}, Pattern {value = 1, elements = []}]}
```

---

## Common Patterns

### Extract-Extend Law

The extract-extend law states that extracting from an extended computation gives the original result:

```haskell
let depthFunc p = depth p
    p = patternWith "root" [pattern "a", pattern "b"]
    result1 = extract (extend depthFunc p)
    result2 = depthFunc p
-- result1 == result2  -- True (both equal depth of p)
```

### Extend-Extract Law

The extend-extract law states that extending with extract is identity:

```haskell
let p = patternWith "root" [pattern "a", pattern "b"]
    result = extend extract p
-- result == p  -- True
```

### Extend Composition Law

The extend composition law states that extend is associative:

```haskell
let depthFunc p = depth p
    sizeFunc p = size p
    p = patternWith "root" [pattern "a", pattern "b"]
    result1 = (extend depthFunc . extend sizeFunc) p
    result2 = extend (depthFunc . extend sizeFunc) p
-- result1 == result2  -- True
```

### Nested Pattern Context-Aware Computation

Apply context-aware functions recursively to nested patterns:

```haskell
let depthFunc p = depth p
    p = patternWith "root" 
        [ patternWith "a" [pattern "x"]
        , pattern "b"
        ]
    depths = extend depthFunc p
-- depths = Pattern {
--   value = 2,
--   elements = [
--     Pattern {value = 1, elements = [Pattern {value = 0, elements = []}]},
--     Pattern {value = 0, elements = []}
--   ]
-- }
```

---

## Advanced Usage

### Helper Functions (Optional)

If helper functions are implemented, they provide convenient access to common context-aware operations:

```haskell
-- Compute depth at each position
let p = patternWith "root" [pattern "a", pattern "b"]
    depths = depthAt p
-- depths = Pattern {value = 1, elements = [Pattern {value = 0, elements = []}, Pattern {value = 0, elements = []}]}

-- Compute size at each position
let p = patternWith "root" [pattern "a", pattern "b"]
    sizes = sizeAt p
-- sizes = Pattern {value = 3, elements = [Pattern {value = 1, elements = []}, Pattern {value = 1, elements = []}]}

-- Compute indices at each position
let p = patternWith "root" [pattern "a", pattern "b"]
    indices = indicesAt p
-- indices = Pattern {value = [], elements = [Pattern {value = [0], elements = []}, Pattern {value = [1], elements = []}]}
```

### Custom Context-Aware Functions

Create custom context-aware functions for specific computations:

```haskell
-- Compute maximum depth in subtree
let maxDepthFunc p = maximum (0 : map depth (elements p))
    p = patternWith "root" [patternWith "a" [pattern "x"], pattern "b"]
    maxDepths = extend maxDepthFunc p
-- maxDepths = Pattern {value = 1, elements = [Pattern {value = 1, elements = [Pattern {value = 0, elements = []}]}, Pattern {value = 0, elements = []}]}

-- Compute sum of all values in subtree
let sumFunc p = sum (values p)
    p = patternWith 10 [pattern 5, pattern 7]
    sums = extend sumFunc p
-- sums = Pattern {value = 22, elements = [Pattern {value = 5, elements = []}, Pattern {value = 7, elements = []}]}
```

### Relationship to Zippers

The Comonad instance provides zipper-like capabilities:

```haskell
-- Focus: extract gets the value at the current focus
let p = patternWith "root" [pattern "a", pattern "b"]
    focus = extract p
-- focus = "root"

-- Context: duplicate creates contexts at every position
let p = patternWith "root" [pattern "a", pattern "b"]
    contexts = duplicate p
-- contexts contains full pattern structure at each position

-- Navigation: extend lets you compute based on full context
let depthFunc p = depth p
    p = patternWith "root" [pattern "a", pattern "b"]
    depths = extend depthFunc p
-- depths contains depth at each position
```

---

## Relationship to Other Typeclasses

### Comonad vs Foldable

- **Foldable**: Provides values in sequence - `toList :: Pattern v -> [v]` extracts all values
- **Comonad**: Provides context-aware computation - `extend :: (Pattern v -> w) -> Pattern v -> Pattern w` applies functions with full context

```haskell
-- Foldable: Extract all values
let p = patternWith "root" [pattern "a", pattern "b"]
    values = toList p
-- values = ["root", "a", "b"]

-- Comonad: Compute depth at each position
let p = patternWith "root" [pattern "a", pattern "b"]
    depths = extend depth p
-- depths = Pattern {value = 1, elements = [Pattern {value = 0, elements = []}, Pattern {value = 0, elements = []}]}
```

### Comonad vs Traversable

- **Traversable**: Provides effectful traversal - `traverse :: (a -> f b) -> Pattern a -> f (Pattern b)` applies effects to values
- **Comonad**: Provides context-aware computation - `extend :: (Pattern v -> w) -> Pattern v -> Pattern w` applies functions with full context

```haskell
-- Traversable: Validate values
let validate x = if x > 0 then Just x else Nothing
    p = patternWith 10 [pattern 5, pattern 7]
    result = traverse validate p
-- result = Just (Pattern {value = 10, elements = [pattern 5, pattern 7]})

-- Comonad: Compute size at each position
let p = patternWith "root" [pattern "a", pattern "b"]
    sizes = extend size p
-- sizes = Pattern {value = 3, elements = [Pattern {value = 1, elements = []}, Pattern {value = 1, elements = []}]}
```

---

## Best Practices

### Use Context-Aware Functions for Structural Computations

Use `extend` when you need to compute based on structural context (depth, size, indices, relationships):

```haskell
-- Good: Use extend for context-aware computations
let depthFunc p = depth p
    p = patternWith "root" [pattern "a", pattern "b"]
    depths = extend depthFunc p

-- Avoid: Using Foldable when you need structural context
-- Foldable only provides values, not structural context
```

### Verify Comonad Laws

Always verify that your context-aware functions satisfy Comonad laws:

```haskell
-- Extract-extend law
extract . extend f = f

-- Extend-extract law
extend extract = id

-- Extend composition law
extend f . extend g = extend (f . extend g)
```

### Use Helper Functions When Available

If helper functions (depthAt, sizeAt, indicesAt) are implemented, use them for common operations:

```haskell
-- Good: Use helper function
let p = patternWith "root" [pattern "a", pattern "b"]
    depths = depthAt p

-- Also valid: Use extend directly
let p = patternWith "root" [pattern "a", pattern "b"]
    depths = extend depth p
```

---

## Troubleshooting

### Common Issues

**Issue**: `extract` returns unexpected value

**Solution**: `extract` returns the decoration value (root value), not element values. Use `toList` from Foldable to get all values.

```haskell
-- extract returns decoration value
let p = patternWith "root" [pattern "a", pattern "b"]
    value = extract p
-- value = "root"

-- Use toList to get all values
let p = patternWith "root" [pattern "a", pattern "b"]
    allValues = toList p
-- allValues = ["root", "a", "b"]
```

**Issue**: Context-aware function doesn't have access to expected context

**Solution**: Ensure your function receives the full pattern structure. The function signature should be `Pattern v -> w`, not `v -> w`.

```haskell
-- Good: Function receives full pattern structure
let depthFunc p = depth p
    p = patternWith "root" [pattern "a", pattern "b"]
    depths = extend depthFunc p

-- Bad: Function only receives value
let depthFunc v = -- can't compute depth from just value
```

---

## Further Reading

- [Comonad typeclass documentation](https://hackage.haskell.org/package/comonad)
- [Data.Tree comonad implementation](https://hackage.haskell.org/package/containers/docs/Data-Tree.html) (reference implementation)
- [Zippers and Comonads](https://bartoszmilewski.com/2017/01/02/comonads/) (category theory perspective)
- Pattern library documentation for related typeclasses (Functor, Foldable, Traversable, Applicative)

