# Migration Guide: Constructor Function Renaming

**Date**: 2025-01-28  
**Breaking Change**: Yes  
**Impact**: All code using `pattern` or `patternWith` constructors

## Overview

The Pattern library constructor functions have been renamed to better align with the conceptual model of "decorated sequences":

- **`patternWith`** → **`pattern`** (primary constructor for patterns with elements)
- **`pattern`** (atomic) → **`point`** (atomic pattern constructor)

This change makes `pattern` the primary way to create patterns (which typically have elements), and `point` the special case for atomic patterns (patterns with no elements).

## Breaking Changes

### Function Renames

| Old API | New API | Purpose |
|---------|---------|---------|
| `pattern v` | `point v` | Create atomic pattern (no elements) |
| `patternWith v es` | `pattern v es` | Create pattern with elements |

### Import Changes

**Before:**
```haskell
import Pattern.Core (pattern, patternWith, fromList)
```

**After:**
```haskell
import Pattern.Core (point, pattern, fromList)
```

## Migration Process

### Step 1: Update Imports

Replace all imports that reference `patternWith`:

```haskell
-- Old
import Pattern.Core (pattern, patternWith, ...)

-- New
import Pattern.Core (point, pattern, ...)
```

### Step 2: Replace Atomic Pattern Construction

Find all uses of `pattern` for atomic patterns and replace with `point`:

```haskell
-- Old
atom1 = pattern "atom1"
atom2 = pattern 42
person = pattern (Person "Alice" (Just 30))

-- New
atom1 = point "atom1"
atom2 = point 42
person = point (Person "Alice" (Just 30))
```

**Search pattern**: Look for `pattern "` or `pattern (` where there's no list argument following.

### Step 3: Replace Pattern Construction with Elements

Find all uses of `patternWith` and replace with `pattern`:

```haskell
-- Old
singular = patternWith "soccer" [pattern "a team sport"]
pair = patternWith "knows" [pattern "Alice", pattern "Bob"]
extended = patternWith "graph" [pattern "elem1", pattern "elem2"]

-- New
singular = pattern "soccer" [point "a team sport"]
pair = pattern "knows" [point "Alice", point "Bob"]
extended = pattern "graph" [point "elem1", point "elem2"]
```

**Note**: Also update any nested `pattern` calls to `point` for atomic patterns.

### Step 4: Update Nested Patterns

When updating nested patterns, ensure atomic patterns use `point`:

```haskell
-- Old
nested = patternWith "outer" 
  [ patternWith "middle" 
      [ patternWith "inner" 
          [ pattern "innermost" ]
      ]
  ]

-- New
nested = pattern "outer" 
  [ pattern "middle" 
      [ pattern "inner" 
          [ point "innermost" ]
      ]
  ]
```

### Step 5: Update fromList Usage

The `fromList` function remains unchanged, but its implementation now uses `pattern` and `point`:

```haskell
-- This still works the same way
p = fromList "graph" ["Alice", "Bob", "Charlie"]

-- Internally equivalent to:
p' = pattern "graph" [point "Alice", point "Bob", point "Charlie"]
```

## Common Patterns

### Creating Atomic Patterns

```haskell
-- Old
atom = pattern "test"

-- New
atom = point "test"
```

### Creating Patterns with One Element

```haskell
-- Old
singular = patternWith "soccer" [pattern "a team sport"]

-- New
singular = pattern "soccer" [point "a team sport"]
```

### Creating Patterns with Multiple Elements

```haskell
-- Old
pair = patternWith "knows" [pattern "Alice", pattern "Bob"]

-- New
pair = pattern "knows" [point "Alice", point "Bob"]
```

### Building Graph Structures

```haskell
-- Old
alice = pattern "Alice"
bob = pattern "Bob"
knows = patternWith "knows" [alice, bob]
graph = patternWith "socialGraph" [alice, bob, knows]

-- New
alice = point "Alice"
bob = point "Bob"
knows = pattern "knows" [alice, bob]
graph = pattern "socialGraph" [alice, bob, knows]
```

### Using with Applicative

```haskell
-- Old
fs = patternWith id [pattern (*2), pattern (+10)]
xs = patternWith 5 [pattern 3, pattern 7]

-- New
fs = pattern id [point (*2), point (+10)]
xs = pattern 5 [point 3, point 7]
```

## Automated Migration

### Using Find and Replace

1. **Replace `patternWith` → `pattern`**:
   ```bash
   find . -name "*.hs" -exec sed -i '' 's/patternWith/pattern/g' {} +
   ```

2. **Replace atomic `pattern` → `point`** (requires manual review):
   - Search for: `pattern "` or `pattern (` followed by a value (not a list)
   - Replace with: `point "` or `point (`
   - **Be careful**: This requires context awareness to avoid replacing variable names

### Recommended Manual Process

Due to the ambiguity between `pattern` as a function and `pattern` as a variable name, manual migration is recommended:

1. Update imports first
2. Replace all `patternWith` → `pattern`
3. Review each `pattern` call to determine if it's atomic (use `point`) or has elements (keep `pattern`)

## Verification

After migration, verify your code:

1. **Build check**:
   ```bash
   cabal build
   ```

2. **Test check**:
   ```bash
   cabal test
   ```

3. **Type check**: The compiler will catch any incorrect usage:
   - `point` takes one argument: `point :: v -> Pattern v`
   - `pattern` takes two arguments: `pattern :: v -> [Pattern v] -> Pattern v`

## Quick Reference

| Use Case | Old | New |
|----------|-----|-----|
| Atomic pattern | `pattern "atom"` | `point "atom"` |
| Pattern with elements | `patternWith "root" [p1, p2]` | `pattern "root" [p1, p2]` |
| Nested atomic | `patternWith "root" [pattern "child"]` | `pattern "root" [point "child"]` |
| From list | `fromList "root" ["a", "b"]` | `fromList "root" ["a", "b"]` (unchanged) |

## Rationale

This change aligns the API with the conceptual model:
- **`pattern`** is now the primary constructor (patterns typically have elements)
- **`point`** is the special case (atomic patterns are a special case)
- The naming better reflects that patterns are "decorated sequences" where elements form the pattern

## Questions?

If you encounter issues during migration:
1. Check the [API documentation](../api/pattern-construction.md)
2. Review the [examples](../../../libs/pattern/examples/examples.md)
3. Verify type signatures match the new API

