# Quickstart: Applicative Instance for Pattern

**Feature**: 013-applicative-instance  
**Date**: 2025-01-28

## Overview

This quickstart guide demonstrates how to use the Applicative instance for `Pattern` to apply functions stored in patterns to values stored in patterns. The Applicative instance enables structured function application where both functions and values are organized as pattern structures.

---

## Installation

The Applicative instance is part of the `pattern` package. Once the project is built:

```bash
# Using Cabal
cabal install pattern

# Or add to your project's dependencies
# In your .cabal file:
build-depends: pattern >= 0.1.0.0
```

---

## Basic Usage

### Importing

```haskell
import Pattern
-- Applicative instance is automatically available via the Pattern module
```

### Wrapping Values with `pure`

```haskell
-- Wrap an integer value in a pattern
let x = pure 5
-- x = Pattern {value = 5, elements = []}

-- Wrap a string value
let s = pure "hello"
-- s = Pattern {value = "hello", elements = []}

-- Wrap a function value
let f = pure (+1)
-- f = Pattern {value = (+1), elements = []}
```

### Applying Functions to Values

```haskell
-- Apply a function to a value (both in patterns)
let f = pure (+1)
    x = pure 5
    result = f <*> x
-- result = Pattern {value = 6, elements = []}

-- Apply multiple functions to multiple values
let fs = patternWith id [pure (*2), pure (+10)]
    xs = patternWith 5 [pure 3, pure 7]
    result = fs <*> xs
-- result = Pattern {value = 5, elements = [Pattern {value = 6, elements = []}, Pattern {value = 17, elements = []}]}
```

---

## Common Patterns

### Homomorphism Law

The homomorphism law states that applying a pure function to a pure value equals pure application:

```haskell
let f = (+1)
    x = 5
    result1 = pure f <*> pure x
    result2 = pure (f x)
-- result1 == result2  -- True (both equal pattern 6)
```

### Consistency with Functor

Applicative operations are consistent with Functor operations:

```haskell
let f = (+1)
    x = pattern 5
    result1 = fmap f x
    result2 = pure f <*> x
-- result1 == result2  -- True (both equal pattern 6)
```

### Nested Pattern Application

Apply functions recursively to nested patterns:

```haskell
let fs = patternWith id 
      [ patternWith (*2) [pure (*3)]
      , patternWith (+1) []
      ]
    xs = patternWith 1
      [ patternWith 2 [pure 3]
      , patternWith 4 []
      ]
    result = fs <*> xs
-- result = Pattern {value = 1, elements = [Pattern {value = 4, elements = [Pattern {value = 9, elements = []}]}, Pattern {value = 5, elements = []}]}
```

### Mismatched Element Counts

When function and value patterns have different element counts, zip-like truncation applies:

```haskell
-- Function pattern has 2 elements, value pattern has 3 elements
let fs = patternWith id [pure (*2), pure (+10)]  -- 2 elements
    xs = patternWith 5 [pure 3, pure 7, pure 11]       -- 3 elements
    result = fs <*> xs
-- result = Pattern {value = 5, elements = [Pattern {value = 6, elements = []}, Pattern {value = 17, elements = []}]}
-- Note: Third element (11) is ignored due to truncation
```

---

## Advanced Usage

### Composition Law

The composition law allows composing function applications:

```haskell
let u = pattern (+1)
    v = pattern (*2)
    w = pattern 5
    result1 = pure (.) <*> u <*> v <*> w
    result2 = u <*> (v <*> w)
-- result1 == result2  -- True
```

### Interchange Law

The interchange law allows swapping function and value patterns:

```haskell
let u = pattern (+1)
    y = 5
    result1 = u <*> pure y
    result2 = pure ($ y) <*> u
-- result1 == result2  -- True (both equal pattern 6)
```

### Identity Law

The identity law ensures that applying the identity function produces the same pattern:

```haskell
let p = patternWith "test" [pattern "a", pattern "b"]
    result = pure id <*> p
-- result == p  -- True
```

---

## Type Transformations

The Applicative instance allows applying functions that change value types:

```haskell
-- Apply length function to string pattern
let fs = pure length
    xs = pure "hello"
    result = fs <*> xs
-- result = Pattern {value = 5, elements = []}

-- Apply show function to integer pattern
let fs = pure show
    xs = pure 42
    result = fs <*> xs
-- result = Pattern {value = "42", elements = []}
```

---

## Best Practices

1. **Use `pure` for wrapping values**: Always use `pure` to wrap values when you need to use them in applicative operations
2. **Structure matching**: For best results, ensure function and value patterns have matching structures (same element counts and nesting)
3. **Leverage laws**: Use Applicative laws (homomorphism, composition, etc.) to simplify and reason about your code
4. **Consistency with Functor**: Remember that `fmap f x = pure f <*> x`, so you can use either approach
5. **Handle truncation**: Be aware that mismatched element counts result in zip-like truncation, which may silently ignore data

---

## Troubleshooting

### Type Errors

If you encounter type errors, ensure that:
- Function patterns contain functions of the correct type
- Value patterns contain values of the correct type
- Function and value types are compatible (function input type matches value type)

### Unexpected Truncation

If results seem incomplete, check for mismatched element counts:
- Function pattern element count vs. value pattern element count
- Truncation applies up to the minimum element count

### Structure Mismatches

For nested patterns, ensure that:
- Nesting depths match between function and value patterns
- Element structures match at each level
- Truncation applies at each level independently

---

## Further Reading

- [Data Model](data-model.md): Detailed explanation of the Applicative instance data model
- [Type Signatures](contracts/type-signatures.md): Complete type signatures and API documentation
- [Specification](spec.md): Full feature specification with requirements and success criteria
- [Research](research.md): Research decisions and rationale for implementation approach

---

## Examples Summary

The Applicative instance enables:

- ✅ Wrapping values in patterns with `pure`
- ✅ Applying functions stored in patterns to values stored in patterns
- ✅ Structure-preserving function application
- ✅ Recursive application to nested patterns
- ✅ Zip-like truncation for mismatched structures
- ✅ Type transformations (e.g., `String -> Int`)
- ✅ Consistency with Functor instance
- ✅ Satisfaction of all Applicative laws

