# Creating Patterns

## Introduction

Now that you understand what Patterns are, let's learn how to create them. The Pattern library provides functions for constructing Patterns:

- **`point`** - Create atomic patterns (the standard way)
- **`pure`** - Alias for `point` from the Applicative instance (functionally equivalent)
- **`pattern`** - Create patterns with elements (primary constructor)

This section covers how to use these functions to build Pattern structures.

## Atomic Patterns

An **atomic pattern** is a pattern with no elements (`elements == []`). Atomic patterns are the fundamental building blocks from which all other patterns are constructed.

### Using `point`

The `point` function creates an atomic pattern from a value. This is the standard way to create atomic patterns:

```haskell
import Pattern.Core (point, Pattern(..))

-- Create an atomic pattern with string value
atomicString :: Pattern String
atomicString = point "atom1"

-- The value field stores the decoration
value atomicString  -- "atom1"

-- The elements field is empty (this IS the pattern - an empty sequence)
elements atomicString  -- []
```

**Gram notation:**
```gram
["atom1"]
```

You can also use `pure` (from the Applicative instance), which is functionally equivalent to `point`:

```haskell
import Pattern.Core (Pattern(..))
import Control.Applicative (pure)

-- pure is equivalent to point
atomicInt :: Pattern Int
atomicInt = pure 42  -- Same as: point 42

point "test" == pure "test"  -- True
```

Use `point` as the standard way to create atomic patterns. Use `pure` when working in an Applicative context or when leveraging typeclass polymorphism.

## Patterns with Elements

The `pattern` function creates patterns with one or more elements:

```haskell
import Pattern.Core (pattern, point)

-- Create atomic patterns first
atomA = point "A"
atomB = point "B"

-- Create a pattern with elements
pairPattern :: Pattern String
pairPattern = pattern "knows" [atomA, atomB]

value pairPattern  -- "knows"
length (elements pairPattern)  -- 2
```

**Gram notation:**
```gram
["knows" | A, B]
```

### Pattern Function Signature

```haskell
pattern :: v -> [Pattern v] -> Pattern v
```

- First argument: The decoration value
- Second argument: List of pattern elements (the pattern itself)

### Multiple Elements

You can create patterns with any number of elements:

```haskell
-- Create multiple atomic patterns
elem1 = point "elem1"
elem2 = point "elem2"
elem3 = point "elem3"

-- Create a pattern with multiple elements
manyPattern :: Pattern String
manyPattern = pattern "sequence" [elem1, elem2, elem3]

value manyPattern  -- "sequence"
length (elements manyPattern)  -- 3
```

**Gram notation:**
```gram
["sequence" | elem1, elem2, elem3]
```

## Nested Patterns

Patterns can contain other patterns, enabling nested structures:

```haskell
-- Create inner pattern
inner = point "inner"

-- Create middle pattern containing inner
middle = pattern "middle" [inner]

-- Create outer pattern containing middle
outer = pattern "outer" [middle]

-- Access nested structure
value outer  -- "outer"
value (head (elements outer))  -- "middle"
value (head (elements (head (elements outer))))  -- "inner"
```

**Gram notation:**
```gram
["outer" | ["middle" | ["inner"]]]
```

### Building Complex Structures

You can build arbitrarily complex nested patterns:

```haskell
-- Create atomic patterns
a = point "A"
b = point "B"
c = point "C"

-- Create intermediate patterns
ab = pattern "pair" [a, b]
bc = pattern "pair" [b, c]

-- Create top-level pattern containing intermediate patterns
topLevel = pattern "sequence" [ab, bc]

-- The structure: sequence containing two pairs
value topLevel  -- "sequence"
length (elements topLevel)  -- 2
```

**Gram notation:**
```gram
["sequence" | ["pair" | A, B], ["pair" | B, C]]
```

## Porting Guidance

When porting Pattern construction to other languages, choose function names that are idiomatic for that language:

### JavaScript/TypeScript

**Recommended**: `of` (following Array.of, Promise.of pattern)

```typescript
// Idiomatic JavaScript/TypeScript
function of<T>(value: T): Pattern<T> {
  return { value, elements: [] };
}

// Usage
const atom = Pattern.of("atom1");
```

### Python

**Recommended**: `point` or `from_value`

```python
@classmethod
def point(cls, value):
    return Pattern(value=value, elements=[])

# Usage
atom = Pattern.point("atom1")
```

### Rust

**Recommended**: `new` or `point`

```rust
impl<T> Pattern<T> {
    pub fn new(value: T) -> Self {
        Pattern { value, elements: Vec::new() }
    }
}

// Usage
let atom = Pattern::new("atom1");
```

### Java

**Recommended**: `of` (following Optional.of, List.of pattern)

```java
public static <T> Pattern<T> of(T value) {
    return new Pattern<>(value, List.of());
}

// Usage
Pattern<String> atom = Pattern.of("atom1");
```

See the [Pattern Construction API documentation](../api/pattern-construction.md) for complete porting guidance across languages.

## Summary

- **Atomic patterns**: Use `point` to create patterns with no elements (or `pure` from Applicative, which is equivalent)
- **Patterns with elements**: Use `pattern` to create patterns with one or more elements
- **Nested patterns**: Patterns can contain other patterns, enabling complex structures
- **Porting**: Choose idiomatic function names when porting to other languages

## Next Steps

Now that you can create Patterns, learn how to access their components and query their properties in [Basic Operations](04-basic-operations.md).

