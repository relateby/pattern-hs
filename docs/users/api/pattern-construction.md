# Pattern Construction Functions

**Last Updated**: 2025-01-27  
**Module**: `Pattern.Core`  
**Purpose**: Guide for constructing atomic patterns and choosing the appropriate function name when porting to other languages

## Overview

The Pattern library provides two functionally equivalent ways to create atomic patterns (patterns with no elements) from a value:

1. **`point`** - Category-theory aligned name (pointed functor)
2. **`pure`** - Applicative typeclass method

Both functions are identical in behavior: they take a value and return an atomic pattern containing that value with an empty elements list.

**Note**: The `pattern` function is now used for creating patterns with elements (the primary constructor). For atomic patterns, use `point` or `pure`.

## Function Signatures

```haskell
point   :: v -> Pattern v
pure    :: v -> Pattern v  -- From Applicative instance
pattern :: v -> [Pattern v] -> Pattern v  -- For patterns with elements
```

## Functional Equivalence

`point` and `pure` are functionally equivalent:

```haskell
point x   == pure x
-- Both produce: Pattern { value = x, elements = [] }
```

## When to Use Each Name

### In Haskell

**`point`** - Recommended for:
- Category-theory focused code
- When emphasizing the mathematical foundation
- Educational or theoretical contexts
- When the "pointed functor" concept is relevant

**`pattern`** - Now used for creating patterns with elements (the primary constructor):
- Creating patterns with one or more elements
- The main way to construct complete pattern structures
- Takes a decoration value and a list of pattern elements

**`pure`** - Recommended for:
- Applicative-style code (`pure f <*> pure x`)
- When using Applicative combinators
- Generic code that works with any Applicative
- When leveraging typeclass polymorphism

### Examples

```haskell
import Pattern.Core (point, pattern, Pattern(..))
import Control.Applicative (pure)

-- Using point (category-theory aligned)
atom1 = point "atom1"

-- Using pure (Applicative instance)
atom2 = pure "atom2"

-- Using pattern for patterns with elements
p = pattern "root" [point "a", point "b"]

-- point and pure are equivalent:
atom1 == atom2  -- True
```

## Porting Guidance

When porting the Pattern library to other languages, choose the function name that is most idiomatic for that language and its ecosystem. The following guidance helps you make the right choice:

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

**Alternative**: `point` (if category-theory libraries are common in your codebase)

**Rationale**: JavaScript/TypeScript ecosystems commonly use `of` for factory functions that lift values into containers (Array.of, Promise.of, etc.). This is the most idiomatic choice.

### Python

**Recommended**: `point` or `from_value`

```python
# Option 1: point (category-theory aligned)
@classmethod
def point(cls, value):
    return Pattern(value=value, elements=[])

# Option 2: from_value (explicit and clear)
@classmethod
def from_value(cls, value):
    return Pattern(value=value, elements=[])

# Usage
atom = Pattern.point("atom1")
# or
atom = Pattern.from_value("atom1")
```

**Rationale**: Python doesn't have a strong convention for this operation. `point` aligns with category theory, while `from_value` is more explicit. Avoid `pattern` as it could conflict with Python's `pattern` keyword in match statements.

### Rust

**Recommended**: `new` or `point`

```rust
impl<T> Pattern<T> {
    // Standard Rust convention
    pub fn new(value: T) -> Self {
        Pattern { value, elements: Vec::new() }
    }
    
    // Category-theory aligned alternative
    pub fn point(value: T) -> Self {
        Pattern { value, elements: Vec::new() }
    }
}

// Usage
let atom = Pattern::new("atom1");
// or
let atom = Pattern::point("atom1");
```

**Rationale**: Rust commonly uses `new` for constructors. `point` is also acceptable if you want to emphasize the category-theory connection. The `pure` method would come from implementing the `Applicative` trait (if available).

### Scala

**Recommended**: `point` or `pure` (via Applicative)

```scala
object Pattern {
  // Category-theory aligned
  def point[A](value: A): Pattern[A] = 
    Pattern(value, List.empty)
}

// Or via Applicative typeclass
implicit val patternApplicative: Applicative[Pattern] = 
  new Applicative[Pattern] {
    def pure[A](value: A): Pattern[A] = 
      Pattern(value, List.empty)
    // ... other methods
  }

// Usage
val atom = Pattern.point("atom1")
// or
val atom = Applicative[Pattern].pure("atom1")
```

**Rationale**: Scala has strong support for category theory concepts. Both `point` and `pure` (via Applicative) are idiomatic. The `pure` method is standard for Applicative instances.

### OCaml

**Recommended**: `point` or `pure` (via Applicative module)

```ocaml
module Pattern = struct
  type 'a t = { value : 'a; elements : 'a t list }
  
  let point value = { value; elements = [] }
end

(* Or via Applicative functor *)
module PatternApplicative = struct
  include Pattern
  let pure value = { value; elements = [] }
end

(* Usage *)
let atom = Pattern.point "atom1"
```

**Rationale**: OCaml has strong functional programming traditions. `point` is clear and aligns with category theory. `pure` would come from an Applicative module.

### Go

**Recommended**: `New` or `Point`

```go
// Standard Go convention
func NewPattern[T any](value T) *Pattern[T] {
    return &Pattern[T]{
        Value:    value,
        Elements: []*Pattern[T]{},
    }
}

// Category-theory aligned alternative
func Point[T any](value T) *Pattern[T] {
    return &Pattern[T]{
        Value:    value,
        Elements: []*Pattern[T]{},
    }
}

// Usage
atom := NewPattern("atom1")
// or
atom := Point("atom1")
```

**Rationale**: Go uses `New` prefix for constructors. `Point` (capitalized for export) is also acceptable if you want to emphasize category theory.

### Java

**Recommended**: `of` (following Optional.of, List.of pattern) or `point`

```java
public class Pattern<T> {
    // Following Java 9+ conventions
    public static <T> Pattern<T> of(T value) {
        return new Pattern<>(value, List.of());
    }
    
    // Category-theory aligned alternative
    public static <T> Pattern<T> point(T value) {
        return new Pattern<>(value, List.of());
    }
}

// Usage
Pattern<String> atom = Pattern.of("atom1");
// or
Pattern<String> atom = Pattern.point("atom1");
```

**Rationale**: Modern Java (9+) uses `of` for factory methods (Optional.of, List.of, Set.of). This is the most idiomatic choice. `point` is also acceptable.

### C#

**Recommended**: `Create` or `Point`

```csharp
public class Pattern<T> {
    // Standard C# convention
    public static Pattern<T> Create(T value) {
        return new Pattern<T>(value, new List<Pattern<T>>());
    }
    
    // Category-theory aligned alternative
    public static Pattern<T> Point(T value) {
        return new Pattern<T>(value, new List<Pattern<T>>());
    }
}

// Usage
var atom = Pattern.Create("atom1");
// or
var atom = Pattern.Point("atom1");
```

**Rationale**: C# commonly uses `Create` for factory methods. `Point` is also acceptable for category-theory alignment.

## Category Theory Background

The three function names reflect different perspectives on the same mathematical operation:

### Pointed Functor

A **pointed functor** is a functor equipped with a natural transformation from the identity functor. This transformation is often called `point`:

```
point : Id → F
```

For Pattern, this means lifting a value into the Pattern functor, creating an atomic pattern.

### Applicative Functor

An **Applicative functor** extends a pointed functor with the ability to apply functions in a structured context. The `pure` method is the pointed functor operation:

```
pure : a → F a
```

For Pattern, `pure` creates an atomic pattern, which is the same operation as `point`.

### Domain-Specific Naming

The `pattern` name is domain-specific to this library, emphasizing that we're creating a Pattern instance. It's the most explicit name for users who think in terms of the Pattern type rather than category theory.

## Summary Table

| Language | Recommended Name | Alternative | Rationale |
|----------|-----------------|-------------|-----------|
| Haskell | `point` / `pattern` / `pure` | All three available | Multiple idioms supported |
| JavaScript/TypeScript | `of` | `point` | Follows Array.of, Promise.of pattern |
| Python | `point` or `from_value` | - | No strong convention; explicit naming |
| Rust | `new` | `point` | Standard constructor convention |
| Scala | `point` or `pure` | - | Strong category theory support |
| OCaml | `point` or `pure` | - | Functional programming tradition |
| Go | `New` or `Point` | - | Constructor naming convention |
| Java | `of` | `point` | Modern Java factory method pattern |
| C# | `Create` | `Point` | Factory method convention |

## Key Principles for Porting

1. **Choose idiomatic names**: Use names that feel natural in the target language
2. **Maintain functional equivalence**: All names should produce identical results
3. **Document the choice**: Explain why you chose a particular name in your port
4. **Consider the ecosystem**: Look at similar libraries in the target language
5. **Preserve semantics**: The mathematical operation (lifting a value into Pattern) remains the same regardless of name

## Related Functions

- **`pattern`**: Creates patterns with elements (primary constructor, takes value and list of elements)
- **`fromList`**: Creates patterns from lists of values (convenience function)
- **`pure`**: Also available via Applicative instance for use in applicative style

## See Also

- [Pattern.Core Haddock Documentation](../api/pattern-core.md) - Full API reference
- [Category Theory Foundations](../../design/DESIGN.md) - Mathematical foundations
- [Construction Functions Specification](../../specs/004-construction-functions/spec.md) - Detailed specification

