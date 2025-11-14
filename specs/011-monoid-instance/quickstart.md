# Quickstart: Monoid Instance for Pattern

**Feature**: 011-monoid-instance  
**Date**: 2025-01-27  
**Status**: Design Complete

## Overview

The `Monoid` instance for `Pattern` extends the `Semigroup` instance by providing an identity element (`mempty`). The identity pattern has `mempty` value (from value type's Monoid) and empty elements list, enabling identity-based operations and standard Monoid combinators like `mconcat`.

## Basic Usage

### Identity Pattern

```haskell
import Pattern.Core
import Data.Monoid

-- Get the identity pattern
mempty :: Pattern String
-- Result: Pattern { value = "", elements = [] }

-- Identity pattern structure
value mempty  -- "" (empty string for String values)
elements mempty  -- [] (empty list)
```

### Combining with Identity

```haskell
-- Left identity: mempty <> p = p
let p = pattern "test"
mempty <> p  -- Pattern { value = "test", elements = [] }

-- Right identity: p <> mempty = p
p <> mempty  -- Pattern { value = "test", elements = [] }

-- Identity with patterns having elements
let p2 = patternWith "root" [pattern "a", pattern "b"]
mempty <> p2  -- Pattern { value = "root", elements = [pattern "a", pattern "b"] }
p2 <> mempty  -- Pattern { value = "root", elements = [pattern "a", pattern "b"] }
```

### Pattern Accumulation with Empty Initial State

```haskell
-- Start with identity and build incrementally
let patterns = [pattern "a", pattern "b", pattern "c"]
let combined = foldr (<>) mempty patterns
-- Result: Pattern { value = "abc", elements = [pattern "a", pattern "b", pattern "c"] }

-- Or use mconcat
import Data.Monoid (mconcat)
let combined2 = mconcat patterns
-- Result: Same as above
```

## Standard Monoid Combinators

### mconcat

Combine a list of patterns (handles empty list):

```haskell
import Data.Monoid (mconcat)

-- Combine list of patterns
let patterns = [pattern "a", pattern "b", pattern "c"]
mconcat patterns
-- Result: Pattern { value = "abc", elements = [pattern "a", pattern "b", pattern "c"] }

-- Empty list returns identity
mconcat [] :: Pattern String
-- Result: Pattern { value = "", elements = [] }

-- List with only identity
mconcat [mempty, mempty, mempty] :: Pattern String
-- Result: Pattern { value = "", elements = [] }
```

### mappend

Alias for `<>` (inherited from Semigroup):

```haskell
import Data.Monoid (mappend)

mappend (pattern "a") (pattern "b")
-- Result: Pattern { value = "ab", elements = [] }
```

## Use Cases

### Use Case 1: Pattern Accumulation with Empty Initial State

Accumulate patterns from a collection starting with identity:

```haskell
-- Accumulate patterns from a list
let patterns = [pattern "a", pattern "b", pattern "c"]
let accumulated = foldr (<>) mempty patterns
-- Result: Pattern { value = "abc", elements = [pattern "a", pattern "b", pattern "c"] }

-- Or use mconcat
let accumulated2 = mconcat patterns
-- Result: Same as above
```

### Use Case 2: Optional Pattern Construction

Build patterns conditionally, handling optional values:

```haskell
import Data.Maybe (maybe)

-- Conditional pattern building
let optionalValue = Just "test"
maybe mempty pattern optionalValue
-- Result: pattern "test"

let optionalValue2 = Nothing
maybe mempty pattern optionalValue2
-- Result: mempty

-- Combining optional patterns
let maybePattern1 = maybe mempty pattern (Just "a")
let maybePattern2 = maybe mempty pattern (Just "b")
maybePattern1 <> maybePattern2
-- Result: Pattern { value = "ab", elements = [pattern "a", pattern "b"] }
```

### Use Case 3: Pattern Initialization and Default Values

Initialize pattern structures with default/empty state:

```haskell
-- Initialize empty pattern
let p = mempty

-- Build pattern incrementally
let p1 = mempty <> pattern "a"
let p2 = p1 <> pattern "b"
let p3 = p2 <> pattern "c"
-- p3: Pattern { value = "abc", elements = [pattern "a", pattern "b", pattern "c"] }

-- Using as default in pattern matching
case maybePattern of
  Nothing -> mempty
  Just p -> p
```

## Value Type Monoid Examples

### String Values (Empty String)

```haskell
mempty :: Pattern String
-- Result: Pattern { value = "", elements = [] }

mempty <> pattern "test"
-- Result: pattern "test"
```

### Sum Int Values (Sum 0)

```haskell
import Data.Monoid (Sum)

mempty :: Pattern (Sum Int)
-- Result: Pattern { value = Sum 0, elements = [] }

mempty <> pattern (Sum 5)
-- Result: Pattern { value = Sum 5, elements = [] }
```

### Product Int Values (Product 1)

```haskell
import Data.Monoid (Product)

mempty :: Pattern (Product Int)
-- Result: Pattern { value = Product 1, elements = [] }

mempty <> pattern (Product 5)
-- Result: Pattern { value = Product 5, elements = [] }
```

### All Values (All True)

```haskell
import Data.Monoid (All)

mempty :: Pattern All
-- Result: Pattern { value = All True, elements = [] }

mempty <> pattern (All False)
-- Result: Pattern { value = All False, elements = [] }
```

### Any Values (Any False)

```haskell
import Data.Monoid (Any)

mempty :: Pattern Any
-- Result: Pattern { value = Any False, elements = [] }

mempty <> pattern (Any True)
-- Result: Pattern { value = Any True, elements = [] }
```

## Edge Cases

### Identity with Atomic Patterns

```haskell
-- Left identity
mempty <> pattern "a"
-- Result: pattern "a"

-- Right identity
pattern "a" <> mempty
-- Result: pattern "a"
```

### Identity with Patterns Having Elements

```haskell
let p = patternWith "root" [pattern "a", pattern "b"]

-- Left identity
mempty <> p
-- Result: Pattern { value = "root", elements = [pattern "a", pattern "b"] }

-- Right identity
p <> mempty
-- Result: Pattern { value = "root", elements = [pattern "a", pattern "b"] }
```

### Identity with Nested Patterns

```haskell
let nested = patternWith "outer" [patternWith "inner" [pattern "a"]]

-- Left identity
mempty <> nested
-- Result: Pattern { value = "outer", elements = [Pattern { value = "inner", elements = [pattern "a"] }] }

-- Right identity
nested <> mempty
-- Result: Pattern { value = "outer", elements = [Pattern { value = "inner", elements = [pattern "a"] }] }
```

### Using mconcat with Empty List

```haskell
mconcat [] :: Pattern String
-- Result: Pattern { value = "", elements = [] }
```

### Using mconcat with List Containing Only Identity

```haskell
mconcat [mempty, mempty, mempty] :: Pattern String
-- Result: Pattern { value = "", elements = [] }
```

## Type Constraints

The `Monoid` instance requires the value type to have a `Monoid` instance:

```haskell
-- This works (String has Monoid instance)
mempty :: Pattern String

-- This doesn't work if CustomType doesn't have Monoid instance
mempty :: Pattern CustomType
-- Error: No instance for (Monoid CustomType)
```

## Identity Laws

The Monoid instance satisfies identity laws:

```haskell
let p = pattern "test"

-- Left identity: mempty <> p = p
mempty <> p == p  -- True

-- Right identity: p <> mempty = p
p <> mempty == p  -- True
```

These laws hold for all pattern structures (atomic, with elements, nested).

## Consistency with Semigroup

The Monoid instance is consistent with Semigroup:

```haskell
let p1 = pattern "a"
let p2 = pattern "b"

-- Same result using Semigroup or Monoid instance
p1 <> p2  -- Pattern { value = "ab", elements = [] }
```

The `<>` operation is inherited from Semigroup, ensuring consistency.

## Notes

- Identity pattern has `mempty` value (from value type's Monoid) and empty elements list
- Identity laws hold for all pattern structures (atomic, with elements, nested)
- Monoid instance extends Semigroup, inheriting `<>` operation
- `mconcat` handles empty lists gracefully by returning `mempty`
- The instance enables identity-based operations and standard Monoid combinators
- Performance: Identity pattern creation is O(1), combination with identity is O(n) where n is the number of elements in the non-identity pattern

