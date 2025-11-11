# Quickstart: Construction Functions

**Feature**: 004-construction-functions  
**Date**: 2025-01-28

## Overview

This quickstart guide shows how to use the pattern constructor functions `pattern`, `patternWith`, and `fromList` to create patterns more conveniently than using verbose record syntax.

## Prerequisites

- Haskell project with Pattern library installed
- GHC 9.10.3 or compatible version
- Basic understanding of Haskell functions and the Pattern type

## Getting Started

### Import the Functions

```haskell
import Pattern.Core (pattern, patternWith, fromList)
```

Or import from the main Pattern module:

```haskell
import Pattern (pattern, patternWith, fromList)
```

## Creating Atomic Patterns

Use `pattern` to create atomic patterns (patterns with no elements):

```haskell
-- String atomic pattern
atom1 :: Pattern String
atom1 = pattern "atom1"

-- Integer atomic pattern
atom2 :: Pattern Int
atom2 = pattern 42

-- Custom type atomic pattern
data Person = Person { name :: String, age :: Maybe Int }
  deriving Show

alice :: Pattern Person
alice = pattern (Person "Alice" (Just 30))
```

**Comparison with record syntax**:

```haskell
-- Using constructor function (concise)
atom1 = pattern "test"

-- Using record syntax (verbose)
atom2 = Pattern { value = "test", elements = [] }

-- These are equivalent
atom1 == atom2  -- True
```

## Creating Patterns with Elements

Use `patternWith` to create patterns with elements:

### Singular Pattern (One Element)

```haskell
-- Create a singular pattern
singular :: Pattern String
singular = patternWith "soccer" [pattern "a team sport involving kicking a ball"]

-- Verify structure
value singular        -- "soccer"
length (elements singular)  -- 1
```

### Role-Based Singular Pattern (Custom Types)

```haskell
-- Role-based singular patterns: "The goalie" is "Hans"
data Person = Person { name :: String, age :: Maybe Int }
  deriving (Eq, Show)

-- "The goalie" is "Hans"
goalie :: Pattern Person
goalie = patternWith (Person "Goalie" Nothing) 
  [ pattern (Person "Hans" (Just 25)) ]

-- "The bus driver" is "Alice"
busDriver :: Pattern Person
busDriver = patternWith (Person "Bus Driver" Nothing) 
  [ pattern (Person "Alice" (Just 30)) ]

-- "The waiter" is "Bob"
waiter :: Pattern Person
waiter = patternWith (Person "Waiter" Nothing) 
  [ pattern (Person "Bob" (Just 25)) ]

-- Verify structure
value goalie        -- Person "Goalie" Nothing
length (elements goalie)  -- 1
value (head (elements goalie))  -- Person "Hans" (Just 25)
```

### Pair Pattern (Two Elements)

```haskell
-- Create a pair pattern (often used for relationships)
pair :: Pattern String
pair = patternWith "knows" [pattern "Alice", pattern "Bob"]

-- Verify structure
value pair           -- "knows"
length (elements pair)  -- 2
```

### Extended Pattern (Many Elements)

```haskell
-- Create an extended pattern
extended :: Pattern String
extended = patternWith "graph" 
  [ pattern "elem1"
  , pattern "elem2"
  , pattern "elem3"
  , pattern "elem4"
  ]

-- Verify structure
value extended           -- "graph"
length (elements extended)  -- 4
```

### Empty List (Atomic Pattern)

When `patternWith` receives an empty list, it produces an atomic pattern:

```haskell
-- Empty list produces atomic pattern
atomic :: Pattern String
atomic = patternWith "empty" []

-- Equivalent to:
atomic' = pattern "empty"

-- These are equivalent
atomic == atomic'  -- True
```

## Nested Patterns

Both functions work correctly with nested patterns:

```haskell
-- Deeply nested pattern
nested :: Pattern String
nested = patternWith "outer" 
  [ patternWith "middle" 
      [ patternWith "inner" 
          [ pattern "innermost" ]
      ]
  ]

-- Verify nesting
value nested  -- "outer"
value (head (elements nested))  -- "middle"
value (head (elements (head (elements nested))))  -- "inner"
```

## Building Graph Structures

Constructor functions make it easy to build graph structures:

```haskell
-- Create atomic patterns
alice = pattern "Alice"
bob = pattern "Bob"
charlie = pattern "Charlie"

-- Create relationships (pair patterns)
knows1 = patternWith "knows" [alice, bob]
knows2 = patternWith "knows" [bob, charlie]

-- Create a graph (extended pattern)
graph :: Pattern String
graph = patternWith "socialGraph" 
  [ alice
  , bob
  , charlie
  , knows1
  , knows2
  ]
```

## Functional Equivalence

Patterns created with constructor functions are functionally identical to patterns created with record syntax:

```haskell
-- Using constructor function
p1 = pattern "test"
p2 = patternWith "soccer" [pattern "a team sport involving kicking a ball"]

-- Using record syntax
p1' = Pattern { value = "test", elements = [] }
p2' = Pattern { value = "soccer", elements = [Pattern { value = "a team sport involving kicking a ball", elements = [] }] }

-- They are equivalent
p1 == p1'  -- True
p2 == p2'  -- True
```

## Element Order Preservation

The `patternWith` function preserves the order of elements:

```haskell
-- Order matters
p1 = patternWith "seq" [pattern "a", pattern "b", pattern "c"]
p2 = patternWith "seq" [pattern "c", pattern "b", pattern "a"]

-- Different order produces different patterns
p1 == p2  -- False

-- Verify order
map value (elements p1)  -- ["a", "b", "c"]
map value (elements p2)  -- ["c", "b", "a"]
```

## Working with Different Value Types

Both functions work with any value type:

```haskell
-- String values
strPattern = pattern "text"
strPatternWith = patternWith "soccer" [pattern "a team sport involving kicking a ball"]

-- Integer values
intPattern = pattern 42
intPatternWith = patternWith 100 [pattern 10, pattern 20]

-- Custom types
data Person = Person { name :: String, age :: Maybe Int }
personPattern = pattern (Person "Alice" (Just 30))
-- Role-based singular pattern: "The goalie" is "Hans"
personPatternWith = patternWith (Person "Goalie" Nothing) 
  [ pattern (Person "Hans" (Just 25)) ]
```

## Common Patterns

### Creating a Simple Relationship

```haskell
-- Two atomic patterns
personA = pattern "Person A"
personB = pattern "Person B"

-- Relationship between them
relationship = patternWith "knows" [personA, personB]
```

### Creating a Graph with Multiple Relationships

```haskell
-- Atomic patterns
alice = pattern "Alice"
bob = pattern "Bob"
charlie = pattern "Charlie"

-- Relationships
aliceKnowsBob = patternWith "knows" [alice, bob]
bobKnowsCharlie = patternWith "knows" [bob, charlie]

-- Graph containing all nodes and relationships
socialGraph = patternWith "socialNetwork"
  [ alice
  , bob
  , charlie
  , aliceKnowsBob
  , bobKnowsCharlie
  ]
```

## Creating Patterns from Lists

Use `fromList` to create patterns from lists of values:

```haskell
-- Convert list of strings to pattern
names :: Pattern String
names = fromList "people" ["Alice", "Bob", "Charlie"]

-- Verify structure
value names           -- "people"
length (elements names)  -- 3
map value (elements names)  -- ["Alice", "Bob", "Charlie"]
```

**Comparison with manual construction**:

```haskell
-- Using fromList (concise)
p1 = fromList "graph" ["Alice", "Bob", "Charlie"]

-- Using patternWith with manual pattern calls (verbose)
p2 = patternWith "graph" [pattern "Alice", pattern "Bob", pattern "Charlie"]

-- They are equivalent
p1 == p2  -- True
```

### Empty List

When `fromList` receives an empty list, it produces an atomic pattern:

```haskell
-- Empty list produces atomic pattern
atomic :: Pattern String
atomic = fromList "empty" []

-- Equivalent to:
atomic' = pattern "empty"

-- These are equivalent
atomic == atomic'  -- True
```

### Different Value Types

`fromList` works with any value type:

```haskell
-- String values
strPattern = fromList "words" ["hello", "world"]

-- Integer values
intPattern = fromList "numbers" [1, 2, 3, 4, 5]

-- Custom types
data Person = Person { name :: String, age :: Maybe Int }
people = fromList (Person "Team" Nothing) 
  [ Person "Alice" (Just 30)
  , Person "Bob" (Just 25)
  , Person "Charlie" (Just 35)
  ]
```

## Benefits of Constructor Functions

1. **Reduced verbosity**: `pattern "test"` vs `Pattern { value = "test", elements = [] }`
2. **Improved readability**: Function names clearly indicate purpose
3. **Consistency**: Same API for all pattern creation
4. **Type safety**: Full type checking preserved
5. **Functional equivalence**: Identical behavior to record syntax
6. **List conversion**: `fromList` makes it easy to create patterns from raw data

## Next Steps

- See [data-model.md](./data-model.md) for detailed function specifications
- See [contracts/type-signatures.md](./contracts/type-signatures.md) for complete API documentation
- See [spec.md](./spec.md) for feature requirements and acceptance criteria

