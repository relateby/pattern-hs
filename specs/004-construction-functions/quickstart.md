# Quickstart: Construction Functions

**Feature**: 004-construction-functions  
**Date**: 2025-01-28

## Overview

This quickstart guide shows how to use the pattern constructor functions `point`, `pattern`, and `fromList` to create patterns more conveniently than using verbose record syntax.

## Prerequisites

- Haskell project with Pattern library installed
- GHC 9.12.2 or compatible version
- Basic understanding of Haskell functions and the Pattern type

## Getting Started

### Import the Functions

```haskell
import Pattern.Core (point, pattern, fromList)
```

Or import from the main Pattern module:

```haskell
import Pattern (point, pattern, fromList)
```

## Creating Atomic Patterns

Use `point` to create atomic patterns (patterns with no elements):

```haskell
-- String atomic pattern
atom1 :: Pattern String
atom1 = point "atom1"

-- Integer atomic pattern
atom2 :: Pattern Int
atom2 = point 42

-- Custom type atomic pattern
data Person = Person { name :: String, age :: Maybe Int }
  deriving Show

alice :: Pattern Person
alice = point (Person "Alice" (Just 30))
```

**Comparison with record syntax**:

```haskell
-- Using constructor function (concise)
atom1 = point "test"

-- Using record syntax (verbose)
atom2 = Pattern { value = "test", elements = [] }

-- These are equivalent
atom1 == atom2  -- True
```

## Creating Patterns with Elements

Use `pattern` to create patterns with elements:

### Singular Pattern (One Element)

```haskell
-- Create a singular pattern
singular :: Pattern String
singular = pattern "soccer" [point "a team sport involving kicking a ball"]

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
goalie = pattern (Person "Goalie" Nothing) 
  [ point (Person "Hans" (Just 25)) ]

-- "The bus driver" is "Alice"
busDriver :: Pattern Person
busDriver = pattern (Person "Bus Driver" Nothing) 
  [ point (Person "Alice" (Just 30)) ]

-- "The waiter" is "Bob"
waiter :: Pattern Person
waiter = pattern (Person "Waiter" Nothing) 
  [ point (Person "Bob" (Just 25)) ]

-- Verify structure
value goalie        -- Person "Goalie" Nothing
length (elements goalie)  -- 1
value (head (elements goalie))  -- Person "Hans" (Just 25)
```

### Pair Pattern (Two Elements)

```haskell
-- Create a pair pattern (often used for relationships)
pair :: Pattern String
pair = pattern "knows" [point "Alice", point "Bob"]

-- Verify structure
value pair           -- "knows"
length (elements pair)  -- 2
```

### Extended Pattern (Many Elements)

```haskell
-- Create an extended pattern
extended :: Pattern String
extended = pattern "graph" 
  [ point "elem1"
  , point "elem2"
  , point "elem3"
  , point "elem4"
  ]

-- Verify structure
value extended           -- "graph"
length (elements extended)  -- 4
```

### Empty List (Atomic Pattern)

When `pattern` receives an empty list, it produces an atomic pattern:

```haskell
-- Empty list produces atomic pattern
atomic :: Pattern String
atomic = pattern "empty" []

-- Equivalent to:
atomic' = point "empty"

-- These are equivalent
atomic == atomic'  -- True
```

## Nested Patterns

Both functions work correctly with nested patterns:

```haskell
-- Deeply nested pattern
nested :: Pattern String
nested = pattern "outer" 
  [ pattern "middle" 
      [ pattern "inner" 
          [ point "innermost" ]
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
alice = point "Alice"
bob = point "Bob"
charlie = point "Charlie"

-- Create relationships (pair patterns)
knows1 = pattern "knows" [alice, bob]
knows2 = pattern "knows" [bob, charlie]

-- Create a graph (extended pattern)
graph :: Pattern String
graph = pattern "socialGraph" 
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
p1 = point "test"
p2 = pattern "soccer" [point "a team sport involving kicking a ball"]

-- Using record syntax
p1' = Pattern { value = "test", elements = [] }
p2' = Pattern { value = "soccer", elements = [Pattern { value = "a team sport involving kicking a ball", elements = [] }] }

-- They are equivalent
p1 == p1'  -- True
p2 == p2'  -- True
```

## Element Order Preservation

The `pattern` function preserves the order of elements:

```haskell
-- Order matters
p1 = pattern "seq" [point "a", point "b", point "c"]
p2 = pattern "seq" [point "c", point "b", point "a"]

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
strPattern = point "text"
strPatternWith = pattern "soccer" [point "a team sport involving kicking a ball"]

-- Integer values
intPattern = point 42
intPatternWith = pattern 100 [point 10, point 20]

-- Custom types
data Person = Person { name :: String, age :: Maybe Int }
personPattern = point (Person "Alice" (Just 30))
-- Role-based singular pattern: "The goalie" is "Hans"
personPatternWith = pattern (Person "Goalie" Nothing) 
  [ point (Person "Hans" (Just 25)) ]
```

## Common Patterns

### Creating a Simple Relationship

```haskell
-- Two atomic patterns
personA = point "Person A"
personB = point "Person B"

-- Relationship between them
relationship = pattern "knows" [personA, personB]
```

### Creating a Graph with Multiple Relationships

```haskell
-- Atomic patterns
alice = point "Alice"
bob = point "Bob"
charlie = point "Charlie"

-- Relationships
aliceKnowsBob = pattern "knows" [alice, bob]
bobKnowsCharlie = pattern "knows" [bob, charlie]

-- Graph containing all nodes and relationships
socialGraph = pattern "socialNetwork"
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

-- Using pattern with manual point calls (verbose)
p2 = pattern "graph" [point "Alice", point "Bob", point "Charlie"]

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
atomic' = point "empty"

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

1. **Reduced verbosity**: `point "test"` vs `Pattern { value = "test", elements = [] }`
2. **Improved readability**: Function names clearly indicate purpose
3. **Consistency**: Same API for all pattern creation
4. **Type safety**: Full type checking preserved
5. **Functional equivalence**: Identical behavior to record syntax
6. **List conversion**: `fromList` makes it easy to create patterns from raw data

## Next Steps

- See [data-model.md](./data-model.md) for detailed function specifications
- See [contracts/type-signatures.md](./contracts/type-signatures.md) for complete API documentation
- See [spec.md](./spec.md) for feature requirements and acceptance criteria

