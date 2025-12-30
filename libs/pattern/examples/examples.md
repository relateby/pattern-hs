# Idiomatic Examples: Pattern Library

**Feature**: 003-pattern-structure-review  
**Date**: 2025-01-27  
**Status**: Comprehensive Examples Collection

This document provides idiomatic examples demonstrating correct usage of the Pattern library with consistent terminology. All examples use the standard terminology: "value" (not "metadata"), "elements" (not "children"), and "atomic pattern" (not "node" when referring to structure).

---

## Table of Contents

1. [Creating Atomic Patterns](#creating-atomic-patterns)
2. [Creating Patterns with Constructor Functions](#creating-patterns-with-constructor-functions)
3. [Creating Patterns with Elements](#creating-patterns-with-elements)
4. [Accessing Pattern Values and Elements](#accessing-pattern-values-and-elements)
5. [Nested Patterns and Sequences](#nested-patterns-and-sequences)
6. [Sequence-Based Conceptual Model](#sequence-based-conceptual-model)

---

## Creating Atomic Patterns

An **atomic pattern** is a pattern with no elements (`elements == []`). Atomic patterns are the fundamental building blocks from which all other patterns are constructed. The value field provides decoration about what kind of pattern it is.

### Atomic Patterns with String Values

```haskell
import Pattern.Core (Pattern(..))

-- Create an atomic pattern with string value
atomicString :: Pattern String
atomicString = Pattern { value = "atom1", elements = [] }

-- The value field stores the decoration
value atomicString  -- "atom1"

-- The elements field is empty (this IS the pattern - an empty sequence)
elements atomicString  -- []
```

**Gram notation:**
```gram
["atom1"]
```

### Atomic Patterns with Integer Values

```haskell
-- Create an atomic pattern with integer value
atomicInt :: Pattern Int
atomicInt = Pattern { value = 42, elements = [] }

value atomicInt  -- 42
elements atomicInt  -- []
```

**Gram notation:**
```gram
[42]
```

### Atomic Patterns with Custom Types

```haskell
-- Define a custom type
data Person = Person { name :: String, age :: Maybe Int }
  deriving (Eq, Show)

-- Create an atomic pattern with custom type value
atomicPerson :: Pattern Person
atomicPerson = Pattern { value = Person "Alice" (Just 30), elements = [] }

value atomicPerson  -- Person {name = "Alice", age = Just 30}
elements atomicPerson  -- []
```

**Gram notation:**
```gram
[:Person {name: "Alice", age: 30}]
```

### Multiple Atomic Patterns

```haskell
-- Create multiple atomic patterns with the same value type
atomA :: Pattern String
atomA = Pattern { value = "A", elements = [] }

atomB :: Pattern String
atomB = Pattern { value = "B", elements = [] }

atomC :: Pattern String
atomC = Pattern { value = "C", elements = [] }

-- All share the same value type (String)
-- All have empty elements (empty sequences)
```

**Gram notation:**
```gram
["A"]
["B"]
["C"]
```

### Atomic Patterns with Different Value Types

```haskell
-- Atomic patterns can have different value types
stringPattern :: Pattern String
stringPattern = Pattern { value = "text", elements = [] }

intPattern :: Pattern Int
intPattern = Pattern { value = 123, elements = [] }

-- Type system ensures they cannot be mixed
-- stringPattern and intPattern are different types
```

**Gram notation:**
```gram
["text"]
[123]
```

---

## Creating Patterns with Constructor Functions

The Pattern library provides convenient constructor functions that make it easier to create patterns without verbose record syntax.

### Using `point` for Atomic Patterns

```haskell
import Pattern.Core (point)

-- Create atomic patterns using the point function
atom1 :: Pattern String
atom1 = point "atom1"

atom2 :: Pattern Int
atom2 = point 42

-- Custom types
data Person = Person { name :: String, age :: Maybe Int }
  deriving (Eq, Show)

personAtom :: Pattern Person
personAtom = point (Person "Alice" (Just 30))

-- Equivalent to: Pattern { value = "atom1", elements = [] }
-- Equivalent to: Pattern { value = 42, elements = [] }
-- Equivalent to: Pattern { value = Person "Alice" (Just 30), elements = [] }
```

### Using `pattern` for Patterns with Elements

```haskell
import Pattern.Core (pattern, point)

-- Create singular pattern (one element)
singular :: Pattern String
singular = pattern "soccer" [point "a team sport involving kicking a ball"]

-- Create pair pattern (two elements)
pair :: Pattern String
pair = pattern "knows" [point "Alice", point "Bob"]

-- Create extended pattern (many elements)
extended :: Pattern String
extended = pattern "graph" 
  [ point "elem1"
  , point "elem2"
  , point "elem3"
  ]

-- Role-based singular pattern with custom type
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
```

### Using `fromList` for Patterns from Lists

```haskell
import Pattern.Core (fromList)

-- Create pattern from list of strings
wordsPattern :: Pattern String
wordsPattern = fromList "words" ["hello", "world", "haskell"]

-- Create pattern from list of integers
numbersPattern :: Pattern Int
numbersPattern = fromList (0 :: Int) [1, 2, 3, 4, 5]

-- Create pattern from list of custom types
data Person = Person { name :: String, age :: Maybe Int }
  deriving (Eq, Show)

teamPattern :: Pattern Person
teamPattern = fromList (Person "Team" Nothing) 
  [ Person "Alice" (Just 30)
  , Person "Bob" (Just 25)
  , Person "Charlie" (Just 35)
  ]

-- Equivalent to: pattern "words" (map point ["hello", "world", "haskell"])
```

---

## Creating Patterns with Elements

A pattern with **elements** contains one or more pattern elements in sequence. The elements form the pattern itself; the value provides decoration about that pattern.

### Singular Pattern

```haskell
-- Create an element (atomic pattern)
elem1 :: Pattern String
elem1 = Pattern { value = "elem1", elements = [] }

-- Create a singular pattern (exactly one element)
singularPattern :: Pattern String
singularPattern = Pattern { value = "pattern", elements = [elem1] }

value singularPattern  -- "pattern"
length (elements singularPattern)  -- 1
head (elements singularPattern)  -- Pattern {value = "elem1", elements = []}
```

**Gram notation:**
```gram
["pattern" | elem1]
```

### Pattern with Multiple Elements

```haskell
-- Create multiple elements
elem1 :: Pattern String
elem1 = Pattern { value = "elem1", elements = [] }

elem2 :: Pattern String
elem2 = Pattern { value = "elem2", elements = [] }

elem3 :: Pattern String
elem3 = Pattern { value = "elem3", elements = [] }

-- Create a pattern containing multiple elements
patternMany :: Pattern String
patternMany = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }

value patternMany  -- "pattern"
length (elements patternMany)  -- 3
elements patternMany  -- [elem1, elem2, elem3]
```

**Gram notation:**
```gram
["pattern" | elem1, elem2, elem3]
```

### Pattern with Two Elements (Pair Structure)

```haskell
-- Create two atomic patterns
atomA :: Pattern String
atomA = Pattern { value = "A", elements = [] }

atomB :: Pattern String
atomB = Pattern { value = "B", elements = [] }

-- Create a pattern with exactly 2 elements
-- This structure represents a pair of patterns
pairLike :: Pattern String
pairLike = Pattern { value = "knows", elements = [atomA, atomB] }

value pairLike  -- "knows"
length (elements pairLike)  -- 2
```

**Gram notation:**
```gram
["knows" | A, B]
```

### Pattern with Varying Numbers of Elements

```haskell
-- Zero elements (atomic pattern)
zeroElements :: Pattern String
zeroElements = Pattern { value = "zero", elements = [] }

-- Singular pattern (one element)
singularPattern :: Pattern String
singularPattern = Pattern { value = "one", elements = [Pattern { value = "elem", elements = [] }] }

-- Many elements
manyElements :: Pattern String
manyElements = Pattern 
  { value = "many"
  , elements = [ Pattern { value = "e1", elements = [] }
               , Pattern { value = "e2", elements = [] }
               , Pattern { value = "e3", elements = [] }
               ]
  }

length (elements zeroElements)  -- 0
length (elements singularPattern)   -- 1
length (elements manyElements)  -- 3
```

**Gram notation:**
```gram
["zero"]
["one" | elem]
["many" | e1, e2, e3]
```

---

## Accessing Pattern Values and Elements

Patterns provide direct access to their value and elements fields. The value is decoration about the pattern; the elements are the pattern itself.

### Accessing the Value Field

```haskell
-- Access value from atomic pattern
atomicPattern :: Pattern String
atomicPattern = Pattern { value = "test", elements = [] }

value atomicPattern  -- "test"

-- Access value from pattern with elements
patternWithElements :: Pattern String
patternWithElements = Pattern 
  { value = "pattern"
  , elements = [Pattern { value = "elem1", elements = [] }]
  }

value patternWithElements  -- "pattern"
```

**Gram notation:**
```gram
["test"]
["pattern" | elem1]
```

### Accessing the Elements Field

```haskell
-- Access elements from atomic pattern
atomicPattern :: Pattern String
atomicPattern = Pattern { value = "atom", elements = [] }

elements atomicPattern  -- []

-- Access elements from pattern with elements
patternWithElements :: Pattern String
patternWithElements = Pattern 
  { value = "pattern"
  , elements = [ Pattern { value = "elem1", elements = [] }
               , Pattern { value = "elem2", elements = [] }
               ]
  }

elements patternWithElements  -- [Pattern {value = "elem1", elements = []}, ...]
length (elements patternWithElements)  -- 2
```

**Gram notation:**
```gram
["atom"]
["pattern" | elem1, elem2]
```

### Accessing Individual Elements

```haskell
-- Create a pattern with multiple elements
elem1 :: Pattern String
elem1 = Pattern { value = "first", elements = [] }

elem2 :: Pattern String
elem2 = Pattern { value = "second", elements = [] }

elem3 :: Pattern String
elem3 = Pattern { value = "third", elements = [] }

pattern :: Pattern String
pattern = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }

-- Access elements by position
let elems = elements pattern
head elems  -- Pattern {value = "first", elements = []}
elems !! 1  -- Pattern {value = "second", elements = []}
last elems  -- Pattern {value = "third", elements = []}

-- Access values of elements
map value elems  -- ["first", "second", "third"]
```

**Gram notation:**
```gram
["pattern" | first, second, third]
```

### Working with Element Values

```haskell
-- Extract all values from elements
pattern :: Pattern String
pattern = Pattern 
  { value = "pattern"
  , elements = [ Pattern { value = "a", elements = [] }
               , Pattern { value = "b", elements = [] }
               , Pattern { value = "c", elements = [] }
               ]
  }

-- Get all element values
let elems = elements pattern
map value elems  -- ["a", "b", "c"]

-- Check if pattern has elements
null (elements pattern)  -- False
length (elements pattern) > 0  -- True
```

**Gram notation:**
```gram
["pattern" | a, b, c]
```

---

## Nested Patterns and Sequences

Patterns can contain patterns that themselves contain patterns, enabling arbitrary nesting depth. This recursive structure supports nested sequences while maintaining the sequence semantic.

### Two-Level Nesting

```haskell
-- Create an inner pattern (empty)
inner :: Pattern String
inner = Pattern { value = "inner", elements = [] }

-- Create a middle pattern containing the inner pattern
middle :: Pattern String
middle = Pattern { value = "middle", elements = [inner] }

-- Create an outer pattern containing the middle pattern
outer :: Pattern String
outer = Pattern { value = "outer", elements = [middle] }

-- Access nested structure
value outer  -- "outer"
value (head (elements outer))  -- "middle"
value (head (elements (head (elements outer))))  -- "inner"
```

**Gram notation:**
```gram
["outer" | ["middle" | ["inner"]]]
```

### Three-Level Nesting

```haskell
-- Level 3: innermost pattern
level3 :: Pattern String
level3 = Pattern { value = "level3", elements = [] }

-- Level 2: contains level3
level2 :: Pattern String
level2 = Pattern { value = "level2", elements = [level3] }

-- Level 1: contains level2
level1 :: Pattern String
level1 = Pattern { value = "level1", elements = [level2] }

-- Root: contains level1
root :: Pattern String
root = Pattern { value = "root", elements = [level1] }

-- Traverse nested structure
value root  -- "root"
let rootElems = elements root
value (head rootElems)  -- "level1"
let level1Elems = elements (head rootElems)
value (head level1Elems)  -- "level2"
let level2Elems = elements (head level1Elems)
value (head level2Elems)  -- "level3"
```

**Gram notation:**
```gram
["root" | ["level1" | ["level2" | ["level3"]]]]
```

### Nested Patterns with Multiple Elements

```haskell
-- Create multiple inner patterns
inner1 :: Pattern String
inner1 = Pattern { value = "inner1", elements = [] }

inner2 :: Pattern String
inner2 = Pattern { value = "inner2", elements = [] }

-- Create a middle pattern containing multiple inner patterns
middle :: Pattern String
middle = Pattern { value = "middle", elements = [inner1, inner2] }

-- Create an outer pattern containing the middle pattern
outer :: Pattern String
outer = Pattern { value = "outer", elements = [middle] }

-- Access nested elements
let outerElems = elements outer
length outerElems  -- 1 (one middle element)

let middleElems = elements (head outerElems)
length middleElems  -- 2 (two inner elements)
map value middleElems  -- ["inner1", "inner2"]
```

**Gram notation:**
```gram
["outer" | ["middle" | inner1, inner2]]
```

### Complex Nested Structure

```haskell
-- Build a complex nested pattern structure
-- This demonstrates how patterns can represent hierarchical graph structures

-- Atomic patterns
atomA :: Pattern String
atomA = Pattern { value = "A", elements = [] }

atomB :: Pattern String
atomB = Pattern { value = "B", elements = [] }

atomC :: Pattern String
atomC = Pattern { value = "C", elements = [] }

-- Pair patterns (patterns with 2 elements)
pairAB :: Pattern String
pairAB = Pattern { value = "knows", elements = [atomA, atomB] }

pairBC :: Pattern String
pairBC = Pattern { value = "knows", elements = [atomB, atomC] }

-- Pattern containing multiple pair patterns
groupPattern :: Pattern String
groupPattern = Pattern { value = "social", elements = [pairAB, pairBC] }

-- Access the nested structure
value groupPattern  -- "social"
length (elements groupPattern)  -- 2 (two pairs)

let firstPair = head (elements groupPattern)
value firstPair  -- "knows"
length (elements firstPair)  -- 2 (two atoms)

let firstAtom = head (elements firstPair)
value firstAtom  -- "A"
elements firstAtom  -- [] (atomic pattern)
```

**Gram notation:**
```gram
["social" | ["knows" | A, B], ["knows" | B, C]]
```

---

## Sequence-Based Conceptual Model

Patterns are conceptually **decorated sequences**: the elements form the pattern itself, and the value provides decoration about that pattern. While implemented as recursive trees, the primary semantic is that elements form the pattern sequence itself.

### Understanding the Sequence Semantic

```haskell
-- The pattern "A B B A" with decoration "Enclosed rhyme"
-- represents a sequence pattern where the elements ARE the pattern

-- Create the sequence elements
elemA1 :: Pattern String
elemA1 = Pattern { value = "A", elements = [] }

elemB1 :: Pattern String
elemB1 = Pattern { value = "B", elements = [] }

elemB2 :: Pattern String
elemB2 = Pattern { value = "B", elements = [] }

elemA2 :: Pattern String
elemA2 = Pattern { value = "A", elements = [] }

-- The pattern is the sequence [A, B, B, A]
-- The value "Enclosed rhyme" is decoration about that pattern
enclosedRhyme :: Pattern String
enclosedRhyme = Pattern 
  { value = "Enclosed rhyme"
  , elements = [elemA1, elemB1, elemB2, elemA2]  -- This IS the pattern
  }

-- The elements form the pattern sequence
map value (elements enclosedRhyme)  -- ["A", "B", "B", "A"]

-- The value describes what kind of pattern it is
value enclosedRhyme  -- "Enclosed rhyme"
```

**Gram notation:**
```gram
["Enclosed rhyme" | A, B, B, A]
```

### Sequence Order is Essential

```haskell
-- The order of elements matters - it's part of the pattern itself

-- Pattern "A B"
patternAB :: Pattern String
patternAB = Pattern 
  { value = "pattern"
  , elements = [ Pattern { value = "A", elements = [] }
               , Pattern { value = "B", elements = [] }
               ]
  }

-- Pattern "B A" (different order, different pattern)
patternBA :: Pattern String
patternBA = Pattern 
  { value = "pattern"
  , elements = [ Pattern { value = "B", elements = [] }
               , Pattern { value = "A", elements = [] }
               ]
  }

-- These are different patterns because element order matters
patternAB /= patternBA  -- True (different sequences)

-- Access elements in order
map value (elements patternAB)  -- ["A", "B"]
map value (elements patternBA)  -- ["B", "A"]
```

**Gram notation:**
```gram
["pattern" | A, B]
["pattern" | B, A]
```

### Sequence Operations

```haskell
-- Patterns support sequence operations because elements form the sequence

pattern :: Pattern String
pattern = Pattern 
  { value = "pattern"
  , elements = [ Pattern { value = "a", elements = [] }
               , Pattern { value = "b", elements = [] }
               , Pattern { value = "c", elements = [] }
               ]
  }

-- Length of sequence (number of elements)
length (elements pattern)  -- 3

-- Check if sequence is empty
null (elements pattern)  -- False

-- Access by position
elements pattern !! 0  -- First element
elements pattern !! 1  -- Second element
elements pattern !! 2  -- Third element

-- Iterate over sequence
map value (elements pattern)  -- ["a", "b", "c"]
```

**Gram notation:**
```gram
["pattern" | a, b, c]
```

### Tree Implementation Supports Sequence Semantic

```haskell
-- While implemented as a tree, the tree supports sequence operations

-- Create a nested pattern
inner :: Pattern String
inner = Pattern { value = "inner", elements = [] }

middle :: Pattern String
middle = Pattern { value = "middle", elements = [inner] }

outer :: Pattern String
outer = Pattern { value = "outer", elements = [middle] }

-- Tree traversal preserves sequence order
-- The tree structure stores sequences (lists) at each level
let outerElems = elements outer  -- Sequence at outer level
length outerElems  -- 1

let middleElems = elements (head outerElems)  -- Sequence at middle level
length middleElems  -- 1

let innerElems = elements (head middleElems)  -- Sequence at inner level
length innerElems  -- 0 (empty sequence)

-- The tree implementation enables:
-- - Sequence operations (length, indexing, iteration)
-- - Nested sequences (patterns containing patterns)
-- - Recursive traversal while preserving order
```

**Gram notation:**
```gram
["outer" | ["middle" | ["inner"]]]
```

### Conceptual Model vs Implementation

```haskell
-- Conceptually: Pattern is a decorated sequence
-- - elements form the pattern sequence itself
-- - value provides decoration about that pattern
-- - sequence order is essential

-- Implementation: Pattern is a recursive tree
-- - tree levels store sequences (lists) of pattern elements
-- - tree traversal preserves sequence order
-- - recursive structure enables nested sequences

-- Example: Pattern "A B" with decoration "pair"
elemA :: Pattern String
elemA = Pattern { value = "A", elements = [] }

elemB :: Pattern String
elemB = Pattern { value = "B", elements = [] }

pairPattern :: Pattern String
pairPattern = Pattern { value = "pair", elements = [elemA, elemB] }

-- Conceptual view: sequence [A, B] decorated with "pair"
-- Implementation view: tree level with value "pair" containing list [elemA, elemB]
-- Both views are correct - the tree is how the sequence is represented
```

**Gram notation:**
```gram
["pair" | A, B]
```

---

## Effectful Traversal with Traversable

The Pattern type has a Traversable instance that enables effectful operations while preserving pattern structure. This allows you to validate values, handle errors, perform IO operations, and work with stateful computations.

### Traversing with Identity

```haskell
import Pattern
import Data.Functor.Identity (Identity(..))

-- Traverse with Identity (no effects, preserves structure)
let p = pattern "root" [point "a", point "b"]
    result = traverse Identity p
    p' = runIdentity result
-- p' = pattern "root" [point "a", point "b"]
-- Structure is preserved exactly
```

### Validating with Maybe

```haskell
import Pattern
import Data.Maybe (Maybe(..))

-- Validate all values are positive
let validate x = if x > 0 then Just x else Nothing
    p = pattern 10 [point 5, point 3]
    result = traverse validate p
-- result = Just (pattern 10 [point 5, point 3])

-- If any value is invalid, returns Nothing
let p = pattern 10 [point 5, point (-3)]
    result = traverse validate p
-- result = Nothing
```

### Validating with Either

```haskell
import Pattern
import Data.Either (Either(..))

-- Validate with error messages
let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
    p = pattern 10 [point 5, point 3]
    result = traverse validate p
-- result = Right (pattern 10 [point 5, point 3])

-- Returns first error encountered
let p = pattern 10 [point 5, point (-3)]
    result = traverse validate p
-- result = Left "Invalid: -3"
```

### Sequencing Applicative Effects

```haskell
import Pattern
import Data.Maybe (Maybe(..))

-- Sequence Maybe values
let p = pattern (Just 10) [Just 5, Just 3]
    result = sequenceA p
-- result = Just (pattern 10 [point 5, point 3])

-- Short-circuits on first Nothing
let p = pattern (Just 10) [Just 5, Nothing]
    result = sequenceA p
-- result = Nothing
```

### Validating Nested Patterns

```haskell
import Pattern

-- Validation works recursively on nested structures
let validate x = if x > 0 then Just x else Nothing
    inner = point 1
    middle = pattern 2 [inner]
    outer = pattern 3 [middle]
    p = pattern 4 [outer]
    result = traverse validate p
-- result = Just (pattern 4 [pattern 3 [pattern 2 [point 1]]])

-- Fails if any value at any level is invalid
let inner = point (-1)
    middle = pattern 2 [inner]
    p = pattern 4 [middle]
    result = traverse validate p
-- result = Nothing
```

---

## Summary

These examples demonstrate:

1. **Atomic Patterns**: Patterns with no elements, where the value provides decoration. Atomic patterns are the fundamental building blocks from which all other patterns are constructed.
2. **Patterns with Elements**: Patterns containing one or more pattern elements in sequence
3. **Accessing Values and Elements**: Direct access to pattern decoration and sequence elements
4. **Nested Patterns**: Recursive structures enabling arbitrary nesting depth
5. **Sequence-Based Model**: Understanding patterns as decorated sequences where elements form the pattern itself
6. **Effectful Traversal**: Using Traversable to validate values, handle errors, and perform effectful operations while preserving pattern structure

All examples use consistent terminology:
- **value**: Decoration about what kind of pattern it is
- **elements**: The pattern itself, represented as a sequence
- **atomic pattern**: A pattern with no elements (`elements == []`). Atomic patterns are the fundamental building blocks from which all other patterns are constructed.

The sequence semantic is primary; the tree structure is the implementation detail that supports sequence operations.

