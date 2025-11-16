# Idiomatic Examples: Subject Library

**Feature**: Subject Library  
**Date**: 2025-01-28  
**Status**: Comprehensive Examples Collection

This document provides idiomatic examples demonstrating correct usage of the Subject library with gram notation mappings. All examples show how subjects represent self-descriptive objects with identity, labels, and property records.

---

## Table of Contents

1. [Creating Subjects](#creating-subjects)
2. [Subject Components](#subject-components)
3. [Property Records](#property-records)
4. [Value Types](#value-types)
5. [Gram Notation Mapping](#gram-notation-mapping)
6. [Integration with Pattern](#integration-with-pattern)

---

## Creating Subjects

A **Subject** is a self-descriptive object with three components: identity (required), labels (set), and properties (key-value map). Labels are a set (no duplicates, order doesn't matter) as per gram notation.

**Note**: While gram notation allows anonymous (unidentified) subjects, the Subject data type requires identity to be mandatory. When implementing serialization, we'll need to decide how to handle assigning identity to anonymous subjects during serialization.

### Empty Subject

An empty subject has default identity (Symbol ""), no labels, and no properties:

```haskell
import Subject.Core (Subject(..), Symbol(..))
import Data.Map (empty)
import Data.Set (Set)
import qualified Data.Set as Set

-- Create empty subject
emptySubj :: Subject
emptySubj = Subject (Symbol "") Set.empty empty

-- Or using constructor function
import Subject.Construction (subject)
emptySubj' = subject
```

**Gram notation:**
```gram
()  -- Empty attributes (anonymous in gram, but has default identity in Subject)
```

### Subject with Identity Only

```haskell
import Subject.Core (Subject(..), Symbol(..))
import Data.Map (empty)
import Data.Set (Set)
import qualified Data.Set as Set

-- Subject with symbol identity
subjWithId :: Subject
subjWithId = Subject (Symbol "n") Set.empty empty
```

**Gram notation:**
```gram
(n)
```

### Subject with Labels Only

```haskell
import Subject.Core (Subject(..), Symbol(..))
import Data.Map (empty)
import Data.Set (Set)
import qualified Data.Set as Set

-- Subject with single label
subjWithLabel :: Subject
subjWithLabel = Subject (Symbol "n") (Set.fromList ["Person"]) empty

-- Subject with multiple labels
subjWithLabels :: Subject
subjWithLabels = Subject (Symbol "n") (Set.fromList ["Person", "Employee"]) empty
```

**Gram notation:**
```gram
(:Person)
(::Person::Employee)
```

### Subject with Properties Only

```haskell
import Subject.Core (Subject(..))
import Data.Map (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Subject.Value (VString, VInteger)

-- Subject with properties
subjWithProps :: Subject
subjWithProps = Subject (Symbol "n") Set.empty (fromList [("name", VString "Alice"), ("age", VInteger 30)])
```

**Gram notation:**
```gram
({name:"Alice", age:30})
```

### Subject with All Components

```haskell
import Subject.Core (Subject(..), Symbol(..))
import Data.Map (fromList)
import Subject.Value (VString, VInteger)

-- Subject with identity, labels, and properties
fullSubj :: Subject
fullSubj = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice"), ("age", VInteger 30)])

-- Or using constructor function
import Subject.Construction (subjectWith)
fullSubj' = subjectWith (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice"), ("age", VInteger 30)])
```

**Gram notation:**
```gram
(n:Person {name:"Alice", age:30})
```

---

## Subject Components

### Identity

The identity component is a required symbol identifier. The serialization layer will handle converting symbols to the appropriate gram notation syntax (unquoted symbols, quoted strings, or numbers):

```haskell
import Subject.Core (Symbol(..))

-- Symbol identity
symbolId :: Symbol
symbolId = Symbol "n"

-- Symbol from string (can represent quoted strings)
stringId :: Symbol
stringId = Symbol "myId"

-- Symbol from number string (can represent numeric identifiers)
intId :: Symbol
intId = Symbol "42"

-- Subject with symbol identity
subj1 = Subject symbolId Set.empty empty

-- Subject with string identity
subj2 = Subject stringId Set.empty empty

-- Subject with integer identity
subj3 = Subject intId Set.empty empty
```

### Labels

Labels are a set of strings that categorize the subject (no duplicates, order doesn't matter):

```haskell
import Subject.Core (Subject(..))
import Data.Map (empty)
import Data.Set (Set)
import qualified Data.Set as Set

-- Single label
singleLabel = Subject (Symbol "n") (Set.fromList ["Person"]) empty

-- Multiple labels
multipleLabels = Subject (Symbol "n") (Set.fromList ["Person", "Employee", "Manager"]) empty

-- No labels
noLabels = Subject (Symbol "n") Set.empty empty
```

### Properties

Properties are a key-value map where values can be any Value type:

```haskell
import Subject.Core (Subject(..))
import Data.Map (fromList, empty)
import Subject.Value (VString, VInteger, VBoolean)

-- Properties with standard types
props = fromList
  [ ("name", VString "Alice")
  , ("age", VInteger 30)
  , ("active", VBoolean True)
  ]

subjWithProps = Subject (Symbol "n") Set.empty props
```

---

## Property Records

### Adding Properties

```haskell
import Subject.Construction (subject, addProperty)
import Subject.Value (VString, VInteger)

-- Start with empty subject
s = subject

-- Add properties one by one
s1 = addProperty "name" (VString "Alice") s
s2 = addProperty "age" (VInteger 30) s1
s3 = addProperty "city" (VString "NYC") s2
```

### Updating Properties

```haskell
import Subject.Construction (subjectWith, updateProperty)
import Data.Map (fromList)
import Subject.Value (VString)

-- Subject with existing property
s = subjectWith (Symbol "n") Set.empty (fromList [("name", VString "Bob")])

-- Update the property
s' = updateProperty "name" (VString "Alice") s
```

### Removing Properties

```haskell
import Subject.Construction (subjectWith, removeProperty)
import Data.Map (fromList)
import Subject.Value (VString, VInteger)

-- Subject with multiple properties
s = subjectWith (Symbol "n") Set.empty (fromList [("name", VString "Alice"), ("age", VInteger 30)])

-- Remove a property
s' = removeProperty "age" s
```

### Checking Properties

```haskell
import Subject.Construction (subjectWith, hasProperty)
import Data.Map (fromList)
import Subject.Value (VString)

-- Subject with property
s = subjectWith (Symbol "n") Set.empty (fromList [("name", VString "Alice")])

-- Check if property exists
hasProperty "name" s  -- True
hasProperty "age" s   -- False
```

---

## Value Types

### Standard Types

```haskell
import Subject.Value (Value(..))

-- Integer
intVal :: Value
intVal = VInteger 42

-- Decimal
decVal :: Value
decVal = VDecimal 3.14

-- Boolean
boolVal :: Value
boolVal = VBoolean True

-- String
strVal :: Value
strVal = VString "hello"

-- Symbol
symVal :: Value
symVal = VSymbol "mySymbol"
```

### Extended Types

```haskell
import Subject.Value (Value(..), RangeValue(..))
import Data.Map (fromList)

-- Tagged string
taggedVal :: Value
taggedVal = VTaggedString "url" "https://example.com"

-- Array
arrayVal :: Value
arrayVal = VArray [VInteger 1, VInteger 2, VInteger 3]

-- Map
mapVal :: Value
mapVal = VMap (fromList [("key1", VString "value1"), ("key2", VInteger 42)])

-- Range
rangeVal :: Value
rangeVal = VRange (RangeValue (Just 1) (Just 10))

-- Measurement
measureVal :: Value
measureVal = VMeasurement "kg" 5.0
```

### Nested Values

```haskell
import Subject.Value (Value(..))
import Data.Map (fromList)

-- Array of arrays
nestedArray :: Value
nestedArray = VArray [VArray [VInteger 1, VInteger 2], VArray [VInteger 3, VInteger 4]]

-- Map with array values
mapWithArray :: Value
mapWithArray = VMap (fromList [("numbers", VArray [VInteger 1, VInteger 2, VInteger 3])])

-- Map with map values
mapWithMap :: Value
mapWithMap = VMap (fromList [("nested", VMap (fromList [("key", VString "value")]))])
```

---

## Gram Notation Mapping

### Node Pattern

```haskell
import Subject.Core (Subject(..), Symbol(..))
import Data.Map (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Subject.Value (VString)

-- Gram: (n:Person {name:"ABK"})
nodeSubj :: Subject
nodeSubj = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "ABK")])
```

### Relationship Pattern

```haskell
import Subject.Core (Subject(..), Symbol(..))
import Data.Map (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Subject.Value (VInteger)

-- Gram: (a)-[r:KNOWS {since: 2024}]->(b)
relSubj :: Subject
relSubj = Subject (Symbol "r") (Set.fromList ["KNOWS"]) (fromList [("since", VInteger 2024)])
```

### Pattern Subject

```haskell
import Subject.Core (Subject(..), Symbol(..))
import Data.Map (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Subject.Value (VString)

-- Gram: [pat:Pattern {k:"v"}]
patternSubj :: Subject
patternSubj = Subject (Symbol "pat") (Set.fromList ["Pattern"]) (fromList [("k", VString "v")])
```

### Complex Example

```haskell
import Subject.Core (Subject(..), Symbol(..))
import Data.Map (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Subject.Value (VString, VInteger, VArray, VTaggedString)

-- Gram: (user:Person {name:"Alice", tags:["admin", "user"], email:url`alice@example.com`})
complexSubj :: Subject
complexSubj = Subject
  (Symbol "user")
  (Set.fromList ["Person"])
  (fromList
    [ ("name", VString "Alice")
    , ("tags", VArray [VString "admin", VString "user"])
    , ("email", VTaggedString "url" "alice@example.com")
    ])
```

---

## Integration with Pattern

Subject is designed to work as the value type for Pattern:

```haskell
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import Data.Map (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Subject.Value (VString, VInteger)

-- Create a subject
subj :: Subject
subj = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])

-- Use subject as Pattern value
patternWithSubject :: Pattern Subject
patternWithSubject = Pattern { value = subj, elements = [] }

-- Access subject from pattern
value patternWithSubject  -- Returns the Subject

-- Create pattern with subject elements
subj1 = Subject (Symbol "a") (Set.fromList ["Person"]) empty
subj2 = Subject (Symbol "b") (Set.fromList ["Person"]) empty
patternSubj1 = Pattern { value = subj1, elements = [] }
patternSubj2 = Pattern { value = subj2, elements = [] }
relationshipPattern = Pattern { value = subj, elements = [patternSubj1, patternSubj2] }
```

This enables patterns to contain subjects as their decoration values, creating a powerful combination where patterns provide structure and subjects provide self-descriptive content.

---

## Typeclass Usage

### Semigroup

```haskell
import Data.Semigroup ((<>))
import Subject.Core (Subject(..), Symbol(..))
import Data.Map (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Subject.Value (VString, VInteger)

-- Combine subjects
s1 = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
s2 = Subject (Symbol "m") (Set.fromList ["Employee"]) (fromList [("age", VInteger 30)])

combined = s1 <> s2
-- Result: Subject with identity "n", labels fromList ["Person", "Employee"],
--         and properties {name:"Alice", age:30}
```

### Monoid

```haskell
import Data.Monoid (mempty, mconcat)
import Subject.Core (Subject(..))
import Data.Map (empty)
import Data.Set (Set)
import qualified Data.Set as Set

-- Empty subject
emptySubj = mempty :: Subject

-- Combine multiple subjects
s1 = Subject (Symbol "a") (Set.fromList ["A"]) empty
s2 = Subject (Symbol "b") (Set.fromList ["B"]) empty
s3 = Subject (Symbol "c") (Set.fromList ["C"]) empty

combined = mconcat [s1, s2, s3]
-- Result: Subject with labels fromList ["A", "B", "C"]
```

### Hashable

```haskell
import Data.Hashable (hash)
import qualified Data.HashSet as HashSet
import Subject.Core (Subject(..), Symbol(..))
import Data.Map (empty)
import Data.Set (Set)
import qualified Data.Set as Set

-- Use subjects in hash sets
s1 = Subject (Symbol "n") (Set.fromList ["Person"]) empty
s2 = Subject (Symbol "m") (Set.fromList ["Person"]) empty

set = HashSet.fromList [s1, s2]
```

---

## Best Practices

1. **Use constructor functions** for cleaner code:
   ```haskell
   -- Prefer this
   s = subjectWith (Symbol "n") (Set.fromList ["Person"]) props
   
   -- Over this
   s = Subject (Symbol "n") (Set.fromList ["Person"]) props
   ```

2. **Build subjects incrementally**:
   ```haskell
   s = subject
     & addProperty "name" (VString "Alice")
     & addProperty "age" (VInteger 30)
   ```

3. **Use appropriate value types** for different data:
   - Use `VTaggedString` for qualified strings (URLs, emails, etc.)
   - Use `VArray` for lists of values
   - Use `VMap` for nested key-value structures
   - Use `VRange` for numeric ranges

4. **Leverage typeclass instances**:
   - Use `<>` to combine subjects
   - Use `mconcat` to combine multiple subjects
   - Use subjects as keys in hash maps/sets

