# Data Model: Basic Pattern Type

**Feature**: 002-basic-pattern-type  
**Date**: 2025-01-27

## Core Entity: Pattern

### Definition

A **Pattern** is a **decorated sequence**: the elements form the pattern itself, and the value provides decoration (metadata) about that pattern. For example, the pattern "A B B A" with decoration "Enclosed rhyme" represents a specific sequence pattern (A B B A) that is classified as an "Enclosed rhyme". The elements ARE the pattern; the value describes what kind of pattern it is.

While implemented using a recursive tree structure, the primary semantic is that elements form the pattern sequence itself, not that they are children of a node. The tree structure is an implementation detail that supports the sequence representation.

### Structure

```haskell
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  }
  deriving (Eq)
```

**Note**: `Show` is implemented as a manual instance. `Functor`, `Foldable`, and `Traversable` are planned but not yet implemented (see Implementation Status section).

### Fields

- **value** (`v`): The decoration or metadata associated with the pattern. The value field stores data of any type that describes or classifies the pattern sequence. For example, "Enclosed rhyme" describes the pattern "A B B A". The value is decoration about the pattern, not part of the pattern itself. Type parameter `v` allows for different value types.

- **elements** (`[Pattern v]`): The pattern itself, represented as a sequence of elements. The elements ARE the pattern; they are not children or subordinate to the value. Each element in the sequence is itself a Pattern, enabling recursive nesting. An empty list `[]` represents a pattern with no elements (an empty sequence). A non-empty list represents a pattern containing one or more pattern elements in sequence.

### Conceptual Model: Decorated Sequences

**Primary Semantic**: Patterns are decorated sequences where the elements form the pattern itself.

- The `elements` field IS the pattern - it contains the sequence that defines the pattern
- The `value` field provides decoration/metadata about what kind of pattern it is
- Elements maintain their sequence order - this order is essential to the pattern
- Each element in the sequence is itself a Pattern, enabling nested patterns

**Example**: The pattern "A B B A" with decoration "Enclosed rhyme" represents:
- Pattern: the sequence `[A, B, B, A]` (the elements)
- Decoration: "Enclosed rhyme" (the value describing what kind of pattern this is)

The pattern itself is the sequence; the value is metadata about that pattern.

### Implementation Model: Recursive Tree

**Implementation Detail**: Patterns are implemented as recursive trees, but this is purely an implementation detail.

- The tree structure is how sequences are represented in memory
- Each tree node stores a decoration (value) and contains the pattern elements
- The recursive structure enables arbitrary nesting depth
- Tree traversal provides access to sequence elements

**Relationship**: The tree implementation is how decorated sequences are represented in memory. Conceptually, developers should think of patterns as decorated sequences where elements form the pattern itself. The tree structure is an implementation detail that supports sequence operations (ordering, length, access by position).

### Type Constraints

- `Pattern` is parameterized over value type `v`
- All patterns in a structure must share the same value type `v` (enforced by type system)
- Pattern structures must be finite (no infinite recursion)
- Type consistency is enforced by Haskell's type system

### Relationships

- **Self-referential**: Each `Pattern` contains zero or more Pattern elements that form the pattern sequence
- **Recursive**: Pattern elements can themselves be patterns, enabling arbitrarily deep nesting (patterns containing patterns containing patterns, etc.)
- **Compositional**: Patterns can be composed by including other patterns as elements in the sequence

### Field Accessors

Haskell record syntax automatically provides field accessors:

- `value :: Pattern v -> v` - Extract the value from a pattern
- `elements :: Pattern v -> [Pattern v]` - Extract the list of element patterns

### Pattern Variants

Pattern variants are **structural classifications** based on their element structure that can be **interpreted through different graph views**. Variants are determined by the structure of elements, and views provide different semantic interpretations of those structures.

#### Empty Pattern

A pattern with no elements (`elements == []`).

**Structure**: Empty sequence
**Status**: ✅ Implemented (this is the basic Pattern structure)

**Example**:
```haskell
emptyPattern :: Pattern String
emptyPattern = Pattern { value = "node1", elements = [] }
```

#### Node

A pattern interpreted as a **node** when it has no elements that are graph elements themselves. Typically, this means `elements == []` (an empty pattern).

**Structure**: Empty sequence (empty pattern)
**Status**: ⏳ Planned (classification function `isNode` not yet implemented)

#### Relationship

A pattern interpreted as a **relationship** when it has exactly 2 elements, and both elements are nodes (empty patterns).

**Structure**: Exactly 2 elements, both are empty patterns
**Status**: ⏳ Planned (classification function `isRelationship` not yet implemented)

**Example** (structure exists, classification function planned):
```haskell
-- Node A
nodeA :: Pattern String
nodeA = Pattern { value = "A", elements = [] }

-- Node B
nodeB :: Pattern String
nodeB = Pattern { value = "B", elements = [] }

-- Relationship from A to B
relationship :: Pattern String
relationship = Pattern 
  { value = "knows"
  , elements = [nodeA, nodeB]
  }
```

#### Subgraph

A pattern interpreted as a **subgraph** when all elements are graph elements (nodes, relationships, or other subgraphs).

**Structure**: All elements are graph elements
**Status**: ⏳ Planned (classification function `isSubgraph` not yet implemented)

**Example** (structure exists, classification function planned):
```haskell
-- A graph containing nodes and relationships
graphPattern :: Pattern String
graphPattern = Pattern 
  { value = "myGraph"
  , elements = [ nodeA                    -- node
               , nodeB                    -- node
               , relationship             -- relationship
               ]
  }
```

This demonstrates how a Pattern can represent:
- **Nodes**: Empty patterns (empty `elements`)
- **Relationships**: Patterns with 2 elements that are empty patterns
- **Graphs**: Patterns whose `elements` contain a mix of node and relationship patterns

The recursive structure allows graphs to contain subgraphs, which are themselves Patterns containing graph elements.

### Validation Rules

1. **Well-formed**: All `Pattern` values must be finite (no infinite recursion)
2. **Type consistency**: All elements in a `Pattern` must have the same value type `v` (enforced by type system)
3. **Structure integrity**: Field accessors must always return valid values (no null/undefined values)

### State Transitions

#### Pattern Construction

- **Empty pattern creation**: `Pattern v []` - creates a pattern with value `v` and empty element list
- **Pattern with elements creation**: `Pattern v [p1, p2, ...]` - creates a pattern with value `v` and element patterns `p1, p2, ...`

#### Pattern Inspection

- **Value access**: `value pattern` - retrieves the value stored in the pattern
- **Elements access**: `elements pattern` - retrieves the list of element patterns

### Design Principles

1. **Decorated Sequence Semantics**: Patterns are conceptually decorated sequences where elements form the pattern itself; tree structure is implementation detail
2. **Simplicity**: The basic type is intentionally minimal - just value storage and element sequence
3. **Recursive**: The recursive structure enables representing complex nested pattern sequences
4. **Type-safe**: Type parameter ensures all patterns in a structure share the same value type
5. **Accessible**: Record syntax provides automatic, type-safe field accessors

### Future Extensions

This basic type will be extended in future phases with:
- Typeclass instances (Functor, Foldable, Traversable)
- Classification functions (isNode, isRelationship, etc.)
- Navigation functions (source, target, nodes, relationships)
- Graph view interpretations

All of these will build upon this foundational Pattern type definition.

