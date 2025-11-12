# Data Model: Basic Pattern Type

**Feature**: 002-basic-pattern-type  
**Date**: 2025-01-27

## Core Entity: Pattern

### Definition

A **Pattern** is a **decorated sequence**: the elements form the pattern itself, and the value provides decoration about that pattern. For example, the pattern "A B B A" with decoration "Enclosed rhyme" represents a specific sequence pattern (A B B A) that is classified as an "Enclosed rhyme". The elements ARE the pattern; the value describes what kind of pattern it is.

While implemented using a recursive tree structure, the primary semantic is that elements form the pattern sequence itself. The tree structure is an implementation detail that supports the sequence representation.

### Structure

```haskell
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  }
  deriving (Eq)
```

**Note**: `Show` and `Eq` are implemented as manual instances. `Functor` is implemented (Feature 005). `Foldable` and `Traversable` are planned but not yet implemented (see Implementation Status section).

### Fields

- **value** (`v`): The decoration (value) associated with the pattern. The value field stores data of any type that describes or classifies the pattern sequence. For example, "Enclosed rhyme" describes the pattern "A B B A". The value is decoration about the pattern, not part of the pattern itself. Type parameter `v` allows for different value types.

- **elements** (`[Pattern v]`): The pattern itself, represented as a sequence of elements. The elements ARE the pattern; they are not subordinate to the value. Each element in the sequence is itself a Pattern, enabling recursive nesting. An empty list `[]` represents a pattern with no elements (an empty sequence). A non-empty list represents a pattern containing one or more pattern elements in sequence.

### Conceptual Model: Decorated Sequences

**Primary Semantic**: Patterns are decorated sequences where the elements form the pattern itself.

- The `elements` field IS the pattern - it contains the sequence that defines the pattern
- The `value` field provides decoration about what kind of pattern it is
- Elements maintain their sequence order - this order is essential to the pattern
- Each element in the sequence is itself a Pattern, enabling nested patterns

**Example**: The pattern "A B B A" with decoration "Enclosed rhyme" represents:
- Pattern: the sequence `[A, B, B, A]` (the elements)
- Decoration: "Enclosed rhyme" (the value describing what kind of pattern this is)

The pattern itself is the sequence; the value is decoration about that pattern.

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

### Pattern Structural Classifications

Pattern structural classifications describe what patterns **are** structurally, not how they are interpreted. These are based on the element structure of the pattern itself.

#### Atomic Pattern

A pattern with no elements (`elements == []`). Atomic patterns are the fundamental building blocks from which all other patterns are constructed.

**Structure**: Empty sequence  
**Status**: ✅ Implemented (this is the basic Pattern structure)

**Example**:
```haskell
atomicPattern :: Pattern String
atomicPattern = Pattern { value = "node1", elements = [] }
```

#### Pattern with Elements

A pattern containing one or more pattern elements in sequence.

**Structure**: Non-empty sequence of patterns  
**Status**: ✅ Implemented (this is the basic Pattern structure)

**Example**:
```haskell
patternWithElements :: Pattern String
patternWithElements = Pattern 
  { value = "pattern"
  , elements = [ Pattern { value = "elem1", elements = [] }
               , Pattern { value = "elem2", elements = [] }
               ]
  }
```

#### Nested Pattern

A pattern containing patterns that themselves contain patterns, enabling arbitrary nesting depth.

**Structure**: Recursive nesting of patterns  
**Status**: ✅ Implemented (this is the basic Pattern structure)

**Note**: Patterns are a data structure for representing graphs (like an adjacency matrix or adjacency list), optimized for expressiveness of layered, hierarchical graph structures rather than performance optimization over a single, "flat" graph.

### Graph Interpretations (Views)

Patterns can be **interpreted** as graph elements through different views. These are interpretations/views of pattern structures, **not pattern variants themselves**. The following interpretation functions are planned but not yet implemented:

#### Node Interpretation

A pattern can be **interpreted** as a **node** when it has no elements that are graph elements themselves. Typically, this means `elements == []` (an atomic pattern).

**Structure**: Empty sequence (atomic pattern)  
**Status**: ⏳ Planned (interpretation function `isNode` not yet implemented)

**Note**: This is an interpretation/view of a pattern structure, not a pattern variant. An atomic pattern can be interpreted as a node through a graph view.

#### Relationship Interpretation

A pattern can be **interpreted** as a **relationship** when it has exactly 2 elements, and both elements are nodes (atomic patterns).

**Structure**: Exactly 2 elements, both are atomic patterns  
**Status**: ⏳ Planned (interpretation function `isRelationship` not yet implemented)

**Example** (structure exists, interpretation function planned):
```haskell
-- Atomic patterns (can be interpreted as nodes)
nodeA :: Pattern String
nodeA = Pattern { value = "A", elements = [] }

nodeB :: Pattern String
nodeB = Pattern { value = "B", elements = [] }

-- Pattern with 2 elements (can be interpreted as a relationship)
relationship :: Pattern String
relationship = Pattern 
  { value = "knows"
  , elements = [nodeA, nodeB]
  }
```

**Note**: This is an interpretation/view of a pattern structure, not a pattern variant. A pattern with 2 elements can be interpreted as a relationship through a graph view.

#### Subgraph Interpretation

A pattern can be **interpreted** as a **subgraph** when all elements are graph elements (nodes, relationships, or other subgraphs).

**Structure**: All elements are graph elements  
**Status**: ⏳ Planned (interpretation function `isSubgraph` not yet implemented)

**Example** (structure exists, interpretation function planned):
```haskell
-- A pattern with elements (can be interpreted as a subgraph)
graphPattern :: Pattern String
graphPattern = Pattern 
  { value = "myGraph"
  , elements = [ nodeA                    -- atomic pattern (interpreted as node)
               , nodeB                    -- atomic pattern (interpreted as node)
               , relationship             -- pattern with 2 elements (interpreted as relationship)
               ]
  }
```

**Note**: This is an interpretation/view of a pattern structure, not a pattern variant. A pattern with elements can be interpreted as a subgraph through a graph view. The recursive structure allows patterns to contain patterns containing patterns, enabling layered, hierarchical graph representations.

### Validation Rules

1. **Well-formed**: All `Pattern` values must be finite (no infinite recursion)
2. **Type consistency**: All elements in a `Pattern` must have the same value type `v` (enforced by type system)
3. **Structure integrity**: Field accessors must always return valid values (no null/undefined values)

### State Transitions

#### Pattern Construction

- **Atomic pattern creation**: `Pattern v []` - creates a pattern with value `v` and empty element list
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

