# Data Model: Pattern Structure (Authoritative Definition)

**Feature**: 003-pattern-structure-review  
**Date**: 2025-01-27  
**Status**: Authoritative Reference

This document establishes the authoritative definition of the Pattern data structure and related concepts. All other documentation should align with these definitions.

---

## Core Entity: Pattern

### Definition

A **Pattern** is a **decorated sequence**: the elements form the pattern itself, and the value provides metadata (decoration) about that pattern. For example, the pattern "A B B A" with decoration "Enclosed rhyme" represents a specific sequence pattern (A B B A) that is classified as an "Enclosed rhyme". The elements ARE the pattern; the value describes what kind of pattern it is.

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

---

## Pattern Structural Classifications

Pattern structural classifications describe what patterns **are** structurally, not how they are interpreted. These are based on the element structure of the pattern itself.

### Atomic Pattern

A pattern with no elements (`elements == []`). Atomic patterns are the fundamental building blocks from which all other patterns are constructed.

**Structure**: Empty sequence  
**Status**: ✅ Implemented (this is the basic Pattern structure)

**Example**:
```haskell
atomicPattern :: Pattern String
atomicPattern = Pattern { value = "node1", elements = [] }
```

### Singular Pattern

A pattern with exactly one element (`length (elements p) == 1`). Singular patterns contain a single element in their sequence.

**Structure**: Sequence with exactly one element  
**Status**: ✅ Implemented (this is the basic Pattern structure)

**Example**:
```haskell
singularPattern :: Pattern String
singularPattern = Pattern 
  { value = "parent"
  , elements = [ Pattern { value = "child", elements = [] } ]
  }
```

### Pattern with Elements

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

### Nested Pattern

A pattern containing patterns that themselves contain patterns, enabling arbitrary nesting depth.

**Structure**: Recursive nesting of patterns  
**Status**: ✅ Implemented (this is the basic Pattern structure)

**Note**: Patterns are a data structure for representing graphs (like an adjacency matrix or adjacency list), optimized for expressiveness of layered, hierarchical graph structures rather than performance optimization over a single, "flat" graph.

---

## Graph Interpretations (Views)

Patterns can be **interpreted** as graph elements through different views. These are interpretations/views of pattern structures, **not pattern variants themselves**. The following interpretation functions are planned but not yet implemented:

### Node Interpretation

A pattern can be **interpreted** as a **node** when it has no elements that are graph elements themselves. Typically, this means `elements == []` (an atomic pattern).

**Structure**: Empty sequence (atomic pattern)  
**Status**: ⏳ Planned (interpretation function `isNode` not yet implemented)

**Validation** (planned): `isNode :: Pattern v -> Bool`

**Note**: This is an interpretation/view of a pattern structure, not a pattern variant. An atomic pattern can be interpreted as a node through a graph view.

### Relationship Interpretation

A pattern can be **interpreted** as a **relationship** when it has exactly 2 elements, and both elements are nodes (atomic patterns).

**Structure**: Exactly 2 elements, both are atomic patterns  
**Status**: ⏳ Planned (interpretation function `isRelationship` not yet implemented)

**Validation** (planned): `isRelationship :: Pattern v -> Bool`

**Constraints** (planned):
- `length (elements p) == 2`
- `all isNode (elements p)`

**Example** (structure exists, interpretation function planned):
```haskell
nodeA = Pattern { value = "A", elements = [] }
nodeB = Pattern { value = "B", elements = [] }
relationship = Pattern { value = "knows", elements = [nodeA, nodeB] }
```

**Note**: This is an interpretation/view of a pattern structure, not a pattern variant. A pattern with 2 elements can be interpreted as a relationship through a graph view.

### Subgraph Interpretation

A pattern can be **interpreted** as a **subgraph** when all elements are graph elements (nodes, relationships, or other subgraphs).

**Structure**: All elements are graph elements  
**Status**: ⏳ Planned (interpretation function `isSubgraph` not yet implemented)

**Validation** (planned): `isSubgraph :: Pattern v -> Bool`

**Constraints** (planned):
- `all isGraphElement (elements p)`

**Note**: This is an interpretation/view of a pattern structure, not a pattern variant. A pattern with elements can be interpreted as a subgraph through a graph view.

### Path Interpretation

A pattern can be **interpreted** as a **path** when it is a subgraph and all relationships in the path chain correctly (target of one equals source of next).

**Structure**: Subgraph with chained relationships  
**Status**: ⏳ Planned (interpretation function `isPath` not yet implemented)

**Validation** (planned): `isPath :: Pattern v -> Bool`

**Constraints** (planned):
- `isSubgraph p`
- `chainsCorrectly (elements p)`

**Note**: This is an interpretation/view of a pattern structure, not a pattern variant. A pattern structure can be interpreted as a path through a graph view.

---

## Graph Views

Graph views provide different semantic interpretations of pattern structures. Views are planned but not yet implemented.

### GraphView Typeclass (Planned)

```haskell
class GraphView view where
  type Direction view :: *
  
  interpretNode :: view -> Pattern v -> Bool
  interpretRel  :: view -> Pattern v -> Bool
  
  direction :: view -> Pattern v -> Direction view
  canChain  :: view -> Pattern v -> Pattern v -> Bool
  
  toGraph :: view -> Pattern v -> Graph (Direction view) v
```

**Status**: ⏳ Planned (not yet implemented)

### Standard Views (Planned)

- **DirectedView**: Relationships have explicit direction (source → target)
- **UndirectedView**: Relationships are undirected (edge set)

**Status**: ⏳ Planned (not yet implemented)

---

## Typeclass Instances

### Implemented

- **Eq**: ✅ Implemented (`deriving Eq`)
- **Show**: ✅ Implemented (manual instance)

### Planned

- **Functor**: ⏳ Planned (TODO.md Feature 4) - Enables value transformation while preserving structure
- **Foldable**: ⏳ Planned (TODO.md Feature 5) - Enables aggregation over pattern values
- **Traversable**: ⏳ Planned (TODO.md Feature 6) - Enables effectful traversal

---

## Terminology Standards

### Primary Terms

- **Pattern**: A decorated sequence - the elements form the pattern itself, the value decorates it
- **value**: The decoration field storing metadata about what kind of pattern it is
- **elements**: The pattern itself, represented as a sequence of elements
- **decorated sequence**: The conceptual model - elements are the pattern, value is decoration
- **tree**: The implementation model (supporting detail)

### Avoid These Terms

- ❌ "metadata" (use "value" or "decoration")
- ❌ "children" (use "elements" - elements ARE the pattern, not children)
- ❌ "child patterns" (use "elements" - elements form the pattern itself)
- ❌ "tree" as primary model (use "decorated sequence" as primary, "tree" as implementation)
- ❌ Language that suggests elements are subordinate to the value (elements ARE the pattern)

---

## Validation Rules

### Pattern Structure

1. **Well-formed**: All `Pattern` values must be finite (no infinite recursion)
2. **Type consistency**: All elements in a `Pattern` must have the same value type `v` (enforced by type system)
3. **Structure integrity**: Field accessors must always return valid values

### Pattern Variant Classification (Planned)

1. **Mutual exclusivity**: A pattern can be classified as node, relationship, subgraph, or path, but not multiple simultaneously (when classification functions are implemented)
2. **Structure-dependent**: Classification depends on the structure of the pattern's elements
3. **Recursive**: Classification applies recursively to pattern elements

---

## State Transitions

### Pattern Construction

- **Atomic pattern creation**: `Pattern v []` - creates a pattern with value `v` and empty element list
- **Pattern with elements creation**: `Pattern v [p1, p2, ...]` - creates a pattern with value `v` and element patterns `p1, p2, ...`

### Pattern Inspection

- **Value access**: `value pattern` - retrieves the value stored in the pattern
- **Elements access**: `elements pattern` - retrieves the list of element patterns

---

## Design Principles

1. **Decorated Sequence Semantics**: Patterns are conceptually decorated sequences where elements form the pattern itself; tree structure is implementation detail
2. **Schema-Lazy**: Patterns don't commit to specific graph semantics; interpretation happens in views (when implemented)
3. **Compositional**: Views can be composed, stacked, or swapped without changing underlying patterns (when implemented)
4. **Open-ended**: New views can be defined for any graph-like interpretation (when implemented)
5. **Categorical**: Each view defines a functor; forgetful pattern matching uses functor composition (when implemented)

---

## Implementation Status Summary

| Component | Status | Notes |
|-----------|--------|-------|
| Pattern data type | ✅ Implemented | Basic structure with value and elements |
| Eq instance | ✅ Implemented | `deriving Eq` |
| Show instance | ✅ Implemented | Manual instance |
| Functor instance | ⏳ Planned | TODO.md Feature 4 |
| Foldable instance | ⏳ Planned | TODO.md Feature 5 |
| Traversable instance | ⏳ Planned | TODO.md Feature 6 |
| Classification functions | ⏳ Planned | TODO.md Features 8-11 |
| Navigation functions | ⏳ Planned | TODO.md Feature 12 |
| GraphView typeclass | ⏳ Planned | TODO.md Feature 15 |
| Graph operations | ⏳ Planned | TODO.md Feature 15 |

---

## References

- **Implementation**: `src/Pattern/Core.hs`
- **Design Documentation**: `DESIGN.md`
- **Project Overview**: `README.md`
- **Implementation Roadmap**: `TODO.md`

