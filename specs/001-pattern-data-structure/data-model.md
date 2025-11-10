# Data Model: Pattern Data Structure

**Feature**: 001-pattern-data-structure  
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

**Note**: `Show` is implemented as a manual instance. `Functor`, `Foldable`, and `Traversable` are ⏳ Planned but not yet implemented (see Implementation Status section).

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
- Each tree node stores a decoration (value) and contains the pattern elements as a list
- The recursive structure enables arbitrary nesting depth
- Tree traversal provides access to sequence elements in order

### Relationship: Sequence Conceptual Model and Tree Implementation

The relationship between the sequence conceptual model and tree implementation is:

**Primary Semantic (Conceptual)**: Patterns are decorated sequences where elements form the pattern itself. The sequence order is essential to the pattern.

**Implementation Detail**: The tree structure is how sequences are represented in memory. Each tree node stores a decoration (value) and contains the pattern elements as a list, enabling recursive nesting.

**How They Relate**: The tree implementation supports sequence semantics:
- Tree nodes store sequences (lists) of pattern elements
- Tree traversal preserves sequence order
- The recursive structure enables nested sequences (patterns containing patterns)
- Sequence operations (ordering, length, access by position) are implemented via tree operations

**Key Principle**: Conceptually, developers should think of patterns as decorated sequences where elements form the pattern itself. The tree structure is an implementation detail that supports sequence operations. There is no contradiction between these views - the tree is simply how sequences are represented in memory.

### Type Constraints

- `Pattern` is parameterized over value type `v`
- All patterns in a structure must share the same value type `v` (enforced by type system)
- Pattern structures must be finite (no infinite recursion)
- Type consistency is enforced by Haskell's type system

### Relationships

- **Self-referential**: Each `Pattern` contains zero or more Pattern elements that form the pattern sequence
- **Recursive**: Pattern elements can themselves be patterns, enabling arbitrarily deep nesting (patterns containing patterns containing patterns, etc.)
- **Compositional**: Patterns can be composed by including other patterns as elements in the sequence
- **Graph representation**: Patterns are a data structure for representing graphs (like an adjacency matrix or adjacency list), optimized for expressiveness of layered, hierarchical graph structures. Patterns can be **interpreted** as different graph elements through views.

---

## Pattern Structural Classifications

Pattern structural classifications describe what patterns **are** structurally, not how they are interpreted. These are based on the element structure of the pattern itself.

### Empty Pattern

A pattern with no elements (`elements == []`).

**Structure**: Empty sequence  
**Status**: ✅ Implemented (this is the basic Pattern structure)

**Example**:
```haskell
emptyPattern :: Pattern String
emptyPattern = Pattern { value = "node1", elements = [] }
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

**Example**:
```haskell
nestedPattern :: Pattern String
nestedPattern = Pattern 
  { value = "outer"
  , elements = [ Pattern { value = "middle"
                         , elements = [ Pattern { value = "inner", elements = [] } ]
                         }
               ]
  }
```

**Note**: Patterns are a data structure for representing graphs, optimized for expressiveness of layered, hierarchical graph structures rather than performance optimization over a single, "flat" graph.

---

## Graph Interpretations (Views)

Patterns can be **interpreted** as graph elements through different views. These are interpretations/views of pattern structures, **not pattern variants themselves**. The following interpretation functions are planned but not yet implemented:

### Node Interpretation

A pattern can be **interpreted** as a **node** when it has no elements that are graph elements themselves. Typically, this means `elements == []` (an empty pattern).

**Structure**: Empty sequence (empty pattern)  
**Status**: ⏳ Planned (interpretation function `isNode` not yet implemented)

**Validation** (planned): `isNode :: Pattern v -> Bool`

**Note**: This is an interpretation/view of a pattern structure, not a pattern variant. An empty pattern can be interpreted as a node through a graph view.

### Relationship Interpretation

A pattern can be **interpreted** as a **relationship** when it has exactly 2 elements, and both elements are nodes (empty patterns).

**Structure**: Exactly 2 elements, both are empty patterns  
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

## Pattern Morphisms

### Type

```haskell
type PatternMorphism v w = Pattern v -> Pattern w
```

### Standard Morphisms

#### homomorphism

Structure-preserving map that transforms values:

```haskell
homomorphism :: (v -> w) -> PatternMorphism v w
homomorphism f = fmap f
```

#### forget

Forgetful morphism that removes value information:

```haskell
forget :: PatternMorphism v ()
forget = forgetValues
forgetValues :: Pattern v -> Pattern ()
forgetValues = fmap (const ())
```

---

## Graph Representation

### Graph Type

```haskell
data Graph dir v = Graph
  { nodes :: Set (Pattern v)
  , edges :: Set (Edge dir v)
  }
```

### Edge Types

```haskell
data Edge dir v where
  DirectedEdge   :: Pattern v -> Pattern v -> Edge Ordered v
  UndirectedEdge :: Set (Pattern v) -> Edge Unordered v
```

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

### Category-Theoretic Laws

1. **Functor laws**: `fmap` must preserve identity and composition
2. **Naturality**: Graph view transformations must be natural transformations
3. **Composition**: Pattern morphisms must compose correctly

---

## State Transitions

### Pattern Construction

- **Empty pattern creation**: `Pattern v []` - creates a pattern with value `v` and empty element list
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

