# Data Model: Basic Pattern Type

**Feature**: 002-basic-pattern-type  
**Date**: 2025-01-27

## Core Entity: Pattern

### Definition

The `Pattern` type is a recursive tree structure that serves as the foundational building block for representing graph elements. Each pattern stores a value and contains zero or more child Pattern instances.

### Structure

```haskell
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  }
```

### Fields

- **value** (`v`): The value associated with this pattern node. Type parameter `v` allows for different value types. This is the data stored at this node in the tree.

- **elements** (`[Pattern v]`): A list of child patterns, forming the recursive tree structure. Empty list `[]` represents a leaf pattern (no children). Non-empty list represents a pattern with child elements.

### Type Constraints

- `Pattern` is parameterized over value type `v`
- All patterns in a structure must share the same value type `v` (enforced by type system)
- Pattern structures must be finite (no infinite recursion)

### Relationships

- **Self-referential**: Each `Pattern` contains zero or more child `Pattern` values
- **Recursive**: The tree structure can be arbitrarily deep (patterns containing patterns containing patterns, etc.)
- **Hierarchical**: Child elements form a tree hierarchy with the parent pattern as the root

### Field Accessors

Haskell record syntax automatically provides field accessors:

- `value :: Pattern v -> v` - Extract the value from a pattern
- `elements :: Pattern v -> [Pattern v]` - Extract the list of child patterns

### Pattern Variants

#### Leaf Pattern

A pattern with no child elements (`elements == []`):

```haskell
leafPattern :: Pattern String
leafPattern = Pattern { value = "node1", elements = [] }
```

#### Pattern with Children

A pattern with one or more child elements:

```haskell
parentPattern :: Pattern String
parentPattern = Pattern 
  { value = "parent"
  , elements = [ leafPattern, anotherLeaf ]
  }
```

### Validation Rules

1. **Well-formed**: All `Pattern` values must be finite (no infinite recursion)
2. **Type consistency**: All elements in a `Pattern` must have the same value type `v` (enforced by type system)
3. **Structure integrity**: Field accessors must always return valid values (no null/undefined values)

### State Transitions

#### Pattern Construction

- **Leaf pattern creation**: `Pattern v []` - creates a pattern with value `v` and empty child list
- **Pattern with children creation**: `Pattern v [p1, p2, ...]` - creates a pattern with value `v` and child patterns `p1, p2, ...`

#### Pattern Inspection

- **Value access**: `value pattern` - retrieves the value stored in the pattern
- **Elements access**: `elements pattern` - retrieves the list of child patterns

### Design Principles

1. **Simplicity**: The basic type is intentionally minimal - just value storage and child references
2. **Recursive**: The recursive structure enables representing complex hierarchical relationships
3. **Type-safe**: Type parameter ensures all patterns in a structure share the same value type
4. **Accessible**: Record syntax provides automatic, type-safe field accessors

### Future Extensions

This basic type will be extended in future phases with:
- Typeclass instances (Functor, Foldable, Traversable)
- Classification functions (isNode, isRelationship, etc.)
- Navigation functions (source, target, nodes, relationships)
- Graph view interpretations

All of these will build upon this foundational Pattern type definition.

