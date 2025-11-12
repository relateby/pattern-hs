# Type Signatures: Pattern Library API

**Feature**: 001-pattern-data-structure  
**Date**: 2025-01-27

## Overview

This document defines the public API type signatures for the Pattern library. For a library project, these serve as the "contracts" that define the interface users will interact with.

---

## Core Module: Pattern.Core

### Pattern Type

```haskell
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  } deriving (Show, Eq)
  -- ⏳ Planned: Functor, Foldable, Traversable, Ord instances
```

### Pattern Construction

```haskell
-- ⏳ Planned: Construct an atomic pattern (can be interpreted as a node)
pattern :: v -> Pattern v

-- ⏳ Planned: Construct a pattern with elements
patternWith :: v -> [Pattern v] -> Pattern v
```

### Pattern Classification

```haskell
-- ⏳ Planned: Check if pattern is a graph element (node, relationship, subgraph, or path)
isGraphElement :: Pattern v -> Bool

-- ⏳ Planned: Check if pattern is a node
isNode :: Pattern v -> Bool

-- ⏳ Planned: Check if pattern is a relationship
isRelationship :: Pattern v -> Bool

-- ⏳ Planned: Check if pattern is a subgraph
isSubgraph :: Pattern v -> Bool

-- ⏳ Planned: Check if pattern is a path
isPath :: Pattern v -> Bool
```

### Pattern Navigation

```haskell
-- ⏳ Planned: Get source node from a relationship pattern
source :: Pattern v -> Pattern v

-- ⏳ Planned: Get target node from a relationship pattern
target :: Pattern v -> Pattern v

-- ⏳ Planned: Get all nodes in a pattern
nodes :: Pattern v -> [Pattern v]

-- ⏳ Planned: Get all relationships in a pattern
relationships :: Pattern v -> [Pattern v]
```

---

## Views Module: Pattern.Views

### GraphView Typeclass

⏳ **Planned**: GraphView typeclass and views are not yet implemented.

```haskell
-- ⏳ Planned: GraphView typeclass for graph interpretations
class GraphView view where
  -- Associated type for direction representation
  type Direction view :: *
  
  -- Interpret whether a pattern is a node in this view
  interpretNode :: view -> Pattern v -> Bool
  
  -- Interpret whether a pattern is a relationship in this view
  interpretRel :: view -> Pattern v -> Bool
  
  -- Extract direction information from a relationship
  direction :: view -> Pattern v -> Direction view
  
  -- Check if two relationships can chain in this view
  canChain :: view -> Pattern v -> Pattern v -> Bool
  
  -- Convert pattern to graph representation
  toGraph :: view -> Pattern v -> Graph (Direction view) v
```

### Standard Views

```haskell
-- ⏳ Planned: Directed graph view
data DirectedView = DirectedView

instance GraphView DirectedView where
  type Direction DirectedView = Ordered
  
  -- ... implementation

-- ⏳ Planned: Undirected graph view
data UndirectedView = UndirectedView

instance GraphView UndirectedView where
  type Direction UndirectedView = Unordered
  
  -- ... implementation
```

---

## Graph Module: Pattern.Graph

### Graph Type

```haskell
data Graph dir v = Graph
  { nodes :: Set (Pattern v)
  , edges :: Set (Edge dir v)
  }
  deriving (Show, Eq)

data Edge dir v where
  DirectedEdge   :: Pattern v -> Pattern v -> Edge Ordered v
  UndirectedEdge :: Set (Pattern v) -> Edge Unordered v
  deriving (Show, Eq)
```

### Graph Operations

```haskell
-- Create empty graph
emptyGraph :: Graph dir v

-- Add node to graph
addNode :: Pattern v -> Graph dir v -> Graph dir v

-- Add edge to graph
addEdge :: Edge dir v -> Graph dir v -> Graph dir v

-- Check if graph contains node
hasNode :: Pattern v -> Graph dir v -> Bool

-- Check if graph contains edge
hasEdge :: Edge dir v -> Graph dir v -> Bool
```

---

## Morphisms Module: Pattern.Morphisms

### Pattern Morphism Type

```haskell
type PatternMorphism v w = Pattern v -> Pattern w
```

### Standard Morphisms

```haskell
-- Structure-preserving homomorphism
homomorphism :: (v -> w) -> PatternMorphism v w

-- Forgetful morphism (removes value information)
forget :: PatternMorphism v ()

-- Forget values, preserve structure
forgetValues :: Pattern v -> Pattern ()

-- Compose morphisms
composeMorphism :: PatternMorphism w u -> PatternMorphism v w -> PatternMorphism v u
```

---

## Main Module: Pattern

### Exports

```haskell
module Pattern
  ( -- Core types
    Pattern(..)
  , GraphView(..)
  , Graph(..)
  , Edge(..)
  , PatternMorphism
    
    -- Core operations
  , pattern
  , patternWith
  , isGraphElement
  , isNode
  , isRelationship
  , isSubgraph
  , isPath
  , source
  , target
  , nodes
  , relationships
    
    -- Views
  , DirectedView(..)
  , UndirectedView(..)
    
    -- Graph operations
  , emptyGraph
  , addNode
  , addEdge
  , hasNode
  , hasEdge
  , toGraph
    
    -- Morphisms
  , homomorphism
  , forget
  , forgetValues
  , composeMorphism
  ) where
```

---

## Type Constraints

### Required Instances

- `Pattern v` instances:
  - ✅ Implemented: `Show`, `Eq`, `Functor`
  - ⏳ Planned: `Foldable`, `Traversable`, `Ord` (required for `Set (Pattern v)` in Graph structures)
- `Graph dir v` must be instances of: `Show`, `Eq` (⏳ Planned)
- `Edge dir v` must be instances of: `Show`, `Eq` (⏳ Planned)

### Type Families

- `Direction view` is an associated type family in the `GraphView` typeclass
- Must be `Ordered` for `DirectedView` and `Unordered` for `UndirectedView`

---

## Error Handling

### Preconditions

Many functions assume well-formed patterns:
- `source` and `target` assume the pattern is a relationship (exactly 2 elements)
- `canChain` assumes both patterns are relationships

### Error Strategy

For a reference implementation, we may use:
- `Maybe` return types for operations that can fail
- Or document preconditions and assume they are satisfied

**Decision**: Use `Maybe` for operations with preconditions to provide clear error handling.

---

## Documentation Requirements

All public functions must have:
- Haddock documentation
- Type signature
- Usage examples
- Mathematical description (for category-theoretic operations)

