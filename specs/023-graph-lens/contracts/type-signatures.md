# Type Signatures: Graph Lens API

**Feature**: 023-graph-lens  
**Date**: 2025-01-27  
**Phase**: 1 - Design

## Overview

This document defines the complete type signature API for the Graph Lens module. All functions are pure (no side effects) and operate on the GraphLens data structure to interpret Pattern structures as graphs.

## Core Data Structure

### GraphLens

```haskell
-- | A Graph Lens provides an interpretive view of a Pattern as a graph structure.
-- 
-- The lens consists of:
-- * @scopePattern@: The Pattern that defines the boundary for all graph operations.
--   Only direct elements of this pattern are considered for graph structure.
-- * @isNode@: A predicate determining which direct elements are nodes.
--   All other graph concepts (relationships, walks) derive from this predicate.
--
-- == Categorical Interpretation
--
-- Graph Lens provides a functorial interpretation where Pattern structures are
-- transformed into graph interpretations. The transformation Pattern → Graph
-- interpretation is functorial in nature.
--
-- == Design Principles
--
-- 1. Scope-bounded: All operations only consider direct elements of scopePattern
-- 2. Single predicate foundation: Only isNode is required, all else derives
-- 3. Context at construction: Predicate context captured when lens is created
-- 4. Interpretation, not intrinsic: Graph structure is an interpretation, not
--    a property of Pattern itself
--
-- == Example
--
-- >>> let atomicLens = GraphLens pattern (\(Pattern _ els) -> null els)
-- >>> nodes atomicLens
-- [[a], [b], [c]]
data GraphLens v = GraphLens
  { scopePattern :: Pattern v
    -- ^ The Pattern that defines the graph scope
  , isNode       :: Pattern v -> Bool
    -- ^ Predicate determining which elements are nodes
  }
```

---

## Node Operations

### nodes

```haskell
-- | Extract all nodes from the graph lens.
--
-- Nodes are direct elements of scopePattern that satisfy the isNode predicate.
--
-- == Time Complexity
-- O(n) where n is the number of direct elements in scopePattern
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> nodes lens
-- [[a], [b], [c]]
nodes :: GraphLens v -> [Pattern v]
```

### isNode (context-aware)

```haskell
-- | Determine if a Pattern is a node according to the lens.
--
-- This is the context-aware version that uses the lens's isNode predicate.
-- The lens parameter provides the predicate context.
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> isNode lens (pattern "a")
-- True
-- >>> isNode lens (patternWith "rel" [pattern "a", pattern "b"])
-- False
--
-- Note: This function name may conflict with the field accessor.
-- Consider using a qualified import or renaming if needed.
isNode :: GraphLens v -> Pattern v -> Bool
isNode lens p = isNode (lens :: GraphLens v) p
-- Note: This conflicts with field accessor - implementation will need
-- to handle this (e.g., use qualified access or different name)
```

---

## Relationship Operations

### isRelationship

```haskell
-- | Determine if a Pattern is a relationship according to the lens.
--
-- A relationship is a non-node pattern with exactly two node elements.
--
-- == Properties
-- * Must not be a node (does not satisfy isNode predicate)
-- * Must have exactly two elements
-- * Both elements must be nodes (according to the lens)
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> let rel = patternWith "knows" [pattern "Alice", pattern "Bob"]
-- >>> isRelationship lens rel
-- True
isRelationship :: GraphLens v -> Pattern v -> Bool
```

### relationships

```haskell
-- | Extract all relationships from the graph lens.
--
-- Relationships are non-node patterns with exactly two node elements.
--
-- == Time Complexity
-- O(n) where n is the number of direct elements in scopePattern
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> relationships lens
-- [[knows | [Alice], [Bob]], [likes | [Bob], [Charlie]]]
relationships :: GraphLens v -> [Pattern v]
```

### source

```haskell
-- | Extract the source node from a relationship.
--
-- For directed relationships, the source is the first element.
-- Returns Nothing if the pattern is not a relationship.
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> let rel = patternWith "knows" [pattern "Alice", pattern "Bob"]
-- >>> source lens rel
-- Just (pattern "Alice")
source :: GraphLens v -> Pattern v -> Maybe (Pattern v)
```

### target

```haskell
-- | Extract the target node from a relationship.
--
-- For directed relationships, the target is the second element.
-- Returns Nothing if the pattern is not a relationship.
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> let rel = patternWith "knows" [pattern "Alice", pattern "Bob"]
-- >>> target lens rel
-- Just (pattern "Bob")
target :: GraphLens v -> Pattern v -> Maybe (Pattern v)
```

### reverseRel

```haskell
-- | Reverse the direction of a relationship pattern.
--
-- Swaps the first and second elements, effectively reversing the
-- relationship direction.
--
-- == Example
--
-- >>> let rel = patternWith "knows" [pattern "Alice", pattern "Bob"]
-- >>> reverseRel rel
-- patternWith "knows" [pattern "Bob", pattern "Alice"]
reverseRel :: Pattern v -> Pattern v
```

---

## Walk Operations

### isWalk

```haskell
-- | Determine if a Pattern is a walk according to the lens.
--
-- A walk is a non-node pattern whose elements are all relationships,
-- where consecutive relationships share nodes (target of one equals
-- source of next).
--
-- == Properties
-- * Must not be a node (does not satisfy isNode predicate)
-- * All elements must be relationships (according to the lens)
-- * Consecutive relationships must be connected
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> let walk = patternWith "path" [rel1, rel2, rel3]
-- >>> isWalk lens walk
-- True
isWalk :: GraphLens v -> Pattern v -> Bool
```

### walks

```haskell
-- | Extract all walks from the graph lens.
--
-- Walks are non-node patterns whose elements are all relationships,
-- where consecutive relationships share nodes.
--
-- == Time Complexity
-- O(n) where n is the number of direct elements in scopePattern
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> walks lens
-- [[path | [rel1], [rel2], [rel3]]]
walks :: GraphLens v -> [Pattern v]
```

### walkNodes

```haskell
-- | Extract nodes from a walk in traversal order.
--
-- Returns the source of the first relationship, followed by the targets
-- of subsequent relationships. Returns empty list if the pattern is not
-- a valid walk.
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> let walk = patternWith "path" [rel1, rel2]
-- >>> walkNodes lens walk
-- [pattern "A", pattern "B", pattern "C"]
walkNodes :: GraphLens v -> Pattern v -> [Pattern v]
```

---

## Navigation Operations

### neighbors

```haskell
-- | Find all neighbors of a node.
--
-- Neighbors are nodes connected to the given node via relationships
-- (either as source or target).
--
-- == Time Complexity
-- O(r) where r is the number of relationships in the graph
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> neighbors lens (pattern "Alice")
-- [pattern "Bob", pattern "Charlie"]
neighbors :: Eq v => GraphLens v -> Pattern v -> [Pattern v]
```

### incidentRels

```haskell
-- | Find all relationships involving a node.
--
-- Returns relationships where the node is either source or target.
--
-- == Time Complexity
-- O(r) where r is the number of relationships in the graph
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> incidentRels lens (pattern "Alice")
-- [[knows | [Alice], [Bob]], [likes | [Charlie], [Alice]]]
incidentRels :: Eq v => GraphLens v -> Pattern v -> [Pattern v]
```

### degree

```haskell
-- | Compute the degree of a node (number of incident relationships).
--
-- The degree is the count of relationships where the node is either
-- source or target.
--
-- == Time Complexity
-- O(r) where r is the number of relationships in the graph
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> degree lens (pattern "Alice")
-- 3
degree :: Eq v => GraphLens v -> Pattern v -> Int
```

---

## Graph Analysis Operations

### connectedComponents

```haskell
-- | Find all connected components in the graph.
--
-- A connected component is a set of nodes that are reachable from
-- each other via relationships. Returns a list of lists, where each
-- inner list represents a component.
--
-- == Time Complexity
-- O(n + r) where n is number of nodes and r is number of relationships
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> connectedComponents lens
-- [[pattern "A", pattern "B", pattern "C"], [pattern "D", pattern "E"]]
connectedComponents :: Eq v => GraphLens v -> [[Pattern v]]
```

### bfs

```haskell
-- | Perform breadth-first search from a starting node.
--
-- Returns all nodes reachable from the starting node via relationships.
--
-- == Time Complexity
-- O(n + r) where n is number of nodes and r is number of relationships
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> bfs lens (pattern "Alice")
-- [pattern "Alice", pattern "Bob", pattern "Charlie"]
bfs :: Eq v => GraphLens v -> Pattern v -> [Pattern v]
```

### findPath

```haskell
-- | Find a path between two nodes if one exists.
--
-- Returns Just [nodes] if a path exists, Nothing otherwise.
-- The path is a sequence of nodes connecting start to end.
--
-- == Time Complexity
-- O(n + r) where n is number of nodes and r is number of relationships
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> findPath lens (pattern "Alice") (pattern "Charlie")
-- Just [pattern "Alice", pattern "Bob", pattern "Charlie"]
findPath :: Eq v => GraphLens v -> Pattern v -> Pattern v -> Maybe [Pattern v]
```

---

## Helper Functions

### consecutivelyConnected

```haskell
-- | Check if a list of relationships are consecutively connected.
--
-- Relationships are consecutively connected if the target of one
-- equals the source of the next.
--
-- == Internal Function
-- This is an internal helper function used by isWalk.
-- May be exported for testing or advanced use cases.
consecutivelyConnected :: Eq v => GraphLens v -> [Pattern v] -> Bool
```

---

## Module Export Structure

```haskell
module Pattern.Graph
  ( -- * Graph Lens Type
    GraphLens(..)
    -- * Node Operations
  , nodes
  , isNode
    -- * Relationship Operations
  , isRelationship
  , relationships
  , source
  , target
  , reverseRel
    -- * Walk Operations
  , isWalk
  , walks
  , walkNodes
    -- * Navigation Operations
  , neighbors
  , incidentRels
  , degree
    -- * Graph Analysis Operations
  , connectedComponents
  , bfs
  , findPath
  ) where
```

---

## Type Constraints

### Eq Constraint

Functions requiring node/relationship comparison use `Eq v` constraint:
- `neighbors`
- `incidentRels`
- `degree`
- `connectedComponents`
- `bfs`
- `findPath`
- `consecutivelyConnected`

This is necessary because:
- Nodes and relationships are compared for equality
- Graph traversal requires checking if nodes have been visited
- Path finding requires tracking visited nodes

### No Constraints

Functions that only filter or transform patterns don't require constraints:
- `nodes`
- `relationships`
- `walks`
- `isNode`
- `isRelationship`
- `isWalk`
- `source`
- `target`
- `reverseRel`
- `walkNodes`

---

## Error Handling

All functions handle edge cases gracefully:

- **Empty patterns**: Return empty lists or `False`/`Nothing`
- **No nodes**: Return empty lists for node-dependent operations
- **Invalid structures**: Return `False` for predicates, `Nothing` for extractors
- **Non-existent nodes**: Return empty lists or `Nothing` for navigation operations

No exceptions are thrown - all error cases return safe default values.

---

## Performance Characteristics

| Function | Time Complexity | Space Complexity |
|----------|----------------|------------------|
| `nodes` | O(n) | O(n) |
| `relationships` | O(n) | O(n) |
| `walks` | O(n) | O(n) |
| `neighbors` | O(r) | O(n) |
| `incidentRels` | O(r) | O(r) |
| `degree` | O(r) | O(1) |
| `connectedComponents` | O(n + r) | O(n) |
| `bfs` | O(n + r) | O(n) |
| `findPath` | O(n + r) | O(n) |

Where:
- `n` = number of direct elements in scopePattern
- `r` = number of relationships in the graph
- Space complexity assumes result storage, not including input
