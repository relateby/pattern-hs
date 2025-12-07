# Quick Start: Graph Lens

**Feature**: 023-graph-lens  
**Date**: 2025-01-27  
**Phase**: 1 - Design

## Overview

This quick start guide provides examples of using Graph Lens to interpret Pattern structures as graphs. Graph Lens enables multiple graph interpretations of the same Pattern through a minimal, elegant design based on a single predicate.

## Basic Usage

### Import

```haskell
import Pattern
import Pattern.Graph
```

### Creating a Graph Lens

A Graph Lens requires two components:
1. A `scopePattern` that defines the graph boundary
2. An `isNode` predicate that identifies which elements are nodes

```haskell
-- Example: Atomic patterns as nodes
let graphPattern = patternWith "graph"
      [ pattern "a"           -- atomic node
      , pattern "b"           -- atomic node
      , pattern "c"           -- atomic node
      , patternWith "r1" [pattern "a", pattern "b"]  -- relationship
      , patternWith "r2" [pattern "b", pattern "c"]  -- relationship
      ]

-- Predicate: atomic patterns (no elements) are nodes
let isAtomic (Pattern _ els) = null els

-- Create lens
let atomicLens = GraphLens graphPattern isAtomic
```

### Querying Nodes

```haskell
-- Get all nodes
let allNodes = nodes atomicLens
-- Result: [pattern "a", pattern "b", pattern "c"]

-- Check if a pattern is a node
let isNodeA = isNode atomicLens (pattern "a")  -- True
let isNodeR1 = isNode atomicLens (patternWith "r1" [pattern "a", pattern "b"])  -- False
```

### Querying Relationships

```haskell
-- Get all relationships
let allRels = relationships atomicLens
-- Result: [patternWith "r1" [pattern "a", pattern "b"], 
--          patternWith "r2" [pattern "b", pattern "c"]]

-- Check if a pattern is a relationship
let isRel = isRelationship atomicLens (patternWith "r1" [pattern "a", pattern "b"])  -- True

-- Extract source and target
let src = source atomicLens (patternWith "r1" [pattern "a", pattern "b"])  -- Just (pattern "a")
let tgt = target atomicLens (patternWith "r1" [pattern "a", pattern "b"])  -- Just (pattern "b")
```

### Navigation

```haskell
-- Find neighbors of a node
let neighborsOfB = neighbors atomicLens (pattern "b")
-- Result: [pattern "a", pattern "c"]

-- Find incident relationships
let incidentToB = incidentRels atomicLens (pattern "b")
-- Result: [patternWith "r1" [pattern "a", pattern "b"],
--          patternWith "r2" [pattern "b", pattern "c"]]

-- Compute node degree
let degreeOfB = degree atomicLens (pattern "b")  -- 2
```

## Advanced Examples

### Walks

```haskell
-- Create a walk pattern (sequence of connected relationships)
let walkPattern = patternWith "walk"
      [ patternWith "r1" [pattern "a", pattern "b"]
      , patternWith "r2" [pattern "b", pattern "c"]
      ]

-- Check if it's a walk
let isValidWalk = isWalk atomicLens walkPattern  -- True

-- Extract nodes in traversal order
let walkNodes = walkNodes atomicLens walkPattern
-- Result: [pattern "a", pattern "b", pattern "c"]
```

### Graph Analysis

```haskell
-- Find connected components
let components = connectedComponents atomicLens
-- Result: [[pattern "a", pattern "b", pattern "c"]]
-- (all nodes are in one component)

-- Breadth-first search from a node
let reachable = bfs atomicLens (pattern "a")
-- Result: [pattern "a", pattern "b", pattern "c"]

-- Find path between nodes
let path = findPath atomicLens (pattern "a") (pattern "c")
-- Result: Just [pattern "a", pattern "b", pattern "c"]
```

### Multiple Lenses

The same Pattern can be interpreted through different lenses:

```haskell
-- Original pattern
let metaGraph = patternWith "meta"
      [ patternWith "r1" [pattern "a", pattern "b"]
      , patternWith "r2" [pattern "b", pattern "c"]
      , patternWith "dep1" [patternWith "r1" [pattern "a", pattern "b"],
                            patternWith "r2" [pattern "b", pattern "c"]]
      ]

-- Lens 1: Atomic patterns as nodes
let lens1 = GraphLens metaGraph (\(Pattern _ els) -> null els)
let nodes1 = nodes lens1
-- Result: [pattern "a", pattern "b", pattern "c"]

-- Lens 2: Relationships as nodes (meta-graph)
let isRelNode (Pattern _ els) = length els == 2 && all (\(Pattern _ e) -> null e) els
let lens2 = GraphLens metaGraph isRelNode
let nodes2 = nodes lens2
-- Result: [patternWith "r1" [pattern "a", pattern "b"],
--          patternWith "r2" [pattern "b", pattern "c"]]
let rels2 = relationships lens2
-- Result: [patternWith "dep1" [patternWith "r1" [...], patternWith "r2" [...]]]
```

### Value-Based Predicates

```haskell
-- Predicate based on value prefix
let isPersonNode (Pattern v _) = "person:" `isPrefixOf` show v

let socialGraph = patternWith "social"
      [ pattern "person:Alice"
      , pattern "person:Bob"
      , pattern "place:NYC"
      , patternWith "knows" [pattern "person:Alice", pattern "person:Bob"]
      ]

let personLens = GraphLens socialGraph isPersonNode
let people = nodes personLens
-- Result: [pattern "person:Alice", pattern "person:Bob"]
```

### Context-Aware Predicates

```haskell
-- Predicate with captured context
let validIds = Set.fromList ["node_1", "node_2", "node_3"]
let isValidNode (Pattern v _) = show v `Set.member` validIds

let graph = patternWith "graph"
      [ pattern "node_1"
      , pattern "node_2"
      , pattern "invalid_node"
      , patternWith "rel" [pattern "node_1", pattern "node_2"]
      ]

let validatedLens = GraphLens graph isValidNode
let validNodes = nodes validatedLens
-- Result: [pattern "node_1", pattern "node_2"]
```

## Common Patterns

### Atomic Node Predicate

```haskell
-- Most common: atomic patterns (no elements) are nodes
isAtomic :: Pattern v -> Bool
isAtomic (Pattern _ els) = null els
```

### Value-Based Node Predicate

```haskell
-- Nodes identified by value prefix
isPersonNode :: Pattern String -> Bool
isPersonNode (Pattern v _) = "person:" `isPrefixOf` v
```

### Type-Based Node Predicate

```haskell
-- Nodes identified by value type
data NodeType = Person String | Place String | Thing String

isNodeType :: Pattern NodeType -> Bool
isNodeType (Pattern v _) = case v of
  Person _ -> True
  Place _ -> True
  Thing _ -> True
  _ -> False
```

## Edge Cases

### Empty Graph

```haskell
let emptyGraph = pattern "empty"
let emptyLens = GraphLens emptyGraph isAtomic

let nodes = nodes emptyLens  -- []
let rels = relationships emptyLens  -- []
```

### No Nodes

```haskell
-- All elements fail isNode predicate
let noNodesGraph = patternWith "graph"
      [ patternWith "x" [pattern "a"]
      , patternWith "y" [pattern "b"]
      ]

let lens = GraphLens noNodesGraph isAtomic
let nodes = nodes lens  -- [] (no atomic patterns)
let rels = relationships lens  -- [] (no relationships possible without nodes)
```

### Isolated Nodes

```haskell
let isolatedGraph = patternWith "graph"
      [ pattern "a"
      , pattern "b"
      -- no relationships
      ]

let lens = GraphLens isolatedGraph isAtomic
let neighborsOfA = neighbors lens (pattern "a")  -- []
let degreeOfA = degree lens (pattern "a")  -- 0
let components = connectedComponents lens
-- Result: [[pattern "a"], [pattern "b"]] (two separate components)
```

## Performance Tips

1. **Reuse Lenses**: Create a lens once and reuse it for multiple queries
2. **Filter Early**: Use specific predicates to reduce the scope of operations
3. **Consider Indexing**: For very large graphs, consider building an indexed variant (future work)

## Next Steps

- See `data-model.md` for detailed data structure documentation
- See `contracts/type-signatures.md` for complete API reference
- See `design/graph-lens.md` for design rationale and advanced patterns
