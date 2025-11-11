# Quickstart: Pattern Data Structure

**Feature**: 001-pattern-data-structure  
**Date**: 2025-01-27

## Overview

This quickstart guide demonstrates how to use the Pattern data structure to represent and work with graph elements. The Pattern type is a recursive tree structure that can be interpreted as nodes, relationships, subgraphs, and paths through different categorical views.

---

## Installation

Once the project is built and published:

```bash
# Using Cabal
cabal install pattern

# Or add to your project's dependencies
# In your .cabal file:
build-depends: pattern >= 0.1.0.0
```

---

## Basic Usage

### Creating Patterns

#### Simple Node

```haskell
import Pattern

-- Create a node with value "A"
nodeA :: Pattern String
nodeA = Pattern "A" []
```

#### Relationship

```haskell
-- Create a relationship from node A to node B
nodeB = Pattern "B" []
relAB = Pattern "rel" [nodeA, nodeB]
```

#### Subgraph

```haskell
-- Create a subgraph containing multiple relationships
nodeC = Pattern "C" []
relBC = Pattern "rel" [nodeB, nodeC]
subgraph = Pattern "graph" [relAB, relBC]
```

---

## Graph Element Classification

### Checking Pattern Types

```haskell
-- Check if a pattern is a node
isNode nodeA        -- True
isNode relAB        -- False

-- Check if a pattern is a relationship
isRelationship relAB    -- True
isRelationship nodeA    -- False

-- Check if a pattern is a subgraph
isSubgraph subgraph     -- True
isSubgraph nodeA        -- False

-- Check if a pattern is a path
isPath pathPattern      -- True (if it chains correctly)
```

### Extracting Graph Elements

```haskell
-- Get source and target of a relationship
sourceNode = source relAB  -- Returns nodeA
targetNode = target relAB  -- Returns nodeB

-- Get all nodes in a pattern
allNodes = nodes subgraph  -- [nodeA, nodeB, nodeC]

-- Get all relationships in a pattern
allRels = relationships subgraph  -- [relAB, relBC]
```

---

## Graph Views

### Directed Graph View

```haskell
import Pattern.Views

-- Create a directed graph view
directedView = DirectedView

-- Interpret pattern as directed graph
directedGraph :: Graph Ordered String
directedGraph = toGraph directedView subgraph

-- Check if relationships can chain
canChain directedView relAB relBC  -- True if target(relAB) == source(relBC)
```

### Undirected Graph View

```haskell
-- Create an undirected graph view
undirectedView = UndirectedView

-- Interpret pattern as undirected graph
undirectedGraph :: Graph Unordered String
undirectedGraph = toGraph undirectedView subgraph
```

---

## Pattern Morphisms

### Value Transformation

```haskell
-- Transform pattern values using fmap (Functor instance)
upperPattern :: Pattern String -> Pattern String
upperPattern = fmap (map toUpper)

-- Apply transformation
nodeAUpper = upperPattern nodeA  -- Pattern "A" [] -> Pattern "A" []
```

### Homomorphism

```haskell
import Pattern.Morphisms

-- Create a homomorphism that transforms values
toLength :: Pattern String -> Pattern Int
toLength = homomorphism length

lengthPattern = toLength nodeA  -- Pattern "A" [] -> Pattern 1 []
```

### Forgetful Morphism

```haskell
-- Forget values, preserve structure
structureOnly :: Pattern String -> Pattern ()
structureOnly = forget

forgotten = structureOnly subgraph  -- Pattern () [Pattern () [...], ...]
```

---

## Complete Example: Building a Graph

```haskell
import Pattern
import Pattern.Views
import qualified Data.Set as Set

-- Step 1: Create nodes
nodeA = Pattern "A" []
nodeB = Pattern "B" []
nodeC = Pattern "C" []

-- Step 2: Create relationships
relAB = Pattern "knows" [nodeA, nodeB]
relBC = Pattern "knows" [nodeB, nodeC]

-- Step 3: Create subgraph
graph = Pattern "social" [relAB, relBC]

-- Step 4: Interpret as directed graph
directedView = DirectedView
socialGraph = toGraph directedView graph

-- Step 5: Work with the graph
allNodesInGraph = Set.toList $ nodes socialGraph
allEdgesInGraph = Set.toList $ edges socialGraph
```

---

## Testing Your Patterns

### Property-Based Testing

```haskell
import Test.QuickCheck
import Pattern

-- Test functor laws
prop_functorIdentity :: Pattern Int -> Bool
prop_functorIdentity p = fmap id p == p

prop_functorComposition :: Pattern Int -> (Int -> Int) -> (Int -> Int) -> Bool
prop_functorComposition p f g = fmap (f . g) p == (fmap f . fmap g) p

-- Test pattern classification
prop_nodeHasNoRelationships :: Pattern String -> Bool
prop_nodeHasNoRelationships p = 
  isNode p == (null $ relationships p)
```

---

## Common Patterns

### Pattern Matching

```haskell
-- Pattern match on Pattern structure
case somePattern of
  Pattern v [] -> -- This is an atomic pattern
  Pattern v [elem] -> -- Singular pattern (one element)
  Pattern v [left, right] -> -- Possibly a relationship
  Pattern v elems -> -- Multiple elements (subgraph)
```

### Transforming Patterns

```haskell
-- Map over all values in a pattern tree
transformValues :: (a -> b) -> Pattern a -> Pattern b
transformValues = fmap

-- Fold over pattern structure
sumValues :: Num a => Pattern a -> a
sumValues = foldr (+) 0 . fmap id
```

---

## Next Steps

1. **Read the Design Document**: See `DESIGN.md` for the category-theoretic foundations
2. **Explore Views**: Create custom `GraphView` instances for your specific use cases
3. **Study Morphisms**: Understand how pattern transformations preserve structure
4. **Property Testing**: Write property-based tests for category-theoretic laws

---

## Troubleshooting

### Common Issues

**Issue**: "Pattern is not a relationship" error
- **Solution**: Ensure the pattern has exactly 2 elements, and both are nodes

**Issue**: "Cannot chain relationships"
- **Solution**: Verify that `target r1 == source r2` for directed views, or that relationships share nodes for undirected views

**Issue**: Type errors with GraphView
- **Solution**: Ensure you're using the correct view instance and that the `Direction` associated type matches

---

## References

- **Design Document**: `DESIGN.md` - Category-theoretic framework
- **Data Model**: `data-model.md` - Detailed entity definitions
- **Type Signatures**: `contracts/type-signatures.md` - Complete API reference

