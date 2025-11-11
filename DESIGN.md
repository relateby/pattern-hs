# Patterns as Graph Views: A Categorical Framework

## Core Data Structure

Patterns form a recursive data structure representing decorated sequences that can be interpreted as graphs through different views:

```haskell
data Pattern v = Pattern 
  { value    :: v              -- Decoration about what kind of pattern it is
  , elements :: [Pattern v]    -- The pattern itself, as a sequence of elements
  }
  deriving (Eq)
```

**Note**: `Show` is implemented as a manual instance. `Functor`, `Foldable`, and `Traversable` are ⏳ Planned but not yet implemented.

**Key Insight**: The `elements` field IS the pattern - it contains the sequence that defines the pattern. The `value` field provides decoration about what kind of pattern it is. For example, the pattern "A B B A" with decoration "Enclosed rhyme" represents a specific sequence pattern (A B B A) that is classified as an "Enclosed rhyme".

## Sequence vs Tree: Conceptual Model and Implementation

Patterns have two complementary views: the **conceptual model** (decorated sequences) and the **implementation model** (recursive trees). Understanding their relationship is essential:

### Primary Semantic: Decorated Sequences

**Conceptually**, patterns are decorated sequences where:
- The `elements` field IS the pattern - it contains the sequence that defines the pattern
- The `value` field provides decoration about what kind of pattern it is
- Elements maintain their sequence order - this order is essential to the pattern
- Each element in the sequence is itself a Pattern, enabling nested patterns

The sequence semantic is primary because:
- Patterns are fundamentally about ordered sequences (e.g., "A B B A")
- The order of elements matters for pattern matching
- Sequence operations (length, indexing, concatenation) are natural operations on patterns

### Implementation Detail: Recursive Tree Structure

**Implementation-wise**, patterns are represented as recursive trees:
- The tree structure is how sequences are represented in memory
- Each tree node stores a decoration (value) and contains the pattern elements as a list
- The recursive structure enables arbitrary nesting depth
- Tree traversal provides access to sequence elements in order

### Relationship Between Models

The tree implementation **supports** the sequence semantic:
- Tree nodes store sequences (lists) of pattern elements
- Tree traversal preserves sequence order
- The recursive structure enables nested sequences (patterns containing patterns)
- Sequence operations (ordering, length, access by position) are implemented via tree operations

**Key Principle**: Conceptually, developers should think of patterns as decorated sequences where elements form the pattern itself. The tree structure is an implementation detail that supports sequence operations. There is no contradiction between these views - the tree is simply how sequences are represented in memory.

## Pattern Structural Classifications

Patterns have structural classifications based on their element structure. These describe what patterns **are** structurally, not how they are interpreted:

### Atomic Pattern

A pattern with no elements (`elements == []`). Atomic patterns are the fundamental building blocks from which all other patterns are constructed.

**Structure**: Empty sequence  
**Status**: ✅ Implemented (this is the basic Pattern structure)

### Singular Pattern

A pattern with exactly one element (`length (elements p) == 1`). Singular patterns contain a single element in their sequence.

**Structure**: Sequence with exactly one element  
**Status**: ✅ Implemented (this is the basic Pattern structure)

### Pattern with Elements

A pattern containing one or more pattern elements in sequence.

**Structure**: Non-empty sequence of patterns  
**Status**: ✅ Implemented (this is the basic Pattern structure)

### Nested Pattern

A pattern containing patterns that themselves contain patterns, enabling arbitrary nesting depth.

**Structure**: Recursive nesting of patterns  
**Status**: ✅ Implemented (this is the basic Pattern structure)

**Note**: Patterns are a data structure for representing graphs (like an adjacency matrix or adjacency list), optimized for expressiveness of layered, hierarchical graph structures rather than performance optimization over a single, "flat" graph.

## Graph Interpretations (Views)

Patterns can be **interpreted** as graph elements through different views. These are interpretations/views of pattern structures, not pattern variants themselves. The following interpretation functions are planned but not yet implemented:

```haskell
-- ⏳ Planned: Graph interpretation functions
-- These interpret pattern structures as graph elements through views

-- ⏳ Planned: Check if pattern is a graph element (node, relationship, subgraph, or path)
isGraphElement :: Pattern v -> Bool

-- ⏳ Planned: Interpret pattern as a node (typically atomic pattern)
isNode :: Pattern v -> Bool
isNode p = all (not . isGraphElement) (elements p)

-- ⏳ Planned: Interpret pattern as a relationship (typically 2 elements that are nodes)
isRelationship :: Pattern v -> Bool
isRelationship p = length (elements p) == 2 && all isNode (elements p)

-- ⏳ Planned: Interpret pattern as a subgraph (all elements are graph elements)
isSubgraph :: Pattern v -> Bool
isSubgraph p = all isGraphElement (elements p)

-- ⏳ Planned: Interpret pattern as a path (subgraph with chained relationships)
isPath :: Pattern v -> Bool
isPath p = isSubgraph p && chainsCorrectly (elements p)
  where
    chainsCorrectly [] = True
    chainsCorrectly [_] = True
    chainsCorrectly (r1:r2:rs) = 
      isRelationship r1 && isRelationship r2 &&
      target r1 == source r2 && chainsCorrectly (r2:rs)
```

**Status**: ⏳ Planned (graph interpretation functions not yet implemented)

**Note**: These functions interpret pattern structures as graph elements. Patterns themselves are decorated sequences; graph interpretations (nodes, relationships, subgraphs, paths) are views of those structures, not pattern variants.

## Pattern Navigation Functions

Navigation functions extract graph elements from patterns. These functions are planned but not yet implemented:

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

**Status**: ⏳ Planned (navigation functions not yet implemented)

## Category Theoretic Perspective

### Patterns as a Free Structure

The Pattern type forms a category **Pat** where:
- Objects: Individual patterns
- Morphisms: Structural transformations preserving pattern shape

### Graph Views as Functors

Each graph view defines a functor F: **Pat** → **Graph_k** for some specific graph category:

```haskell
class GraphView view where
  type Direction view :: *
  
  -- Interpret pattern structure
  interpretNode :: view -> Pattern v -> Bool
  interpretRel  :: view -> Pattern v -> Bool
  
  -- Extract graph semantics  
  direction :: view -> Pattern v -> Direction view
  canChain  :: view -> Pattern v -> Pattern v -> Bool
  
  -- The functor mapping
  toGraph :: view -> Pattern v -> Graph (Direction view) v
```

## Standard Views (Planned)

Graph views provide different semantic interpretations of pattern structures. Views are planned but not yet implemented.

### Directed Graph View (Planned)

```haskell
data DirectedView = DirectedView

instance GraphView DirectedView where
  type Direction DirectedView = Ordered
  
  direction _ r = Directed (elements r !! 0) (elements r !! 1)
  canChain _ r1 r2 = target r1 == source r2
```

**Status**: ⏳ Planned (not yet implemented)

### Undirected Graph View (Planned)

```haskell
data UndirectedView = UndirectedView

instance GraphView UndirectedView where
  type Direction UndirectedView = Unordered
  
  direction _ r = Undirected (Set.fromList $ elements r)
  canChain _ r1 r2 = not $ Set.null $ 
    Set.intersection (nodes r1) (nodes r2)
```

**Status**: ⏳ Planned (not yet implemented)

## Forgetful Pattern Matching

Views form a hierarchy of forgetful functors:

```
Pat[Full] --F₁--> Pat[Shape] --F₂--> Pat[Topology] --F₃--> Pat[Connected]
```

Where each functor forgets information:

```haskell
-- Forget decorations, preserve structure
forgetValues :: Pattern v -> Pattern ()
forgetValues = fmap (const ())

-- Forget direction, preserve connectivity  
forgetDirection :: GraphView view => view -> Graph Ordered v -> Graph Unordered v

-- Forget specific edges, preserve paths
forgetPaths :: Graph dir v -> ConnectivityClass
```

## Composition and Navigation

### Zipper for Focus (future work)

```haskell
data Zipper v = Zipper
  { focus   :: Pattern v
  , context :: Context v
  }

data Context v = Context
  { parent  :: v
  , left    :: [Pattern v]
  , right   :: [Pattern v]  
  , above   :: Maybe (Context v)
  }
```

**Status**: ⏳ Planned (future work)

### Pattern Morphisms (Planned)

Morphisms between patterns respect structure while potentially forgetting decorations:

```haskell
type PatternMorphism v w = Pattern v -> Pattern w

-- Structure-preserving map
homomorphism :: (v -> w) -> PatternMorphism v w
homomorphism f = fmap f

-- Forgetful morphism  
forget :: PatternMorphism v ()
forget = forgetValues
```

**Status**: ⏳ Planned (not yet implemented)

## Key Properties

1. **Schema-lazy**: Patterns don't commit to specific graph semantics; interpretation happens in the view
2. **Compositional**: Views can be composed, stacked, or swapped without changing underlying patterns  
3. **Open-ended**: New views can be defined for any graph-like interpretation
4. **Categorical**: Each view defines a functor; forgetful pattern matching uses functor composition

## Example: Multiple Interpretations

```haskell
-- Same pattern, different views
let p = Pattern "graph" 
          [ Pattern "A" []
          , Pattern "rel" [Pattern "A" [], Pattern "B" []]
          , Pattern "B" []
          ]

-- As directed graph: A → B
directedGraph = toGraph DirectedView p

-- As undirected graph: A — B  
undirectedGraph = toGraph UndirectedView p

-- Custom view using decorations
data WeightedView = WeightedView

instance GraphView WeightedView where
  -- Use value v to determine direction
  direction view rel = 
    if weight (value rel) > 0.5 
    then Directed (source rel) (target rel)
    else Undirected (Set.fromList [source rel, target rel])
```

## Analogical Reasoning via Forgetful Matching

```haskell
-- Match patterns that are "similar" under forgetting
analogicalMatch :: (GraphView v1, GraphView v2) 
                => v1 -> v2 
                -> Pattern a -> Pattern b 
                -> Bool
analogicalMatch view1 view2 p1 p2 =
  toGraph view1 (forget p1) `isIsomorphic` toGraph view2 (forget p2)
```

This framework provides a principled way to handle graph-like structures while maintaining flexibility in interpretation, enabling both strict pattern matching and analogical reasoning through categorical abstractions.

