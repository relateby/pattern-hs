# Patterns as Graph Views: A Categorical Framework

**Status**: ✅ Implemented  
**Implementation**: Core Pattern type and typeclass instances are implemented. Graph Lens (Feature 23) is implemented. Some planned features (Zipper, Pattern Morphisms) are deferred.

## Core Data Structure

Patterns form a recursive data structure representing decorated sequences that can be interpreted as graphs through different views:

```haskell
data Pattern v = Pattern 
  { value    :: v              -- Decoration about what kind of pattern it is
  , elements :: [Pattern v]    -- The pattern itself, as a sequence of elements
  }
  deriving (Eq)
```

**Note**: All typeclass instances are ✅ Implemented. See `docs/reference/SPECIFICATION.md` for current implementation status.

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

Patterns can be **interpreted** as graph elements through different views. These are interpretations/views of pattern structures, not pattern variants themselves.

### Nodes

**Definition**: A node is an atomic pattern - a pattern with no elements.

**Representation**:
```haskell
node :: v -> Pattern v
node v = Pattern v []
```

**Notation**:
```
Cypher:  (n:Person)
Pattern: [n:Person]
```

**Properties**:
- Nodes are leaves in the Pattern tree
- Node identity is determined by the value `v`
- All atomic patterns are valid nodes

### Relationships

#### Undirected Relationships

**Definition**: An undirected relationship is a pattern with exactly two atomic patterns as elements.

**Representation**:
```haskell
undirectedRel :: v -> Pattern v -> Pattern v -> Pattern v
undirectedRel relValue node1 node2 = Pattern relValue [node1, node2]
```

**Notation**:
```
Cypher:  (a)-[r:KNOWS]-(b)
Pattern: [r:KNOWS | (a), (b)]
```

**Semantic Interpretation**: The order of elements is not semantically meaningful for undirected relationships. `[r | (a), (b)]` and `[r | (b), (a)]` represent the same undirected relationship.

#### Directed Relationships

**Definition**: A directed relationship is a pattern with exactly two atomic patterns as elements, where element order encodes direction. By convention:
- **Element[0]** = source node
- **Element[1]** = target node

**Representation**:
```haskell
directedRel :: v -> Pattern v -> Pattern v -> Pattern v
directedRel relValue source target = Pattern relValue [source, target]

-- Accessors
source :: Pattern v -> Pattern v
source (Pattern _ (s:_)) = s

target :: Pattern v -> Pattern v
target (Pattern _ [_, t]) = t

-- Reverse direction
reverse :: Pattern v -> Pattern v
reverse (Pattern v [a, b]) = Pattern v [b, a]
```

**Notation**:
```
Cypher:  (a)-[r:KNOWS]->(b)
Pattern: [r:KNOWS | (a), (b)]

Cypher:  (a)<-[r:KNOWS]-(b)
Pattern: [r:KNOWS | (b), (a)]
```

**Design Rationale**:
1. **Natural use of ordered sequences**: Pattern's fundamental property (ordered elements) directly encodes direction
2. **Structural distinction**: Directed vs. undirected relationships are distinguished at the semantic layer, not the structural layer
3. **No value pollution**: The relationship value remains pure domain metadata (type, properties, etc.)
4. **Clean operations**: Reversing, extracting source/target are simple structural operations

### Walks

**Definition**: A walk is an ordered sequence of relationships where consecutive relationships allow for a continuous traversal. This means entering a relationship at one node (the "entry" node) and exiting at the other (the "exit" node), which becomes the entry node for the next relationship.

**Representation**: A pattern whose elements are relationships (patterns with exactly two elements).

```haskell
walk :: v -> [Pattern v] -> Pattern v
walk walkMeta relationships = Pattern walkMeta relationships
```

**Notation**:
```
Cypher:  (a)-[r1:KNOWS]->(b)<-[r2:LIKES]-(c)-[r3:FOLLOWS]->(d)

Pattern: [walk_metadata | 
           [r1:KNOWS | (a), (b)],
           [r2:LIKES | (c), (b)],
           [r3:FOLLOWS | (c), (d)]
         ]
```

Valid walks, in Pattern and Path notation equivalents:

```
[walk | [r1 | a, b]] =~ [walk | (a)-[r1]->(b)]

[walk | [r1 | a, b], [r2 | c, b]] =~ (a)-[r1]->(b)<-[r2]-(c)

[walk | [r1 | a, b], [r2 | b, a]] =~ (a)-[r1]->(b)-[r2]->(a)

[walk | [r1 | a, b], [r2 | a, b]] =~ (a)-[r1]->(b)<-[r2]-(a)
```

### Summary Table

| Concept | Pattern Structure | Convention |
|---------|------------------|------------|
| Node | `[v]` | Atomic pattern (no elements) |
| Undirected Rel | `[r \| (a), (b)]` | Order semantically irrelevant |
| Directed Rel | `[r \| (a), (b)]` | Element[0]=source, Element[1]=target |
| Walk | `[meta \| rel1, rel2, ...]` | Consecutive rels share nodes |

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

-- DOM-like parent chain access
-- Returns list of parent values from immediate parent to root
-- Similar to DOM's parentElement.parentElement... chain
parents :: Zipper v -> [v]
parents (Zipper _ ctx) = 
  parent ctx : case above ctx of
    Nothing -> []
    Just parentCtx -> parents (Zipper (Pattern (parent ctx) []) parentCtx)

-- Alternative name: 'ancestors' (more tree-theoretic terminology)
-- Returns list of parent Patterns from immediate parent to root
ancestors :: Zipper v -> [Pattern v]
ancestors (Zipper focus ctx) = 
  let parentPattern = Pattern (parent ctx) (left ctx ++ [focus] ++ right ctx)
  in parentPattern : case above ctx of
    Nothing -> []
    Just parentCtx -> ancestors (Zipper parentPattern parentCtx)
```

**Status**: ⏳ Planned (future work)

**Note on `parents()` / `ancestors()`**: 
- `parents :: Zipper v -> [v]` returns a list of parent **values** (decoration values) from immediate parent to root, similar to DOM's `parentElement.parentElement...` chain
- `ancestors :: Zipper v -> [Pattern v]` returns a list of parent **Patterns** (full structures) from immediate parent to root
- Both functions walk up the `above :: Maybe (Context v)` chain
- **Naming consideration**: `parents` is familiar from DOM APIs, while `ancestors` is more tree-theoretic. Both are valid; choose based on API style preference.
- This functionality belongs in Zipper (not Comonad) because it requires explicit parent context storage, which Zipper provides via the `above` field

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
