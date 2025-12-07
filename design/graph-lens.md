# Graph Lens Design Document

## Overview

A **Graph Lens** provides an interpretive view of a Pattern as a graph structure. Rather than defining graph concepts (nodes, relationships, walks) as intrinsic properties of Pattern, they emerge through the lens's interpretation. This design enables multiple graph views of the same Pattern and supports higher-order graphs where relationships or entire graphs become nodes.

## Core Design

### Graph Lens Definition

```haskell
data GraphLens v = GraphLens
  { scopePattern :: Pattern v
  , isNode       :: Pattern v -> Bool
  }
```

**Components**:
- **scopePattern**: Defines the boundary of all graph operations. Only the direct elements of this pattern are considered for graph structure.
- **isNode**: Predicate determining which direct elements are nodes. All other graph concepts derive from this single predicate.

### Design Principles

1. **Scope-bounded operations**: All graph operations only consider direct elements of `scopePattern`, never descending into nested structures.

2. **Single predicate foundation**: Only `isNode` is required. All other graph predicates (relationships, walks, etc.) are derived from this.

3. **Context captured at construction**: If a predicate needs context, that context must be captured when the predicate is created, not during evaluation.

4. **Interpretation, not intrinsic**: Graph structure is not a property of Pattern itself, but an interpretation through the lens.

## Derived Graph Concepts

### Nodes

**Definition**: Direct elements of `scopePattern` that satisfy the `isNode` predicate.

```haskell
nodes :: GraphLens v -> [Pattern v]
nodes (GraphLens (Pattern _ elements) isNode) = 
  filter isNode elements
```

### Relationships

**Definition**: A non-node pattern with exactly two node elements.

```haskell
isRelationship :: GraphLens v -> Pattern v -> Bool
isRelationship lens@(GraphLens _ isNode) p@(Pattern _ els) =
  not (isNode p) &&
  length els == 2 &&
  all isNode els

relationships :: GraphLens v -> [Pattern v]
relationships lens@(GraphLens (Pattern _ elements) isNode) = 
  [ p | p@(Pattern _ els) <- elements
      , not (isNode p)
      , length els == 2
      , all isNode els
  ]
```

**Directionality**: By convention, for directed relationships:
- `elements[0]` = source node
- `elements[1]` = target node

```haskell
source :: GraphLens v -> Pattern v -> Maybe (Pattern v)
source lens p@(Pattern _ (s:_))
  | isRelationship lens p = Just s
  | otherwise = Nothing

target :: GraphLens v -> Pattern v -> Maybe (Pattern v)
target lens p@(Pattern _ [_, t])
  | isRelationship lens p = Just t
  | otherwise = Nothing

reverseRel :: Pattern v -> Pattern v
reverseRel (Pattern v [a, b]) = Pattern v [b, a]
```

### Walks

**Definition**: A non-node pattern whose elements are all relationships, where consecutive relationships share nodes.

```haskell
isWalk :: GraphLens v -> Pattern v -> Bool
isWalk lens@(GraphLens _ isNode) p@(Pattern _ elements) =
  not (isNode p) &&
  all (isRelationship lens) elements &&
  consecutivelyConnected lens elements

consecutivelyConnected :: GraphLens v -> [Pattern v] -> Bool
consecutivelyConnected lens rels =
  and $ zipWith connects rels (tail rels)
  where
    connects r1 r2 = target lens r1 == source lens r2

walks :: GraphLens v -> [Pattern v]
walks lens@(GraphLens (Pattern _ elements) isNode) =
  [ p | p <- elements
      , not (isNode p)
      , all (isRelationship lens) (elementsOf p)
      , consecutivelyConnected lens (elementsOf p)
  ]
  where
    elementsOf (Pattern _ els) = els
```

**Walk Properties**:
```haskell
-- Extract nodes in traversal order
nodesInOrder :: GraphLens v -> Pattern v -> [Pattern v]
nodesInOrder lens (Pattern _ rels) = 
  case rels of
    [] -> []
    (r:rest) -> source lens r : mapMaybe (target lens) (r:rest)

-- Reverse a walk
reverseWalk :: Pattern v -> Pattern v
reverseWalk (Pattern v rels) = Pattern v (reverse $ map reverseRel rels)

-- Validate walk structure
isValidWalk :: Eq v => GraphLens v -> Pattern v -> Bool
isValidWalk lens p = isWalk lens p  -- already includes validation
```

## Usage Examples

### Basic Graph (Atomic Nodes)

```haskell
-- Simple graph with atomic nodes
simpleGraph = [graph | 
                [a],              -- atomic node
                [b],              -- atomic node  
                [c],              -- atomic node
                [r1 | [a], [b]],  -- relationship a->b
                [r2 | [b], [c]]   -- relationship b->c
              ]

-- Lens treating atomic patterns as nodes
atomicLens = GraphLens simpleGraph (\(Pattern _ els) -> null els)

-- Query results
nodes atomicLens         -- [[a], [b], [c]]
relationships atomicLens -- [[r1 | [a], [b]], [r2 | [b], [c]]]

-- Walk could be represented as
walk = [walk_meta | [r1 | [a], [b]], [r2 | [b], [c]]]
isWalk atomicLens walk  -- True
```

### Graph of Relationships (Meta-Graph)

Relationships from one graph become nodes in a higher-level graph:

```haskell
-- Meta-graph where relationships become nodes
metaGraph = [meta |
              [r1 | [a], [b]],     -- relationship (node at this level)
              [r2 | [b], [c]],     -- relationship (node at this level)
              [r3 | [c], [d]],     -- relationship (node at this level)
              [dep1 | [r1], [r2]], -- dependency: r1 depends on r2
              [dep2 | [r2], [r3]]  -- dependency: r2 depends on r3
            ]

-- Lens that treats composite patterns with 2 atomic elements as nodes
relAsNodeLens = GraphLens metaGraph $ \p@(Pattern _ els) ->
  length els == 2 && all isAtomic els
  where 
    isAtomic (Pattern _ []) = True
    isAtomic _ = False

-- Query results
nodes relAsNodeLens         -- [[r1|...], [r2|...], [r3|...]]
relationships relAsNodeLens -- [[dep1|...], [dep2|...]]

-- Now we have "relationships between relationships"!
-- dep1 represents: r1 -> r2 (at the meta level)
```

### Graph of Graphs

Entire subgraphs become nodes in a system-level graph:

```haskell
-- System of interconnected subsystems
system = [system |
           [subsys1 | [n1], [n2], [e1 | [n1], [n2]]],  -- subgraph 1
           [subsys2 | [n3], [n4], [e2 | [n3], [n4]]],  -- subgraph 2
           [interface1 | [subsys1], [subsys2]],        -- connection
           [subsys3 | [n5], [n6], [e3 | [n5], [n6]]],  -- subgraph 3
           [interface2 | [subsys2], [subsys3]]         -- connection
         ]

-- Subsystems-as-nodes lens
subsystemLens = GraphLens system $ \p@(Pattern v _) ->
  "subsys" `isPrefixOf` v

-- System-level view
nodes subsystemLens         -- [[subsys1|...], [subsys2|...], [subsys3|...]]
relationships subsystemLens -- [[interface1|...], [interface2|...]]

-- Each subsystem can have its own lens for internal structure
subsys1Lens = GraphLens subsys1 isAtomic
-- nodes subsys1Lens = [[n1], [n2]]
-- relationships subsys1Lens = [[e1|...]]
```

### Predicate with Captured Context

```haskell
-- Context-aware node predicate
let validIds = Set.fromList ["node_1", "node_2", "node_3"]
    isValidNode (Pattern v _) = v `Set.member` validIds
    lens = GraphLens scope isValidNode

-- Schema-based node predicate
let schema = loadSchema "graph-schema.json"
    nodeMatchesSchema (Pattern v _) = 
      case parseValue v of
        Just val -> validateAgainstSchema schema val
        Nothing -> False
    lens = GraphLens scope nodeMatchesSchema

-- Type-based node predicate
data NodeType = Person | Place | Thing
let isNodeType (Pattern v _) = 
      case v of
        Person _ -> True
        Place _ -> True
        Thing _ -> True
        _ -> False
    lens = GraphLens scope isNodeType
```

## Graph Operations

### Basic Queries

```haskell
-- Extract all nodes
nodes :: GraphLens v -> [Pattern v]

-- Extract all relationships  
relationships :: GraphLens v -> [Pattern v]

-- Extract all walks
walks :: GraphLens v -> [Pattern v]

-- Check if pattern is a node/relationship/walk
isNode :: GraphLens v -> Pattern v -> Bool
isRelationship :: GraphLens v -> Pattern v -> Bool
isWalk :: GraphLens v -> Pattern v -> Bool
```

### Navigation

```haskell
-- Find neighbors of a node
neighbors :: Eq v => GraphLens v -> Pattern v -> [Pattern v]
neighbors lens node = 
  [ target lens r | r <- relationships lens
                  , source lens r == Just node ]
  ++
  [ source lens r | r <- relationships lens
                  , target lens r == Just node ]

-- Find all relationships involving a node
incidentRels :: Eq v => GraphLens v -> Pattern v -> [Pattern v]
incidentRels lens node =
  [ r | r <- relationships lens
      , source lens r == Just node || target lens r == Just node ]

-- Node degree
degree :: Eq v => GraphLens v -> Pattern v -> Int
degree lens node = length (incidentRels lens node)
```

### Graph Analysis

```haskell
-- Find connected components
connectedComponents :: Eq v => GraphLens v -> [[Pattern v]]
connectedComponents lens = 
  components (nodes lens) Set.empty
  where
    components [] _ = []
    components (n:ns) visited
      | n `Set.member` visited = components ns visited
      | otherwise = 
          let component = bfs lens [n] Set.empty
              newVisited = visited `Set.union` Set.fromList component
          in component : components ns newVisited

-- Breadth-first search from node
bfs :: Eq v => GraphLens v -> [Pattern v] -> Set (Pattern v) -> [Pattern v]
bfs lens queue visited
  | null queue = Set.toList visited
  | otherwise = 
      let (current:rest) = queue
          newNeighbors = filter (`Set.notMember` visited) (neighbors lens current)
          newVisited = Set.insert current visited
      in bfs lens (rest ++ newNeighbors) newVisited

-- Find path between nodes
findPath :: Eq v => GraphLens v -> Pattern v -> Pattern v -> Maybe [Pattern v]
findPath lens start end = search [start] Set.empty Map.empty
  where
    search [] _ _ = Nothing
    search (current:queue) visited parents
      | current == end = Just (reconstructPath parents end)
      | current `Set.member` visited = search queue visited parents
      | otherwise =
          let neighs = neighbors lens current
              newParents = Map.union parents 
                           (Map.fromList [(n, current) | n <- neighs])
              newVisited = Set.insert current visited
          in search (queue ++ neighs) newVisited newParents
    
    reconstructPath parents node =
      case Map.lookup node parents of
        Nothing -> [node]
        Just parent -> reconstructPath parents parent ++ [node]
```

### Walk Operations

```haskell
-- Validate walk structure
isValidWalk :: Eq v => GraphLens v -> Pattern v -> Bool

-- Extract nodes from walk in traversal order
walkNodes :: GraphLens v -> Pattern v -> [Pattern v]
walkNodes lens walk@(Pattern _ rels) 
  | isWalk lens walk = nodesInOrder lens walk
  | otherwise = []

-- Check if walk is simple (no repeated nodes)
isSimpleWalk :: Eq v => GraphLens v -> Pattern v -> Bool
isSimpleWalk lens walk = 
  let ns = walkNodes lens walk
  in length ns == length (nub ns)

// Check if walk is a cycle
isCycle :: Eq v => GraphLens v -> Pattern v -> Bool
isCycle lens walk@(Pattern _ rels) =
  isWalk lens walk && 
  case (rels, reverse rels) of
    (r1:_, rn:_) -> source lens r1 == target lens rn
    _ -> False
```

## Integration with Pattern Matching DSL

The Graph Lens integrates with the Pattern Matching DSL by providing lens-aware pattern expressions:

```haskell
-- Lens-aware pattern constructors
data PatternExpr v where
  PNode :: PatternExpr v                        -- Matches nodes per lens
  PRel  :: PatternExpr v                        -- Matches relationships per lens
  PWalk :: PatternExpr v                        -- Matches walks per lens
  -- ... other constructors from DSL

-- Matching requires lens context
match :: GraphLens v -> PatternExpr v -> [MatchResult v]
match lens expr = 
  let candidates = case expr of
        PNode -> nodes lens
        PRel -> relationships lens
        PWalk -> walks lens
        -- ... other cases
  in filter (matches expr) candidates

-- Example queries
-- Find all nodes with degree > 2
highDegreeNodes lens = 
  [ n | n <- nodes lens, degree lens n > 2 ]

-- Find all triangles (3-cycles)
triangles lens = 
  [ w | w <- walks lens
      , length (walkNodes lens w) == 3
      , isCycle lens w ]

-- Find all walks of length 3
threeHopWalks lens =
  [ w | w <- walks lens
      , length (elementsOf w) == 3 ]
  where elementsOf (Pattern _ els) = els
```

## Advanced Patterns

### Multiple Lens Views

The same Pattern can be interpreted through different lenses:

```haskell
pattern = [root | [a], [b], [r | [a], [b]], [meta_r | [r]]]

-- View 1: Atomic nodes
lens1 = GraphLens pattern (\(Pattern _ els) -> null els)
nodes lens1 -- [[a], [b]]

-- View 2: All single-element patterns as nodes
lens2 = GraphLens pattern (\(Pattern _ els) -> length els <= 1)
nodes lens2 -- [[a], [b], [r|...], [meta_r|...]]

-- View 3: Only patterns with "meta" prefix
lens3 = GraphLens pattern (\(Pattern v _) -> "meta" `isPrefixOf` v)
nodes lens3 -- [[meta_r|...]]
```

### Hierarchical Graph Navigation

While lens composition is deferred, hierarchical navigation is natural:

```haskell
-- System with subsystems
system = [sys | [sub1 | [n1], [n2]], [sub2 | [n3], [n4]]]

-- Top-level lens
topLens = GraphLens system (\(Pattern v _) -> "sub" `isPrefixOf` v)
topNodes = nodes topLens  -- [[sub1|...], [sub2|...]]

// Create lens for subsystem
let [sub1Pattern, _] = topNodes
    sub1Lens = GraphLens sub1Pattern isAtomic
    sub1Nodes = nodes sub1Lens  -- [[n1], [n2]]
```

### Custom Graph Interpretations

```haskell
-- Bipartite graph: distinguish two node types
data BipartiteValue = TypeA String | TypeB String | Edge String

bipartiteGraph = [graph |
                   [TypeA "a1"], [TypeA "a2"],
                   [TypeB "b1"], [TypeB "b2"],
                   [Edge "e1" | [TypeA "a1"], [TypeB "b1"]],
                   [Edge "e2" | [TypeA "a2"], [TypeB "b2"]]]

-- Lens for type A nodes only
typeALens = GraphLens bipartiteGraph $ \(Pattern v _) ->
  case v of
    TypeA _ -> True
    _ -> False

-- Lens for type B nodes only  
typeBLens = GraphLens bipartiteGraph $ \(Pattern v _) ->
  case v of
    TypeB _ -> True
    _ -> False

-- Full bipartite lens (both types are nodes)
fullLens = GraphLens bipartiteGraph $ \(Pattern v _) ->
  case v of
    TypeA _ -> True
    TypeB _ -> True
    _ -> False
```

## Design Rationale

### Why Single Predicate?

Starting with only `isNode` keeps the design minimal and forces clarity about what constitutes the fundamental graph element. All other concepts derive naturally:

- **Relationships**: Non-nodes connecting two nodes
- **Walks**: Non-nodes containing connected relationships
- **Hypergraphs**: Non-nodes connecting more than two nodes

### Why Scope-Bounded?

Operating only on direct elements of `scopePattern` provides:

1. **Clear boundaries**: No ambiguity about what's "in" the graph
2. **Composability**: Nested patterns can have their own lenses
3. **Performance**: No recursive traversal needed
4. **Predictability**: Same predicate, same scope → same results

### Why Context at Construction?

Requiring context to be captured when predicates are created:

1. **Purity**: Predicates remain pure functions
2. **Clarity**: All dependencies explicit
3. **Testability**: Predicates can be tested in isolation
4. **Composability**: Predicates can be combined without hidden state

## Future Considerations

### Lens Composition (Deferred)

Potential patterns for combining lenses:

```haskell
-- Hypothetical: nodes in lens1 that are relationships in lens2
liftNodes :: GraphLens v -> GraphLens v -> GraphLens v

-- Hypothetical: hierarchical lens navigation
containing :: GraphLens v -> GraphLens v -> GraphLens v

-- Hypothetical: lens intersection/union
intersect :: GraphLens v -> GraphLens v -> GraphLens v
union :: GraphLens v -> GraphLens v -> GraphLens v
```

### Incremental Queries

For large graphs, consider:

```haskell
-- Indexed lens for efficient queries
data IndexedLens v = IndexedLens
  { lens :: GraphLens v
  , nodeIndex :: Map v (Pattern v)
  , adjIndex :: Map (Pattern v) [Pattern v]
  }

buildIndex :: GraphLens v -> IndexedLens v
```

### Temporal Graphs

Lenses over time-varying patterns:

```haskell
data TemporalLens v = TemporalLens
  { snapshots :: [(Time, GraphLens v)]
  , isNode :: Pattern v -> Bool
  }

atTime :: TemporalLens v -> Time -> GraphLens v
between :: TemporalLens v -> Time -> Time -> [GraphLens v]
```

## Summary

**Graph Lens** provides a flexible, composable way to interpret Pattern structures as graphs:

- **Single predicate foundation**: Only `isNode` is required
- **Scope-bounded operations**: Clear boundaries, no implicit traversal
- **Context at construction**: Pure predicates with explicit dependencies
- **Multiple interpretations**: Same Pattern, different lenses, different graphs
- **Natural hierarchies**: Relationships-as-nodes, graphs-as-nodes emerge naturally
- **DSL integration**: Lens-aware pattern matching for graph queries

The design treats graph structure as an **interpretation** rather than an intrinsic property, enabling rich multi-scale graph analysis while maintaining Pattern's simplicity and composability.