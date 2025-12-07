# Data Model: Graph Lens

**Feature**: 023-graph-lens  
**Date**: 2025-01-27  
**Phase**: 1 - Design

## Overview

The Graph Lens data model provides a minimal, elegant interpretation of Pattern structures as graphs. The design follows a single predicate foundation where all graph concepts (nodes, relationships, walks) derive from a single `isNode` predicate. This document describes the core data structures and their relationships.

## Core Data Structures

### GraphLens

**Definition**: The primary data structure that defines a graph interpretation of a Pattern.

```haskell
data GraphLens v = GraphLens
  { scopePattern :: Pattern v
  , isNode       :: Pattern v -> Bool
  }
```

**Fields**:
- `scopePattern`: The Pattern that defines the boundary for all graph operations. Only direct elements of this pattern are considered for graph structure.
- `isNode`: A predicate function that determines which direct elements of `scopePattern` are nodes. All other graph concepts derive from this predicate.

**Invariants**:
- `isNode` must be a pure function (no side effects, deterministic)
- All graph operations only consider direct elements of `scopePattern`, never nested structures
- The same Pattern can have multiple GraphLens instances with different interpretations

**Categorical Interpretation**: GraphLens provides a functorial interpretation where Pattern structures are transformed into graph interpretations. The transformation Pattern → Graph interpretation is functorial in nature.

---

### Node

**Definition**: A direct element of `scopePattern` that satisfies the `isNode` predicate.

**Type**: `Pattern v` (not a separate type - nodes are Patterns that satisfy the predicate)

**Properties**:
- Nodes are identified by the `isNode` predicate
- Nodes are the fundamental graph elements
- All other graph concepts (relationships, walks) are defined in terms of nodes

**Derivation**:
```haskell
nodes :: GraphLens v -> [Pattern v]
nodes (GraphLens (Pattern _ elements) isNode) = 
  filter isNode elements
```

---

### Relationship

**Definition**: A non-node pattern with exactly two node elements.

**Type**: `Pattern v` (not a separate type - relationships are Patterns that satisfy the relationship criteria)

**Properties**:
- Must not be a node (does not satisfy `isNode` predicate)
- Must have exactly two elements
- Both elements must be nodes (according to the lens)
- First element is the source node, second element is the target node (for directed relationships)

**Derivation**:
```haskell
isRelationship :: GraphLens v -> Pattern v -> Bool
isRelationship lens@(GraphLens _ isNode) p@(Pattern _ els) =
  not (isNode p) &&
  length els == 2 &&
  all isNode els
```

**Directionality**: By convention, for directed relationships:
- `elements[0]` = source node
- `elements[1]` = target node

---

### Walk

**Definition**: A non-node pattern whose elements are all relationships, where consecutive relationships share nodes.

**Type**: `Pattern v` (not a separate type - walks are Patterns that satisfy the walk criteria)

**Properties**:
- Must not be a node (does not satisfy `isNode` predicate)
- All elements must be relationships (according to the lens)
- Consecutive relationships must be connected (target of one equals source of next)

**Derivation**:
```haskell
isWalk :: GraphLens v -> Pattern v -> Bool
isWalk lens@(GraphLens _ isNode) p@(Pattern _ elements) =
  not (isNode p) &&
  all (isRelationship lens) elements &&
  consecutivelyConnected lens elements
```

**Connection Property**: For a walk to be valid, consecutive relationships must share nodes:
- `target lens r1 == source lens r2` for consecutive relationships `r1` and `r2`

---

## Derived Concepts

### Source and Target

**Definition**: Functions that extract the source and target nodes from relationships.

**Type**:
```haskell
source :: GraphLens v -> Pattern v -> Maybe (Pattern v)
target :: GraphLens v -> Pattern v -> Maybe (Pattern v)
```

**Properties**:
- Return `Just node` if the pattern is a relationship, `Nothing` otherwise
- Source is the first element, target is the second element
- Only valid for relationship patterns

---

### Neighbors

**Definition**: Nodes connected to a given node via relationships.

**Type**: `[Pattern v]` (list of node patterns)

**Properties**:
- Includes nodes where the given node is either source or target
- Empty list for isolated nodes
- May include duplicates if multiple relationships connect to the same node

---

### Incident Relationships

**Definition**: Relationships involving a given node (as source or target).

**Type**: `[Pattern v]` (list of relationship patterns)

**Properties**:
- Includes all relationships where the node is source or target
- Empty list for isolated nodes
- Used to compute node degree

---

### Node Degree

**Definition**: Count of incident relationships for a node.

**Type**: `Int`

**Properties**:
- Zero for isolated nodes
- Sum of relationships where node is source or target
- May count self-loops twice if they exist

---

### Connected Components

**Definition**: Sets of nodes that are reachable from each other via relationships.

**Type**: `[[Pattern v]]` (list of lists of nodes)

**Properties**:
- Each inner list represents a connected component
- Nodes within a component are reachable from each other
- Nodes in different components are not reachable
- Isolated nodes form single-node components

---

## State Transitions

### GraphLens Construction

**Initial State**: Pattern and predicate function provided

**Transition**: GraphLens created with scopePattern and isNode predicate

**Final State**: GraphLens ready for graph operations

**Validation**: 
- Predicate must be pure function (no validation possible at construction, documented requirement)
- Scope pattern can be empty (valid - results in empty graph)

---

### Graph Operations

**State**: GraphLens with scopePattern containing elements

**Operations**: 
- Query nodes: Filter elements by `isNode` predicate
- Query relationships: Filter elements by relationship criteria
- Query walks: Filter elements by walk criteria
- Navigate: Find neighbors, incident relationships, degree
- Analyze: Find connected components, paths, BFS traversal

**State Changes**: None - all operations are queries, no mutation

---

## Validation Rules

### GraphLens Validation

1. **Predicate Purity**: `isNode` predicate must be pure (no side effects, deterministic)
   - Cannot be validated at runtime, documented requirement
   - Users must ensure predicate purity

2. **Scope Bounded**: All operations only consider direct elements of `scopePattern`
   - Enforced by implementation (never recurses into nested structures)
   - No validation needed - structural guarantee

### Relationship Validation

1. **Exactly Two Elements**: Relationship must have exactly 2 elements
   - Validated by `isRelationship` function
   - Returns `False` if element count != 2

2. **Both Elements Are Nodes**: Both elements must satisfy `isNode` predicate
   - Validated by `isRelationship` function
   - Returns `False` if any element is not a node

### Walk Validation

1. **All Elements Are Relationships**: All elements must be relationships
   - Validated by `isWalk` function
   - Returns `False` if any element is not a relationship

2. **Consecutive Connection**: Consecutive relationships must share nodes
   - Validated by `consecutivelyConnected` function
   - Returns `False` if relationships are not connected

---

## Edge Cases

### Empty Patterns

- **Empty scopePattern**: Results in empty graph (no nodes, no relationships, no walks)
- **Empty relationship list**: Valid walk (empty walk)
- **Empty node list**: Valid component (empty component)

### No Nodes

- **All elements fail isNode predicate**: Results in graph with no nodes
- **No relationships possible**: All relationship queries return empty
- **No walks possible**: All walk queries return empty

### Invalid Structures

- **Relationship with wrong element count**: Not identified as relationship
- **Relationship with non-node elements**: Not identified as relationship
- **Walk with non-relationship elements**: Not identified as walk
- **Walk with disconnected relationships**: Not identified as walk

**Handling**: All edge cases handled gracefully with empty results or `False`/`Nothing` returns. No runtime errors.

---

## Relationships Between Entities

```
GraphLens
  ├── scopePattern: Pattern v
  └── isNode: Pattern v -> Bool
       │
       ├──> Nodes: [Pattern v] (filtered by isNode)
       │
       ├──> Relationships: [Pattern v] (non-nodes with 2 node elements)
       │     ├──> source: Pattern v (first element)
       │     └──> target: Pattern v (second element)
       │
       └──> Walks: [Pattern v] (non-nodes with connected relationships)
             └──> walkNodes: [Pattern v] (nodes in traversal order)
```

**Key Relationships**:
- GraphLens defines interpretation of Pattern as graph
- Nodes are identified by `isNode` predicate
- Relationships connect nodes (exactly 2 node elements)
- Walks are sequences of connected relationships
- Navigation operations (neighbors, incidentRels, degree) operate on nodes and relationships
- Analysis operations (connectedComponents, findPath, bfs) operate on the full graph structure

---

## Mathematical Properties

### Graph Structure Laws

1. **Relationship Connectivity**: All relationships connect exactly two nodes
   - Verified by `isRelationship` definition
   - Property: `∀ r ∈ relationships(lens). isRelationship lens r → length (elements r) == 2`

2. **Walk Validity**: All walks contain consecutively connected relationships
   - Verified by `isWalk` definition
   - Property: `∀ w ∈ walks(lens). isWalk lens w → consecutivelyConnected lens (elements w)`

3. **Path Reachability**: Paths found by `findPath` are valid (nodes are connected)
   - Verified by `findPath` implementation
   - Property: `findPath lens start end == Just path → allConnected path`

4. **Component Completeness**: All nodes belong to exactly one connected component
   - Verified by `connectedComponents` implementation
   - Property: `∀ n ∈ nodes(lens). ∃! c ∈ connectedComponents(lens). n ∈ c`

---

## Implementation Notes

- All graph concepts are derived from the single `isNode` predicate
- No separate types for Node, Relationship, Walk - they are all `Pattern v` with different interpretations
- Operations are scope-bounded (only direct elements of `scopePattern`)
- All operations are pure (no mutation, no side effects)
- Edge cases handled gracefully with empty results or `False`/`Nothing` returns
