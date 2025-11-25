# Graph Representation in Pattern - Design Document

## Overview

This document specifies how graph structures (nodes, relationships, and walks) are represented using the `Pattern` data structure. The design maintains Pattern's core principle as a "decorated sequence" while providing natural encodings for graph concepts.

## Core Principle

Pattern is fundamentally an **ordered structure**: `Pattern v [Pattern v]` represents a value paired with an ordered sequence of sub-patterns. This ordered nature is leveraged to encode graph directionality without requiring modifications to the Pattern type itself.

## Nodes

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

## Relationships

### Undirected Relationships

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

### Directed Relationships

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

### Distinguishing Directed from Undirected

The Pattern structure itself doesn't distinguish directed from undirected relationships - both are patterns with two elements. The distinction is made at the semantic layer through:

1. **Value type metadata**: The value `v` can indicate relationship kind
2. **Domain conventions**: Application-specific interpretation rules
3. **Explicit marking**: Additional metadata in the value

```haskell
-- Example: value type carries directionality
data RelValue = Directed String | Undirected String

-- Or: separate types
type DirectedRel v = Pattern v
type UndirectedRel v = Pattern v
```

## Walks

**Definition**: A walk is an ordered sequence of relationships where consecutive relationships allow for a continuous traversal. This means entering a relationship at one node (the "entry" node) and exiting at the other (the "exit" node), which becomes the entry node for the next relationship.

**Representation**: A pattern whose elements are relationships (patterns with exactly two elements).

```haskell
walk :: v -> [Pattern v] -> Pattern v
walk walkMeta relationships = Pattern walkMeta relationships

-- Validation: Ensure a continuous path exists through the sequence
isWalk :: Eq v => Pattern v -> Bool
isWalk (Pattern _ []) = True
isWalk (Pattern _ [_]) = True  
isWalk (Pattern _ (r1:r2:rest)) = 
  case sharedNode r1 r2 of
    Nothing -> False
    -- Once we find the pivot (connection) between r1 and r2,
    -- we must ensure the flow continues correctly from there.
    Just pivot -> validateFlow pivot r2 rest
  where
    -- Find a node shared between two relationships
    sharedNode a b = 
      let aNodes = [source a, target a]
          bNodes = [source b, target b]
      in listToMaybe $ filter (`elem` bNodes) aNodes

    -- Check that we can traverse from the entry node through currentRel to the next
    validateFlow _ _ [] = True
    validateFlow entryNode currentRel (nextRel:more) =
      let 
          -- The exit node is the other node in the current relationship
          exitNode = if source currentRel == entryNode 
                     then target currentRel 
                     else source currentRel
          
          -- The next relationship must connect to this exit node
          nextConnects = source nextRel == exitNode || target nextRel == exitNode
      in nextConnects && validateFlow exitNode nextRel more
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

**Validation Example**:
```
Given: [walk_meta | [r1 | (a), (b)], [r2 | (c), (b)], [r3 | (c), (d)]]

Check r1->r2: r1 has nodes [(a), (b)], r2 has nodes [(c), (b)]
              (b) is in both ✓ (shared node)

Check r2->r3: r2 has nodes [(c), (b)], r3 has nodes [(c), (d)]
              (c) is in both ✓ (shared node)

This is VALID - a valid walk: (a)-[r1]->(b)<-[r2]-(c)-[r3]->(d)

Note: The walk connects through shared nodes regardless of direction:
- r1 and r2 share (b): r1's target connects to r2's target
- r2 and r3 share (c): r2's source connects to r3's source
```

Valid walks, in Pattern and Path notation equivalents:

```
[walk | [r1 | a, b]] =~ [walk | (a)-[r1]->(b)]

[walk | [r1 | a, b], [r2 | c, b]] =~ (a)-[r1]->(b)<-[r2]-(c)

[walk | [r1 | a, b], [r2 | b, a]] =~ (a)-[r1]->(b)-[r2]->(a)

[walk | [r1 | a, b], [r2 | a, b]] =~ (a)-[r1]->(b)<-[r2]-(a)
```

### Walk Properties

**Key Operations**:
```haskell
-- Extract all nodes from relationships (may include duplicates)
allNodes :: [Pattern v] -> [Pattern v]
allNodes = concatMap (\r -> [source r, target r])

-- Get unique nodes from a walk
uniqueNodes :: Eq v => [Pattern v] -> [Pattern v]
uniqueNodes = nub . allNodes

-- Reverse a walk
reverseWalk :: [Pattern v] -> [Pattern v]
reverseWalk = reverse . map reverseRel
  where reverseRel (Pattern v [a,b]) = Pattern v [b,a]

-- Get all relationships
relationships :: Pattern v -> [Pattern v]
relationships (Pattern _ rels) = rels

-- Check consecutive connectivity
isValidWalk :: Eq v => Pattern v -> Bool
isValidWalk = isWalk -- See validation logic in Definition section
```

**Walk Variants**:
```haskell
-- Trail: walk with no repeated edges (relationships)
isTrail :: Eq v => Pattern v -> Bool
isTrail (Pattern _ rels) = 
  length rels == length (nub rels) && isWalk (Pattern undefined rels)

-- Simple walk: no repeated vertices (nodes)  
isSimpleWalk :: Eq v => Pattern v -> Bool
isSimpleWalk p = 
  case recoverNodeSequence p of
    Just nodes -> length nodes == length (nub nodes)
    Nothing -> False

-- Cycle: walk where start and end nodes are the same
isCycle :: Eq v => Pattern v -> Bool
isCycle p = 
  case recoverNodeSequence p of
    Just nodes -> head nodes == last nodes
    Nothing -> False
```

### Design Rationale for Walks

**Why Flat Sequence of Relationships?**

1. **Consistency**: A walk is naturally a sequence of relationships, each self-contained
2. **Extraction**: Easy to extract any relationship independently
3. **Composition**: Walks can be concatenated if they connect properly
4. **Pattern matching**: DSL can match on walk structure naturally

**Trade-off**: Node duplication vs. simplicity

- **Con**: Each node (except endpoints) appears twice - once as target, once as source
- **Pro**: Each relationship is self-contained and independently meaningful
- **Pro**: No mixing of atomic and composite patterns in the same sequence
- **Pro**: Simpler operations (reverse, validate, traverse)

**Alternative Rejected**: Alternating nodes and relationships `[(a), [r1|...], (b), [r2|...], (c)]`
- Would require relationships to be non-self-contained
- Mixes atomic patterns (nodes) with composite patterns (rels) in same sequence
- More complex to validate and traverse

## Recovering Node Traversal

Given a pattern representation of relationships in a walk, we can recover the exact sequence of nodes traversed. Since the walk is an ordered sequence, we simply follow the connections from one relationship to the next.

**Algorithm**:
```haskell
-- Recovers the sequence of nodes visited: [startNode, node2, node3, ..., endNode]
recoverNodeSequence :: Eq v => Pattern v -> Maybe [Pattern v]
recoverNodeSequence (Pattern _ []) = Just []
recoverNodeSequence (Pattern _ [r]) = Just [source r, target r] -- Ambiguous start, defaults to source->target
recoverNodeSequence (Pattern _ (r1:r2:rest)) = do
  -- Find the pivot between r1 and r2
  pivot <- sharedNode r1 r2
  
  -- Deduce start of r1 (the node that isn't the pivot)
  let start = if source r1 == pivot then target r1 else source r1
  
  -- Trace the rest
  path <- tracePath pivot r2 rest
  return (start : path)

  where
    sharedNode a b = 
      let aNodes = [source a, target a]
          bNodes = [source b, target b]
      in listToMaybe $ filter (`elem` bNodes) aNodes

    tracePath entryNode currentRel [] = 
      let exitNode = if source currentRel == entryNode then target currentRel else source currentRel
      in Just [entryNode, exitNode]
      
    tracePath entryNode currentRel (nextRel:more) = 
      let exitNode = if source currentRel == entryNode then target currentRel else source currentRel
      in if source nextRel == exitNode || target nextRel == exitNode
         then do
           rest <- tracePath exitNode nextRel more
           return (entryNode : rest)
         else Nothing -- Broken link
```

**Example**:
```
Pattern: [meta | [r1 | (a), (b)], [r2 | (c), (b)], [r3 | (c), (d)]]

Trace:
1. r1[(a),(b)] and r2[(c),(b)] share (b).
   Start node is (a). Path so far: (a)->(b).
2. Current node (b) entering r2. r2 connects (b) to (c).
   Exit node is (c). Path so far: (a)->(b)->(c).
3. Current node (c) entering r3. r3 connects (c) to (d).
   Exit node is (d). Final path: (a)->(b)->(c)->(d).

Result: Valid traversal.
```

**Self-Loop Example**:
```
Pattern: [meta | [r1 | (a), (b)], [r2 | (b), (b)], [r3 | (b), (c)]]

Trace:
1. r1[(a),(b)] and r2[(b),(b)] share (b).
   Start node is (a). Path: (a)->(b).
2. Current node (b) entering r2[(b),(b)].
   Entry is (b). r2 connects (b) to (b). Exit is (b).
   Path: (a)->(b)->(b).
3. Current node (b) entering r3[(b),(c)].
   Entry is (b). r3 connects (b) to (c). Exit is (c).
   Path: (a)->(b)->(b)->(c).

Result: Valid traversal [a, b, b, c].
```

**Invalid Flow Example**:
```
Pattern: [meta | [r1 | (a), (b)], [r2 | (a), (c)], [r3 | (a), (d)]]

Trace:
1. r1[(a),(b)] and r2[(a),(c)] share (a).
   Start node is (b). Path so far: (b)->(a).
2. Current node (a) entering r2. r2 connects (a) to (c).
   Exit node is (c). Path so far: (b)->(a)->(c).
3. Current node (c) entering r3[(a),(d)].
   Does r3 connect to (c)? NO.
   
Result: Invalid walk (broken chain at r2->r3).
```

## Terminology

**Atomic Pattern**: A pattern with no elements; equivalent to a node

**Decorated Sequence**: The fundamental Pattern abstraction - a value paired with ordered sub-elements

**Walk**: An ordered sequence of connected relationships where consecutive relationships share nodes

**Trail**: A walk with no repeated relationships

**Simple Walk**: A walk with no repeated nodes

**Cycle**: A walk where the first and last relationships share at least one node

## Integration with Pattern Matching DSL

The walk representation integrates naturally with the Pattern Matching DSL (see separate design document):

```haskell
-- Match any walk of length 3
threeHops :: PatternExpr v
threeHops = PSequence walkMeta 
  [relationship, relationship, relationship]

-- Match walk with specific pattern
knowsLikes :: PatternExpr v  
knowsLikes = PSequence walkMeta
  [ PSequence knowsRel [any, any]
  , PSequence likesRel [any, any]
  ]
  `satisfying` isValidWalk
```

## Future Considerations

### General Graphs (Non-walks)

For graph structures that aren't walks (have branching, cycles, or disconnected components), additional abstractions are needed:

1. **Graph Pattern Type**: Higher-level construct in the DSL (Layer 3)
2. **Multiple Walks**: Represent as list of walk patterns with shared node variables
3. **Adjacency Representation**: Alternative Pattern interpretation for graphs

### Hypergraphs

Relationships with more than two nodes could be represented as patterns with > 2 elements:

```
Hyperedge: (a)-[r]-(b)--(c)--(d)
Pattern: [r | (a), (b), (c), (d)]
```

This extends naturally from the current design but requires different semantic interpretation.

## Summary

| Concept | Pattern Structure | Convention |
|---------|------------------|------------|
| Node | `[v]` | Atomic pattern (no elements) |
| Undirected Rel | `[r \| (a), (b)]` | Order semantically irrelevant |
| Directed Rel | `[r \| (a), (b)]` | Element[0]=source, Element[1]=target |
| Walk | `[meta \| rel1, rel2, ...]` | Consecutive rels share nodes |

**Key Design Principles**:
1. Leverage Pattern's ordered sequence property for directionality
2. Keep structure clean; push semantics to value types
3. Maintain self-contained relationships for composability
4. Validate walks through connectivity analysis
5. Extend naturally to more complex graph structures