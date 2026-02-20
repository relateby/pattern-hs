# Data Model: GraphQuery — Portable, Composable Graph Query Interface

**Branch**: `035-graph-query` | **Date**: 2026-02-20  
**Purpose**: Concrete type definitions, field invariants, and relationships between all entities introduced by this feature.

---

## Overview

This feature introduces three new types (`GraphQuery v`, `TraversalDirection`, `TraversalWeight v`) and two new modules (`Pattern.Graph.GraphQuery`, `Pattern.Graph.Algorithms`). It extends `Pattern.Graph` with `fromGraphLens` and backward-compatible wrappers, and extends `Pattern.PatternGraph` with `fromPatternGraph`. The functions `toGraphLens` and `toGraphLensWithScope` were removed from `Pattern.PatternGraph`; use `fromPatternGraph` instead (see research.md Decision 7 implementation deviation).

---

## Core Types

### `TraversalDirection`

```
Module: Pattern.Graph.GraphQuery

data TraversalDirection = Forward | Backward
```

**Categorical interpretation**: An element of a two-element set representing the two orientations along a directed relationship. `Forward` follows the relationship from source to target; `Backward` follows it from target to source.

**Invariants**: None. A plain enumeration.

---

### `TraversalWeight v`

```
Module: Pattern.Graph.GraphQuery

type TraversalWeight v = Pattern v -> TraversalDirection -> Double
```

**Categorical interpretation**: A function that assigns a traversal cost to each (relationship, direction) pair. Encodes both directionality and edge weight as a single concept. Infinity (`1/0 :: Double`) encodes impassability — the traversal is blocked in that direction.

**Canonical values** (provided by the library):

| Name | Behavior |
|------|----------|
| `undirected` | Returns `1.0` for all inputs. Direction is ignored; cost is uniform. |
| `directed` | Returns `1.0` for `Forward`, `1/0` for `Backward`. Only forward traversal is passable. |
| `directedReverse` | Returns `1/0` for `Forward`, `1.0` for `Backward`. Only backward traversal is passable. |

**Invariants**:
- Must return a non-negative `Double` (including `+Infinity`).
- `0.0` is a valid weight (zero-cost traversal).
- Negative weights are not supported by the standard algorithms (Dijkstra assumption). Callers using negative weights must use Bellman-Ford variants.

---

### `GraphQuery v`

```
Module: Pattern.Graph.GraphQuery

data GraphQuery v = GraphQuery
  { queryNodes            :: [Pattern v]
  , queryRelationships    :: [Pattern v]
  , queryIncidentRels     :: Pattern v -> [Pattern v]
  , querySource           :: Pattern v -> Maybe (Pattern v)
  , queryTarget           :: Pattern v -> Maybe (Pattern v)
  , queryDegree           :: Pattern v -> Int
  , queryNodeById         :: Id v -> Maybe (Pattern v)
  , queryRelationshipById :: Id v -> Maybe (Pattern v)
  , queryContainers       :: Pattern v -> [Pattern v]
  }
```

**Categorical interpretation**: A record-of-functions representing a graph query interface. Analogous to a coalgebra: given a graph element, it produces the elements reachable from it. `queryContainers` is the upward dual — given an element, it produces the structures that contain it. Together they form a bidirectional traversal interface.

**Field semantics**:

| Field | Type | Semantics | Complexity (in-memory) |
|-------|------|-----------|------------------------|
| `queryNodes` | `[Pattern v]` | All node-classified elements in the graph | O(n) |
| `queryRelationships` | `[Pattern v]` | All relationship-classified elements | O(r) |
| `queryIncidentRels` | `Pattern v -> [Pattern v]` | All relationships where the given node is source or target | O(r) |
| `querySource` | `Pattern v -> Maybe (Pattern v)` | The source (first endpoint) of a relationship; `Nothing` if not a relationship | O(1) |
| `queryTarget` | `Pattern v -> Maybe (Pattern v)` | The target (second endpoint) of a relationship; `Nothing` if not a relationship | O(1) |
| `queryDegree` | `Pattern v -> Int` | Count of incident relationships for a node | O(r) default; O(1) if indexed |
| `queryNodeById` | `Id v -> Maybe (Pattern v)` | Node lookup by identity | O(log n) from PatternGraph; O(n) from GraphLens |
| `queryRelationshipById` | `Id v -> Maybe (Pattern v)` | Relationship lookup by identity | O(log r) from PatternGraph; O(r) from GraphLens |
| `queryContainers` | `Pattern v -> [Pattern v]` | All higher-order structures (relationships, walks, annotations) that directly contain the given element | O(r + w + a) |

**Invariants**:
- `querySource r = Just s` implies `s ∈ queryNodes`.
- `queryTarget r = Just t` implies `t ∈ queryNodes`.
- `r ∈ queryIncidentRels n` implies `querySource r = Just n ∨ queryTarget r = Just n`.
- `queryDegree n = length (queryIncidentRels n)` (default; implementations may provide faster versions).
- `queryNodeById (identify (value n)) = Just n` for all `n ∈ queryNodes`.
- `queryRelationshipById (identify (value r)) = Just r` for all `r ∈ queryRelationships`.
- `queryContainers` returns only direct containers — it does not recurse transitively.

**`queryNeighbors` is intentionally absent**: Neighbors are direction-dependent. Algorithms derive reachable neighbors from `queryIncidentRels` + `querySource` + `queryTarget` + a supplied `TraversalWeight`.

---

## Constructors

### `fromGraphLens`

```
Module: Pattern.Graph.GraphQuery

fromGraphLens :: (GraphValue v, Eq v) => GraphLens v -> GraphQuery v
```

Constructs a `GraphQuery v` from a `GraphLens v`. All fields are derived from existing `Pattern.Graph` functions. `queryNodeById` and `queryRelationshipById` perform O(n) / O(r) scans (no index available from `GraphLens`). `queryContainers` scans relationships and walks.

**Relationship to existing code**: This is the bridge that allows existing `GraphLens`-based code to use the new algorithm module without changes.

---

### `fromPatternGraph`

```
Module: Pattern.Graph.GraphQuery  (re-exported from Pattern.PatternGraph)

fromPatternGraph :: (GraphValue v, Eq v) => PatternGraph extra v -> GraphQuery v
```

Constructs a `GraphQuery v` directly from a `PatternGraph extra v` by reading from the typed maps (`pgNodes`, `pgRelationships`, `pgWalks`, `pgAnnotations`). `queryNodeById` and `queryRelationshipById` are O(log n) / O(log r) map lookups. `queryContainers` filters across all four maps.

**Relationship to existing code**: Supersedes `toGraphLens` for algorithm access. `toGraphLens` and `toGraphLensWithScope` were removed in this feature; use `fromPatternGraph` (see research.md Decision 7).

---

## Combinators

### `frameQuery`

```
Module: Pattern.Graph.GraphQuery

frameQuery :: (Pattern v -> Bool) -> GraphQuery v -> GraphQuery v
```

Produces a new `GraphQuery v` that filters nodes and relationships by a predicate. `queryIncidentRels` on the framed query excludes relationships whose endpoints fall outside the frame. All other fields are filtered accordingly.

**Invariants preserved**: All `GraphQuery` invariants hold on the result — `querySource`, `queryTarget`, and `queryIncidentRels` remain consistent within the frame.

---

### `memoizeIncidentRels`

```
Module: Pattern.Graph.GraphQuery

memoizeIncidentRels :: Ord (Id v) => GraphQuery v -> GraphQuery v
```

Wraps `queryIncidentRels` with a memoization layer keyed by node identity. Useful for algorithms that call `queryIncidentRels` repeatedly on the same node (e.g., betweenness centrality). All other fields are passed through unchanged.

**Note**: The memoization is per-`GraphQuery` value, not global. A new `GraphQuery` produced by `memoizeIncidentRels` carries its own cache.

---

## Algorithms Module

### Module: `Pattern.Graph.Algorithms`

All functions accept `GraphQuery v` as their first argument. Traversal algorithms also accept `TraversalWeight v`.

#### Traversal

```haskell
bfs :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]
dfs :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]
```

#### Paths

```haskell
shortestPath :: Ord (Id v) => GraphQuery v -> TraversalWeight v
             -> Pattern v -> Pattern v -> Maybe [Pattern v]

hasPath :: Ord (Id v) => GraphQuery v -> TraversalWeight v
        -> Pattern v -> Pattern v -> Bool

allPaths :: Ord (Id v) => GraphQuery v -> TraversalWeight v
         -> Pattern v -> Pattern v -> [[Pattern v]]
```

#### Boolean Queries

```haskell
isNeighbor :: Eq (Id v) => GraphQuery v -> TraversalWeight v
           -> Pattern v -> Pattern v -> Bool

isConnected :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Bool
```

#### Structural

```haskell
connectedComponents :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> [[Pattern v]]
topologicalSort     :: Ord (Id v) => GraphQuery v -> Maybe [Pattern v]
hasCycle            :: Ord (Id v) => GraphQuery v -> Bool
```

**Note**: `topologicalSort` and `hasCycle` do not take `TraversalWeight` — they operate on the directed structure implied by relationship endpoint order.

#### Spanning

```haskell
minimumSpanningTree :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> [Pattern v]
```

#### Centrality

```haskell
degreeCentrality      :: Ord (Id v) => GraphQuery v -> Map (Id v) Double
betweennessCentrality :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Map (Id v) Double
```

#### Context Query Helpers

```haskell
queryAnnotationsOf   :: GraphClassifier extra v -> GraphQuery v -> Pattern v -> [Pattern v]
queryWalksContaining :: GraphClassifier extra v -> GraphQuery v -> Pattern v -> [Pattern v]
queryCoMembers       :: GraphQuery v -> Pattern v -> Pattern v -> [Pattern v]
```

These are derived functions — they call `queryContainers` and filter by `GraphClass`. No new `GraphQuery` fields are required.

---

## Module Dependency Graph

```
Pattern.Core
    └── Pattern.Graph.GraphClassifier   (GraphValue, GraphClass, GraphClassifier)
            ├── Pattern.Graph           (GraphLens — existing)
            ├── Pattern.Graph.GraphQuery (GraphQuery, TraversalWeight, TraversalDirection,
            │                            fromGraphLens, fromPatternGraph, frameQuery,
            │                            memoizeIncidentRels)
            │       └── Pattern.Graph.Algorithms  (all algorithms + context helpers)
            └── Pattern.PatternGraph    (PatternGraph — existing; fromPatternGraph added;
                                         toGraphLens / toGraphLensWithScope removed)
```

---

## Changes to Existing Modules

### `Pattern.Graph` (existing)

- Add `fromGraphLens` re-export (or define here and re-export from `GraphQuery`).
- Replace `bfs`, `findPath`, `connectedComponents` implementations with one-line wrappers:
  ```haskell
  bfs lens start = Algorithms.bfs (fromGraphLens lens) undirected start
  findPath lens s e = Algorithms.shortestPath (fromGraphLens lens) undirected s e
  connectedComponents lens = Algorithms.connectedComponents (fromGraphLens lens) undirected
  ```
- All existing exports remain; no removals.

### `Pattern.PatternGraph` (existing)

- Add `fromPatternGraph` to exports.
- Remove `toGraphLens` and `toGraphLensWithScope` (replaced by `fromPatternGraph`; see research.md Decision 7 implementation deviation).

### `pattern.cabal`

Add to `exposed-modules`:
```
Pattern.Graph.GraphQuery
Pattern.Graph.Algorithms
```

Add to `other-modules` in `test-suite`:
```
Spec.Pattern.Graph.GraphQuerySpec
Spec.Pattern.Graph.AlgorithmsSpec
```
