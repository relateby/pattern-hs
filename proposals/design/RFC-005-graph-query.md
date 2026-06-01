# RFC-005: GraphQuery — Portable, Composable Graph Query Interface

**Status:** draft
**Date:** 2026-02-19
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Supersedes:** [`proposals/graph-query.md`](../graph-query.md)
**Depends on:** RFC-004 (GraphClassifier)
**Followed by:** RFC-008 (GraphTransform) → RFC-009 (GraphMutation)
**Related modules:** `Pattern.Graph.GraphQuery`, `Pattern.Graph.Algorithms`, `Pattern.PatternGraph`

## Summary

Introduce `GraphQuery v` as a record-of-functions that abstracts over graph traversal and
lookup, decoupling graph algorithms from any particular graph representation. Algorithms
are expressed once against `GraphQuery v` and apply to `GraphLens`, `PatternGraph`, a
database-backed graph, a filtered frame, or any other representation that can produce a
`GraphQuery v`. Traversal direction and weighting are expressed as a `TraversalWeight`
function supplied per-call — not embedded in `GraphQuery` — making directed, undirected,
and weighted traversal constraints applied to the general case.

## Motivation

Graph algorithms currently live on `GraphLens` and take a `GraphLens` as their first
argument. This means:

- Algorithms are unavailable to `PatternGraph` without converting to `GraphLens` via
  `toGraphLens`, which materializes a scope pattern and loses categorical structure.
- Algorithms are unavailable to any non-`GraphLens` representation without wrapping,
  which may not be natural or efficient.
- Adding a new graph representation requires re-implementing or re-exposing all algorithms.

The deeper issue: graph algorithms don't need a `GraphLens`. They need a small set of
traversal primitives — neighbor enumeration, endpoint access, node lookup.

A record-of-functions representing a query interface is a first-class value. This enables:

- **Graph frames** — a view over a subset of a larger graph, filtered by label, time
  window, or property predicate, is just a `GraphQuery v` wrapping another `GraphQuery v`
  with filter logic.
- **Database-backed graphs** — a `GraphQuery v` can close over a database connection.
  The algorithm layer never knows whether it operates in memory or over a network.
- **Logging, caching, projection** — all expressible as `GraphQuery v -> GraphQuery v`
  transformations.

## Design

### `TraversalWeight` — Generalized Traversal Policy

Direction and weight are properties of *how* an algorithm traverses, not of graph
structure. A `TraversalWeight` function encodes both:

```haskell
data TraversalDirection = Forward | Backward

type TraversalWeight v = Pattern v -> TraversalDirection -> Double
```

Canonical values:

```haskell
undirected :: TraversalWeight v
undirected _ _ = 1.0

directed :: TraversalWeight v
directed _ Forward  = 1.0
directed _ Backward = 1/0   -- infinity blocks reverse traversal

directedReverse :: TraversalWeight v
directedReverse _ Forward  = 1/0
directedReverse _ Backward = 1.0
```

"Directed vs. undirected" is not a distinct graph type — it is a `TraversalWeight`
supplied at the call site.

### `GraphQuery` — the Interface

```haskell
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

**`queryNeighbors` is absent**: algorithms derive reachable neighbors from
`queryIncidentRels`, `querySource`, `queryTarget`, and the supplied `TraversalWeight`.

**`queryDegree`** is derivable from `queryIncidentRels` but included explicitly because
implementations may provide O(1) versions (e.g. a database with a degree index).

**`queryContainers`** returns all higher-order structures that directly contain a given
element — the relationships a node participates in, the walks those relationships belong
to, the annotations attached to an element. This is the upward traversal dual to
downward decomposition. Required by RFC-009 (GraphMutation) for coherent deletion;
independently useful for impact analysis and pattern matching.

`Id v` comes from the `GraphValue` typeclass (`identify :: v -> Id v`).

### Constructing a `GraphQuery`

#### From `GraphLens`

```haskell
fromGraphLens :: Eq v => GraphLens v -> GraphQuery v
fromGraphLens lens = GraphQuery
  { queryNodes            = nodes lens
  , queryRelationships    = relationships lens
  , queryIncidentRels     = incidentRels lens
  , querySource           = source lens
  , queryTarget           = target lens
  , queryDegree           = degree lens
  , queryNodeById         = \i -> find (\p -> identify (value p) == i) (nodes lens)
  , queryRelationshipById = \i -> find (\p -> identify (value p) == i) (relationships lens)
  , queryContainers       = \p -> filter (elem p . elements) (relationships lens)
                                ++ filter (elem p . elements) (walks lens)
  }
```

#### From `PatternGraph`

```haskell
fromPatternGraph :: (GraphValue v, Eq v) => PatternGraph v -> GraphQuery v
fromPatternGraph pg = GraphQuery
  { queryNodes            = Map.elems (pgNodes pg)
  , queryRelationships    = Map.elems (pgRelationships pg)
  , queryIncidentRels     = \n -> filter (touches n) (Map.elems (pgRelationships pg))
  , querySource           = \r -> listToMaybe (elements r)
  , queryTarget           = \r -> listToMaybe (tail (elements r))
  , queryDegree           = \n -> length (filter (touches n) (Map.elems (pgRelationships pg)))
  , queryNodeById         = \i -> Map.lookup i (pgNodes pg)
  , queryRelationshipById = \i -> Map.lookup i (pgRelationships pg)
  , queryContainers       = \p ->
      filter (elem p . elements) (Map.elems (pgRelationships pg))
      ++ filter (elem p . elements) (Map.elems (pgWalks pg))
      ++ filter (elem p . elements) (Map.elems (pgAnnotations pg))
  }
  where touches n r = querySource q r == Just n || queryTarget q r == Just n
```

`fromPatternGraph` does not go through `GraphLens`. It reads from typed maps with O(log n)
lookups rather than O(n) scans.

#### Custom (Database, Frame, etc.)

Any value that can supply the required functions produces a `GraphQuery v`. No
inheritance, no typeclass instance, no wrapping required.

### Algorithms

Graph algorithms live in `Pattern.Graph.Algorithms`, each accepting a `GraphQuery v` as
their first argument. Traversal algorithms also accept `TraversalWeight v`:

```haskell
-- Traversal
bfs :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]
dfs :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]

-- Paths
shortestPath :: Ord (Id v) => GraphQuery v -> TraversalWeight v
             -> Pattern v -> Pattern v -> Maybe [Pattern v]
hasPath      :: Ord (Id v) => GraphQuery v -> TraversalWeight v
             -> Pattern v -> Pattern v -> Bool
allPaths     :: Ord (Id v) => GraphQuery v -> TraversalWeight v
             -> Pattern v -> Pattern v -> [[Pattern v]]

-- Structural
connectedComponents :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> [[Pattern v]]
topologicalSort     :: Ord (Id v) => GraphQuery v -> Maybe [Pattern v]
hasCycle            :: Ord (Id v) => GraphQuery v -> Bool

-- Spanning
minimumSpanningTree :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> [Pattern v]

-- Centrality (representative signatures; each has additional tuning parameters)
degreeCentrality      :: Ord (Id v) => GraphQuery v -> Map (Id v) Double
betweennessCentrality :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Map (Id v) Double
```

Existing `GraphLens` algorithm functions (`bfs`, `findPath`, `connectedComponents`) are
retained as backward-compatible wrappers that call `fromGraphLens` and default to
`undirected`.

### Context Query Helpers

Derived functions for common "what is the context of this element?" questions, built on
`GraphQuery` primitives:

```haskell
queryAnnotationsOf   :: GraphClassifier extra v -> GraphQuery v -> Pattern v -> [Pattern v]
queryWalksContaining :: GraphClassifier extra v -> GraphQuery v -> Pattern v -> [Pattern v]
queryCoMembers       :: GraphQuery v -> Pattern v -> Pattern v -> [Pattern v]
```

Required by RFC-008 (GraphTransform) for context-aware mapping.

### Composability Patterns

#### Framing (Subgraph Projection)

```haskell
frameQuery :: (Pattern v -> Bool) -> GraphQuery v -> GraphQuery v
frameQuery include base = GraphQuery
  { queryNodes     = filter include . queryNodes base
  , queryIncidentRels = \n -> filter (all include . endpoints) . queryIncidentRels base n
  , ...
  }
```

#### Logging / Instrumentation

```haskell
tracingQuery :: (String -> IO ()) -> GraphQuery v -> GraphQuery v
```

#### Caching

```haskell
memoizeIncidentRels :: Ord (Id v) => GraphQuery v -> GraphQuery v
```

These compose: `memoizeIncidentRels . frameQuery predicate $ fromPatternGraph pg` is a
valid, well-typed expression.

### What Changes in pattern-hs

| Component | Action | Notes |
|---|---|---|
| `Pattern.Graph.GraphQuery` | **New** | Core new type; includes `queryContainers` |
| `Pattern.Graph.TraversalWeight` | **New** | Traversal policy type; canonical values provided |
| `Pattern.Graph.Algorithms` | **New** | All graph algorithms over `GraphQuery v + TraversalWeight v` |
| Context query helpers | **New** | `queryAnnotationsOf`, `queryWalksContaining`, `queryCoMembers` |
| `Pattern.Graph.fromGraphLens` | **New** | Constructs `GraphQuery v` from `GraphLens v` |
| `Pattern.PatternGraph.fromPatternGraph` | **New** | Constructs `GraphQuery v` from `PatternGraph v` |
| `Pattern.PatternGraph.toGraphLens` | **Retire** | Superseded by `fromPatternGraph` |
| Existing `GraphLens` algorithms | **Retain as wrappers** | Delegate to `Algorithms` with `undirected` default |

## Open Questions

1. **Bulk adjacency for iterative algorithms** — PageRank, Louvain, DeepWalk require
   memory-local adjacency structures, not repeated `queryIncidentRels` calls. A
   `Pattern.Graph.Algorithms.Bulk` module materializing an unboxed vector-based
   adjacency list is recommended. Should not be deferred past the first
   performance-sensitive algorithm.

2. **Module granularity** — one `Pattern.Graph.Algorithms` module, or
   sub-modules by category (`Algorithms.Path`, `Algorithms.Centrality`, etc.)?

## Alternatives

**Typeclass instead of record-of-functions** — Consistent with `GraphClassifier` design,
rejected for the same portability and composability reasons.

**`queryNeighbors` as a field** — "neighbors" is not a fixed concept because it depends
on traversal direction. Algorithms derive reachable neighbors from `queryIncidentRels`
plus `TraversalWeight`.

**Pattern matching in scope** — Cypher-style pattern matching is a query language layer
above `GraphQuery`, not part of the interface.
