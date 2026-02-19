# Proposal: GraphQuery — A Portable, Composable Graph Query Interface

**Status**: 📝 Design Only  
**Date**: 2026-02-19  
**Depends on**: GraphClassifier proposal  
**Relates to**: Feature 23 (GraphLens), Feature 33 (PatternGraph)

---

## Summary

Introduce `GraphQuery v` as a record-of-functions that abstracts over graph traversal and
lookup, decoupling graph algorithms from any particular graph representation. Graph
algorithms (shortest path, connected components, centrality, etc.) are expressed as plain
functions over `GraphQuery v`, written once and applicable to `GraphLens`, `PatternGraph`,
a database-backed graph, a Graph Frame, or any other representation that can produce a
`GraphQuery v` value.

Traversal direction and weighting are expressed as a `TraversalWeight` function supplied
to algorithms as a parameter — not embedded in `GraphQuery`. Directed, undirected, and
weighted traversal are all constraints applied to the general case, not distinct
representations.

---

## Motivation

### The current situation

Graph algorithms currently live on `GraphLens` — they are functions that take a `GraphLens`
as their first argument. This means:

- Algorithms are unavailable to `PatternGraph` without first converting to `GraphLens` via
  `toGraphLens`, which materializes a scope pattern and loses the categorical structure
- Algorithms are unavailable to any non-`GraphLens` representation (database, frame, etc.)
  without wrapping in `GraphLens`, which may not be natural or efficient
- Adding a new graph representation requires re-implementing or re-exposing all algorithms

The deeper issue is that graph algorithms don't actually need a `GraphLens`. They need a
small set of traversal primitives — neighbor enumeration, endpoint access, node lookup.
`GraphLens` provides these, but so would many other things.

### The composability argument

A record-of-functions representing a query interface is a first-class value. This enables
patterns that a typeclass or concrete type cannot:

**Graph Frames** — a view over a subset of a larger graph, filtered by label, time window,
or property predicate, is just a `GraphQuery v` constructed by wrapping another
`GraphQuery v` with filter logic:

```haskell
frameQuery :: (Pattern v -> Bool) -> GraphQuery v -> GraphQuery v
frameQuery include base = GraphQuery
  { queryNodes        = filter include . queryNodes base
  , queryNeighbors    = \n -> filter include . queryNeighbors base n
  , queryIncidentRels = \n -> filter (any include . endpoints) . queryIncidentRels base n
  , ...
  }
```

No new types. No new instances. A frame *is* a `GraphQuery v`.

**Database-backed graphs** — a `GraphQuery v` can close over a database connection or
query cache. The algorithm layer never knows whether it is operating in memory or over
a network.

**Logging, caching, projection** — all expressible as `GraphQuery v -> GraphQuery v`
transformations, composable with function application.

This mirrors the design choice made for `GraphClassifier`: portable, composable,
runtime-variable, no typeclass machinery required.

---

## Design

### `TraversalWeight` — the generalized traversal policy

Direction and weight are not properties of the graph structure; they are properties of
how a particular algorithm traverses it. A `TraversalWeight` function encodes both:

```haskell
data TraversalDirection = Forward | Backward

type TraversalWeight v = Pattern v -> TraversalDirection -> Double
```

The canonical cases are provided by the library:

```haskell
-- Undirected, unweighted: direction is ignored, cost is uniform
undirected :: TraversalWeight v
undirected _ _ = 1.0

-- Directed forward only: reverse direction is impassable
directed :: TraversalWeight v
directed _ Forward  = 1.0
directed _ Backward = 1/0  -- infinity blocks reverse traversal

-- Directed reverse only
directedReverse :: TraversalWeight v
directedReverse _ Forward  = 1/0
directedReverse _ Backward = 1.0
```

A user reading weight from relationship properties, applying direction-based discounts,
or encoding temporal traversal costs simply provides their own `TraversalWeight v`
function. All algorithms accept it as a parameter.

This means "directed vs. undirected" is not a distinct graph type or representation
variant — it is a `TraversalWeight` value supplied at the call site. The graph structure
itself just has endpoints in an order; traversal policy is always external.

### `GraphQuery` — the interface

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

`queryNeighbors` is absent. Because traversal direction is expressed through
`TraversalWeight`, "neighbors" is not a fixed concept — it depends on which direction
is traversable. Algorithms derive reachable neighbors from `queryIncidentRels`,
`querySource`, `queryTarget`, and the supplied `TraversalWeight`.

`queryDegree` is derivable from `queryIncidentRels` but included explicitly because
implementations may provide O(1) versions (e.g. a database with a degree index).

`queryContainers` returns all higher-order structures that directly contain a given
element — the relationships a node participates in, the walks those relationships belong
to, the annotations attached to an element. This is the upward traversal dual to the
downward decomposition that insertion performs. It is required by `GraphMutation` for
coherent deletion (see the GraphMutation proposal) and has independent value for impact
analysis, visualization, and pattern matching: "what is this element part of?"

`Id v` comes from the `GraphValue` typeclass (identity extraction), which `GraphQuery`
depends on but does not extend.

### What `GraphQuery` intentionally omits

**Bulk adjacency** — algorithms like community detection (Louvain) and large-scale
centrality (betweenness) benefit from a bulk adjacency representation rather than
repeated `queryIncidentRels` calls. This is noted as a future extension point: a
`queryAdjacency :: Map (Id v) [Pattern v]` field could be added without breaking
existing usage. Deferred until performance requirements are clearer.

**Pattern matching** — Cypher-style pattern matching is a query *language* whose
execution engine uses `GraphQuery`. It is a distinct layer above this interface,
not part of it.

**Walk enumeration** — walks are a `GraphClassifier` concern. `GraphQuery` works at
the node/relationship level. Algorithms that need walks can derive them from
`queryRelationships` and the chaining predicate.

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

`fromPatternGraph` does not go through `GraphLens`. It reads directly from the typed
maps, which are O(log n) lookups rather than O(n) scans.

#### Custom (database, frame, etc.)

Any value that can provide the required functions produces a `GraphQuery v`. No
inheritance, no typeclass instance, no wrapping required.

---

## Algorithms

Graph algorithms are plain functions in a module `Pattern.Graph.Algorithms`, each
accepting a `GraphQuery v` as their first argument. All traversal algorithms also accept
a `TraversalWeight v`, making direction and weight a call-site concern:

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

-- Boolean
isNeighbor  :: Eq (Id v)  => GraphQuery v -> TraversalWeight v
            -> Pattern v -> Pattern v -> Bool
isConnected :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Bool

-- Structural (topology only; direction affects results)
connectedComponents :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> [[Pattern v]]
topologicalSort     :: Ord (Id v) => GraphQuery v -> Maybe [Pattern v]
hasCycle            :: Ord (Id v) => GraphQuery v -> Bool

-- Spanning
minimumSpanningTree :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> [Pattern v]

-- Centrality (representative examples; each has additional parameters)
degreeCentrality    :: Ord (Id v) => GraphQuery v -> Map (Id v) Double
betweennessCentrality :: Ord (Id v) => GraphQuery v -> TraversalWeight v
                    -> Map (Id v) Double
```

Centrality algorithms (betweenness, closeness, PageRank, etc.) are a family, each with
additional tuning parameters (convergence tolerance, damping factor, etc.). The
signatures above are representative of the category, not a complete specification.

Algorithms currently implemented on `GraphLens` (`bfs`, `findPath`, `connectedComponents`)
are moved here and re-expressed against `GraphQuery`. The existing `GraphLens` functions
are retained as convenience wrappers that call `fromGraphLens` and supply `undirected`
as the default `TraversalWeight`, preserving backward compatibility.

---

## Composability patterns

### Framing (subgraph projection)

```haskell
frameQuery :: (Pattern v -> Bool) -> GraphQuery v -> GraphQuery v
```

Filter nodes and relationships by predicate. `queryIncidentRels` is filtered to exclude
relationships whose endpoints fall outside the frame. Useful for label-based subgraphs,
temporal windows, or property filters.

### Logging / instrumentation

```haskell
tracingQuery :: (String -> IO ()) -> GraphQuery v -> GraphQuery v
```

Wraps each function to emit a log entry. Useful for debugging or performance profiling
without touching algorithm code.

### Caching

```haskell
memoizeIncidentRels :: Ord (Id v) => GraphQuery v -> GraphQuery v
```

Wraps `queryIncidentRels` with a memoization layer. Algorithms that call
`queryIncidentRels` repeatedly on the same node (e.g. betweenness centrality) benefit
automatically.

These are composable: `memoizeIncidentRels . frameQuery predicate $ fromPatternGraph pg`
is a valid, well-typed expression.

---

## Relationship to existing code

### `GraphLens` algorithms

`bfs`, `findPath`, and `connectedComponents` currently live in `Pattern.Graph` as
functions over `GraphLens`. They are moved to `Pattern.Graph.Algorithms` as functions
over `GraphQuery`. Backward-compatible wrappers remain in `Pattern.Graph`, supplying
`undirected` as the default `TraversalWeight`:

```haskell
-- Pattern.Graph (backward compat)
bfs :: Ord v => GraphLens v -> Pattern v -> [Pattern v]
bfs lens start = Algorithms.bfs (fromGraphLens lens) undirected start
```

### `toGraphLens` on `PatternGraph`

`PatternGraph.toGraphLens` currently exists to bridge the two representations for
algorithm access. Under this proposal it is retired — `fromPatternGraph` is the path
to graph algorithms, and `GraphLens`-specific use cases are covered by constructing a
`GraphLens` directly.

---

## What changes in pattern-hs

| Component | Action | Notes |
|---|---|---|
| `Pattern.Graph.GraphQuery` | **New** | Core new type; record of traversal functions including `queryContainers` |
| `Pattern.Graph.TraversalWeight` | **New** | Traversal policy type; canonical values provided |
| `Pattern.Graph.Algorithms` | **New** | All graph algorithms; operates on `GraphQuery v` + `TraversalWeight v` |
| `Pattern.Graph` (existing algorithms) | **Retain as wrappers** | `bfs`, `findPath`, `connectedComponents` delegate to `Algorithms` with `undirected` default |
| `Pattern.Graph.fromGraphLens` | **New** | Constructs `GraphQuery v` from `GraphLens v` |
| `Pattern.PatternGraph.fromPatternGraph` | **New** | Constructs `GraphQuery v` from `PatternGraph v` directly |
| `Pattern.PatternGraph.toGraphLens` | **Retire** | Superseded by `fromPatternGraph`; no remaining use cases |

---

## Open questions

1. **`queryAdjacency` for bulk algorithms** — community detection and large-scale
   centrality would benefit from a bulk adjacency structure. Deferred until performance
   requirements are established. Could be added as an optional field with a `Maybe`
   wrapper, or as a separate `GraphQueryBulk` extension record.

2. **`Pattern.Graph.Algorithms` module granularity** — one module for all algorithms, or
   sub-modules by category (`Algorithms.Path`, `Algorithms.Centrality`, etc.)? Depends
   on how large the algorithms surface grows.

---

## Summary of decisions

- **`GraphQuery` is a record-of-functions**: portable, composable, runtime-variable.
  Consistent with `GraphClassifier` design.
- **Algorithms are decoupled from representation**: written once against `GraphQuery`,
  applicable to `GraphLens`, `PatternGraph`, databases, frames, and anything else that
  can produce a `GraphQuery v`.
- **Traversal direction and weight are external via `TraversalWeight`**: directed,
  undirected, and weighted traversal are constraints applied to the general case, not
  distinct graph types. `queryNeighbors` is absent from `GraphQuery`; algorithms derive
  reachability from `queryIncidentRels` plus a supplied `TraversalWeight`.
- **Canonical traversal weights are provided**: `undirected`, `directed`, and
  `directedReverse` cover the common cases out of the box.
- **Composability is first-class**: `GraphQuery v -> GraphQuery v` transformations
  (frames, caching, logging) are the extension pattern, not subclassing or instances.
- **Pattern matching is out of scope**: it is a query language layer above `GraphQuery`,
  not part of the interface.
- **Centrality algorithms are a family**: betweenness, closeness, PageRank, and others
  each have additional tuning parameters. Signatures in the Algorithms section are
  representative of the category, not complete specifications.
- **`queryContainers` is a first-class primitive**: upward traversal — "what contains
  this element?" — is the dual of downward decomposition. Required by `GraphMutation`
  for coherent deletion; independently useful for impact analysis and pattern matching.
- **Bulk adjacency deferred**: noted as a future extension point for performance-sensitive
  algorithms.
- **`toGraphLens` retired**: superseded by `fromPatternGraph`; no remaining use cases.
- **Backward compatibility preserved**: existing `GraphLens` algorithm functions are
  retained as wrappers over the new `Algorithms` module, defaulting to `undirected`.
