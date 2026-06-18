# RFC-008: GraphTransform — Construction, Transformation, and Pipeline

**Status:** draft
**Date:** 2026-02-19
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Supersedes:** [`proposals/graph-transform.md`](../graph-transform.md), [`proposals/pipeline-scenarios.md`](../pipeline-scenarios.md)
**Depends on:** RFC-004 (GraphClassifier), RFC-005 (GraphQuery)
**Followed by:** RFC-009 (GraphMutation), RFC-007 (RepresentationMap)
**Related modules:** `Pattern.Core`, `Pattern.Graph`, `Pattern.Graph.Transform`

## Summary

Introduce `GraphView` as the universal graph-like interface and pipeline entry point,
along with graph construction and transformation operations: `unfold`/`unfoldGraph` for
construction from seed data, `mapGraph`/`filterGraph`/`foldGraph` for categorized
transformation, `mapWithContext` for context-aware mapping, and `paraGraph` for
structure-and-topology-aware folding. All transformation operations work over `GraphView`
rather than any specific representation; `materialize` explicitly produces a concrete
`PatternGraph v` on demand.

## Motivation

Every graph processing pipeline shares a common shape: impose a graph interpretation on
source data, transform it, and optionally materialize a concrete representation. Whether
the source is a CSV, a JSON document, a relational table, or an existing `PatternGraph`,
the pipeline is identical from the graph interpretation onward. Without a universal
graph-like interface, each source type requires bespoke ingestion, creating an impedance
mismatch between "raw data processing" and "graph operations."

`GraphView` eliminates this mismatch. A source is a graph from the moment a view is
applied. This mirrors the GraphFrames model, where DataFrames *are* graphs when vertex
and edge columns are designated.

Three motivating pipeline scenarios ground the design:
1. **ETL from structured data** — CSV/JSON rows become nodes; shared field values become
   relationships or shared nodes.
2. **Graph-to-graph transformation** — normalize labels, enrich nodes with computed
   properties, project a subgraph.
3. **Knowledge graph construction** — multi-stage pipeline from raw documents through
   chunking, NER, and relationship extraction.

All three follow the same canonical shape:

```
source data
  → GraphView  (impose a graph interpretation)
  → [GraphView → GraphView transformations]
  → materialize → PatternGraph  (when concrete storage is required)
```

## Design

### `GraphView` — Universal Graph-Like Interface

`GraphView` pairs a `GraphQuery v` with classified element enumeration:

```haskell
data GraphView extra v = GraphView
  { viewQuery    :: GraphQuery v
  , viewElements :: [(GraphClass extra, Pattern v)]
  }
```

`GraphQuery v` provides traversal (nodes, relationships, containers, incident rels).
`viewElements` provides the classified enumeration needed by categorized operations.
Together they express everything a transformation needs to know about the source graph.

`GraphView` is constructable from any graph-like thing:

```haskell
fromPatternGraph :: GraphClassifier extra v -> PatternGraph v   -> GraphView extra v
fromGraphLens    :: GraphClassifier extra v -> GraphLens v      -> GraphView extra v
-- fromCSV, fromJSON live in adapter modules; they target GraphView
```

### `materialize` — Explicit Boundary

`materialize` is the explicit step that turns a `GraphView` into a `PatternGraph`:

```haskell
materialize :: GraphClassifier extra v
            -> ReconciliationPolicy (MergeStrategy v)
            -> GraphView extra v
            -> PatternGraph v
```

Transformations operate lazily over `GraphView`. Materialization is deferred until the
caller explicitly requests it, enabling `GraphView → GraphView` pipelines without
copying data at each step.

**The canonical pipeline shape:**

```haskell
pipeline :: CSV -> PatternGraph Subject
pipeline csv =
  materialize canonicalClassifier LastWriteWins  -- GraphView → PatternGraph
  . mapWithContext enrich                         -- GraphView → GraphView
  . filterGraph isRelevant dissolve              -- GraphView → GraphView
  . fromCSV canonicalClassifier                  -- CSV → GraphView
  $ csv
```

### Construction: `unfold` and `unfoldGraph`

`unfold` is the anamorphism dual to `para` (already implemented). It builds a `Pattern v`
from a seed value:

```haskell
-- Pattern.Core
unfold :: (a -> (v, [a])) -> a -> Pattern v
```

`unfoldGraph` maps a collection of seeds through an expansion function and batch-merges:

```haskell
-- Pattern.Graph.Transform
unfoldGraph :: GraphClassifier extra v
            -> ReconciliationPolicy (MergeStrategy v)
            -> (a -> [Pattern v])
            -> [a]
            -> PatternGraph v
```

**ETL example** — each CSV row expands to a person node and a department node:

```haskell
rowToPatterns :: Row -> [Pattern Subject]
rowToPatterns row = [ personNode row, departmentNode row, worksInRel row ]

graph = unfoldGraph canonicalClassifier LastWriteWins rowToPatterns rows
```

Shared elements (the same department appearing in many rows) are reconciled
automatically. `unfoldGraph` is `fromPatterns` generalized to arbitrary seed types.

**Hierarchy construction** — building a document hierarchy uses the recursive form:

```haskell
docExpand :: DocNode -> (Subject, [DocNode])
docExpand node = (nodeSubject node, children node)

chunkHierarchy = unfold docExpand documentRoot
```

### Categorized Transformation: `mapGraph`, `filterGraph`, `foldGraph`

All operate on `GraphView extra v` with `GraphClass` information available and produce
a new `GraphView extra v`, enabling composition before materialization.

#### `mapGraph`

```haskell
mapGraph :: GraphClassifier extra v
         -> (Pattern v -> Pattern v)  -- nodes
         -> (Pattern v -> Pattern v)  -- relationships
         -> (Pattern v -> Pattern v)  -- walks
         -> (Pattern v -> Pattern v)  -- annotations
         -> (Pattern v -> Pattern v)  -- other
         -> GraphView extra v -> GraphView extra v

mapAllGraph :: (Pattern v -> Pattern v) -> GraphView extra v -> GraphView extra v
```

#### `filterGraph`

```haskell
filterGraph :: GraphClassifier extra v
            -> (GraphClass extra -> Pattern v -> Bool)
            -> Substitution v
            -> GraphView extra v -> GraphView extra v
```

The predicate receives both the element's class and the element itself. `Substitution`
determines what happens to containers of filtered-out elements (see RFC-009).

#### `foldGraph`

```haskell
foldGraph :: Monoid m
          => (GraphClass extra -> Pattern v -> m)
          -> GraphView extra v -> m
```

**Example** — count elements by category:

```haskell
countByClass :: GraphView extra v -> Map (GraphClass extra) Int
countByClass = foldGraph (\cls _ -> Map.singleton cls 1)
```

### Context-Aware Mapping: `mapWithContext`

Map over elements where each mapping function has access to the full graph context:

```haskell
mapWithContext :: GraphClassifier extra v
               -> (GraphQuery v -> Pattern v -> Pattern v)
               -> GraphView extra v -> GraphView extra v
```

The `GraphQuery` is derived from the *original* view — snapshot semantics. All elements
are transformed against the same source graph, not against incrementally modified results.
(Incremental semantics produce non-deterministic outputs depending on map iteration order,
which is a correctness problem, not just a predictability concern.)

**Example** — enrich each node with a count of its annotations:

```haskell
enrichWithAnnotationCount :: GraphView () Subject -> GraphView () Subject
enrichWithAnnotationCount =
  mapWithContext canonicalClassifier enrich
  where
    enrich q node =
      let count = length (queryAnnotationsOf canonicalClassifier q node)
      in  setAnnotationCount count node
```

### `paraGraph` — Structure-and-Topology-Aware Folding

`paraGraph` extends `para` to graph topology: each element's result depends on both its
structural decomposition and computed results from graph neighbors.

```haskell
paraGraph :: (GraphQuery v -> Pattern v -> [r] -> r)
          -> GraphView extra v
          -> Map (Id v) r
```

Each element receives the `GraphQuery` (full context), the element itself, and the
recursively computed results from its structural children. This is the foundation for
iterative message-passing algorithms — the Pregel / GraphFrames `AggregateMessages`
analogue.

For general graphs (with cycles), fixpoint iteration is provided:

```haskell
paraGraphFixed :: (r -> r -> Bool)         -- convergence predicate
               -> (GraphQuery v -> Pattern v -> [r] -> r)
               -> r                         -- initial value
               -> GraphView extra v
               -> Map (Id v) r
```

A user-supplied convergence predicate (not `Eq r`) is required because floating-point
values — the common case for centrality and propagation — rarely converge to strict
equality. Callers supply `\old new -> abs (old - new) < epsilon` for floats, `(==)` for
integral types.

### Deferred Operations

**`mergeByPredicate`** — fuzzy identity matching for coreference resolution. Required
for knowledge graph quality (reconciling "Apple Inc." and "Apple"). Deferred because:
- Mutation-adjacent (semantically an insertion operation with a search step prepended)
- Cross-cutting (requires both `GraphQuery` and `GraphMutation`)
- Unresolved `MultiMatchPolicy` question and O(n) performance implications

**`expandNode`** and **`rewireRelationship`** — higher-order graph editing. Deferred
pending a dedicated higher-order graph editing proposal.

### Module Placement

**Recommendation: Option C**
- `Pattern.Core.unfold` alongside its categorical dual `para`
- `GraphView` and `materialize` in `Pattern.Graph` alongside `GraphLens`, `GraphQuery`
- Everything else in `Pattern.Graph.Transform`

`GraphView` belongs near `GraphLens` and `GraphQuery` because it *is* the abstraction
that unifies them.

### What Changes in pattern-hs

| Component | Action | Notes |
|---|---|---|
| `Pattern.Core.unfold` | **New** | Anamorphism dual to `para` |
| `Pattern.Graph.GraphView` | **New** | Universal graph-like interface |
| `Pattern.Graph.materialize` | **New** | `GraphView → PatternGraph`; explicit materialization |
| `Pattern.Graph.fromPatternGraph` | **Update** | Returns `GraphView` in addition to `GraphQuery` |
| `Pattern.Graph.fromGraphLens` | **Update** | Returns `GraphView` in addition to `GraphQuery` |
| `Pattern.Graph.Transform` | **New module** | All graph-level transformation functions |
| `Pattern.Graph.Transform.unfoldGraph` | **New** | Seed-based graph construction |
| `Pattern.Graph.Transform.mapGraph` | **New** | Categorized map over `GraphView` |
| `Pattern.Graph.Transform.mapAllGraph` | **New** | Uniform map over all categories |
| `Pattern.Graph.Transform.filterGraph` | **New** | Predicate filter with `Substitution` |
| `Pattern.Graph.Transform.foldGraph` | **New** | Categorized fold |
| `Pattern.Graph.Transform.mapWithContext` | **New** | Context-aware map via `GraphQuery` |
| `Pattern.Graph.Transform.paraGraph` | **New** | Structure-and-topology-aware fold |
| `Pattern.Graph.Transform.paraGraphFixed` | **New** | Fixpoint variant for iterative algorithms |

### Implementation Order

1. `GraphView` and `materialize` in `Pattern.Graph`
2. `unfold` in `Pattern.Core`
3. `unfoldGraph`, `mapGraph`, `mapAllGraph`, `foldGraph` (no `GraphQuery` dependency)
4. `mapWithContext`, `filterGraph` (require `GraphQuery` and `Substitution`)
5. `paraGraph`, `paraGraphFixed` (require `GraphQuery`; need processing order strategy)

`Substitution v` (used by `filterGraph`) must be defined before `GraphTransform` is
implemented. It can be extracted to a shared types module (`Pattern.Graph.Types`) ahead
of the full RFC-009 (GraphMutation) implementation.

## Open Questions

1. **`paraGraph` processing order for DAGs** — the caller-supplied ordering variant
   (topological sort) is simpler but puts the burden on the user. The recommended
   default for cyclic graphs is `paraGraphFixed`.

2. **`filterGraph` and walk coherence** — filtering out a relationship that is part of
   a walk leaves the walk with a gap. `NoSubstitution` default produces a broken walk
   in `pgOther`. Recommend explicit `Substitution` for now.

## Alternatives

**Special-casing source types** — treating CSV, JSON, and PatternGraph as fundamentally
different ingestion paths was the status quo approach. `GraphView` eliminates this by
making the pipeline source-agnostic from the first step.

**Eager materialization** — materializing a `PatternGraph` at each transformation step
was rejected. Lazy composition over `GraphView` avoids unnecessary copies and makes
pipelines composable as pure functions.

**`paraMap` / `paraWith`** — these were considered as intermediate operations but deferred;
`paraGraph` covers the graph-level use cases.
