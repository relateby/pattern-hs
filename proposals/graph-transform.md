# Proposal: GraphTransform — GraphView, Construction, Transformation, and Context-Aware Mapping

**Status**: 📝 Design Only  
**Date**: 2026-02-19  
**Depends on**: GraphClassifier proposal, GraphQuery proposal  
**Followed by**: GraphMutation proposal  
**Relates to**: Feature 23 (GraphLens), Feature 33 (PatternGraph)  
**Grounding**: `proposals/pipeline-scenarios.md`

---

## Summary

Introduce `GraphView` as the universal graph-like interface and pipeline entry point,
along with a set of graph transformation operations:

1. **`GraphView`** — a lightweight record pairing a `GraphQuery v` with classified
   element enumeration; the universal interface for anything graph-like; the entry point
   for all pipelines
2. **`materialize`** — the explicit step that produces a concrete `PatternGraph v` from
   any `GraphView`
3. **Construction** — building graph elements from non-graph seed data via `unfold` and
   `unfoldGraph`
4. **Transformation** — mapping, filtering, and folding over any `GraphView` with
   category awareness via `mapGraph`, `filterGraph`, `foldGraph`
5. **Context-aware mapping** — transforming elements with access to their full graph
   context via `mapWithContext`
6. **`paraGraph`** — structure-and-topology-aware folding; the graph-level analogue of
   `para`; foundation for iterative message-passing algorithms

All transformation operations work over `GraphView` rather than `PatternGraph`
specifically — `PatternGraph`, `GraphLens`, and any future graph representation can all
produce a `GraphView`. Write operations return a concrete `PatternGraph v`.

`mergeByPredicate` (fuzzy identity matching) is explicitly deferred — it is
mutation-adjacent, cross-cutting, and serves a specific advanced use case that deserves
its own focused proposal.

---

## Motivation

The three motivating pipeline scenarios (see `proposals/pipeline-scenarios.md`) surface
a consistent set of operations not covered by `GraphMutation` or `GraphQuery`. But they
also reveal a deeper insight: **every pipeline should begin by applying a `GraphView`
over the source data**. A CSV, JSON document, relational table, or existing `PatternGraph`
are all source graphs once a view is imposed on them. The entire pipeline then becomes a
sequence of graph transformations — no special ingestion step, no impedance mismatch
between "raw data processing" and "graph operations."

This is the same model as GraphFrames, where DataFrames *are* graphs when you designate
vertex and edge columns. The pipeline starts graph-shaped and stays graph-shaped.

`GraphMutation` handles single-element writes. `GraphQuery` handles reads and traversal.
Neither provides the universal graph-like interface, the bulk transformation operations,
or the construction patterns that pipelines require.

Knowledge graph construction also requires fuzzy merge — reconciling entities by
semantic similarity — but this is deferred. See the Deferred Operations section.

---

## Design

### 1. `GraphView` — the universal graph-like interface

`GraphView` pairs a `GraphQuery v` with classified element enumeration. It is the
universal interface for anything that can be interpreted as a graph:

```haskell
data GraphView extra v = GraphView
  { viewQuery    :: GraphQuery v
  , viewElements :: [(GraphClass extra, Pattern v)]
  }
```

`GraphQuery v` provides traversal (nodes, relationships, containers, incident rels).
`viewElements` provides the classified enumeration needed by categorized operations
(`mapGraph`, `foldGraph`, `filterGraph`). Together they express everything a
transformation needs to know about the source graph.

`GraphView` is constructable from any graph-like thing:

```haskell
fromPatternGraph :: GraphClassifier extra v -> PatternGraph v   -> GraphView extra v
fromGraphLens    :: GraphClassifier extra v -> GraphLens v      -> GraphView extra v
fromCSV          :: GraphClassifier extra v -> CSV              -> GraphView extra v
fromJSON         :: GraphClassifier extra v -> JSON             -> GraphView extra v
```

The construction functions for external sources (`fromCSV`, `fromJSON`) are not part of
this proposal — they live in adapters. But the interface they target is `GraphView`,
making the pipeline entry point uniform regardless of source.

`GraphLens` and `PatternGraph` both implement `fromX` functions as first-class citizens.
Any future graph representation does the same.

### `materialize` — producing a concrete graph

`materialize` is the explicit step that turns a `GraphView` into a `PatternGraph`:

```haskell
materialize :: GraphClassifier extra v
            -> ReconciliationPolicy (MergeStrategy v)
            -> GraphView extra v
            -> PatternGraph v
```

Transformations operate lazily over `GraphView`. Materialization is deferred until the
caller explicitly requests it. This enables pipelines to compose `GraphView → GraphView`
transformations without copying data at each step.

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

No special ingestion step. The CSV is a graph from the moment the view is applied.

---

### 2. Construction: `unfold`

`unfold` is the anamorphism dual to `para` (already implemented). It builds a `Pattern v`
from a seed value by recursively expanding each seed into a value and child seeds:

```haskell
unfold :: (a -> (v, [a])) -> a -> Pattern v
```

For graph construction, `unfoldGraph` maps a collection of seeds through an expansion
function and batch-merges the results:

```haskell
unfoldGraph :: GraphClassifier extra v
            -> ReconciliationPolicy (MergeStrategy v)
            -> (a -> [Pattern v])
            -> [a]
            -> PatternGraph v
```

Each seed `a` expands to `[Pattern v]`; the full list is merged into a `PatternGraph`
using the supplied classifier and reconciliation policy.

**ETL example** — each CSV row expands to a person node and a department node:

```haskell
rowToPatterns :: Row -> [Pattern Subject]
rowToPatterns row = [ personNode row, departmentNode row, worksInRel row ]

graph = unfoldGraph canonicalClassifier LastWriteWins rowToPatterns rows
```

Shared elements (the same department appearing in many rows) are reconciled
automatically by `mergeElement`. `unfoldGraph` is `fromPatterns` generalized to
arbitrary seed types.

**Hierarchy construction** — building a document hierarchy uses the recursive form:

```haskell
-- Expand a document tree node into its Pattern and child seeds
docExpand :: DocNode -> (Subject, [DocNode])
docExpand node = (nodeSubject node, children node)

chunkHierarchy = unfold docExpand documentRoot
```

`unfold` produces nested `Pattern v` structure directly from the recursive seed.

---

### 3. Categorized transformation: `mapGraph`, `filterGraph`, `foldGraph`

These are categorized analogues of standard functional operations, operating on
`GraphView extra v` with `GraphClass` information available. All produce a new
`GraphView extra v` — transformations compose before materialization.

#### `mapGraph`

Apply transformation functions per category:

```haskell
mapGraph :: GraphClassifier extra v
         -> (Pattern v -> Pattern v)  -- nodes
         -> (Pattern v -> Pattern v)  -- relationships
         -> (Pattern v -> Pattern v)  -- walks
         -> (Pattern v -> Pattern v)  -- annotations
         -> (Pattern v -> Pattern v)  -- other
         -> GraphView extra v -> GraphView extra v
```

For cases where all categories receive the same transformation:

```haskell
mapAllGraph :: (Pattern v -> Pattern v) -> GraphView extra v -> GraphView extra v
```

#### `filterGraph`

Retain elements satisfying a predicate; apply `Substitution` to handle containers of
removed elements coherently:

```haskell
filterGraph :: GraphClassifier extra v
            -> (GraphClass extra -> Pattern v -> Bool)
            -> Substitution v
            -> GraphView extra v -> GraphView extra v
```

The predicate receives both the element's class and the element itself. `Substitution`
determines what happens to containers of filtered-out elements.

#### `foldGraph`

Reduce a graph view to a summary value, with category information available:

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

---

### 4. Context-aware mapping: `mapWithContext`

Map over elements where each mapping function has access to the full graph context via
`GraphQuery`. The mapping function receives the element and the `GraphQuery` — it asks
for context lazily, paying only for what it uses:

```haskell
mapWithContext :: GraphClassifier extra v
               -> (GraphQuery v -> Pattern v -> Pattern v)
               -> GraphView extra v -> GraphView extra v
```

The `GraphQuery` is derived from the *original* view — snapshot semantics. All elements
are transformed against the same source, not against incrementally modified results.

**Example** — enrich each node with a count of its annotations:

```haskell
enrichWithAnnotationCount :: GraphView Void Subject -> GraphView Void Subject
enrichWithAnnotationCount =
  mapWithContext canonicalClassifier enrich
  where
    enrich q node =
      let count = length (queryAnnotationsOf canonicalClassifier q node)
      in  setAnnotationCount count node
```

**Example** — label each node with the walks it participates in:

```haskell
labelWithWalks :: GraphView Void Subject -> GraphView Void Subject
labelWithWalks =
  mapWithContext canonicalClassifier label
  where
    label q node =
      let walks = queryWalksContaining canonicalClassifier q node
      in  addWalkLabels (map (identify . value) walks) node
```

---

### 5. `paraGraph` — structure-and-topology-aware folding

`para` folds over a `Pattern v` tree with access to the full subtree at each position.
`paraGraph` extends this to graph topology: each element's result depends on both its
structural decomposition and its computed results from graph neighbors.

```haskell
paraGraph :: (GraphQuery v -> Pattern v -> [r] -> r)
          -> GraphView extra v
          -> Map (Id v) r
```

Each element receives the `GraphQuery` (full context), the element itself, and the
recursively computed results from its structural children (via `elements`). The result
is a map from identity to computed value.

This is the foundation for iterative message-passing algorithms — the graph-level
analogue of Pregel and GraphFrames' `AggregateMessages`. Label propagation, PageRank,
belief propagation, and community detection all follow this pattern.

**Processing order**: for DAGs, topological sort; for general graphs, fixpoint iteration
or caller-supplied ordering. The fixpoint variant:

```haskell
paraGraphFixed :: Eq r
               => (GraphQuery v -> Pattern v -> [r] -> r)
               -> r                   -- initial value for all elements
               -> GraphView extra v
               -> Map (Id v) r
```

Iterates until the result map is stable. The `Eq r` constraint detects convergence.

`paraGraph` belongs here rather than in `Pattern.Graph.Algorithms` because it operates
on `GraphView` (the full classified graph) rather than just `GraphQuery` (traversal
primitives). It is a transformation concern producing a result map over the graph, not
a path-finding or structural analysis concern.

---

## Deferred operations

The following operations were considered for this proposal and explicitly deferred.
They are documented here to inform future proposals.

### `mergeByPredicate` — fuzzy identity matching

Required for coreference resolution and entity reconciliation in knowledge graph
pipelines: two elements may represent the same real-world entity even if their `Id v`
values differ ("Apple Inc." and "Apple").

Deferred because:
- It is mutation-adjacent — semantically it is an insertion operation with a search step
  prepended, not a transformation in the map/fold/filter sense
- It is cross-cutting — it requires both `GraphQuery` (to scan for matches) and
  `GraphMutation` (to write the reconciled element), which are currently kept separate
- Its design has unresolved questions: `MultiMatchPolicy` (what to do when multiple
  existing elements match the predicate), scan scope (should it include `pgOther`?),
  and O(n) performance implications for large graphs

A future proposal should address `mergeByPredicate` alongside higher-order graph editing
operations (expansion, rewiring, reification, duplicate merging) which share similar
cross-cutting characteristics.

### `expandNode` — expand one element into a subgraph

Replace a single element with a set of new elements — for example, promoting a property
to a value node. Deferred pending the higher-order graph editing proposal.

### `rewireRelationship` — change relationship endpoints

Change the source or target of an existing relationship without delete-and-reinsert.
Deferred pending the higher-order graph editing proposal.

---

## Module location

There are three reasonable options for where this functionality lives.

**Option A: `Pattern.Graph.Transform`** — a new module alongside `Pattern.Graph.Algorithms`.

Pros: clean separation between read-only algorithms and transformations; consistent with
the existing `Pattern.Graph.*` namespace; single import for pipeline authors; `GraphView`
and all operations are co-located.
Cons: `unfoldGraph` feels closer to construction than transformation; the module
boundary is slightly arbitrary.

**Option B: Split across existing modules** — `unfoldGraph` in `Pattern.PatternGraph`;
`GraphView`, `mapGraph`, `filterGraph`, `foldGraph`, `mapWithContext`, `paraGraph` in
`Pattern.Graph.Transform`.

Pros: construction functions live near `fromPatterns` and `mergeElement` where users
expect them.
Cons: pipeline authors must import from two places; `unfold` and `unfoldGraph` are
separated from each other.

**Option C: `Pattern.Graph.Transform`** for all graph-level operations; `unfold` added
to `Pattern.Core` (where `para` lives); `GraphView` in `Pattern.Graph` (alongside
`GraphLens` and `GraphQuery`).

Pros: `unfold` alongside its categorical dual `para`; `GraphView` co-located with the
types it relates to; single import for pipeline operations.
Cons: `GraphView` and the operations that use it are in different modules.

**Recommendation: Option C** — `Pattern.Core.unfold` alongside `para`; `GraphView` and
`materialize` in `Pattern.Graph` alongside `GraphLens` and `GraphQuery`; everything else
in `Pattern.Graph.Transform`. `GraphView` belongs near `GraphLens` and `GraphQuery`
because it *is* the abstraction that unifies them — it is not specific to transforms.
Pipeline authors get a single import from `Pattern.Graph.Transform` for operations, and
`Pattern.Graph` for the `GraphView` type itself.

---

## Implementation order

This proposal is step 3 in the sequence: GraphClassifier → GraphQuery → GraphTransform
→ GraphMutation. `GraphMutation`'s construction path depends on `GraphView` being
settled, since `defaultMutation` should accept a `GraphView` rather than a `PatternGraph`
directly.

`Substitution v` (used by `filterGraph`) must be defined before `GraphTransform` is
implemented. It can be extracted to a shared types module (`Pattern.Graph.Types`) ahead
of the full `GraphMutation` implementation.

Suggested internal ordering within this feature:
1. `GraphView` and `materialize` in `Pattern.Graph`
2. `unfold` in `Pattern.Core`
3. `unfoldGraph`, `mapGraph`, `mapAllGraph`, `foldGraph` — no `GraphQuery` dependency
4. `mapWithContext`, `filterGraph` — require `GraphQuery` and `Substitution`
5. `paraGraph`, `paraGraphFixed` — require `GraphQuery`; processing order strategy needed

---

## What changes in pattern-hs

| Component | Action | Notes |
|---|---|---|
| `Pattern.Core.unfold` | **New** | Anamorphism dual to `para`; Pattern-level construction |
| `Pattern.Graph.GraphView` | **New** | Universal graph-like interface; pairs `GraphQuery` with classified elements |
| `Pattern.Graph.materialize` | **New** | `GraphView → PatternGraph`; explicit materialization step |
| `Pattern.Graph.fromPatternGraph` | **Update** | Returns `GraphView` in addition to existing `GraphQuery` |
| `Pattern.Graph.fromGraphLens` | **Update** | Returns `GraphView` in addition to existing `GraphQuery` |
| `Pattern.Graph.Transform` | **New module** | All graph-level transformation functions |
| `Pattern.Graph.Transform.unfoldGraph` | **New** | Seed-based graph construction into `GraphView` |
| `Pattern.Graph.Transform.mapGraph` | **New** | Categorized map over `GraphView` |
| `Pattern.Graph.Transform.mapAllGraph` | **New** | Uniform map over all categories |
| `Pattern.Graph.Transform.filterGraph` | **New** | Predicate filter with `Substitution` for containers |
| `Pattern.Graph.Transform.foldGraph` | **New** | Categorized fold over `GraphView` |
| `Pattern.Graph.Transform.mapWithContext` | **New** | Context-aware map via `GraphQuery` |
| `Pattern.Graph.Transform.paraGraph` | **New** | Structure-and-topology-aware fold; Pregel-style foundation |
| `Pattern.Graph.Transform.paraGraphFixed` | **New** | Fixpoint variant of `paraGraph` for iterative algorithms |

---

## Open questions

1. **`mapWithContext` snapshot semantics** — the proposal specifies snapshot semantics
   (mapping function sees pre-transformation graph). An incremental model where each
   transformation sees previous results enables propagation patterns (e.g. label
   spreading) but is harder to reason about. Worth deciding before implementation.

2. **`filterGraph` and walk coherence** — filtering out a relationship that is part of
   a walk leaves the walk with a gap. The `Substitution` parameter handles this, but
   `NoSubstitution` default produces a broken walk in `pgOther`. Should `filterGraph`
   have a smarter default, or is explicit `Substitution` sufficient? Recommend explicit
   for now.

3. **`paraGraph` processing order** — for general graphs (with cycles), fixpoint
   iteration is the principled approach but requires `Eq r` and a convergence guarantee.
   A caller-supplied ordering is simpler but puts the burden on the user. Worth deciding
   the default strategy before implementation; both variants (`paraGraph` and
   `paraGraphFixed`) are included to cover the two cases.

---

## Summary of decisions

- **`GraphView` is the universal graph-like interface**: any graph representation
  produces a `GraphView`; any pipeline starts with one. `GraphLens`, `PatternGraph`,
  and external sources (CSV, JSON, databases) all enter the pipeline via `GraphView`.
- **`materialize` is the explicit boundary**: transformations compose lazily over
  `GraphView`; concrete `PatternGraph` storage is produced on demand. The pipeline
  is a sequence of `GraphView → GraphView` functions terminated by `materialize`.
- **`GraphView` belongs in `Pattern.Graph`**: it is the abstraction that unifies
  `GraphLens` and `GraphQuery`, not a transform-specific type.
- **Plain functions, not a record-of-functions**: graph transformations are operations
  over `GraphView`, not a capability interface. They don't vary by backing representation.
- **`unfold` belongs in `Pattern.Core`**: it is the categorical dual to `para`.
  `unfoldGraph` wraps it for graph construction.
- **All transformation operations work over `GraphView`**: `mapGraph`, `filterGraph`,
  `foldGraph`, `mapWithContext`, `paraGraph` all take and return `GraphView`. Write
  operations (`materialize`, `unfoldGraph`) return `PatternGraph`.
- **`paraGraph` is the Pregel foundation**: structure-and-topology-aware folding enables
  iterative message-passing algorithms. `paraGraphFixed` handles cyclic graphs via
  fixpoint iteration.
- **Snapshot semantics for `mapWithContext`**: the mapping function sees the
  pre-transformation graph. Predictable, parallelizable, avoids order-dependency.
- **`filterGraph` delegates container handling to `Substitution`**: reuses `GraphMutation`
  machinery; `Substitution` extracted to a shared types module for early availability.
- **Implementation order**: GraphClassifier → GraphQuery → GraphTransform → GraphMutation.
  `GraphView` must be settled before `GraphMutation`'s construction path is finalized.
- **`mergeByPredicate` deferred**: cross-cutting, mutation-adjacent, unresolved design
  questions. Future proposal alongside higher-order graph editing operations.
- **Normalization is out of scope**: as established in the GraphClassifier proposal.
- **`paraMap` and `paraWith` remain deferred**: `paraGraph` covers the graph-level use
  cases; no concrete cases have emerged for the Pattern-level variants.
