# Motivating Scenarios: Graph Pipelines

**Purpose**: Grounding examples for the graph transformation feature proposal and
related design decisions. These scenarios were developed to identify common requirements
across realistic graph construction and transformation workloads.

---

## The unified pipeline model

Every pipeline begins by applying a `GraphView` over the source data. A CSV, JSON
document, relational table, or existing `PatternGraph` are all source graphs once a
view is imposed on them. The pipeline is then a pure sequence of graph transformations,
terminated by an explicit `materialize` step when concrete storage is needed:

```
source data
  → GraphView  (impose a graph interpretation)
  → [GraphView → GraphView transformations]
  → materialize → PatternGraph  (when concrete storage is required)
```

No special ingestion step. No impedance mismatch between raw data and graph operations.
The source is a graph from the moment the view is applied. This mirrors the GraphFrames
model, where DataFrames *are* graphs when vertex and edge columns are designated.

---

## Scenario 1: ETL from Structured Data

Transform a structured dataset (CSV, JSON, relational) into a graph. Each record becomes
a node; relationships are inferred from shared field values.

**Example**: An employee dataset where each row is a person node, and employees sharing
a department are connected via a department node.

```
GraphView (CSV rows as nodes, shared values as relationships)
  → filterGraph (exclude inactive employees)
  → mapWithContext (enrich with computed fields)
  → materialize → PatternGraph
```

### GraphView interpretation

A CSV row is a node. Shared column values (e.g. the same department name) are implicit
relationships — or with a richer view, a department value becomes a department node and
each person gets a relationship to it. The `GraphView` constructor for CSV designates
which columns become node identities, which become relationship targets, and how values
map to `Subject`.

### Steps

1. Apply `fromCSV` (or equivalent adapter) to produce `GraphView` — rows as nodes,
   foreign key columns as relationships to shared value nodes
2. `filterGraph` — exclude rows not meeting criteria
3. `mapWithContext` — enrich nodes using context (e.g. annotation counts, neighborhood
   properties)
4. `materialize` — produce `PatternGraph` when concrete storage is needed

### Characteristics

- **Local expansion**: each seed (row) produces its elements independently
- **Shared element reconciliation**: department nodes appear in multiple rows;
  `mergeElement` with `ReconciliationPolicy` handles deduplication at materialization
- **Batch construction**: the full dataset is processed before querying begins

### Operations surfaced

- `fromCSV :: GraphClassifier extra v -> CSV -> GraphView extra v` — adapter (not in
  this proposal; defines the target interface)
- `filterGraph`, `mapWithContext` — transform before materialization
- `materialize` — explicit `GraphView → PatternGraph` step
- `unfoldGraph` — alternative construction path when expansion logic is complex

---

## Scenario 2: Graph-to-Graph Transformation

Apply a transformation to an existing graph, producing a new graph. Common cases:
normalize labels, enrich nodes with computed properties, project a subgraph, or
convert one graph schema to another.

**Example**: Normalize all node labels to lowercase; add a `visited` flag to every
node reachable from a given starting node via BFS.

```
GraphView (source PatternGraph or GraphLens)
  → filterGraph (select relevant elements)
  → mapWithContext (enrich or normalize)
  → materialize → PatternGraph
```

### GraphView interpretation

An existing `PatternGraph` is viewed via `fromPatternGraph`; a `GraphLens` via
`fromGraphLens`. The transformation pipeline is source-agnostic — the same operations
apply regardless of which representation produced the view.

### Steps

1. `fromPatternGraph` or `fromGraphLens` — produce `GraphView`
2. `filterGraph` — select elements by category and predicate
3. `mapWithContext` — apply transformation with neighborhood access
4. `materialize` — produce new `PatternGraph`

### Characteristics

- **Category-aware transformation**: different functions per `GraphClass` bucket
- **Structure preservation**: value updates don't shift classification under canonical classifier
- **Source agnosticism**: same pipeline works over `PatternGraph`, `GraphLens`, or any
  future representation that produces a `GraphView`

### Operations surfaced

- `fromPatternGraph`, `fromGraphLens` — `GraphView` constructors
- `mapGraph`, `filterGraph`, `foldGraph` — categorized map, filter, fold
- `mapWithContext` — neighborhood-aware transformation

---

## Scenario 3: Knowledge Graph Construction from Unstructured Data

A multi-stage pipeline that constructs a knowledge graph from raw documents. Each stage
enriches the graph further. This is the most complex pipeline and surfaces the most
novel requirements.

```
GraphView (document as graph)
  → mapWithContext / paraGraph (chunk, extract entities)
  → filterGraph (reconcile, validate against schema)
  → materialize → PatternGraph (knowledge graph)
```

### GraphView interpretation

A document is interpreted as a graph from the start. The document itself is a node.
Chunks are nodes. The sequence of chunks is a walk. The document hierarchy (sections,
chapters) is a containment structure. A `fromDocument` adapter produces this `GraphView`
— the pipeline never handles raw text directly.

**Schema as graph**: the entity type schema is itself a `GraphView` over a `PatternGraph`.
NER validation queries the schema via `GraphQuery`. One graph guides the construction
of another.

### Stage 1: Document chunking

```
[doc_sequence | [r1 | chunk_1, chunk_2], [r2 | chunk_2, chunk_3], ...]
```

The sequence is a walk; the hierarchy is a tree of containment relationships.

**Operations surfaced**:

- `unfold :: (a -> (v, [a])) -> a -> Pattern v` — recursive structure construction for
  hierarchy
- `zipAdjacent :: [Pattern v] -> (Pattern v -> Pattern v -> Pattern v) -> [Pattern v]`
  — produce relationships from adjacent pairs for sequence walk

### Stage 2: Named Entity Recognition (NER)

Extract entity mentions from each chunk. Each mention is a candidate node. A schema
graph shapes what entity types are recognized. Extracted entities must be reconciled
with existing entities — within the same chunk, across the document, and against a
pre-existing knowledge base.

**Schema as graph**: entity types, permitted relationship types, and cardinality
constraints are themselves nodes and relationships in a schema graph. NER validation
is a query over the schema graph via `GraphQuery`.

**Coreference resolution**: two mentions with different surface forms may refer to the
same real-world entity ("Apple Inc." and "Apple"). Reconciliation requires semantic
equivalence, not identity equality. This is a *fuzzy merge*:

```haskell
mergeByPredicate :: (Pattern v -> Pattern v -> Bool)
                 -> ReconciliationPolicy (MergeStrategy v)
                 -> Pattern v
                 -> PatternGraph v
                 -> PatternGraph v
```

Searches existing nodes for any satisfying the equivalence predicate against the
incoming node; reconciles if found, inserts if not. O(n) search — expensive but
semantically honest. Raises a further question when multiple existing nodes match:
which takes precedence, or should all matches be merged?

**Operations surfaced**:

- `mergeByPredicate` — fuzzy identity matching; genuinely new, not covered by existing
  `mergeElement` which reconciles by identity only
- Schema-guided classification — `GraphQuery` over a second `PatternGraph` (the schema)
  to validate entity types during construction

### Stage 3: Relationship extraction

Given pairs of entity nodes, extract and assert typed relationships between them.
Relationship types are validated against the schema graph.

**Operations surfaced**:

- Look up source and target nodes by identity or predicate — `queryNodeById` or filter
- Schema validation — `GraphQuery` over schema graph to check permitted relationship
  types between entity type pairs
- `mergeElement` — standard insertion once relationship is validated

### Stage 4: Fact storage

Store extracted facts about entities. Two representations are possible:

**As triples** — `(entity, predicate, value)` expressed as an annotation or relationship:

```
[fact_rel | entity_node, value_node]   -- relationship to a value node
[fact_ann | entity_node]               -- annotation with predicate+value in its value field
```

**As properties** — merge the fact directly into the entity node's value field,
combining properties maps.

**Operations surfaced**:

- **Value-level merge** — `v -> v -> v` combining function for merging properties into
  an existing node's value without changing structure. Related to `ReconciliationPolicy`
  but operating purely on `v`, not on `Pattern v`.
- The choice between triples and properties is a schema decision; both representations
  are valid and the design should support either without privileging one.

---

## Common Requirements Across All Three Scenarios

| Operation | Scenario 1 | Scenario 2 | Scenario 3 |
|---|---|---|---|
| `GraphView` (source as graph) | ✅ CSV as graph | ✅ existing graph | ✅ document as graph |
| `materialize` (GraphView → PatternGraph) | ✅ | ✅ | ✅ |
| `unfold` (seed → elements) | ✅ rows → nodes | — | ✅ doc → chunks → hierarchy |
| `mapGraph` (categorized map) | — | ✅ normalize, enrich | ✅ NER labeling |
| `foldGraph` (categorized fold) | — | ✅ aggregation | ✅ fact summarization |
| `filterGraph` (with coherent deletion) | — | ✅ subgraph projection | ✅ entity filtering |
| `mapWithContext` (context-aware map) | — | ✅ enrichment | ✅ annotation-aware NER |
| `paraGraph` (iterative aggregation) | — | ✅ label propagation | ✅ entity scoring |
| `mergeByPredicate` (fuzzy merge, deferred) | — | — | ✅ coreference resolution |
| Schema as graph | — | — | ✅ NER + relationship validation |
| Value-level merge | ✅ reconciliation | ✅ enrichment | ✅ property accumulation |
| Pipeline composition | ✅ | ✅ | ✅ |

### The unified model

Every scenario is a sequence of `GraphView → GraphView` transformations terminated by
`materialize`. The pipeline is purely compositional. No scenario requires special-casing
of source data — source data is always imposed with a `GraphView` first.

### `mergeByPredicate` deferred

`mergeByPredicate` is genuinely novel and required for knowledge graph quality. It is
not covered by `mergeElement` (which requires identity equality). It is expensive
(O(n) scan), has non-trivial semantics when multiple nodes match, and is
mutation-adjacent rather than a pure transformation. Deferred to a future proposal
alongside higher-order graph editing operations.

### Schema as graph

A schema is just a `PatternGraph v` queried via `GraphQuery`. One `GraphView` guides
the construction of another. This is a usage pattern, not a new type.

### Value-level merge

`v -> v -> v` for property accumulation is a requirement on the value type, not a graph
operation. Worth considering as an optional typeclass or a parameter to affected
operations rather than a hard constraint on `GraphValue`.

---

## Notes for Feature Proposal

These scenarios ground the following design directions in `graph-transform.md`:

- **`GraphView`** is the universal pipeline entry point and graph-like interface —
  every pipeline starts here; every representation produces one
- **`materialize`** is the explicit `GraphView → PatternGraph` step — transformations
  compose lazily; concrete storage is deferred
- **`unfold`/`mapGraph`/`foldGraph`/`filterGraph`/`mapWithContext`/`paraGraph`** form
  the core of `Pattern.Graph.Transform` — operating on `GraphView` throughout
- **`paraGraph`** is the Pregel/AggregateMessages foundation for iterative algorithms
- **`mergeByPredicate`** warrants its own future proposal
- **Schema as graph** is documented as a pattern, not encoded in the type system
