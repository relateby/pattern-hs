# paraGraph Reference

**Audience**: Library users and implementers
**Purpose**: Complete reference for structure-aware folding with `paraGraph` under `topoShapeSort`

## Overview

`paraGraph` is now the graph-scoped wrapper in the same unified scope model that also powers `para`.

Where `para` uses `TrivialScope` over one subtree, `paraGraph` keeps graph scheduling unchanged and derives generic scope answers from the whole `GraphView` snapshot. That graph adapter remains internal; the public API of `paraGraph` is unchanged.

The key distinction is now explicit:

- `para` folds one recursive tree with subtree-only scope
- `paraGraph` folds one `GraphView` snapshot with graph-wide scope

In both cases, the fold step is still driven by already-computed direct child results.

The key challenge: a `GraphView` is a flat list, not a tree. `topoShapeSort` establishes the right processing order before the fold begins, so that every element is processed only after all elements it directly contains.

## Mental Model

Think of a graph as having layers of structural complexity:

```
Layer 0: Nodes          — atomic, contain nothing
Layer 1: Relationships  — contain two nodes
Layer 2: Walks          — contain a chain of relationships
Layer 3: Annotations    — contain one element of any layer
Layer 4: Other (GOther) — contain elements of any structure
```

`paraGraph` processes layers 0 → 4 in order. Within layers 3 and 4, elements are additionally sorted by their dependency on each other: if annotation A annotates annotation B, B is processed before A.

When your fold function `f` is called for element `p`, the `subResults` list contains computed results for every direct sub-element of `p` that has already been processed — which, in the absence of cycles, is all of them.

## Unified Scope Relationship

Graph folds now expose generic scope answers through `scopeDictFromGraphView`:

```haskell
scopeDictFromGraphView :: GraphValue v => GraphView extra v -> ScopeDict (Id v) v
```

This gives higher-order helpers access to:

- `allElements` across the full classified `GraphView`
- identity lookup bounded to that snapshot
- direct containers across relationships, walks, annotations, and `GOther`
- sibling derivation from those direct containers

`paraGraph` itself still passes `GraphQuery v` to the user algebra so existing call sites do not change.

## Function Reference

### `paraGraph`

```haskell
paraGraph
  :: GraphValue v
  => (GraphQuery v -> Pattern v -> [r] -> r)
  -> GraphView extra v
  -> Map (Id v) r
```

**Description**: Single-pass structure-aware fold. Processes elements in `topoShapeSort` order. Returns a map from element identity to fold result.

**Parameters**:
- `f :: GraphQuery v -> Pattern v -> [r] -> r` — Folding function:
  - `GraphQuery v`: Snapshot query of the original graph (immutable; use for incident-element lookups)
  - `Pattern v`: Current element being processed
  - `[r]`: Results for this element's direct sub-elements (see `subResults` contract below)
- `view :: GraphView extra v`: The graph to fold over

**Returns**: `Map (Id v) r` — result keyed by element identity

**The `subResults` Contract**:

`subResults` is a best-effort list of results for direct sub-elements. In the common case (cycle-free graph), it contains exactly one result per direct sub-element, in `elements p` order.

If a sub-element has not yet been processed when `p` is reached — which can occur when a dependency cycle exists within the annotation or `GOther` bucket — that sub-element's result is **omitted** from `subResults`. Your fold function should handle `[]` as a valid input, not an error condition.

**Example**:
```haskell
-- For each element, count how many direct sub-element results are available
countDeps :: GraphValue v => GraphView extra v -> Map (Id v) Int
countDeps = paraGraph (\_ _ subResults -> length subResults)
-- Node:         0
-- Relationship: 2 (both endpoint nodes)
-- Walk A-B-C:   2 (relationships A-B and B-C)
-- Annotation:   1 (the annotated element, if processed)
```

---

### `paraGraphFixed`

```haskell
paraGraphFixed
  :: (GraphValue v, Ord (Id v))
  => (r -> r -> Bool)
  -> (GraphQuery v -> Pattern v -> [r] -> r)
  -> r
  -> GraphView extra v
  -> Map (Id v) r
```

**Description**: Iterates `paraGraph` rounds until the convergence predicate is satisfied. Each round uses the same `topoShapeSort` ordering (the `GraphView` is immutable). The initial value `r0` seeds all elements before the first round.

**Parameters**:
- `conv :: r -> r -> Bool`: Convergence test. `conv old new = True` means stable.
- `f`: Same fold function as `paraGraph`
- `r0 :: r`: Initial result assigned to all elements before the first round
- `view`: The graph to fold over

**Example**:
```haskell
-- Propagate max value until stable
propagateMax :: GraphValue v => GraphView extra v -> Map (Id v) Double
propagateMax = paraGraphFixed
  (\old new -> abs (old - new) < 0.001)
  (\_ _ subResults -> if null subResults then 0.0 else maximum subResults)
  0.0
```

---

## Processing Order: `topoShapeSort`

`topoShapeSort` establishes the fold order in two passes:

### Pass 1 — Inter-bucket ordering

Shape classes are processed in this fixed sequence:

| Priority | Shape Class   | Contains                  | Why this position |
|----------|--------------|---------------------------|-------------------|
| 1st      | `GNode`      | Nothing                   | Atomic — no deps |
| 2nd      | `GRelationship` | 2 × `GNode`           | Deps are in layer 1 |
| 3rd      | `GWalk`      | N × `GRelationship`       | Deps are in layer 2 |
| 4th      | `GAnnotation`| 1 × any element           | Can reference any layer |
| 5th      | `GOther`     | N × any element           | Unconstrained — processed last |

### Pass 2 — Within-bucket ordering (Kahn's algorithm)

Applied to `GAnnotation` and `GOther` buckets. For each element `p` in the bucket, its sub-elements (`elements p`) that also belong to the same bucket are treated as dependencies — they must appear before `p`.

Kahn's algorithm processes zero-dependency elements first, then "unlocks" elements whose dependencies have been satisfied.

`GNode`, `GRelationship`, and `GWalk` require no within-bucket sort: by the definition of `classifyByShape`, their sub-elements always belong to a lower-priority bucket.

### Cycle Handling

If a dependency cycle is detected within a bucket (e.g., annotation A annotates annotation B, and annotation B annotates A), the cycle members are appended after all non-cycle elements in that bucket, in their encountered order. No error is raised. Cycle members will receive `subResults = []` for the other members of their cycle.

Callers are responsible for providing cycle-free data if complete `subResults` are required.

## Laws and Properties

### Containment Before Container

For any element `p` with sub-element `e` in the same shape-class bucket:

```
position(e) < position(p) in topoShapeSort output
  — unless e and p are in a dependency cycle
```

### Sub-element Result Availability

```
If identify (value e) ∈ dom(Map from paraGraph result)
  and e ∈ elements p
  and e and p are not in a cycle
Then result of e ∈ subResults when f is called for p
```

### No Element Dropped

```
length (topoShapeSort elems) == length elems
```

### Immutable Snapshot Semantics

The `GraphQuery v` passed to `f` is the original snapshot — it does not reflect mutations applied to earlier elements in the same fold. This is the same contract as `mapWithContext`.

## Annotation-of-Annotation Example

Consider a graph with:
- Node `n` (identity `"n"`)
- Annotation `b` annotating `n` (identity `"b"`)
- Annotation `a` annotating `b` (identity `"a"`)

**Processing order** (determined by `topoShapeSort`):
1. `n` (GNode, no deps) → `f q n [] = result_n`
2. `b` (GAnnotation, depends on `n` — cross-bucket, always satisfied) → `f q b [result_n] = result_b`
3. `a` (GAnnotation, depends on `b` — within-bucket, satisfied by Pass 2) → `f q a [result_b] = result_a`

Without within-bucket ordering, `a` might be processed before `b`, receiving `subResults = []`.

## Edge Cases

### Graph with only nodes

All elements are `GNode`. `topoShapeSort` returns them in encountered order (no within-bucket deps). All `f` calls receive `subResults = []`.

### Annotation over a Walk

The walk's relationships and their nodes are processed in layers 0–2. The annotation (layer 3) receives the walk's fold result in `subResults`. This is the standard cross-bucket case — no special handling needed.

### Self-referential annotation

An annotation that contains itself in `elements` creates a 1-element cycle. It will be processed in encountered order with `subResults = []`. This is the degenerate case of the cycle soft-failure path.

### `GOther` with deeply nested `GOther`

`topoShapeSort` inspects `elements p` for each `GOther` element and applies Kahn's algorithm within the `GOther` bucket. Deeply nested `GOther` chains are sorted correctly as long as they are cycle-free. Cycles degrade to the soft-failure behaviour described above.

## Relationship to `para` (Pattern Paramorphism)

| | `para` | `paraGraph` |
|-|--------|------------|
| Input | Single `Pattern v` | `GraphView extra v` (flat collection) |
| Scope boundary | `TrivialScope` over one subtree | Internal `GraphView`-backed scope over one snapshot |
| Structure | Recursive tree descent | `topoShapeSort` then linear fold |
| Order | Bottom-up by tree depth | Bottom-up by shape class + within-bucket deps |
| Sub-results | Always complete (all children) | Best-effort (may be empty for cycle members) |
| Output | Single `r` | `Map (Id v) r` |

Use `para` when folding a single pattern structure.
Use `paraGraph` when folding across a classified graph of heterogeneous elements.

## Cross-Language Implementation Notes

Implementations in other languages should replicate the two-pass ordering:

1. **Partition** elements by shape class into five ordered buckets.
2. **Within `GAnnotation` and `GOther` buckets**, run a topological sort using the element identity and `elements` list to determine dependencies (Kahn's algorithm recommended for its natural cycle detection).
3. **Concatenate** buckets in order: nodes, relationships, walks, annotations, other.
4. **Fold** with the step function, maintaining an accumulator map keyed by element identity.

The identity type must be hashable/orderable. The `maybe []` (soft miss) pattern in step 4 is essential: `subResults` for cycle members must yield an empty list, not an error.

## See Also

- [Paramorphism Reference](./paramorphism.md) — `para` for single patterns
- [Pattern Graph Reference](./pattern-graph.md) — `PatternGraph` container and `toGraphView`
- [Graph Lens Reference](./graph-lens.md) — constructing `GraphView` from a `PatternGraph`
