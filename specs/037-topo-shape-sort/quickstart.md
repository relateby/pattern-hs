# Quickstart: Writing Fold Functions for `paraGraph`

**Feature**: 037-topo-shape-sort
**Audience**: Developers writing fold functions with `paraGraph`

## The Core Idea

`paraGraph` folds over a graph's elements in **containment order** — each element is processed only after all elements it directly contains. Your fold function receives:

```haskell
f :: GraphQuery v -> Pattern v -> [r] -> r
```

- `GraphQuery v` — snapshot query (for looking up related elements)
- `Pattern v` — the current element being processed
- `[r]` — results already computed for this element's direct sub-elements

The key insight: by the time `f` is called for a relationship, both its endpoint nodes have already been processed and their results are in `[r]`.

## Simple Example: Counting Sub-Results

```haskell
import Pattern.Graph.Transform (paraGraph)

-- For each element, count how many direct sub-elements it has
countDeps :: GraphValue v => GraphView extra v -> Map (Id v) Int
countDeps = paraGraph (\_ _ subResults -> length subResults)

-- Node        → 0 (atomic, no sub-elements)
-- Relationship → 2 (two endpoint nodes)
-- Walk (A-B-C) → 2 (two relationships A-B and B-C)
-- Annotation   → 1 (the element it annotates)
```

## Annotation-of-Annotation Example

When your graph contains annotation-of-annotation, `topoShapeSort` ensures the inner annotation is processed first. Your fold function can rely on this:

```haskell
-- Collect a "depth" value: how many annotation layers deep is this element?
annotationDepth :: GraphValue v => GraphView extra v -> Map (Id v) Int
annotationDepth = paraGraph step
  where
    step _ _ []           = 0  -- atomic or no sub-results available
    step _ _ subResults   = 1 + maximum subResults
```

Given annotation B annotating a node, and annotation A annotating B:
- Node → depth 0
- Annotation B (annotates Node) → depth 1
- Annotation A (annotates B) → depth 2 ✓

Without `topoShapeSort`'s within-bucket ordering, A might have been processed before B, receiving `[]` and returning `0` incorrectly.

## The `subResults` Contract

`subResults` contains results for sub-elements that have been processed. In most cases this is all direct sub-elements. The exception: **dependency cycles**.

```haskell
-- Safe pattern: treat subResults as best-effort
step _ p subResults =
  case subResults of
    [] -> defaultValue          -- no deps processed (atomic, or cycle)
    rs -> combineResults rs     -- some or all deps available
```

If annotation A references annotation B and B references A (a cycle), one of them will receive `subResults = []`. Your fold function should handle this gracefully — the same way it handles an atomic element.

## Using `paraGraphFixed` for Iterative Algorithms

For algorithms that need multiple passes to stabilise (e.g., propagating values through a graph until no change):

```haskell
-- Propagate maximum value from neighbours until stable
propagateMax :: GraphValue v => GraphView extra v -> Map (Id v) Double
propagateMax view =
  paraGraphFixed
    (\old new -> abs (old - new) < 0.001)  -- convergence test
    (\_ _ subResults -> if null subResults then 0.0 else maximum subResults)
    0.0   -- initial value for all elements
    view
```

`topoShapeSort` is applied every round with the same ordering — the `GraphView` is immutable.

## What `topoShapeSort` Does NOT Handle

- **Cross-class containment**: handled by the fixed inter-bucket order (nodes always before relationships, etc.)
- **GOther with unknown semantics**: `GOther` sub-elements are inspected structurally; if their identity is in the same `GOther` bucket, they are treated as dependencies
- **`paraGraph` callers with custom `GraphClassifier`**: `topoShapeSort` uses `identify (value e)` for dependency tracking, independent of classifier logic — the ordering is based on what a pattern structurally contains, not how it is classified
