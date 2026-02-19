# Proposal: GraphClassifier — A Unified, Extensible Graph View for pattern-hs

**Status**: 📝 Design Only  
**Date**: 2026-02-19  
**Relates to**: Feature 23 (GraphLens), Feature 33 (PatternGraph)  
**Followed by**: GraphQuery proposal

---

## Summary

Introduce `GraphClassifier` as the unifying abstraction for interpreting `Pattern v` as a
graph. The canonical graph form (currently split across `GraphLens` and `PatternGraph`)
is reconceived as a *family of views* over one storage substrate — `Pattern v` — where
the canonical instance uses `isAtomic` as its node predicate and arity-based rules for
higher categories. `GraphLens` is re-derived from `GraphClassifier` as its two-category
specialization; `PatternGraph` is updated to accept a `GraphClassifier` at construction.
`GraphClassifier` becomes the shared vocabulary that unifies them.

---

## Motivation

### The current situation

`GraphLens` and `PatternGraph` were designed with related but separate concerns:

- **`GraphLens`** is a predicate-based interpretive view: given a `Pattern v` and a
  `testNode` predicate, it interprets elements as nodes, relationships, and walks on demand.
  All graph operations live here. The predicate is injected at construction time.

- **`PatternGraph`** is a materialized container: it eagerly classifies `Pattern v`
  elements into four maps (nodes, relationships, walks, annotations) using a fixed
  arity-based classifier baked into the `GraphValue` typeclass. The `PatternGraph` must
  be converted to a `GraphLens` to use graph algorithms.

These share a common insight — graph structure is an *interpretation* of `Pattern v`, not
intrinsic to it — but they express that insight differently and don't share a classification
vocabulary. The result is two parallel type hierarchies with no common interface.

### The unifying observation

`PatternGraph` is equivalent to a `GraphLens` with `isAtomic` as `testNode`, plus an
additional arity-based classification pass that further partitions non-nodes into
relationships, walks, and annotations. Both are views over `Pattern v`. The difference is
*when* classification happens (construction vs. query time) and *how many categories* are
distinguished.

This means the right design is:

> One storage substrate (`Pattern v`). A family of views. The canonical graph is the view
> with `isAtomic` as `testNode` and arity-based upper classification. `GraphClassifier`
> is the value that defines the classification contract for any view.

---

## Design

### `GraphClass` — a shared, extensible classification vocabulary

```haskell
data GraphClass extra
  = GNode
  | GRelationship
  | GAnnotation
  | GWalk
  | GOther extra
```

The `extra` type parameter is the user's extension point. The canonical classifier uses
`Void` (or the equivalent uninhabited type), making `GOther` unreachable by construction.
A user interested in simplicial complexes would define:

```haskell
data SimplexClass = Simplex2 | Simplex3 | SimplexN Int

-- Their classifier produces: GraphClass SimplexClass
```

The `GOther Void` specialization is the canonical, closed form. `GOther extra` is the
open extension path. Both use the same type and the same downstream consumers.

#### Why walks are a named bucket

Walks are prevalent enough in graph data (paths, traversals, gram path notation) to
warrant a first-class bucket rather than relegation to `GOther`. The five named categories
— node, relationship, annotation, walk, other — correspond directly to what gram notation
can express and what `PatternGraph` currently stores.

### `GraphClassifier` — injectable classification logic

```haskell
data GraphClassifier extra v = GraphClassifier
  { classify :: Pattern v -> GraphClass extra
  }
```

This is a record of functions, not a typeclass. This makes it a first-class value that
can be passed, stored, and varied at runtime — more portable across language targets than
a typeclass, and more composable.

The canonical classifier for `Pattern Subject`:

```haskell
canonicalClassifier :: GraphClassifier Void Subject
canonicalClassifier = GraphClassifier
  { classify = classifyByArity
  }
```

where `classifyByArity` is the existing arity-based logic currently in `GraphValue`.

### Eager vs. lazy evaluation

`GraphClassifier.classify` is a pure function. The implementation decides when to call it:

- **Eager** (current `PatternGraph` style): call `classify` at insertion time, store the
  result in typed maps. Subsequent queries are O(1) lookups.
- **Lazy** (current `GraphLens` style): call `classify` at query time. No upfront cost;
  repeated queries re-evaluate.
- **Memoized**: call lazily, cache results. Same interface as lazy, amortized O(1) for
  repeated queries.

Because `GraphClassifier` is a value passed to the container, a user can swap
implementations without changing the container's API. This is the answer to the
computational cost concern for arity-n predicates (walk and `GOther` classification):
expensive predicates can be wrapped in a memoizing `GraphClassifier` without changing
how the container or query layer is written.

---

## Relationship to existing code

### `GraphLens` — re-derived from `GraphClassifier`

`GraphLens` with a `testNode` predicate is the two-category specialization of
`GraphClassifier`: a pattern either satisfies `testNode` (it is a node) or it does not
(it falls into the residual category, from which relationships, walks, etc. are further
derived by structure). `GraphClassifier` generalizes this to five named categories.

`GraphLens` is re-derived by implementing its operations in terms of `GraphClassifier`:

```haskell
-- A two-category classifier built from a testNode predicate.
-- GNode when testNode holds; GOther Void otherwise (relationships and walks
-- are distinguished structurally within the GOther branch, as today).
fromTestNode :: (Pattern v -> Bool) -> GraphClassifier Void v
fromTestNode testNode = GraphClassifier
  { classify = \p -> if testNode p then GNode else GOther absurd
  }
```

All existing `GraphLens` operations (nodes, relationships, walks, neighbors, bfs,
findPath, etc.) are preserved with identical signatures, re-implemented to delegate to
`GraphClassifier` internally. The `GraphLens` data type may be retained as a named
constructor for the two-category case or aliased — that is an implementation decision.

**The public API of `GraphLens` is preserved. The internals are re-implemented.**

### `PatternGraph` — updated, not replaced

`PatternGraph` stays as the canonical eager materialized store. Its classification logic
is extracted from the `GraphValue` typeclass and moved into a `GraphClassifier Void Subject`
value (the canonical classifier). The `GraphValue` typeclass is simplified: it retains
`identify` (identity extraction) but loses `classify` (which moves to `GraphClassifier`).

`MergeResult` is dropped. Construction returns a `PatternGraph v` directly. The
`Unrecognized` concept is replaced by `GOther`, which is a named storage bucket like any
other. A graph is a bag of categorized data structures; every input pattern has a place.
Users who want to inspect what landed in `GOther` query the graph's `pgOther` map.

**This is a breaking change to `PatternGraph` and `GraphValue`**, but `PatternGraph` is
new (Feature 33) and the change is an improvement to the design.

### `GraphValue` — simplified

Before:
```haskell
class Ord (Id v) => GraphValue v where
  type Id v
  identify :: v -> Id v
  classify :: Pattern v -> PatternClass  -- ← moves out
```

After:
```haskell
class Ord (Id v) => GraphValue v where
  type Id v
  identify :: v -> Id v
```

Classification is no longer a typeclass method. It is a `GraphClassifier` value passed
explicitly. This separates two orthogonal concerns: *what is this element's identity*
(belongs on the value type) vs. *what graph category does this element belong to*
(belongs on the classifier, which can vary independently).

Note that traversal direction and weighting are also not classification concerns — they
are traversal policy, which belongs to `GraphQuery` and its `TraversalWeight` parameter.
See the GraphQuery proposal.

---

## Construction ergonomics

The common path should still be simple. A user building a graph from gram notation:

```haskell
-- Using the canonical classifier (most users)
let graph = fromPatterns canonicalClassifier patterns

-- Using a custom classifier (power users)
let graph = fromPatterns myClassifier patterns
```

The canonical classifier is provided by the library and requires no configuration.
The extension path is explicit and opt-in.

---

## Portability notes

This design was evaluated for portability to Rust, TypeScript, and Java.

**`GraphClass extra` type parameter** ports to all targets as a generic/parameterized type.
`GraphClass<Extra>` is idiomatic in all three.

**`Void` / uninhabited type for canonical form:**

| Language | Mechanism | Enforcement |
|---|---|---|
| Haskell | `Void` (from `Data.Void`) | Compiler: `GOther` is unreachable |
| Rust | `!` (never type) | Compiler: `GOther(_)` arm is unreachable |
| TypeScript | `never` | Compiler (with `strictNullChecks`): exhaustiveness checks work |
| Java | Convention (`Void` class, private constructor) | No compile-time enforcement; runtime only |

**`GraphClassifier` as a record of functions** is more natural in TypeScript and Rust
than a typeclass, since those languages handle first-class function objects idiomatically.
Java would use an interface. Haskell could use either; the record-of-functions
representation is preferred here for portability.

**Exhaustiveness on `GraphClass` matching** is guaranteed at compile time in Haskell and
Rust (with `GOther Void` / `GOther!`), achievable with care in TypeScript, and
conventional-only in Java. Implementations should document this difference explicitly in
the porting guide.

---

## What changes in pattern-hs

| Component | Action | Notes |
|---|---|---|
| `Pattern.Graph.GraphLens` | **Re-derive** from `GraphClassifier` | Public API preserved; internals re-implemented |
| `Pattern.PatternGraph.GraphValue` | **Simplify** — remove `classify` | `identify` remains |
| `Pattern.PatternGraph.PatternClass` | **Replace** with `GraphClass extra` | Adds `extra` parameter; `Unrecognized` → `GOther extra` |
| `Pattern.PatternGraph.MergeResult` | **Remove** | Construction returns `PatternGraph v` directly; `GOther` is a bucket |
| `Pattern.PatternGraph.PatternGraph` | **Update** — accept `GraphClassifier` at construction | Storage maps gain `pgOther` |
| `Pattern.Graph.GraphClassifier` | **New** — introduced by this proposal | Core new type |
| `Pattern.Graph.GraphClass` | **New** — introduced by this proposal | Replaces `PatternClass` |
| Canonical classifier | **New** — `canonicalClassifier :: GraphClassifier Void Subject` | Extracted from `classifyByArity` |

---

## Open questions

1. **Where does `GraphClassifier` live?** `Pattern.Graph` (alongside `GraphLens`) or
   `Pattern.Classify` (its own module)? The former keeps graph-related types together;
   the latter separates classification from operations.

2. **`pgOther` storage shape**: `Map (Id v) (Pattern v)` is simple but loses the `extra`
   type information at the storage level. `Map (Id v) (extra, Pattern v)` preserves it.
   Worth investigating during implementation.

---

## Summary of decisions

- **One storage substrate**: `Pattern v` throughout. No non-Pattern graph representations.
- **Family of views**: canonical graph is `isAtomic` + arity classification; others via
  `GraphClassifier`.
- **Five named buckets**: node, relationship, annotation, walk, other (extensible).
  Walks are first-class due to prevalence.
- **`GOther extra` is a bucket, not a rejection**: construction never rejects; it
  categorizes. `MergeResult` is removed. A graph is a bag of categorized data structures;
  documentation communicates this expanded model to users.
- **`GraphClassifier` is a value, not a typeclass**: portable, composable, runtime-variable.
- **Classification separated from identity**: `GraphValue` retains `identify`; `classify`
  moves to `GraphClassifier`.
- **`GraphLens` is re-derived from `GraphClassifier`**: it is the two-category
  specialization, with `testNode` expressed as a `GraphClassifier` value. Public API
  is preserved; internals are re-implemented.
- **`PatternGraph` is updated, not replaced**: breaking changes are confined to the
  new Feature 33 code.
- **Normalization is out of scope**: transforming arbitrary `Pattern v` into canonical
  graph form (e.g. a sequence of nodes into a walk) is a separate problem. This proposal
  describes the target structure, not the transformation paths into it.
