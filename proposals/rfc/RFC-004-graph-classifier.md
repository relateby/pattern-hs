# RFC-004: GraphClassifier — Unified, Extensible Graph View

**Status:** draft
**Date:** 2026-02-19
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Supersedes:** `proposals/graph-classifier.md`, `proposals/pattern-graph.md` (the latter migrated into Appendix A)
**Followed by:** RFC-005 (GraphQuery) → RFC-008 (GraphTransform) → RFC-009 (GraphMutation)
**Related modules:** `Pattern.Graph.GraphLens`, `Pattern.PatternGraph`, `Pattern.Graph.GraphClassifier`

## Summary

Introduce `GraphClassifier` as the unifying abstraction for interpreting `Pattern v` as a
graph. The canonical graph form — currently split between `GraphLens` (lazy, two-category)
and `PatternGraph` (eager, four-category) — is reconceived as a *family of views* over a
single storage substrate (`Pattern v`), where the canonical instance uses `isAtomic` as
its node predicate and arity-based rules for higher categories. `GraphClassifier` becomes
the shared vocabulary that unifies `GraphLens` and `PatternGraph` under a single
classification contract.

## Motivation

`GraphLens` and `PatternGraph` were designed with related but separate concerns:

- **`GraphLens`**: a predicate-based interpretive view. Given a `Pattern v` and a
  `testNode` predicate, it interprets elements as nodes, relationships, and walks on
  demand. Classification is lazy — evaluated at query time.
- **`PatternGraph`**: a materialized container. It eagerly classifies elements into four
  typed maps (nodes, relationships, walks, annotations) using arity-based logic baked
  into the `GraphValue` typeclass.

Both share the insight that graph structure is an *interpretation* of `Pattern v`, not
intrinsic to it — but they express that insight differently and have no shared
classification vocabulary. The result: two parallel type hierarchies with no common
interface; `PatternGraph` must be converted to a `GraphLens` to use graph algorithms.

The unifying observation: `PatternGraph` is equivalent to a `GraphLens` with `isAtomic`
as `testNode`, plus an additional arity-based classification pass that further partitions
non-nodes into relationships, walks, and annotations. Both are views over `Pattern v`.
The right design is one storage substrate with a family of views. `GraphClassifier` is
the value that defines the classification contract for any view.

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
`()` (unit), making `GOther ()` a named bucket for patterns that don't fit the four
named categories. A user interested in simplicial complexes would define:

```haskell
data SimplexClass = Simplex2 | Simplex3 | SimplexN Int
-- Their classifier produces: GraphClass SimplexClass
```

**Why walks are a named bucket**: walks are prevalent in graph data (paths, traversals,
gram path notation) and warrant a first-class bucket. The five named categories correspond
directly to what gram notation can express and what `PatternGraph` currently stores.

**`GOther` is a bucket, not a rejection**: construction never rejects a pattern. Every
pattern has a place. Users who want to inspect what landed in `GOther` query `pgOther`.

### `GraphClassifier` — injectable classification logic

```haskell
data GraphClassifier extra v = GraphClassifier
  { classify :: Pattern v -> GraphClass extra
  }
```

A record of functions, not a typeclass. This makes it a first-class value that can be
passed, stored, varied at runtime, wrapped with memoization, and composed — more
portable across language targets than a typeclass.

The canonical classifier for `Pattern Subject`:

```haskell
canonicalClassifier :: GraphClassifier () Subject
canonicalClassifier = GraphClassifier { classify = classifyByArity }
```

where `classifyByArity` implements the arity-based logic currently in `GraphValue`:
- `elements == []` → `GNode`
- `length elements == 2` → `GRelationship`
- `length elements == 1` → `GAnnotation`
- `length elements >= 3` (with all elements themselves having arity 2) → `GWalk`
- otherwise → `GOther ()`

### Eager vs. Lazy Evaluation

`GraphClassifier.classify` is a pure function. The implementation decides when to call it:

- **Eager** (`PatternGraph` style): call at insertion time, store in typed maps. O(1) queries.
- **Lazy** (`GraphLens` style): call at query time. No upfront cost; repeated queries re-evaluate.
- **Memoized**: call lazily, cache results. Amortized O(1) for repeated queries.

Because `GraphClassifier` is a value passed to the container, expensive predicates can
be wrapped in a memoizing `GraphClassifier` without changing the container API.

### Relationship to Existing Code

#### `GraphLens` — re-derived from `GraphClassifier`

`GraphLens` with a `testNode` predicate is the two-category specialization of
`GraphClassifier`. It is re-derived by implementing its operations in terms of
`GraphClassifier`:

```haskell
fromTestNode :: (Pattern v -> Bool) -> GraphClassifier () v
fromTestNode testNode = GraphClassifier
  { classify = \p -> if testNode p then GNode else GOther ()
  }
```

All existing `GraphLens` operations (`nodes`, `relationships`, `walks`, `neighbors`,
`bfs`, `findPath`, etc.) are preserved with identical signatures, re-implemented to
delegate to `GraphClassifier` internally. **The public API of `GraphLens` is preserved;
the internals are re-implemented.**

#### `PatternGraph` — updated, not replaced

`PatternGraph` stays as the canonical eager materialized store. Its classification
logic is extracted from the `GraphValue` typeclass and moved into
`canonicalClassifier :: GraphClassifier () Subject`. `GraphValue` is simplified:

```haskell
-- Before
class Ord (Id v) => GraphValue v where
  type Id v
  identify :: v -> Id v
  classify :: Pattern v -> PatternClass  -- ← moves out

-- After
class Ord (Id v) => GraphValue v where
  type Id v
  identify :: v -> Id v
```

`MergeResult` is dropped; construction returns a `PatternGraph v` directly. `PatternClass`
is replaced by `GraphClass extra`. `Unrecognized` becomes `GOther extra`.

**This is a breaking change to `PatternGraph` and `GraphValue`**, but `PatternGraph` is
a recent feature (Feature 33) and the change is an improvement.

### Construction Ergonomics

The common path remains simple:

```haskell
-- Canonical classifier (most users)
let graph = fromPatterns canonicalClassifier patterns

-- Custom classifier (power users)
let graph = fromPatterns myClassifier patterns
```

### What Changes in pattern-hs

| Component | Action | Notes |
|---|---|---|
| `Pattern.Graph.GraphClassifier` | **New** | Core new type introduced by this RFC |
| `Pattern.Graph.GraphClass` | **New** | Replaces `PatternClass`; adds `extra` parameter |
| `canonicalClassifier` | **New** | `GraphClassifier () Subject`; extracted from `classifyByArity` |
| `Pattern.Graph.GraphLens` | **Re-derive** | Public API preserved; internals re-implemented |
| `Pattern.PatternGraph.GraphValue` | **Simplify** | Remove `classify`; retain `identify` |
| `Pattern.PatternGraph.PatternClass` | **Replace** | With `GraphClass extra` |
| `Pattern.PatternGraph.MergeResult` | **Remove** | Construction returns `PatternGraph v` directly |
| `Pattern.PatternGraph.PatternGraph` | **Update** | Accept `GraphClassifier` at construction; add `pgOther` |

## Open Questions

1. **Module placement for `GraphClassifier`** — `Pattern.Graph` (alongside `GraphLens`)
   or `Pattern.Classify` (its own module)?

2. **`pgOther` storage shape** — `Map (Id v) (Pattern v)` loses the `extra` type
   information. `Map (Id v) (extra, Pattern v)` preserves it. Worth evaluating during
   implementation.

## Alternatives

**Typeclass instead of record-of-functions** — A `Classifiable v` typeclass was
considered. Rejected because a record is more portable to Rust, TypeScript, and Java,
more composable (wrapper transformations), and more convenient for runtime-variable
classifiers (e.g. different views of the same graph at different times).

**Keeping `GraphLens` and `PatternGraph` separate** — The status quo was rejected
because it forces algorithm authors to commit to one representation or the other,
prevents sharing classification logic, and creates a conceptual gap that users have
to bridge manually.

**Normalization in the classifier** — transforming arbitrary `Pattern v` into canonical
graph form (e.g. a sequence of nodes into a walk) was considered and deferred. This RFC
describes the target structure; transformation paths into it belong in a separate RFC.

## Portability Notes

`GraphClass extra` ports to all targets:

| Language | `()` / unit | Enforcement |
|---|---|---|
| Haskell | `()` | Compiler: `GOther` takes unit |
| Rust | `()` | Compiler: `GOther(())` arm required |
| TypeScript | `null` / `undefined` | Compiler-checked with care |
| Java | `Void` (as null) | Conventional only |

## Appendix A: PatternGraph Container Design

> Migrated from the former `proposals/pattern-graph.md` (Design Only, 2026-02-18). This
> appendix records the original design of the `PatternGraph` materialized container, which
> RFC-004 builds on and modifies. Where the two disagree, **the RFC-004 body above is
> authoritative** — specifically: `PatternClass` is replaced by `GraphClass extra`,
> `Unrecognized` becomes `GOther extra` (a bucket, never a rejection), `classify` moves out
> of `GraphValue` into `canonicalClassifier`, and `MergeResult` is dropped. The container
> design, merge semantics, and recursive-decomposition behavior below remain the basis for
> `PatternGraph`.

### A.1 Purpose

`PatternGraph` is a concrete, opinionated graph representation backed by `Pattern v`, for the
common use case of working with atomic nodes, relationships, walks, and annotations parsed
from gram notation. It complements `GraphLens` (the interpretive view over arbitrary
`Pattern` structures) by providing a concrete *owning* container for the round-trip workflow:

```
gram file → parse → PatternGraph → query/modify → serialize → gram file
```

For this workflow a concrete owning data structure with clear semantics is more ergonomic
than constructing and managing a lens.

### A.2 Type

`PatternGraph` is generic over its value type, like `Pattern` itself:

```haskell
data PatternGraph v = PatternGraph
  { pgNodes         :: Map (Id v) (Pattern v)
  , pgRelationships :: Map (Id v) (Pattern v)
  , pgWalks         :: Map (Id v) (Pattern v)
  , pgAnnotations   :: Map (Id v) (Pattern v)
  }
```

`Subject` is the primary intended `v`, where `Id Subject = Symbol`. (Per the RFC-004 body,
the container also gains a `pgOther` collection and accepts a `GraphClassifier` at
construction rather than relying on a `classify` method on `GraphValue`.)

### A.3 Smart Construction via `merge`

The primary operation for adding data is `merge`, named after Cypher's `MERGE` clause:

```haskell
merge :: GraphValue v => Pattern v -> PatternGraph v -> PatternGraph v
```

`merge` classifies the incoming `Pattern v`, dispatches it to the appropriate collection, and
reconciles any existing entry at the same identity using the `ReconciliationPolicy` machinery
from `Pattern.Reconcile`. It also decomposes recursively: merging a walk merges its
constituent relationships, and merging a relationship merges its endpoint nodes. The graph is
always internally consistent. Constructing a graph from parsed patterns is then a fold:

```haskell
fromPatterns :: GraphValue v => [Pattern v] -> PatternGraph v
fromPatterns = foldr merge empty
```

### A.4 Annotations and Walks

- **Annotations** (`@@id:Label @key("value")`) are stored as first-class entries in
  `pgAnnotations` with their own identity, labels, and properties. Their single inner element
  is also recursively merged into its appropriate collection. This preserves the
  entity-component-system character of annotations: multiple independent systems can annotate
  the same node without colliding in the node's own `Subject`.
- **Walks** are stored whole in `pgWalks` *and* recursively merged into their component
  relationships and nodes — independently addressable by identity while their components stay
  accessible through `pgNodes` and `pgRelationships`.

### A.5 Relationship to GraphLens

`PatternGraph` and `GraphLens` are complementary, not competing. A `PatternGraph` can always
be converted to a `GraphLens` by constructing a scope pattern from its contents and providing
the atomic predicate, giving access to the richer graph algorithms built on `GraphLens`.
`PatternGraph` is the concrete, owning, modifiable container; `GraphLens` is the interpretive,
read-only view. (RFC-004 unifies both under the shared `GraphClassifier` contract.)

### A.6 Original Open Questions (historical)

These were the open questions in the original proposal; RFC-004's body resolves several of
them (e.g. `GOther` replaces the `Unrecognized`/`Either` dilemma, and `classify` is separated
from `GraphValue`):

1. Should `merge` be total (result plus warnings) or partial (`Either` on unrecognized input)?
   — *Resolved by RFC-004: every pattern lands in a bucket; `GOther` replaces rejection.*
2. Should `PatternGraph` expose a `Monoid` instance via `overlay` to complement element-wise
   `merge`, aligning with the `algebraic-graphs` style? — *Still open.*
3. Should `classify` be separated from the identity scheme so discrimination logic can be
   overridden independently? — *Resolved by RFC-004: classification moves into the
   injectable `GraphClassifier` value.*
