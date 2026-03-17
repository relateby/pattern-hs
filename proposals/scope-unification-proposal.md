# Proposal: Scope Unification — `ScopeQuery` and `paraWithScope`

**Status:** Design — ready for Haskell prototype  
**Date:** 2026-03-17  
**Depends on:** GraphQuery proposal (design only; `GraphQuery` record must be in scope)  
**Part of:** `representation-map-proposal.md` (scope unification layer only)  
**Followed by:** RepresentationMap proposal (depends on this proposal being implemented)  
**Location:** `proposals/scope-unification.md` within the `pattern-hs` workspace

---

## Summary

Introduce `ScopeQuery` as a typeclass abstracting over *what is in scope* for a fold
operation, and `paraWithScope` as the unified primitive fold that replaces `para` and
`paraGraph` as derivable special cases.

This is a purely additive refactoring. No existing API is removed or broken. No
behavior changes. The goal is to reveal structure that is already implicit in
`Pattern<V>`: that `para` and `paraGraph` are the same operation at different scope
boundaries, and that the scope boundary is determined by a containing element, not by
the query interface used to access it.

This proposal is self-contained and does not depend on `GraphTransform`,
`RepresentationMap`, or `pattern-equivalence`. It can be implemented, tested, and
merged independently.

---

## Background: the current situation

`pattern-hs` currently has two fold operations:

- **`para`** — a paramorphism over `Pattern v`. Each node receives its own value and
  the bottom-up results of its direct children. Scope is implicit: the fold sees only
  the immediate subtree.

- **`paraGraph`** — a fold over a `GraphView`. Each element receives a `GraphQuery v`
  giving access to the full graph context: containers, neighbors, incident
  relationships. Scope is the whole enclosing `GraphView`.

These look like two different operations serving two different purposes. The insight
behind this proposal is that they are the same operation at different scope boundaries.
The difference is not "pattern vs. graph" — it is **which containing element defines
the scope**. In `para`, scope is the current node's immediate children. In
`paraGraph`, scope is the enclosing `GraphView` element.

Making this explicit has two benefits:

1. `para` and `paraGraph` can be redefined as derived operations, eliminating the
   duplication in their implementations.
2. New scope kinds — a gram document, a project corpus, a subgraph — can be introduced
   without adding new fold primitives. Any element that can produce a `ScopeQuery`
   value participates in the same fold.

---

## Design

### `ScopeQuery` — the typeclass

A `ScopeQuery` is a query interface into a scope defined by some containing element.
Different containers provide different operations; richer containers extend simpler ones.

```haskell
-- Pattern.Core

class ScopeQuery q v where
  containers  :: q v -> Pattern v -> [Pattern v]
    -- ^ patterns that directly contain this element within the scope
  siblings    :: q v -> Pattern v -> [Pattern v]
    -- ^ co-elements of the same immediate parent within the scope
  byIdentity  :: q v -> Id v -> Maybe (Pattern v)
    -- ^ look up any element in scope by identity
  allElements :: q v -> [Pattern v]
    -- ^ enumerate all elements within the scope
```

The type variable `q` is the scope query implementation. Different containers
instantiate `q` differently: `TrivialScope` for immediate-children scope, `GraphQuery`
for graph-classified scope, and any future scope the caller cares to define.

The caller is always responsible for constructing the scope. The scope is not inferred
from the fold target — it is passed in explicitly.

### `TrivialScope` — the immediate-children scope

The simplest `ScopeQuery` instance. Scope is defined by a single `Pattern v` and
covers only that pattern's subtree. This is the scope appropriate for `para`.

```haskell
-- Pattern.Core

newtype TrivialScope v = TrivialScope (Pattern v)

trivialScope :: Pattern v -> TrivialScope v
trivialScope = TrivialScope

instance ScopeQuery TrivialScope v where
  containers  _ _               = []
    -- no parent information available in trivial scope
  siblings    _ _               = []
    -- no sibling information available in trivial scope
  byIdentity  (TrivialScope p) i = findInSubtree i p
    -- search within the subtree only
  allElements (TrivialScope p)   = subtreeElements p
    -- all elements reachable within the subtree
```

`findInSubtree` and `subtreeElements` are straightforward recursive traversals over
`Pattern v`. Both should already exist or be trivial to add in `Pattern.Core`.

### `GraphScope` — graph-topology-aware scope

A subclass of `ScopeQuery` that adds operations meaningful only when the scoped
patterns are graph-classified (i.e. satisfy `GNode`, `GRelationship`, etc.).

```haskell
-- Pattern.Graph

class ScopeQuery q v => GraphScope q v where
  source    :: q v -> Pattern v -> Maybe (Pattern v)
  target    :: q v -> Pattern v -> Maybe (Pattern v)
  incidents :: q v -> Pattern v -> [Pattern v]
  degree    :: q v -> Pattern v -> Int
  nodes     :: q v -> [Pattern v]
  rels      :: q v -> [Pattern v]
```

`source`, `target`, and `incidents` are only meaningful on `GRelationship`- and
`GNode`-kinded patterns. The constraint hierarchy mirrors this: code that needs graph
topology requires `GraphScope q v`; code that needs only containment and lookup
requires only `ScopeQuery q v`.

### `GraphQuery` gains instances

The existing `GraphQuery v` record-of-functions gains `ScopeQuery` and `GraphScope`
instances. The existing record fields become the implementations. **No fields are
removed or renamed.**

```haskell
-- Pattern.Graph

instance ScopeQuery GraphQuery v where
  containers  q p = queryContainers q p
  siblings    q p = concatMap elements (queryContainers q p)
  byIdentity  q i = queryNodeById q i
  allElements q   = queryNodes q ++ queryRelationships q

instance GraphScope GraphQuery v where
  source    = querySource
  target    = queryTarget
  incidents = queryIncidentRels
  degree    = queryDegree
  nodes     = queryNodes
  rels      = queryRelationships
```

All existing code that passes `GraphQuery v` values around continues to work
unchanged. These instances are additive declarations only.

### `ScopeDict` — the first-class value form

For situations where a `ScopeQuery` instance needs to be stored in a data structure,
passed to a non-polymorphic higher-order function, or constructed dynamically at
runtime, a `ScopeDict` record is provided alongside the typeclass. The record is itself
a `ScopeQuery` instance, and any instance can be reified into a `ScopeDict` via
`toDict`.

```haskell
-- Pattern.Core

data ScopeDict v = ScopeDict
  { dictContainers  :: Pattern v -> [Pattern v]
  , dictSiblings    :: Pattern v -> [Pattern v]
  , dictByIdentity  :: Id v -> Maybe (Pattern v)
  , dictAllElements :: [Pattern v]
  }

instance ScopeQuery ScopeDict v where
  containers  d   = dictContainers d
  siblings    d   = dictSiblings d
  byIdentity  d   = dictByIdentity d
  allElements d _ = dictAllElements d

toDict :: ScopeQuery q v => q v -> ScopeDict v
toDict q = ScopeDict
  { dictContainers  = containers q
  , dictSiblings    = siblings q
  , dictByIdentity  = byIdentity q
  , dictAllElements = allElements q
  }
```

`ScopeDict` is an escape hatch, not the primary interface. Prefer the typeclass form
in function signatures. Use `ScopeDict` when a first-class value is genuinely needed.

---

## The unified primitive: `paraWithScope`

```haskell
-- Pattern.Core

paraWithScope
  :: ScopeQuery q v
  => q v                               -- scope: the containing element's query interface
  -> (q v -> Pattern v -> [r] -> r)   -- algebra: scope, current node, child results
  -> Pattern v                         -- root pattern to fold over
  -> r
```

The algebra receives three arguments:
- the scope query, for lookups and context
- the current pattern node
- the bottom-up results of the current node's direct children

The fold is bottom-up. The scope is fixed for the duration of the fold — it is the
scope of the containing element, not of each node as the fold descends.

### `para` redefined

```haskell
-- Pattern.Core

para :: (Pattern v -> [r] -> r) -> Pattern v -> r
para f p = paraWithScope (trivialScope p) (\_ pat rs -> f pat rs) p
```

`para` is unchanged at every call site. The `ScopeQuery` parameter is hidden behind
`trivialScope`. Behavior is identical to the current implementation.

### `paraGraph` redefined

```haskell
-- Pattern.Graph

paraGraph
  :: (GraphQuery v -> Pattern v -> [r] -> r)
  -> GraphView extra v
  -> Map (Id v) r
paraGraph f view =
  paraWithScope (viewQuery view) f (viewRoot view)
```

`paraGraph` is unchanged at every call site. The scope is now explicitly the
`GraphQuery v` derived from the `GraphView`, which is what it always was implicitly.
Behavior is identical to the current implementation.

### Other derived operations

Any other operations currently duplicated between `Pattern.Core` and `Pattern.Graph`
(map, filter) can be redefined against `ScopeQuery q v` in the same way. This is
deferred to a follow-on cleanup pass once `paraWithScope` is in place and the pattern
is confirmed.

---

## Module placement

| Definition | Module | Action |
|---|---|---|
| `ScopeQuery` typeclass | `Pattern.Core` | **New** |
| `TrivialScope` + `trivialScope` | `Pattern.Core` | **New** |
| `ScopeDict` + `toDict` | `Pattern.Core` | **New** |
| `paraWithScope` | `Pattern.Core` | **New** |
| `para` | `Pattern.Core` | **Redefine** (no API change) |
| `GraphScope` typeclass | `Pattern.Graph` | **New** |
| `ScopeQuery GraphQuery` instance | `Pattern.Graph` | **New** |
| `GraphScope GraphQuery` instance | `Pattern.Graph` | **New** |
| `paraGraph` | `Pattern.Graph` | **Redefine** (no API change) |

No existing definitions are removed. No existing call sites require changes.

---

## Interface convention note

This proposal introduces `ScopeQuery` and `GraphScope` as typeclasses rather than
records-of-functions. This is a deliberate choice for new interfaces in `pattern-hs`
going forward, and the reasoning is documented here to be explicit.

The existing `GraphQuery` and `GraphClassifier` records are **not** converted — that
would be a breaking change with limited benefit. Instead they gain typeclass instances,
which is purely additive.

For new interfaces from this point forward:

1. Define the interface as a typeclass
2. Provide a `*Dict` record as the first-class value form, with a `toDict` reification
3. Give existing records typeclass instances where they satisfy the interface
4. Document the cross-language correspondence in the porting guide

**Cross-language correspondence:**

| Haskell | Rust | Python / TypeScript (via WASM) |
|---|---|---|
| Typeclass | Trait | Protocol / Interface |
| `*Dict` record | Struct implementing trait | Concrete class |
| `toDict` | `impl Trait for Struct` | `__init__` + protocol conformance |
| `&dyn ScopeQuery<V>` (at boundary) | `Box<dyn ScopeQuery<V>>` | WASM-exported object |

The Rust port uses `&dyn ScopeQuery<V>` at FFI and dynamic-dispatch boundaries. Within
Rust, monomorphic trait bounds (`impl ScopeQuery<V>`) are preferred for performance.
The porting guide should document this correspondence explicitly when the Rust port of
this proposal is undertaken.

---

## Implementation sequence

### Step 1 — helpers in `Pattern.Core`

Confirm that `findInSubtree` and `subtreeElements` exist and are correct. Add them if
missing. These are simple recursive traversals; their correctness is a prerequisite for
`TrivialScope`.

### Step 2 — `ScopeQuery`, `TrivialScope`, `ScopeDict`, `toDict`

Add to `Pattern.Core`. No dependencies on other new definitions. Write unit tests
for `TrivialScope`: `containers` and `siblings` return `[]`; `byIdentity` finds
elements within the subtree; `allElements` enumerates the full subtree.

### Step 3 — `paraWithScope` in `Pattern.Core`

Implement `paraWithScope`. Redefine `para` in terms of it. Run the existing `para`
test suite — all tests must pass without modification. This is the primary correctness
check: if `para` is behaviorally identical before and after, the refactoring is sound.

### Step 4 — `GraphScope`, instances in `Pattern.Graph`

Add `GraphScope` typeclass to `Pattern.Graph`. Add `ScopeQuery GraphQuery` and
`GraphScope GraphQuery` instances. Redefine `paraGraph` in terms of `paraWithScope`.
Run the existing `paraGraph` test suite — all tests must pass without modification.

### Step 5 — verify no regressions

Run the full `pattern-hs` test suite. No failures expected. No call sites should
require changes.

---

## What this does not include

This proposal deliberately excludes the following, which belong to later proposals:

- **`RepresentationMap`** — named invertible maps between kinds of shape. Depends on
  this proposal and on `GraphTransform`. See `representation-map-proposal.md`.
- **`PatternKind`** — named, predicate-defined shape kinds. Part of the
  `RepresentationMap` proposal.
- **Interface convention for `GraphClassifier`** — `GraphClassifier` can gain a
  typeclass instance following the same pattern as `GraphQuery`, but that change is
  deferred to avoid scope creep here.
- **`mapWithContext` and `filterGraph` redefinitions** — these can be redefined against
  `ScopeQuery q v` once `paraWithScope` is in place, but are a follow-on cleanup, not
  a prerequisite.
- **Rust port** — the Rust port of this proposal follows the standard
  typeclass-to-trait correspondence. That work belongs in `pattern-rs` and is not
  scheduled here.

---

## Acceptance criteria

The implementation is complete when:

1. `ScopeQuery`, `TrivialScope`, `ScopeDict`, `toDict`, and `paraWithScope` are
   present in `Pattern.Core` and exported.
2. `GraphScope`, `ScopeQuery GraphQuery`, and `GraphScope GraphQuery` are present in
   `Pattern.Graph` and exported.
3. `para` and `paraGraph` are redefined as derived operations and pass their existing
   test suites without modification.
4. The full `pattern-hs` test suite passes with no failures.
5. No existing call sites required changes.

---

## Related documents

- `representation-map-proposal.md` — the larger proposal this is extracted from;
  `RepresentationMap` depends on this proposal being implemented first
- `proposals/graph-query.md` — `GraphQuery` record that gains `ScopeQuery` and
  `GraphScope` instances here
- `proposals/graph-transform.md` — the next proposal in sequence after this one is
  merged; `RepresentationMap` depends on both
