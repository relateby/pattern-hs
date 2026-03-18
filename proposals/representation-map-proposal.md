# Proposal: RepresentationMap for pattern-hs

**Status:** Design — forward-looking, not yet a specification  
**Date:** 2026-03-17  
**Depends on:** GraphTransform proposal, pattern-equivalence proposal  
**Followed by:** Rust port (`pattern-rs`), pato surface (`pato canonicalize`)  
**Location:** `proposals/representation-map.md` within the `pattern-hs` workspace

---

## Summary

Introduce `RepresentationMap` as a named, invertible, composable isomorphism between
two *kinds of shape* — named, recognizable shapes of `Pattern<V>` — where both kinds
are subtypes of `Pattern<V>` and the isomorphism is witnessed by a machine-checkable
round-trip property.

This proposal also captures a foundational clarification about scope and containment
that applies across both pattern-level and graph-level operations, and identifies
refactoring implied for the `GraphQuery` and `GraphTransform` interfaces.

The intended implementation sequence is: Haskell prototype → confidence in the
abstraction → Rust port (`pattern-rs`) → CLI surface (`pato canonicalize`).

---

## Foundational principle: scope is defined by an element

**Scope is not a query interface. Scope is an element. An element defines the scope
of everything it contains.**

This is already true in `Pattern<V>`: a pattern element is a scope for its direct
elements and for everything transitively nested within it. The gram document root is a
scope for all top-level patterns. A sub-pattern is a scope for its own elements.
Containment is the core structural relationship in `Pattern<V>`, and scope is just
containment viewed from the other direction.

Query interfaces are not *the scope*. They are *efficient accessors into a scope
defined by a containing element*. The element comes first; the query interface is
derived from it. The caller decides what the scope container is and constructs the
appropriate query from it.

This unifies what were previously treated as separate concerns:

- `para` on `Pattern<V>` gives each node access to its *immediate* context — the
  pattern itself and the bottom-up results of its direct elements. Scope defined by
  the current element looking inward.
- `paraGraph` on `GraphView` gives each element access to *broader* context — the
  scope defined by the enclosing `GraphView` element, looking outward: who contains
  me, who are my neighbors, what relationships am I part of?
- The difference is not "pattern vs graph" — it is **the scope boundary**: immediate
  children vs the whole containing element.

Both are instances of the same general operation: a fold where the folding function
has access to child results *and* to the query interface for the broader containing
scope.

---

## Scope as a typeclass

The scope query interface is a typeclass. Different kinds of scope provide different
operations, with richer kinds extending simpler ones:

```haskell
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

`GraphQuery` is a `ScopeQuery` that operates on patterns classified as graph-shaped —
the graph classification kinds (`GNode`, `GRelationship`, `GWalk`, `GAnnotation`) are
the kinds of shape that make graph-specific operations meaningful. `querySource` is
only valid on a `GRelationship`-kinded pattern; `queryIncidentRels` is only valid on a
`GNode`-kinded pattern. These are not operations bolted onto scope from outside — they
are scope operations that are valid *because* the patterns being queried satisfy
specific kind constraints.

```haskell
-- GraphQuery is a ScopeQuery for graph-classified patterns
instance ScopeQuery GraphQuery v where
  containers  q = queryContainers q
  siblings    q p = concatMap elements (queryContainers q p)
  byIdentity  q = queryNodeById q
  allElements q = queryNodes q ++ queryRelationships q

-- Additional operations valid only within graph kinds
class ScopeQuery q v => GraphScope q v where
  source    :: q v -> Pattern v -> Maybe (Pattern v)
  target    :: q v -> Pattern v -> Maybe (Pattern v)
  incidents :: q v -> Pattern v -> [Pattern v]
  degree    :: q v -> Pattern v -> Int
  nodes     :: q v -> [Pattern v]
  rels      :: q v -> [Pattern v]

instance GraphScope GraphQuery v where
  source    = querySource
  target    = queryTarget
  incidents = queryIncidentRels
  degree    = queryDegree
  nodes     = queryNodes
  rels      = queryRelationships
```

A trivial scope instance handles the immediate-children-only case:

```haskell
newtype TrivialScope v = TrivialScope (Pattern v)

instance ScopeQuery TrivialScope v where
  containers  _ _ = []
  siblings    _ _ = []
  byIdentity  (TrivialScope p) i = findInSubtree i p
  allElements (TrivialScope p)   = subtreeElements p
```

The caller constructs whichever scope is appropriate for their container:

```haskell
trivialScope :: Pattern v -> TrivialScope v
trivialScope = TrivialScope

-- Any other scope the caller wishes to define —
-- e.g. a GraphView, a gram document, a project corpus —
-- is an instance of ScopeQuery constructed the same way.
```

---

## Interface convention: typeclass + dictionary + instances

This proposal introduces `ScopeQuery` and `GraphScope` as typeclasses. This is a
deliberate departure from the existing pattern-hs convention of records-of-functions
(`GraphQuery`, `GraphClassifier`), and the reasoning is worth stating formally because
it establishes the convention for all future interfaces in pattern-hs.

### Why not records-of-functions for the interface?

The original motivation for records-of-functions was portability: records look
structurally similar to Rust structs, which was thought to ease porting. This conflates
*implementation similarity* with *semantic portability*. A Rust developer porting
`ScopeQuery` does not need Haskell to look like Rust — they need the design document
to clearly state the correspondence. Portability is a property of the *design*,
documented in the porting guide, not a property of the syntax.

### The resolved approach

Each language uses its idiomatic equivalent for interface definition:

- **Haskell** — typeclasses (`ScopeQuery`, `GraphScope`). GHC can inline through
  typeclass dispatch and specialise instances, which matters for `paraWithScope` as a
  tight inner loop.
- **Rust** — traits (`ScopeQuery<V>`, `GraphScope<V>`). Trait objects (`&dyn
  ScopeQuery<V>`) at FFI and dynamic-dispatch boundaries.
- **Python / TypeScript via WASM** — protocols or interfaces derived from the Rust
  trait objects at the WASM boundary.

The porting guide states the correspondence explicitly. Visual similarity between
language constructs is not the goal; semantic fidelity is.

### Typeclasses and records-of-functions compose

Within Haskell, typeclasses and records-of-functions are not alternatives — they
compose. A `*Dict` record is provided alongside each typeclass as the first-class value
form, useful when a dictionary needs to be stored in a data structure, passed to a
non-polymorphic higher-order function, or constructed dynamically at runtime. The
`*Dict` record is itself a typeclass instance, and any typeclass instance can be
reified into a `*Dict` via a `toDict` function:

```haskell
data ScopeDict v = ScopeDict
  { dictContainers  :: Pattern v -> [Pattern v]
  , dictSiblings    :: Pattern v -> [Pattern v]
  , dictByIdentity  :: Id v -> Maybe (Pattern v)
  , dictAllElements :: [Pattern v]
  }

instance ScopeQuery ScopeDict v where
  containers  = dictContainers
  siblings    = dictSiblings
  byIdentity  = dictByIdentity
  allElements = const . dictAllElements

-- Reify any ScopeQuery instance into a first-class dictionary value
toDict :: ScopeQuery q v => q v -> ScopeDict v
toDict q = ScopeDict
  { dictContainers  = containers q
  , dictSiblings    = siblings q
  , dictByIdentity  = byIdentity q
  , dictAllElements = allElements q
  }
```

### Existing records are not converted

The existing `GraphQuery` and `GraphClassifier` records are **not** converted to
typeclasses — that would be a breaking change with limited benefit. Instead, they gain
typeclass instances (`ScopeQuery GraphQuery`, `GraphScope GraphQuery`), with their
existing record fields becoming the implementations. All existing code that passes
`GraphQuery` values around continues to work unchanged. The design evolves forward
without disruption.

### Going-forward convention

New interfaces in pattern-hs follow this pattern:

1. Define the interface as a typeclass
2. Provide a `*Dict` record as the first-class value form, with a `toDict` reification
   function
3. Give existing records typeclass instances where they satisfy the interface
4. Document the cross-language correspondence in the porting guide

---

## The unified operation: `paraWithScope`

```haskell
paraWithScope :: ScopeQuery q v
              => q v
              -> (q v -> Pattern v -> [r] -> r)
              -> Pattern v
              -> r
```

`para` and `paraGraph` become derived operations, both instances of the same
underlying fold at different scope levels:

```haskell
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
para f p = paraWithScope (trivialScope p) (\_ pat rs -> f pat rs) p

paraGraph :: (GraphQuery v -> Pattern v -> [r] -> r)
          -> GraphView extra v -> Map (Id v) r
paraGraph f view =
  paraWithScope (viewQuery view) f (viewRoot view)
```

The full set of scope-aware operations — fold, map, filter — can be defined once
against `ScopeQuery q v` and reused at every scope level. `paraGraph`,
`mapWithContext`, and `filterGraph` become derived, not primitive.

---

## Implications for graph interface refactoring

The typeclass formulation makes the refactoring straightforward:

- `ScopeQuery` is a new typeclass in `Pattern.Core`, with `TrivialScope` as its
  simplest instance
- `GraphQuery` gains a `ScopeQuery` instance and a `GraphScope` subclass instance —
  the existing record fields become the implementations
- `paraWithScope` replaces `para` as the primitive; `para` is redefined in terms of it
- `paraGraph`, `mapWithContext`, `filterGraph` are redefined against `ScopeQuery q v`

No existing call sites break. The refactoring is additive and exposes a cleaner
structure without removing anything.

### Concrete changes

| Component | Action | Notes |
|-----------|--------|-------|
| `ScopeQuery` typeclass | **New** in `Pattern.Core` | Scope ops for any `Pattern<V>` |
| `GraphScope` typeclass | **New** in `Pattern.Graph` | Graph-specific ops; subclass of `ScopeQuery` |
| `TrivialScope` | **New** in `Pattern.Core` | `ScopeQuery` instance for immediate-children scope |
| `ScopeQuery GraphQuery` instance | **New** in `Pattern.Graph` | Declares `GraphQuery` as a scope |
| `GraphScope GraphQuery` instance | **New** in `Pattern.Graph` | Graph operations as scope extensions |
| `paraWithScope` | **New** in `Pattern.Core` | General scope-aware fold; unified primitive |
| `para` | **Redefine** as `paraWithScope (trivialScope p)` | No API breakage |
| `paraGraph` | **Redefine** as `paraWithScope (viewQuery view)` | No API breakage |
| `mapWithContext` | **Redefine** to use `ScopeQuery q v` constraint | Broader applicability |
| `filterGraph` | **Redefine** where scope-only predicates suffice | `GraphScope` still needed for topology-dependent predicates |

---

## Kinds of shape

A **kind of shape** is a named, recognizable shape of `Pattern<V>` — a description of
the structural constraints that a pattern must satisfy to be considered an instance of
that kind. It is a subtype of `Pattern<V>` defined by a predicate, not by the type
system.

The vocabulary is already present in gram notation: `{ kind: "diagnostics" }` in a
document header declares the document's kind. `*.schema.gram` files describe kinds.
`::` labels and `==>` arrows signal schema intent. "Kind of shape" gives this existing
practice a precise name.

A kind is not an individual pattern — it is the description. A specific diagnostic
pattern is *of kind* `DiagnosticPattern`; it is not itself a kind.

The graph classification kinds (`GNode`, `GRelationship`, `GWalk`, `GAnnotation`) are
kinds of shape in exactly this sense — they are the recognizable shapes that
`GraphClassifier` detects and that `GraphScope` operations are defined over.
`GraphQuery` is the scope query for patterns of those kinds. This confirms that
`GraphQuery` is not a separate concept from `ScopeQuery` — it is `ScopeQuery`
instantiated for graph-classified kinds.

```haskell
data PatternKind v = PatternKind
  { kindName    :: Text
    -- ^ e.g. "DiagnosticPattern", "DiagnosticGraph", "GNode"
  , kindPred    :: forall q. ScopeQuery q v => q v -> Pattern v -> Bool
    -- ^ true iff a pattern is of this kind within a given scope
  , kindExample :: Pattern v
    -- ^ canonical example for property-based test generation
  }
```

`kindPred` is polymorphic over scope queries: a shape constraint may be purely
structural (check the pattern itself) or scope-relative (check cross-references within
the containing scope). Both cases use the same predicate signature.

The long-term path from `PatternKind` to gram-described schemas is direct: a
`*.schema.gram` file describes a kind, and `pato validate` checks kind membership.
`PatternKind` is the programmatic representation of what a schema describes.

---

## RepresentationMap

A `RepresentationMap` is an isomorphism between two kinds of shape. It maps every
pattern of kind `A` to a pattern of kind `B`, and back, with the round-trip property
as the machine-checkable witness to the isomorphism.

This is `Pattern v <-> Pattern v` in the general case — not specifically
pattern-to-graph. Graph form is one common codomain, but any two kinds of shape can be
related by a `RepresentationMap`. Examples:

- Nested diagnostic pattern ↔ flat node-relationship diagnostic graph
- Positional sequence encoding ↔ labeled property encoding
- Compact scalar representation ↔ expanded structural representation
- Hierarchy-as-nesting ↔ hierarchy-as-membership-edges

```haskell
data RepresentationMap v = RepresentationMap
  { name        :: Text
  , domain      :: PatternKind v
  , codomain    :: PatternKind v
  , conventions :: [Text]
    -- ^ named structural decisions that make the round-trip work
  , forward     :: forall q. ScopeQuery q v => q v -> Pattern v -> Pattern v
  , inverse     :: forall q. ScopeQuery q v => q v -> Pattern v -> Pattern v
  , roundTrip   :: forall q. ScopeQuery q v => q v -> Pattern v -> Bool
    -- ^ witness: (inverse q . forward q) p == p for all p of kind domain
  }
```

`forward` and `inverse` are polymorphic over scope queries. A transform that needs
only general scope operations works at `ScopeQuery q v`. A transform that needs
graph topology constrains its scope further:

```haskell
-- A map whose source is graph-shaped needs GraphScope
graphAwareForward :: (GraphScope q v) => q v -> Pattern v -> Pattern v
```

The constraint hierarchy in the scope typeclasses mirrors the kind hierarchy in the
data. A `RepresentationMap` between graph-kinded patterns uses a `GraphScope`-
constrained transform; one between arbitrary pattern-kinded shapes uses only
`ScopeQuery`. The type system enforces the correspondence.

### Conventions

Invertibility is not free. When a kind of shape does not carry all the information
needed to reconstruct the source from the target, the forward transform must encode
structural metadata into the target. These encoding decisions are **conventions**.

Conventions are part of the map's identity. Two maps with the same `forward` function
but different encoding decisions produce different codeomain patterns and require
different inverse functions. An undocumented convention is a convention that will be
misread.

In the prototype, conventions are named text. Long-term they may be expressed as gram
annotations on the codomain's schema, making them inspectable without reading source
code and connecting `RepresentationMap` directly to the `pato validate` infrastructure.

### Relationship to GraphTransform

`RepresentationMap` sits above `GraphTransform` in the abstraction stack. The
primitive operations from `GraphTransform` (`mapGraph`, `unfoldGraph`, `foldGraph`)
and `Pattern.Core` (`paraWithScope`, `unfold`, `map`) are how you *build* the
`forward` and `inverse` functions. `RepresentationMap` is how you *declare* that they
are each other's inverse, name the kinds they operate between, and record the
conventions that make the round-trip work.

### Composability

Two `RepresentationMap`s compose when the codomain of the first matches the domain of
the second:

```haskell
compose :: RepresentationMap v -> RepresentationMap v -> RepresentationMap v
compose m1 m2 = RepresentationMap
  { name        = name m1 <> " >>> " <> name m2
  , domain      = domain m1
  , codomain    = codomain m2
  , conventions = conventions m1 <> conventions m2
  , forward     = \q p -> forward m2 q (forward m1 q p)
  , inverse     = \q p -> inverse m1 q (inverse m2 q p)
  , roundTrip   = \q p -> roundTrip m1 q p && roundTrip m2 q p
  }
```

The composed map carries the union of both maps' conventions. The space of kinds of
shape is navigable: name intermediate forms, chain mappings, track every structural
decision along the way.

A library of named maps, each with declared kinds and conventions:

```haskell
diagnosticMap :: RepresentationMap Subject
  -- domain:      DiagnosticPattern (nested location/diagnostic/remediation)
  -- codomain:    DiagnosticGraph   (flat nodes + AT/HAS_REMEDIATION edges)
  -- conventions: ["_arity encodes element count", "_depth encodes nesting depth"]

organizationMap :: RepresentationMap Subject
  -- domain:      OrgHierarchy (nested department/person structure)
  -- codomain:    OrgGraph     (nodes + MEMBER_OF edges)

walkMap :: RepresentationMap Subject
  -- domain:      NestedPath    (right-recursive path nesting)
  -- codomain:    FlatEdgeList  (anonymous path + flat relationship list)
```

---

## Relationship to pattern-equivalence

`pattern-equivalence` handles two gram expressions that are syntactically different
but structurally identical — the same `Pattern<Subject>`, the same kind of shape, no
named mapping required. This happens at parse time.

`RepresentationMap` handles two patterns of *different* kinds of shape that are
informationally equivalent under declared conventions. The nesting depth, arity, and
topology differ between kinds; the conventions define the correspondence and the
`roundTrip` property witnesses it.

---

## The two-register model

`RepresentationMap` formalises a recurring design choice: the same information can be
held in a form optimised for reading or a form optimised for querying.

- **Pattern form** — nesting makes local structure immediately visible. Optimised for
  authoring, reading, and sequential processing by humans and LLMs top-to-bottom.
- **Graph form** — flat topology makes global structure immediately traversable.
  Optimised for querying, graph algorithms, and integration with graph-native tools.

Graph form is not a privileged target. It is one codomain among many. The choice is a
deployment concern, made explicit and reversible by naming the kinds and declaring the
map.

---

## Concrete example: diagnostic map

**Kinds:**
- `DiagnosticPattern` — kind predicate: location pattern directly containing a
  diagnostic pattern directly containing zero or more remediation patterns; labels
  `Location`, `Diagnostic`, `Remediation`; specific required properties per label.
- `DiagnosticGraph` — kind predicate: flat atomic patterns with labels `Location`,
  `Diagnostic`, `Remediation`; connected by `AT` and `HAS_REMEDIATION` relationships;
  `_arity` and `_depth` properties present on each.

**Conventions:**
- `_arity` on each graph-form node encodes the element count of the corresponding
  nested pattern node, enabling the inverse to reconstruct arity.
- `_depth` encodes nesting depth, enabling the inverse to reconstruct order.

**Forward** (`DiagnosticPattern` → `DiagnosticGraph`): traverse the nested structure
using `paraWithScope` (with `ScopeQuery` constraint — no graph topology needed here),
emit flat patterns with `_arity`/`_depth` properties, connect with typed
relationships. Use `byIdentity` to resolve cross-references.

**Inverse** (`DiagnosticGraph` → `DiagnosticPattern`): find all `Diagnostic` patterns
via `allElements`, traverse edges using `containers`, reconstruct nesting from
`_arity`/`_depth` properties, emit the nested pattern.

**Round-trip property**: for all `p` satisfying `kindPred DiagnosticPattern q p`,
`(inverse q . forward q) p == p` structurally.

---

## Implementation sequence

**Step 1 — Haskell prototype** (`pattern-hs`)

Prerequisites: `GraphTransform` proposal implemented.

- Define `ScopeQuery` typeclass and `TrivialScope` instance in `Pattern.Core`
- Define `GraphScope` typeclass as subclass of `ScopeQuery` in `Pattern.Graph`
- Add `ScopeQuery` and `GraphScope` instances for `GraphQuery` in `Pattern.Graph`
- Add `paraWithScope` to `Pattern.Core`; redefine `para` and `paraGraph` in terms
  of it; verify no behavioral change via existing tests
- Define `PatternKind v` and `RepresentationMap v`; implement `compose`
- Define `diagnosticMap` with declared kinds and conventions; write Hedgehog property
  tests: for all `p` satisfying `kindPred (domain m) q p`, `roundTrip q m p` holds
  and `kindPred (codomain m) q (forward q p)` holds
- Write composition tests: `roundTrip` holds for composed maps
- If any transform requires a constraint beyond `ScopeQuery` (e.g. needs `GraphScope`
  for graph-sourced maps), express this as a typeclass constraint on the transform
  function; the `RepresentationMap` record's `forward`/`inverse` fields use `forall q`
  but individual helper functions may be more constrained

**Step 2 — Rust port** (`pattern-rs`)

The typeclass hierarchy maps to a trait hierarchy:

```rust
pub trait ScopeQuery<V: GraphValue> {
    fn containers(&self, p: &Pattern<V>) -> Vec<Pattern<V>>;
    fn siblings(&self, p: &Pattern<V>) -> Vec<Pattern<V>>;
    fn by_identity(&self, id: &V::Id) -> Option<Pattern<V>>;
    fn all_elements(&self) -> Vec<Pattern<V>>;
}

pub trait GraphScope<V: GraphValue>: ScopeQuery<V> {
    fn source(&self, p: &Pattern<V>) -> Option<Pattern<V>>;
    fn target(&self, p: &Pattern<V>) -> Option<Pattern<V>>;
    fn incidents(&self, p: &Pattern<V>) -> Vec<Pattern<V>>;
    fn degree(&self, p: &Pattern<V>) -> usize;
}

pub struct PatternKind<V: GraphValue> {
    pub name:    String,
    pub pred:    Box<dyn Fn(&dyn ScopeQuery<V>, &Pattern<V>) -> bool>,
    pub example: Pattern<V>,
}

pub struct RepresentationMap<V: GraphValue> {
    pub name:        String,
    pub domain:      PatternKind<V>,
    pub codomain:    PatternKind<V>,
    pub conventions: Vec<String>,
    pub forward:     Box<dyn Fn(&dyn ScopeQuery<V>, &Pattern<V>) -> Pattern<V>>,
    pub inverse:     Box<dyn Fn(&dyn ScopeQuery<V>, &Pattern<V>) -> Pattern<V>>,
    pub round_trip:  Box<dyn Fn(&dyn ScopeQuery<V>, &Pattern<V>) -> bool>,
}
```

`Arc` / `Rc` feature flag applies throughout. Graph-topology-aware transforms accept
`&dyn GraphScope<V>` rather than `&dyn ScopeQuery<V>`.

**Step 3 — Pato surface** (`pato canonicalize`)

```
pato canonicalize [--map <n>] [--inverse] <files>...
```

- `--map` selects a named `RepresentationMap` from the library
- `--inverse` applies the inverse transform
- Default map: inferred from the document `kind` header
- Scope constructed by pato from the document being transformed
- Output is gram on stdout; errors on stderr

The library owns the capability. Pato is a thin surface.

---

## Open questions for the Haskell prototype

**1. Typeclass interface with dictionary convenience — see §"Interface convention"**

The approach for `ScopeQuery` as a typeclass, the `ScopeDict` first-class value form,
and the treatment of existing records is fully specified in the "Interface convention"
section above. This is no longer an open question.

**2. Kind compatibility in compose**

`compose m1 m2` requires `codomain m1` and `domain m2` to be compatible. In the
prototype this is a runtime check (`kindName (codomain m1) == kindName (domain m2)`
or a predicate-subsumption check). Phantom type tags would make it compile-time but
require kinds to be named at the type level. Defer until the abstraction is stable.

**3. RepresentationMap as a pattern**

Can a `RepresentationMap` itself be represented as a `Pattern<V>`? If so, it could be
serialized to gram like any other pattern — stored, transmitted, inspected with pato,
and composed at the data level rather than only at the code level.

This is an open question about the data model, not about file organization or tooling.
Gram is a serialization format for patterns; the question is whether `RepresentationMap`
— including its domain, codomain, conventions, and the structural rules that relate
them — has a natural representation as a `Pattern<V>`. The `forward` and `inverse`
functions are code and cannot be serialized directly, but the *description* of what
they do — the kinds they operate between and the conventions that make the round-trip
work — may be pattern-representable.

If a `RepresentationMap` is pattern-representable, then gram becomes the lingua franca
for describing mappings as well as data, and `pato` can inspect and validate mappings
using the same machinery it uses for everything else. If it is not — if some essential
part of a mapping is irreducibly procedural — that is also a useful conclusion, and
the boundary between the declarative and procedural parts is worth identifying
precisely.

Park until after the Haskell prototype has at least two concrete maps implemented and
their structure is well understood.

---

## Related documents

- `proposals/graph-transform.md` — primitive operations that `RepresentationMap`
  builds on; `ScopeQuery` and `GraphScope` typeclasses imply additive changes here
- `proposals/graph-query.md` — `GraphQuery` gains `ScopeQuery` and `GraphScope`
  instances; existing record fields become the implementations
- `proposals/pattern-equivalence.md` — syntactic equivalence (same kind, different
  notation); distinct from `RepresentationMap` (different kinds)
- `proposals/pato-feature-proposal.md` — `pato canonicalize` surfaces
  `RepresentationMap`; motivated this design
- `proposals/graph-classifier.md` — existing record-of-functions pattern; gains
  typeclass instances under the convention established in §"Interface convention";
  the `*Dict` / instance pattern applies to all future interfaces
