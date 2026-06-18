# RFC-0001: Frames and Spans — A Property-Graph-Inspired Navigation Framework for Pattern\<Subject\>

**Status:** draft
**Date:** 2026-05-29
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Related modules:** `Pattern.Core`, `Pattern.RepresentationMap`, `Pattern.PatternGraph`, `Pattern.Reconcile`

## Summary

This RFC proposes a navigation framework for `Pattern Subject` consisting of two newtypes over Pattern Subject: **Frame** and **Span**. A *Frame* wraps a self-contained Pattern and offers within-module operations — navigation, search, paramorphism — over its contents. A *Span* wraps a relationship-shaped Pattern whose elements are two Frames and, optionally, a *Bundle*: a Pattern whose own elements are pairwise correspondences between elements of those Frames. Frames are the modularization move (take a Pattern, treat it as a navigable unit); Spans are the composition move (relate Frames into larger structures, with the connecting tissue held externally). Together they recreate property graphs' externality property — that a node does not contain its relationships — one level up: a Frame does not contain its cross-Frame correspondences; the Span does. The substrate (`Pattern v`) is untouched; Frame and Span are typed views defined in terms of it, and either can be unwrapped to its underlying Pattern Subject for substrate-level composition. The framework's recursive self-similarity is preserved at the substrate level, where everything composes uniformly, while the newtypes add capability — and optimization — above it.

## Glossary

The vocabulary below is used precisely throughout the RFC. Readers can refer back to this section when terms need clarification.

### Structures

**Pattern** — The substrate container: a value paired with an ordered sequence of element Patterns. Polymorphic in the value type. A Pattern is not itself a graph; it is a value that can be *interpreted* as graph-structured by external machinery. Traversal of a Pattern descends into every reachable sub-Pattern.

**Subject** — The identity-bearing value used throughout this framework: an identity symbol, a set of labels, and a property record. The framework anchors on `Pattern Subject`.

**Pattern Subject** — A Pattern whose value type is Subject. The substrate's working type, and the type that Frame and Span wrap.

**Frame** — A newtype over Pattern Subject: `newtype Frame = Frame { framePattern :: Pattern Subject }`. A Frame is a *module* — a self-contained scope whose elements do not reach across to other Frames. The newtype exists to offer within-module operations (search, navigation, paramorphism) and to carry optimizations (such as a cached identity index) that a bare Pattern Subject has no place to hold. Unwrapping via `framePattern` drops back to the substrate, where the Frame's content composes like any other Pattern.

**Span** — A newtype over Pattern Subject: `newtype Span = Span { spanPattern :: Pattern Subject }`. The wrapped Pattern is relationship-shaped, conventionally containing two or three elements: the two Frames being related, optionally followed by a Bundle. In gram notation, the underlying Pattern is `[spanA | fra, frb]` (no Bundle) or `[spanA | fra, frb, bundle1]` (with one). A Span offers cross-module operations — following a correspondence from one Frame into another — that a bare Frame cannot, because a Frame holds neither the other side nor the connecting tissue. Unwrapping via `spanPattern` drops back to the substrate.

**Bundle** — A Pattern whose elements are pairwise correspondences between elements of two Frames. Each element of a Bundle is itself a relationship-shaped Pattern — typically `(fra_i)-->(frb_j)`, with an anonymous or named relationship Subject — pairing one element from one Frame with one element from the other. A Bundle has its own identifying Subject and is a first-class Pattern: it can stand alone, appear as the third element of a Span alongside the Frames it pairs, or be referenced by multiple Spans. The Bundle's pair-elements are the cross-Frame edges, externalized from both Frames.

**Portal** — A Pattern Subject that participates in a Bundle. Portal-hood is a *role*, not a type: any Pattern Subject can play the Portal role for some Bundle's pair-element, and the same Subject can be a Portal in multiple Bundles. Portal-hood is established by appearing as an endpoint of a pair-element, not by carrying any special label or marker on the Subject itself.

### The module boundary

**Self-containment (Reading 1)** — A Frame's elements do not reference other Frames. Whatever a Frame contains is reachable by traversing the Frame; nothing inside it points outward. This is a property of how Frames are constructed, not a behavior imposed by Frame's instances. A Frame built by summarizing sub-Patterns of another Frame contains *atomic* Portals (fresh Subjects with no elements), not the summarized sub-Patterns themselves — those remain in the lower Frame, referenced only through a Bundle. Encapsulation is therefore automatic: there is nothing to suppress during traversal, because there are no outward edges in the Frame to follow.

**Crossing the boundary** — When a user wants to follow a correspondence from one Frame into another, they do not reach through a Frame's encapsulation. They work with the *Span*, which is a Pattern that already contains both Frames and the Bundle of cross-Frame pair-elements. The cross-boundary edges exist — as the Bundle's pair-elements — but they live in the Span, not in either Frame. Crossing a boundary is a Span operation; it never punches through a Frame.

### Operations

**Within-module operations (Frame)** — `find`, `containers`, `siblings`, and `framePara`. Each operates over a single Frame's contents. They stay within the Frame because the Frame stays within itself (self-containment); no special suppression is needed.

**Find** — Given a predicate over Patterns and a Frame, return all Patterns in the Frame matching the predicate. Traverses the Frame's content (consistent with the structure-is-distributed principle). The general operation that subsumes by-identity, by-label, by-property, and by-shape lookups, each as a composition of `find` with the appropriate predicate.

**Frame paramorphism** — A paramorphism over a Frame in which the step function receives the whole Frame as ambient context throughout the recursion. The Frame argument is *not* narrowed as the recursion descends; the recursion always sees the same Frame. This embodies the principle that structure is distributed across the original point of inquiry rather than localized to a current subtree.

**Containers / siblings** — Given a Frame and a Pattern within it, find the Patterns in the Frame that contain it / share a container with it.

**Cross-module operations (Span)** — Operations that traverse from a Frame, across a Bundle pair-element, into the other Frame. *Refinement* — following a Portal back to the sub-Pattern(s) it summarizes — is the canonical example. These are Span operations because the connecting tissue is Span-level data.

**Frame-to-Frame transforms** — Pipeline-style operations that produce new Frames from old: `filtered` (keep elements matching a predicate), `intersect` and `union` (element-set composition), `rewriteElements` (element-wise transformation). All take general predicates or functions; specialized variants (by-label, by-property, by-kind) are caller compositions, not primitive operations. Filtering is construction-time, not view-time: a filter produces a new Frame with the filtered elements present as real values.

**Reconciliation** — A Frame-to-Frame operation, conventionally invoked at I/O boundaries, that merges two Frames by Subject identity. Three modes:

  - *1:1* — incoming Frame replaces existing Frame (or there is no existing Frame; the incoming Frame simply lands).
  - *Additive* — incoming Frame's contents merge into existing Frame: new Subjects added, shared Subjects reconciled.
  - *Subsumed* — incoming Frame's contents are absorbed into existing Frame at some non-root location.

### Principles

Four principles are load-bearing for this design. They are stated here because future contributors will face pressures to violate them, and the principles are what keep the design coherent over time.

**1. The substrate is untouched; the framework adds typed views above it.** `Pattern v` gains no fields and no new constraints. Frame and Span are newtypes over `Pattern Subject`, defined entirely in the framework layer. Adding a newtype is not extending the substrate; it is building on it.

**2. Frames are self-contained; crossing between them is a Span operation.** A Frame's elements do not reach across to other Frames (Reading 1). The only way to traverse from one Frame into another is through a Span, which holds the cross-Frame correspondences externally. This is the property-graph externality move applied to modules: a Frame does not contain its cross-Frame edges, just as a node does not contain its relationships.

**3. Structure is distributed, not localized.** Within-module navigation operations always see the whole Frame, never a narrowed local subtree. This is what makes Frame paramorphism powerful for graph-shaped reasoning and what avoids the subtle scoping bugs that come from implicit context narrowing.

**4. Reconciliation is a boundary operation.** Reconciliation happens at I/O boundaries (get/put), not throughout user code. Internal operations may assume already-reconciled data once a boundary operation completes. Defensive reconciliation sprinkled through internal code is the smell of a misplaced boundary.

## Motivation

`Pattern Subject` is a uniform container type. Crucially, a Pattern is not itself a graph — it is a substrate that can be interpreted as graph-structured by external machinery. This separation of substrate from interpretation is load-bearing, and it leaves a navigational gap: there is no shared vocabulary for the moves an analyst makes over a Pattern that respect the substrate's polymorphism while doing the work analysts actually need.

The moves fall into two categories, and the design gives each its own type.

**Modularization** takes a Pattern and treats it as a self-contained unit you can navigate within: search it, walk it, ask what contains what. This is the Frame. A Frame draws a boundary around a Pattern and says "this is a module; here are its members." Crucially, a Frame's members do not reach outside the Frame — the boundary is real because nothing inside crosses it.

**Composition** takes Frames (modules) and relates them into larger structures. This is the Span. A Span connects two Frames, optionally specifying element-level correspondences via a Bundle. The connections live in the Span, not in the Frames — so relating two modules does not violate either module's encapsulation. The Span is where cross-module structure accumulates.

The unifying insight is **externality applied recursively**. The property-graph model's success comes from refusing to embed adjacency in nodes: a node does not list its relationships; the graph holds them, externally. Pattern Subject already shares this property within a single Pattern (an element does not know its container or siblings). Frame and Span extend it one level up: a Frame does not hold its cross-Frame correspondences; the Span does. The Frame is the node; the Span is the graph the node sits in. Cross-boundary traversal is graph traversal over the Span, never node-internal traversal that breaks encapsulation.

The motivating use cases:

  - **Disentangling graphs-of-graphs.** A maintained airplane is simultaneously a mechanical Frame, an electrical Frame, a maintenance Frame, a supplier Frame, a procedure Frame. Each is a self-contained module. Correspondences between them — shared entities at irregular granularities — are Spans. Queries like "the electrical aspects of wing components subject to procedure X owned by technician Y" become navigation within Frames plus traversal across Spans, with no special-purpose graph-of-graphs machinery.

  - **Aspect-aware partitioning.** Spans carry the structure that distinguishes co-accessed regions across modules. A workload-aware partitioner that respects Span boundaries produces splits tuned to query access patterns rather than to topology in isolation.

  - **Query-plan scope reduction.** A query expressed in terms of Frame membership and Span participation carries its own scope information in its statement. The planner does not need to prove irrelevance from arbitrary predicates; the structure names what is in-scope.

The framework lines up cleanly with the spreadsheet model. Pattern-hs's posture is "a substrate for composing structure the way spreadsheets compose values," and the Frame/Span vocabulary maps directly:

| Spreadsheet | Frame + Span |
|---|---|
| Sheet | Frame |
| Sheet name (tab label) | Frame's identifying Subject |
| Sheet contents (cell grid) | The Frame's elements |
| Cell | A Pattern in the Frame (sometimes a Portal) |
| Cross-sheet reference (`Sheet2!A1`) | A pair-element within a Bundle |
| Named range of references | A Bundle |
| Workbook (collection of sheets) | A Pattern Subject whose elements are Frames (or Frame references) |
| Formula bar / "show formula" | Following a Span's Bundle to a Portal's correspondences |

The mapping is sharp because the cross-sheet reference — the thing that connects one sheet to another — is not stored *inside* either sheet's cells; it is its own piece of structure that names cells on both sides. That is exactly what a Bundle pair-element is, and exactly why it lives in the Span rather than in either Frame.

## Design

### Substrate vs. framework

Pattern-hs's substrate (`Pattern v`) is polymorphic in the value type and is not modified by this RFC. The framework anchors on `Pattern Subject` and introduces two newtypes over it:

```haskell
newtype Frame = Frame { framePattern :: Pattern Subject }
newtype Span  = Span  { spanPattern  :: Pattern Subject }
```

Adding these newtypes is not extending the substrate. `Pattern v` gains no fields, no constraints, no instances it didn't have. Frame and Span live in the framework layer (`Pattern.Frame`, `Pattern.Span`), defined entirely in terms of the substrate. The substrate-not-framework principle is honored by the *substrate* staying fixed, not by the framework refusing to define types.

The newtypes earn their keep in two ways a bare `Pattern Subject` cannot. First, they carry distinct operations: Frame offers within-module navigation, Span offers cross-module traversal, and the type you hold tells you which moves are available. Second, they can carry optimizations — a Frame may cache an identity index so that lookups are fast; a Span may cache its endpoint identities or a pair-index for refinement. A bare Pattern Subject has nowhere to hold such caches; a newtype does.

Either newtype unwraps to its underlying Pattern Subject (`framePattern`, `spanPattern`) for substrate-level work. Unwrapping is free (newtype coercion); wrapping may have a cost when the newtype maintains a cache (building the index is work). This shapes idiomatic use: wrap when you are about to do a lot of within-module navigation (amortize the index build); stay in Pattern-land for structural composition (no index needed). The cache is an enrichment, not a requirement — a Frame that holds no index is a perfectly good Frame whose `find` is linear.

### Frame

A **Frame** wraps a self-contained Pattern Subject — one whose elements do not reach across to other Frames. The root Subject of the wrapped Pattern is the Frame's identifying entity (its name at the inter-Frame layer, with whatever labels and properties classify the module as a whole). The elements are the module's members.

Self-containment (Reading 1) is the key property and the reason encapsulation needs no enforcement. A Frame built by summarizing sub-Patterns of a lower Frame contains *atomic Portals* — fresh Subjects with no elements — not the summarized sub-Patterns. The sub-Patterns stay in the lower Frame; the correspondence is recorded in a Bundle. So traversing a Frame stays within the Frame not because traversal is held back at a boundary, but because there are no outward edges to follow. The Frame's `Foldable` and `Traversable` instances are the ordinary Pattern ones; they encapsulate by virtue of the data, not by special behavior.

Frame construction and within-module operations:

```haskell
-- | Wrap a Pattern Subject as a Frame. If the Frame maintains a cache
--   (e.g. an identity index), it is built here.
frame :: Subject -> [Pattern Subject] -> Frame

-- | Wrap an existing Pattern Subject as a Frame without rebuilding it.
asFrame :: Pattern Subject -> Frame

-- | Add / remove an element at the top level (matched by Subject identity).
addElement    :: Pattern Subject -> Frame -> Frame
removeElement :: Pattern Subject -> Frame -> Frame

class FrameOps f where
  -- | Find all Patterns in the Frame matching the predicate. Traverses
  --   the Frame's content; returns matches in traversal order.
  find :: (Pattern Subject -> Bool) -> f -> [Pattern Subject]

  -- | Patterns that contain / share a container with the given Pattern.
  containers :: f -> Pattern Subject -> [Pattern Subject]
  siblings   :: f -> Pattern Subject -> [Pattern Subject]

  -- | Paramorphism with ambient Frame context. The step function sees the
  --   whole Frame (never narrowed), the current sub-Pattern, and the
  --   already-computed results for the sub-Pattern's elements.
  framePara :: (f -> Pattern Subject -> [a] -> a) -> f -> a
```

`find` takes an arbitrary predicate rather than privileging any specific Subject field. Identity lookup is one composition (`find (\p -> identityOf p == sym)`); label filtering is another (`find (hasLabel "Mechanical")`); property filtering is another. The framework has no opinion about which Subject fields warrant a dedicated operation; consumers compose `find` with whatever predicates they need. Predicate helpers (`hasLabel`, `propertySatisfies`, `matchesKind`) are ordinary functions, living alongside the primitives or in a small `Pattern.Predicates` module.

By default `find` is linear in the Frame's size. A Frame that caches an identity index answers the by-identity case in better-than-linear time; this is the kind of optimization the newtype exists to make possible, and is why `frame` (which may build the index) is distinguished from `asFrame` (which wraps without rebuilding).

The key design point of `framePara` is that the Frame argument is *not* narrowed during recursion. At any step, the step function has access to the whole Frame, not just the local subtree. This is what makes `containers` and `siblings` answer "in the Frame" rather than "in the current subtree," and what makes the paramorphism useful for graph-shaped reasoning where local context is misleading.

`FrameOps` has an instance for `Frame` (the primary one) and may also have an instance for bare `Pattern Subject` (so that code holding an unwrapped Pattern can navigate it without wrapping). The `Frame` instance is where caching-backed implementations live.

### Span

A **Span** wraps a relationship-shaped Pattern whose first two elements are the Frames being related. The root Subject of the wrapped Pattern identifies the Span-as-entity and carries properties describing the relationship collectively.

In gram notation, the underlying Pattern of a bare Span is:

```
[spanA | fra, frb]
```

— a relationship-shaped Pattern (root `spanA`, two elements `fra` and `frb`) of the same shape as any gram relationship `[r | a, b]`, applied at the inter-Frame layer. A bare Span asserts that `fra` and `frb` are related, named by `spanA`, with no element-level detail.

When element-level pairing matters, a third element — a **Bundle** — provides it:

```
[spanA | fra, frb, [bundle1 | (fra_1)-->(frb_1), (fra_2)-->(frb_2), ...]]
```

The Bundle's elements are pair-elements, each a relationship-shaped Pattern pairing one element of `fra` with one element of `frb`. The arrow `(fra_1)-->(frb_1)` is gram shorthand for a relationship-shaped Pattern with an anonymous Subject; pair-elements may carry explicit Subjects (`(fra_1)-[pair_1]->(frb_1)`) when an individual pairing needs identity or properties.

Structural notes:

  - **Frames are present by value.** The Frames a Span relates are elements of the Span's underlying Pattern — actual sub-Patterns, not identity references to Frames elsewhere. A Span is self-contained: resolving "what does this Span relate?" needs no external lookup. (By-reference encoding is available at serialization boundaries; that conversion is an I/O concern.)

  - **The Bundle is where cross-Frame edges live.** This is the heart of the design. The pair-elements connecting `fra`'s elements to `frb`'s elements are not stored in `fra` or `frb` — they are in the Bundle, which is in the Span. Neither Frame knows it is spanned. Crossing from `fra` to `frb` means traversing the Span's Bundle, never reaching through a Frame.

  - **A Span typically has 2 or 3 elements, interpretively.** The framework is optimistic about shape: a 2-element relationship-shaped Pattern between Frames is a bare Span, a 3-element one is bundled. Shape validation is deferred; non-conforming shapes have undefined interpretation under the Span convention.

  - **Bundles are first-class Patterns.** A Bundle has its own Subject and is a Pattern in its own right. It can stand alone, be the third element of a Span, or be shared by several Spans.

Span and Bundle construction, and cross-module operations:

```haskell
-- | Wrap two Frames as a bare Span (no Bundle).
span :: Subject -> Frame -> Frame -> Span

-- | Wrap two Frames and a Bundle as a Span.
spanBundled :: Subject -> Frame -> Frame -> Pattern Subject -> Span

-- | Construct a Bundle from a list of pair-elements. Each pair is two
--   Patterns (by value) to be paired. The pair-element Subject is
--   anonymous by default; richer pair construction uses ordinary
--   Pattern construction.
bundle :: Subject -> [(Pattern Subject, Pattern Subject)] -> Pattern Subject

class SpanOps s where
  spanFrames :: s -> (Frame, Frame)
  spanBundle :: s -> Maybe (Pattern Subject)

class BundleOps b where
  pairElements :: b -> [Pattern Subject]
  pairSubjects :: b -> [(Symbol, Symbol)]

-- | Destructure a relationship-shaped Pattern into its two endpoints.
endpointsOf :: Pattern Subject -> (Pattern Subject, Pattern Subject)
```

`SpanOps` and `BundleOps` are narrow on purpose — composition, inversion, and richer operations are ordinary functions, not typeclass methods.

#### Multiple Spans between the same Frames

Nothing constrains how many Spans relate two Frames. A mechanical Frame and an electrical Frame can be related by a `shares-physical-housing` Span, a `shares-power-source` Span, and a `was-serviced-together` Span at once — three Spans, each with its own Bundle (or none), all valid, none preferred. Spans are first-class: a Pattern whose elements are Spans, a Span between two Spans (each unwrapped to its Pattern and re-related), and so on, with no machinery beyond Pattern composition at the substrate level.

#### Portals

A **Portal** is the role a Pattern Subject plays when it appears as an endpoint of a Bundle's pair-element. Portal-hood is not a type, a label, or a structural property of the host Frame — it is the fact of being paired in some Bundle.

This is a deliberate substrate-faithful choice. The alternative — labeling a Subject `Portal` when it becomes paired — embeds knowledge of the pairing into the Pattern, violating the externality principle. The right way to discover Portals is to traverse Bundles and find the entities their pair-elements reference. Consumers needing fast Portal-discovery can build an index (`Map Symbol [Span]`) outside the substrate, the same posture as everywhere else.

#### Bundles standing alone

A Bundle is meaningful without a Span — a Pattern of pair-elements expressing correspondences between two element-sets, useful during construction before the relating Span is built. Wrapping a Bundle in a Span gives the correspondence a Frame-level identity and asserts which two Frames are related; standing alone, the Bundle just lists the pairings. Both are valid; `BundleOps` works either way.

### Within-module vs. cross-module: the operation split

The two newtypes divide the operation space along the module boundary.

**Frame operations stay within a module.** `find`, `containers`, `siblings`, `framePara` all operate over a single Frame's contents and never leave it — because the Frame never leaves itself (self-containment). When you hold a Frame, the moves available to you are within-module moves.

**Span operations cross between modules.** Following a correspondence from one Frame into another is a Span operation, because the connecting tissue (the Bundle) is Span-level data. When you hold a Span, you can additionally traverse across the boundary.

Refinement is the canonical cross-module operation: given a Portal in one Frame, find the element(s) it corresponds to in the other. It is a composition over Span and Bundle access, not a primitive:

```haskell
-- Follow a Span's Bundle from a Portal to its correspondents.
-- Each pair-element is a relationship-shaped Pattern (src)-->(tgt);
-- we match on tgt and return src (or vice versa, per direction).
refine :: Span -> Pattern Subject -> [Pattern Subject]
refine sp portal =
  case spanBundle sp of
    Nothing  -> []
    Just bdl -> [ src | pairElt <- pairElements bdl
                      , let (src, tgt) = endpointsOf pairElt
                      , identityOf tgt == identityOf portal ]
```

A user who wants to go from a neighborhood-Portal to the people it summarizes does not drill into the Portal — the Portal is atomic and has nothing inside (self-containment). They take the Span, find the pair-element whose one endpoint is the Portal, and follow it to the other endpoint in the lower Frame. The crossing is explicit, typed, and external — the property-graph discipline applied to modules.

### Composing Frames, Bundles, and Spans

The primitives — `frame`, `bundle`, `span`, `spanBundled`, the Frame-to-Frame transforms — compose by ordinary function composition. Workflows that combine selection, transformation, summarization, and correspondence-recording are not primitives; they are compositions.

The summarization workflow (sub-Patterns of a lower Frame summarized as Portals in a higher Frame, with the correspondence recorded) is one such composition:

```haskell
let selected    = find predicate lowerFrame          -- [Pattern Subject]
    portals     = map summarize selected             -- caller's summarize :: Pattern Subject -> Pattern Subject
    higherFrame = frame higherSubject portals
    pairs       = zip selected portals
    bdl         = bundle bundleSubject pairs
    result      = spanBundled spanSubject lowerFrame higherFrame bdl
in  result
```

The Portals in `higherFrame` are atomic — `summarize` produces fresh Subjects, not nested copies of the selected sub-Patterns. So `higherFrame` is self-contained (Reading 1), and the correspondence back to the sources lives entirely in `bdl`, inside the resulting Span. Each step is independently useful and uses only the framework's primitives. Variations are equally valid:

  - **Filter without summarizing**: pair each selected sub-Pattern with itself (or a relabeled copy) in the Bundle; the Bundle records "same entity, viewed from the new Frame."
  - **Relate two existing Frames**: skip selection and summarization; `bundle` from a pair-list, then `spanBundled`.
  - **Construct a Frame without recording correspondences**: just `frame` from a transformed element list; no Span, no Bundle.
  - **Construct a Bundle without a Span**: for a local computation that doesn't need the Frame-level relationship asserted.
  - **Summarize without building a Frame**: compute summary Subjects and use them elsewhere.

The framework provides the primitives; the workflows are caller compositions. Higher layers (such as Crossfold, the application built on pattern-hs) can name and package whichever workflows recur in their domain.

A note on injectivity: when `summarize` produces the same Subject for two different sub-Patterns, the resulting Bundle has two pair-elements pointing to the same Portal — which the caller may or may not consider correct. The framework records what the caller produced; the caller decides what's coherent. The concern is visibly the caller's (the zip and summarization are in caller code), not hidden in a primitive.

### Frame-to-Frame transforms

Transforms produce new Frames from old. They are construction-time operations, not view-like: the resulting Frame is a real Pattern Subject with the transformed elements materialized.

```haskell
-- General filtering: keep elements matching the predicate. The Frame-producing
-- counterpart of `find` — same predicate shape, different return.
filtered :: (Pattern Subject -> Bool) -> Frame -> Frame

-- Element-set composition
intersect :: Frame -> Frame -> Frame
union     :: Frame -> Frame -> Frame

-- Element-wise rewriting (projections, external interfaces, etc.)
rewriteElements :: (Pattern Subject -> Pattern Subject) -> Frame -> Frame
```

These are ordinary functions and compose by ordinary composition:

```haskell
result = filtered (\p -> hasLabel "Mechanical" p
                      && propertySatisfies "version" isCurrent p)
                  airplaneFrame
```

The substrate provides the general operations; specialized variants (by-label, by-property, by-kind filtering; label-rewriting, property-bijection) are caller compositions using predicate and function helpers. No combinator algebra is required at the type level; the functions compose because functions compose.

Because filtering is construction-time and Frames wrap real Patterns, an optimizer can fuse pipeline stages to avoid materializing intermediates — a performance concern, not a correctness one, and not something the framework must provide primitively.

### Reconciliation at I/O boundaries

Reconciliation is a Frame-to-Frame operation invoked at boundaries:

```haskell
data ReconciliationMode = OneToOne | Additive | Subsumed

reconcile
  :: ReconciliationMode
  -> ReconciliationPolicy   -- how to resolve property/label conflicts
  -> Frame                  -- existing
  -> Frame                  -- incoming
  -> Frame                  -- merged
```

`ReconciliationPolicy` specifies how to resolve conflicts when two Subjects share an identity but differ in labels or properties; basic policies (`preferIncoming`, `preferExisting`, `union`, `error`) are provided, and consumers can write custom ones.

Because reconciliation happens only at boundaries, internal operations may assume their Frames are already reconciled. This is what lets `find` and by-identity compositions over it stay simple: they need not defensively reconcile, since all Patterns in a Frame are assumed reconciled when the Frame was constructed or last received from I/O. (A Frame that caches an identity index builds it at construction over already-reconciled content, so the cache stays consistent.)

### Inter-Frame topology

The recursive self-similarity lives at the substrate level. A graph of Frames is a Pattern Subject whose elements are Frames and Spans — either by value (as full sub-Patterns) or by reference (atomic Patterns whose Subjects identify Frames or Spans elsewhere).

By value:

```
[workbook | fra, frb, frc, [spanAB | fra, frb, bundleAB], [spanBC | frb, frc, bundleBC]]
```

By reference:

```
[workbook |
  [:FrameRef { identity: "mechanical-frame" }],
  [:FrameRef { identity: "electrical-frame" }],
  [:SpanRef  { identity: "mech-elec-shared-housing" }]
]
```

Either is a Pattern Subject; wrap it as a Frame to navigate it as a module. Lifting again — a Pattern whose elements identify Workbooks — is the same move with no new vocabulary. The newtypes are lenses you apply at whatever level you want module-navigation or cross-module-traversal powers; the composition underneath is always plain Pattern Subject, which is why the recursion has no ceiling and needs no parallel machinery for "a Pattern of Frames" versus "a Pattern of anything else."

Resolving a reference-form Frame or Span to its Pattern requires an external lookup (`Map Symbol (Pattern Subject)`), a consumer concern. The by-value form needs no resolution (the natural in-memory form); the by-reference form is the natural serialization-and-storage form.

### Integration with `Pattern.Graph` and `Pattern.PatternGraph`

The graph-interpretation machinery — `GraphLens`, `PatternGraph`, `GraphQuery`, `Pattern.Graph.Algorithms` — operates on `Pattern Subject`. A Frame unwraps to a Pattern Subject (`framePattern`), so all of it applies to a Frame's content directly. A Span likewise unwraps (`spanPattern`) and is interpretable as a graph in its own right: a relationship-shaped Pattern whose endpoints are the related Frames and whose optional third element is a Bundle. The natural reading is "a Span is a relationship at the Frame level," but the graph interpretation is available for free.

### Relationship to `ScopeQuery`

The library's existing `ScopeQuery` typeclass provided an interface for scope-aware navigation. Its responsibilities are absorbed by `FrameOps`: the scope-defining role is filled by a Frame (a self-contained Pattern Subject), the navigation-operations role by `FrameOps` methods. `ScopeQuery` may be retained as a public name aliasing the same operations for backward compatibility; new code targets `FrameOps`. The first-class scope-as-value pattern is provided by Frame itself — a Frame is a value — so no separate scope-dictionary type is needed.

### What this RFC does not include

  - **Pattern-equivalence and RepresentationMap.** RepresentationMap remains a roadmap item; the Frame+Span model makes it straightforward to land later (a Frame-to-Frame transform whose correspondence is recorded as a Span), but no concrete proposal is included here.

  - **Specific cache designs for the newtypes.** That a Frame *may* cache an identity index (and a Span a pair-index) is part of the rationale for the newtypes, but the concrete cache representation, when it is built, and its invalidation story are deferred until there is a workload to design against. The newtype with no cache is the baseline.

  - **Persistent / lazy / database-backed Frames.** Per the principle that the substrate is in-memory, lazy or unmaterialized scopes are a database concern, handled at a layer above pattern-hs.

  - **Concrete `ReconciliationPolicy` values beyond the basic four.** Domain-specific policies are consumer concerns.

  - **Shape validation for Spans and Bundles.** The 2-or-3-element Span shape and the Bundle's pair-element structure are interpretive conventions, not enforced invariants, pending a broader `gram-schema-conventions` effort.

## Open Questions

1. **Pair-element Subject: anonymous, deterministic, or caller-supplied.** A Bundle's pair-elements are `(a)-->(b)` (anonymous) or `(a)-[p]->(b)` (named). The named form is needed when pairs carry properties or must be addressable. Should the framework standardize a convention for when pairs gain identities (e.g. a deterministic Subject derived from the endpoints), or leave it to the caller? Leaning toward caller choice, with constructor variants for both.

2. **Span direction.** Pair-elements written `(a)-->(b)` imply direction. Some Spans are symmetric (shares-entity), some directed (summarizes-to). Should direction be expressed at the convention level, or should symmetric Spans use the directed form with direction understood as informational? Leaning toward the latter, with a possible `symmetric: true` property convention.

3. **Cache-bearing vs. bare newtypes.** A Frame may carry an identity index; a Span may carry a pair-index. Should the cache-bearing and bare variants be the same type (cache optional, built by `frame`, absent after `asFrame`) or distinct types (`Frame` vs. `IndexedFrame`)? The same-type approach is simpler but makes the performance profile invisible in the type; the distinct-type approach surfaces it. Probably same-type with the cache as an internal optional field, but worth confirming against a real workload.

4. **`FrameOps` instance for bare `Pattern Subject`.** Offering `FrameOps Pattern Subject` lets code navigate an unwrapped Pattern without wrapping. Convenient, but it means navigation is available on Patterns not intended as modules (arbitrary nested sub-Patterns). Probably offer it (convenience wins, and self-containment is a usage property not a type invariant), but it slightly blurs the module boundary at the type level.

5. **Span and Bundle typeclass scope.** Are `spanFrames` / `spanBundle` and `pairElements` / `pairSubjects` the right minimal interfaces, or should composition (`compose :: Span -> Span -> Maybe Span` where endpoints align) and inversion (`invert :: Span -> Span`) be methods? Leaning toward keeping the classes narrow and providing composition/inversion as ordinary functions.

6. **Reconciliation across Spans.** When two Frames are reconciled, Spans referencing their elements may need their Bundles updated (pair-elements pointing at old elements re-pointed at merged ones). Treated here as a separate concern run at the same boundary, with its own policy. A concrete proposal awaits use cases.

7. **By-value vs. by-reference inter-Frame conventions.** Both encodings are valid Patterns. A standardized `FrameRef` / `SpanRef` label convention (for the by-reference form) would let generic Workbook-walkers be written; leaving it open is more substrate-faithful. Probably standardize, but not yet.

## Alternatives

**Frame and Span as type synonyms for Pattern Subject.** `type Frame = Pattern Subject` would make every Pattern Subject automatically a Frame, with the name purely documentary. Rejected because distinct types are the point: they let Frame and Span carry distinct operations (within-module vs. cross-module) and distinct optimizations (identity index, pair-index) that a shared type cannot. The synonym version cannot distinguish a module from an arbitrary Pattern, and cannot cache. The newtype cost (explicit wrap/unwrap) is small and the unwrap direction is free.

**Frame and Span as conventions only, with no type at all.** An earlier instinct kept "no new types" as a hard constraint, expressing Frame and Span purely as label conventions over Pattern Subject. Rejected on the same grounds: the constraint was a proxy for "don't extend the substrate," which newtypes-over-Pattern-Subject already honor. Refusing to define framework types bought nothing and forfeited the capability and optimization that distinct types enable.

**Span as a record type rather than a newtype over a relationship-shaped Pattern.** `data Span = Span { spanSubject :: Subject, leftFrame :: Frame, rightFrame :: Frame, bundle :: Maybe Bundle }` would make the parts explicit fields. Rejected because it forfeits the recursive self-similarity at the substrate level: a Pattern of Spans would need `[Span]` machinery distinct from `[Pattern Subject]`, and "a Span is just a relationship-shaped Pattern" would stop being true. The newtype-over-Pattern approach keeps the underlying value composable as a plain Pattern (via `spanPattern`) while still offering typed Span operations.

**Encapsulation enforced by Frame's traversal instances.** An earlier framing had Frame's `Foldable`/`Traversable` instances actively stop descent at the module boundary ("treat elements as atomic"). Rejected as solving a non-problem: under Reading 1, a Frame's elements do not reach outside the Frame, so there is nothing to stop. Encapsulation is a property of how Frames are built (atomic Portals, correspondences in Bundles), not a behavior the instances impose. Frame's instances are the ordinary Pattern ones.

**Cross-Frame edges stored in the Frames.** Letting a Frame's elements reference elements of other Frames directly (an adjacency-style cross-reference) would put cross-module edges inside modules. Rejected because it violates the externality principle that motivates the whole design: a Frame would then know its neighbors, exactly the property graphs refuse. Edges live in Spans (via Bundles), external to both Frames, so a Frame never knows it is spanned and crossing a boundary is always an explicit Span operation.

**Portal as a label added to participating Subjects.** Tempting for fast discovery (filter by label). Rejected because it embeds knowledge of the pairing into the Pattern, violating externality. Discovery is by Bundle traversal; consumers needing speed build an index.

**`find` privileging identity (a dedicated `identityLookup`).** Rejected because identity is one Subject field among three; privileging it puts the substrate in the position of having opinions about which fields are navigable. `find` takes any predicate; by-identity lookup is one composition. Index-backed Frames make the common by-identity case fast without a dedicated method.

**A bundled `constructHigherFrame` primitive.** A single operation doing selection + summarization + Frame-building + Span-construction was considered and rejected as overly prescriptive: it bundled separable, independently useful steps and implied summarization is the canonical reason to build a Frame. The steps compose from smaller primitives; the workflow, if it recurs, belongs in a higher layer (Crossfold), not the substrate framework.

**Adjunction-shaped Spans.** An adjunction is a canonical, structure-respecting, dual-direction correspondence. Tempting as a constraint making Spans canonical-by-structure. Rejected because adjunctions require categorical structure on Patterns not yet defined; committing now would force that structure to fit the framing rather than the framework's needs. A Span in the everyday sense (a mediating structure relating two sides) is the right framing for now, with adjunctions available as a future refinement if Patterns gain enough categorical structure.
