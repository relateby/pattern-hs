# RFC-0001: Strata and Aspects — A Navigation Framework for Pattern<Subject>

**Status:** draft
**Date:** 2026-05-18 (revised 2026-05-26)
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Related modules:** `Pattern.Core`, `Pattern.RepresentationMap`, `Pattern.PatternGraph`, `Pattern.Reconcile`

## Summary

This RFC proposes a navigation framework for `Pattern Subject` that distinguishes two orthogonal kinds of perspective-shift: **stratum-shifts**, which constructively build new Patterns by crossfolding selected sub-Patterns into the atomic elements of a higher-order Pattern, and **aspect-shifts**, which interpretively re-view the same Pattern through a different scope. The framework introduces a `Stratum` type — a Pattern Subject wrapped with its own identifying `Subject` and origin references back to what it was based on — along with two small modules, `Pattern.Stratum` and `Pattern.Aspect`, built on the existing `Pattern`, `PatternKind`, `ScopeQuery`, and `RepresentationMap` primitives. Because a `Stratum` carries a `Subject`, strata themselves are addressable; a Pattern whose elements are stratum subjects represents *inter-stratum* topology in the same Pattern/Subject vocabulary as intra-stratum content, with no new types. Strata and aspects compose orthogonally, and the framework's own primitives are expressible in the framework — providing a uniform vocabulary for the graph-of-graphs problem at every scale.

## Glossary

The vocabulary below is used precisely throughout the RFC. Readers can refer back to this section when terms need clarification.

### Structures

**Pattern** — The substrate container: a value paired with an ordered sequence of element Patterns. A Pattern is not itself a graph; it is a value that can be *interpreted* as graph-structured by external machinery. Substrate level — polymorphic in the value type.

**Subject** — The identity-bearing value used throughout this framework: an identity symbol, a set of labels, and a property record. The framework anchors on `Pattern Subject` specifically; the substrate remains polymorphic.

**Pattern Subject** — A Pattern whose value type is Subject. The framework's working type.

**Atom** — An atomic Pattern: a Pattern with no elements.

**Sub-Pattern** — A Pattern appearing as an element (directly or transitively) of another Pattern. Preferred over "subgraph" when speaking at the container level.

**Stratum** — A Pattern Subject wrapped with (a) a Subject identifying the stratum-as-entity, distinct from any identity inside the Pattern, and (b) origin references back to what it was based on. A Stratum is *based on* either a Pattern Subject (the base case) or another Stratum (the recursive case). A Stratum is addressable: because it carries its own Subject, strata can themselves be the elements of a Pattern Subject.

**OriginRef** — The provenance record attached to each crossfolded element of a Stratum: the lower Stratum (when held inline) and the sub-Pattern that was crossfolded from it. Plays the role of the formula behind a spreadsheet cell — preserved alongside the cell's displayed value, available for inspection and recomputation.

**Aspect** — A scope interpretation applied to a Pattern Subject or Stratum. Concretely, a ScopeDict together with optional Pattern-transforming machinery (a RepresentationMap) when the aspect needs to project rather than merely restrict. An aspect re-views; it does not construct.

**ScopeDict** — The first-class record form of a scope provider, parameterized at Symbol and Subject. The uniform scope representation used by all aspect combinators.

**Frame** — The pair (Stratum, Aspect): which Stratum you are looking at, through which Aspect.

**Inter-stratum Pattern** — A Pattern Subject whose elements carry the Subjects of Strata. Represents topology *among* Strata using the same Pattern/Subject vocabulary as topology *within* a Stratum.

**Intra-stratum** — Adjective for the content inside a single Stratum.

### Relationships between Strata

**Based on** — A Stratum is *based on* what it was promoted from: either a Pattern Subject (base case) or a lower Stratum (recursive case). The directional dependency that promotion creates.

**Lower / higher** — Relative position in a chain of "based on" relationships. The Stratum a Stratum is based on is *lower*; one built from it is *higher*.

**Crossfolded from** — A crossfolded element at a higher Stratum is *crossfolded from* a sub-Pattern at the lower Stratum. The OriginRef records this.

### Navigation moves

**Stratum-shift** — A constructive navigation move that produces a new Stratum from an existing Pattern Subject or Stratum. Concretely, promotion. Produces new Subject identities and new Pattern values.

**Aspect-shift** — An interpretive navigation move that swaps the Aspect applied to a Pattern Subject or Stratum. Typically produces no new Pattern, only a new scope; when it must transform (e.g., for an external interface), it does so via RepresentationMap.

**Promotion** — The mechanism of a stratum-shift: select sub-Patterns of the lower stratum via a scope; crossfold each into a fresh atom; assemble the crossfolded elements as the contents of a higher Pattern Subject; wrap the result with the OriginRefs recording what each was crossfolded from.

**Crossfold** *(verb / function)* — The structure-collapsing operation that produces a Subject for the higher stratum from a sub-Pattern of the lower stratum. A fold across the stratum boundary: the sub-Pattern's internal structure collapses into a single atomic representative, with the source preserved as provenance in `stratumOrigin`. See the metaphor note below.

**Crossfolded element** — An atomic element of a Stratum that was produced by promotion; the Subject returned by `crossfold` for some sub-Pattern of the lower Stratum. The higher-stratum analog of a spreadsheet cell: a named position whose displayed value summarizes its inputs, with the underlying formula preserved separately.

**Refinement** — The dual of promotion: navigate from a crossfolded element at the higher Stratum back to the sub-Pattern it was crossfolded from, via its OriginRef. The spreadsheet analog is opening the formula bar for a cell to see what inputs produced its value.

**Stratification** — The act or result of building higher Strata. Activity noun, paired with "Stratum" as the unit.

### Scope combinators

**restrict / intersect / union** — Algebraic operations on ScopeDicts producing new ScopeDicts. Closed operations: scopes in, scopes out.

**labelFiltered / propertyFiltered / kindRestricted** — Base scope constructors that filter the underlying scope by a label set, a property predicate, or a PatternKind respectively.

**labelRewrite / propertyBijection** — Helpers for the Pattern-transforming case, producing RepresentationMap values for invertible aspect projections.

### A note on the `crossfold` metaphor

The function that produces a fresh Subject for each promoted element is named `crossfold`. The name composes two ideas already familiar to the audience.

*Fold* is the structure-collapsing operation from functional programming: given a structure and a combining function, produce a summary value. A list folds into a sum, a tree folds into a height, a sub-Pattern folds into a Subject. The collapse is real — internal structure is not visible in the result.

*Cross* names the direction: the fold goes across the stratum boundary, from lower to higher. The result is not part of the structure being folded; it is an atomic element of a different Pattern at a different layer.

The richer mental model is the **spreadsheet**. A spreadsheet's cell holds a value that is the result of a formula applied to other cells; the cell is a single atomic position, but the formula behind it is preserved and inspectable. The cell isn't lossy in any irrecoverable sense — the formula bar lets you see exactly what produced the displayed value, and the cell will recompute if the inputs change. Map this onto promotion:

| Spreadsheet | Stratum |
|---|---|
| Sheet | Stratum |
| Sheet name (tab label) | `stratumSubject` |
| Sheet contents (cell grid) | `stratumPattern` |
| Cell | Crossfolded element |
| Formula in the cell | The `crossfold` function applied to the source sub-Pattern |
| Cell's displayed value | The Subject `crossfold` produces |
| Formula's inputs (referenced cells/ranges) | The sub-Pattern of the lower stratum |
| Formula bar / "show formula" | Refinement via OriginRef |
| Cross-sheet references (`Sheet2!A1`) | Inter-stratum Pattern relationships |
| Workbook (collection of sheets) | A graph of strata expressed as a Pattern Subject |

The Sheet correspondence is worth pausing on. A spreadsheet sheet has a name that lives on its tab — external to any cell inside it — which is exactly why `Sheet2!A1` is a sensible reference even when nothing inside Sheet2 happens to be named `Sheet2`. `stratumSubject` plays the same role: it names the Stratum at the inter-stratum layer, independently of whatever happens to be at the root of `stratumPattern`. The analogy stops short of full identification in one respect — sheets in a workbook are lateral peers, while strata carry a directed "based on" relationship that has no spreadsheet equivalent — so Sheet illuminates the *unit*-level analogy (one sheet = one stratum, with its own external name) while the framework adds structure (the based-on chain) that goes beyond what sheets alone provide.

This is the load-bearing metaphor for the operation. The fold framing captures the structure-collapsing summary semantics; the spreadsheet framing captures the preserved-provenance recoverability story. Together they describe what `crossfold` *is*: a fold whose unfolding is retained as a first-class feature of the surrounding context, the way a spreadsheet retains its formulas.

A secondary metaphor worth keeping for the *structural opacity* property specifically: an embedded webpage. A browser walking a host page's DOM stops at an `<iframe>` element; what's inside is reached through a separate channel, mediated by the surrounding context. Crossfolded elements behave the same way under ordinary Pattern traversal — `Foldable`, `Functor`, `Traversable`, `Comonad`, and `paraWithScope` stop at the crossfold, and lower-stratum content is reached only through `refine`. The webpage-embed metaphor isn't the primary frame for the operation, but it crisply describes one of its most important properties.

## Motivation

`Pattern Subject` is a uniform container type: a value with a decorating `Subject` and an ordered sequence of element Patterns. Crucially, a Pattern is not itself a graph — it is a container that can be *interpreted* as graph-structured by external machinery (`PatternKind` predicates classify sub-Patterns into node, relationship, path, and other kinds; `ScopeQuery` instances supply navigation semantics). This separation of substrate from interpretation is a load-bearing design choice that enables flexibility but leaves a gap: there is no shared vocabulary for the navigational moves analysts actually make over a Pattern.

Those moves fall into two categories that get conflated in practice:

1. **Constructive moves** that build a new Pattern by crossfolding sub-Patterns of an existing one into the atomic elements of a higher-order Pattern — people-Patterns crossfold into atomic elements of a neighborhood-Pattern, neighborhood-Patterns crossfold into atomic elements of a city-Pattern. These produce new `Subject` identities and new `Pattern` values. They are lossy at the new level in the sense that the source sub-Pattern's internal structure is not visible to ordinary traversal of the result.

2. **Interpretive moves** that re-view the same Pattern through a different lens — a person seen as employee, family member, patient, citizen. These do not produce new data; they choose which labels and properties to make salient by supplying a different `ScopeQuery` interpretation.

Without a vocabulary that distinguishes these, every navigation accumulates ad-hoc special cases. Promotion logic gets entangled with view logic. Query optimization cannot exploit the structural difference between "restrict to a sub-view" (cheap, interpretive) and "compute a higher stratum" (expensive, constructive). Partitioning algorithms ignore both because neither is expressed in their input.

The motivating use cases:

- **Disentangling graphs-of-graphs.** A maintained airplane is simultaneously a mechanical Pattern, an electrical Pattern, a maintenance-manual reference Pattern, a supplier Pattern, and a procedure Pattern — all sharing entities at irregular granularities. Without named layers, queries become opaque joins. With named strata and aspects, the same query becomes a sentence an engineer can say: "the electrical aspect of the wing stratum, filtered by procedures-touched-by-Technician-X."

- **Aspect-aware partitioning.** Classical graph partitioning minimizes edge-cut under size constraints, ignoring what queries actually traverse. Aspects are precisely the structure that distinguishes co-accessed regions; partitioning along aspect boundaries produces splits tuned to workload rather than to topology in isolation.

- **Query-plan scope reduction.** A query expressed in terms of named aspects carries its own scope-restriction in its statement. The planner does not need to prove irrelevance from arbitrary predicates; the aspect names what is in-scope.

The unifying theme is complexity management. Real graphs are large and interwoven, and the formalism we adopt determines whether we can reason about them in pieces or are forced to reason about them whole.

A guiding thesis informs the design choices throughout this RFC: **pattern-hs is a substrate for composing structure the way spreadsheets compose values.** Spreadsheets succeed by giving users a single uniform unit (the cell), a single uniform composition mechanism (the formula), and first-class provenance (the formula bar) — and on top of that minimum, every nontrivial spreadsheet practice is built. The same posture applies here: a single uniform container (Pattern Subject), a single uniform composition mechanism (crossfolding across stratum boundaries), and first-class provenance (`stratumOrigin`). The vocabulary that follows — Stratum, crossfold, refinement, aspect — is the structural analog of cells, formulas, formula-bar inspection, and named views. This framing is not load-bearing for any individual piece of the design, but it explains why the pieces fit together the way they do.

## Design

### Substrate vs. framework

This RFC builds a navigation framework one layer above the `Pattern v` substrate. The substrate is rightly polymorphic in `v`: a `Pattern v` is a recursive decorated-sequence container that knows nothing about identity, labels, or properties. The navigation framework proposed here cannot remain that polymorphic. Its primitives need an identity to key the origin map, labels and properties to filter scopes by, and a uniform shape for the values that aspects make salient. All of these are exactly what `Subject` provides.

So: the framework anchors on `Pattern Subject`. `Stratum` wraps `Pattern Subject`, not `Pattern v`. The aspect combinators are `Subject`-specialized. This is a deliberate commitment, not a deferred decision. Pattern-hs's substrate stays parametric in `v`; the navigation framework is the layer where we stop deferring and pick. The substrate-not-framework principle the RFC invokes throughout makes this consistent rather than awkward — substrates are universal, frameworks are opinionated, and Subject is the opinion this framework is built around.

A future framework over a different value type would be a sibling module to this one, not a generalization of it.

### Conceptual model

A **stratum** is a Pattern Subject wrapped as a Stratum — addressable, based on what it was promoted from. Strata stack: one stratum's contents can be crossfolded to become the atomic elements of the next.

An **aspect** is a `ScopeQuery` interpretation of a Pattern — an interpretation that determines which elements are reachable, how `containers`, `siblings`, and `byIdentity` resolve, and which labels and properties are salient. Aspects are interpretations of a Pattern, not transformations of it.

A **frame** is the pair (stratum, aspect): which Pattern you are looking at, and through which scope. Navigation moves either coordinate independently.

**Stratum-shifts** are constructive: promotion produces a new Pattern whose elements are atoms crossfolded from selected sub-Patterns of the lower stratum. The result is wrapped as a `Stratum` value that retains references back to the source sub-Patterns, so refinement (navigation back down) is supported without polluting the Pattern's properties with provenance bookkeeping.

**Aspect-shifts** are interpretive: they swap the `ScopeQuery` instance applied to a Pattern. They typically do not produce new Patterns. When an aspect-shift does need to transform the Pattern (e.g., projecting a label-renamed view for an external interface), it produces a `RepresentationMap` and the existing round-trip machinery applies.

The two compose orthogonally. Any complex navigation is a sequence of stratum-shifts and aspect-shifts, interleaved freely.

### The `Stratum` type

A `Stratum` wraps a Pattern Subject with an identity for the stratum-as-entity and the provenance information that promotion produced:

```haskell
data Stratum = Stratum
  { stratumSubject :: Subject
    -- ^ The stratum-as-entity: a Subject naming this Stratum, with
    --   labels and properties that classify and describe it (e.g.,
    --   identity "airplane-mechanical", labels {"Stratum", "Mechanical"},
    --   properties about ownership, version, or refresh schedule).
  , stratumPattern :: Pattern Subject
    -- ^ The Pattern this Stratum wraps. All ordinary Pattern operations
    --   work through this accessor.
  , stratumOrigin  :: Map Symbol OriginRef
    -- ^ For each crossfolded element (keyed by its Subject identity — `Symbol`,
    --   which is `Id Subject` in the library's `GraphValue` instance), what
    --   lower-stratum sub-Pattern it was crossfolded from.
  }

data OriginRef = OriginRef
  { originStratum :: Maybe Stratum
    -- ^ The lower Stratum, when held inline. `Nothing` indicates the
    --   down-reference is to be resolved externally via the caller's
    --   choice of scope.
  , originPattern :: Pattern Subject
    -- ^ The lower-stratum sub-Pattern that was crossfolded. Always a single
    --   Pattern (Patterns are always rooted in the trivial sense of
    --   being single values); internal structure may be arbitrarily rich.
  }
```

The recursion in `originStratum :: Maybe Stratum` is intentional. The strata-of-strata recursion ("one stratum's contents are another stratum's base") is encoded directly: a multi-level stack of strata is a `Stratum` whose origin references another `Stratum`, whose origin references another, and so on down to a base `Pattern Subject` that was not itself produced by promotion.

The shape of `Stratum` deliberately echoes the shape of `Pattern Subject`: a decorating Subject up top, structured content below. A Pattern Subject has `value :: Subject` and `elements :: [Pattern Subject]`; a Stratum has `stratumSubject :: Subject` and `stratumPattern :: Pattern Subject` (plus provenance). The structural self-similarity is intentional and lets the framework's own primitives become expressible *in* the framework (see "Small-scale and large-scale connections" below). But the two are distinct types, not aliases, because the semantics of the subject differ: a Pattern's `value` *decorates* its `elements` — the subject names what the sequence is — whereas a `stratumSubject` *names a wrapper* around `stratumPattern` — the subject identifies the stratum-as-entity, which holds the Pattern but is not the Pattern's root. Conflating them by defining `stratumSubject = value stratumPattern` would lose a degree of freedom that matters: a stratum is a *named view onto* a Pattern, and the identity by which we refer to the stratum is generally not the identity of whatever happens to be at the root of the Pattern it holds. The spreadsheet analog is exact: a sheet's tab label (`Sheet2`) is external to any cell inside it, which is what makes `Sheet2!A1` a sensible reference even when nothing in Sheet2 happens to be called `Sheet2`. `stratumSubject` is the tab label.

`Stratum` is additive: code operating on plain `Pattern Subject` continues to work unchanged, and a `Stratum` exposes its Pattern via `stratumPattern` for any consumer that wants to treat it as a Pattern. Any Pattern — including ones authored directly from atomic nodes, with no promotion history — can be wrapped as a `Stratum` with an empty origin map (see `stratify` below). The empty origin map is a meaningful state, not a degenerate one: it records that no atoms in this Stratum have down-references, which is exactly the case for authored content. This means authored Patterns and promoted Patterns are interchangeable wherever a `Stratum` is expected, and the base of a strata stack can sit at whatever level of granularity the modeler chooses to author directly. A Stratum's origin map may also be partial — crossfolded elements have entries; authored atoms added at the higher stratum do not — and `refine` behaves correctly on both: returning the source for crossfolded elements, `Nothing` for authored ones. The spreadsheet analog: cells holding literal values coexist with cells holding formulas; both are addressable, both render a value, only the latter has a formula to inspect.

#### Why this shape, and not the alternatives

Several alternative encodings of the down-reference were considered:

- *A reserved property on the crossfolded Subject.* Practical but messy: it conflates user-meaningful properties with library bookkeeping, requires a reserved key, and leaks provenance into serialization.
- *A new field on `Pattern` itself.* Invasive: it modifies the core type, forces every Pattern (crossfolded or not) to carry the field, and breaks existing operations.
- *Structured identity encoding the source stratum.* Conflates "what am I" (identity) with "where did I come from in this promotion" (provenance). Two promotions of the same source should produce distinct higher-stratum identities; their shared origin is a fact about the promotion, not about the entity.
- *An external registry returned alongside the Pattern.* The right information but the wrong packaging: tuples of (Pattern, Registry) have to be carried together everywhere, with no type-level guarantee that the registry corresponds to the Pattern.

The `Stratum` type names what the alternatives gestured at: a value that knows how to navigate to what it was based on. The down-references are structurally part of the type, separate from user-meaningful Pattern content.

### `Pattern.Stratum`

The simplest way to obtain a `Stratum` is to wrap an existing Pattern with an identifying Subject:

```haskell
stratify :: Subject -> Pattern Subject -> Stratum
stratify s p = Stratum
  { stratumSubject = s
  , stratumPattern = p
  , stratumOrigin  = mempty
  }
-- ^ Wrap a Pattern as a Stratum with the given identifying Subject and no
--   down-references. Use this for authored Patterns serving as the base
--   of a strata stack, or for any Pattern that should be navigable
--   through the Stratum API.
```

`stratify` is the identity-like entry point into the `Stratum` API. The caller supplies the stratum's identifying Subject — what to call this Stratum at the inter-stratum layer — and the Pattern that constitutes its contents. The empty origin map is meaningful, not degenerate: it records that no atoms in this Stratum have promotion history, which is exactly the case for authored content. `promote` and `promoteFrom` likewise take a `Subject` argument identifying the higher stratum they produce.

A promotion is a selector plus a crossfold:

```haskell
data Promotion = Promotion
  { promotionKindName :: String
    -- ^ Human-readable name for the kind of higher-stratum atom produced.
  , promotionSelect   :: forall q. ScopeQuery q Subject
                      => q Subject -> Pattern Subject -> [Pattern Subject]
    -- ^ Which sub-Patterns of the lower Pattern become crossfolded elements
    --   of the higher stratum. Patterns are container values, so each
    --   selection is a single sub-Pattern; its graph interpretation
    --   (node, relationship, path, region) is the caller's concern.
  , crossfold         :: Pattern Subject -> Subject
    -- ^ Given a selected sub-Pattern, fold it across the stratum boundary
    --   into a fresh Subject for the higher stratum. The Subject is the
    --   identity by which the crossfolded element will be looked up; the
    --   link back to the source sub-Pattern is stored separately in the
    --   produced Stratum's origin map. Required to be injective.
  }
```

The naming choice deserves a sentence. `crossfold` composes two ideas: *fold* (the structure-collapsing operation from functional programming) and *cross* (the direction — across the stratum boundary, from lower to higher). The function takes a sub-Pattern with arbitrary internal structure and produces a single Subject that summarizes it at the higher layer. The collapse is real, in the sense that ordinary Pattern traversal of the result stops at the produced atom; but the input is preserved as provenance in `stratumOrigin`, so the fold is recoverable. The spreadsheet model captures this exactly: a cell's displayed value is the result of a formula collapsing some inputs, and the formula bar makes the inputs recoverable on demand. See the glossary's "note on the `crossfold` metaphor" for the full mapping.

Promotion produces a `Stratum`:

```haskell
promote
  :: ScopeQuery q Subject
  => Subject             -- identity for the higher stratum
  -> Promotion
  -> q Subject
  -> Pattern Subject     -- lower stratum's Pattern
  -> Stratum             -- higher stratum, with down-references retained

promoteFrom
  :: ScopeQuery q Subject
  => Subject             -- identity for the higher stratum
  -> Promotion
  -> q Subject
  -> Stratum             -- lower stratum is itself already a Stratum
  -> Stratum             -- chains of promotion are first-class
```

`promote` is implemented via `paraWithScope`, applying the selector at each sub-Pattern position and assembling the crossfolded elements as the elements of the result Pattern. The supplied `Subject` becomes the produced Stratum's `stratumSubject` — its identity at the inter-stratum layer. Each crossfolded element's `OriginRef` retains the selected sub-Pattern, so refinement is a direct lookup. `promoteFrom` is equivalent to `promote` applied to a Pattern obtained via `stratumPattern`, modulo the down-reference bookkeeping that retains the source `Stratum` in the new origin map; both functions are exposed because chained-promotion call sites read more clearly with `promoteFrom`, but they offer no additional expressiveness over `promote s . stratify _ . stratumPattern`.

`crossfold` is required by contract to produce distinct `Symbol` identities for distinct selected sub-Patterns. The origin map is a `Map Symbol OriginRef`, so two crossfolded elements with the same identity would silently collapse: the later insertion would overwrite the earlier, leaving one of the originally-selected sub-Patterns unrefinable. Callers who can ensure injectivity (e.g., by minting from a counter, or by deriving deterministically from properties of the source sub-Pattern) get correct behavior. Callers who cannot get a degraded `Stratum` rather than an error — consistent with the substrate posture of providing primitives and letting policies live above. The spreadsheet analog: two distinct formulas live in distinct cell addresses by definition; you don't get to put `=A1+B1` and `=C7*D7` in the same cell. Future revisions may integrate `Pattern.Reconcile`'s `ReconciliationPolicy` to make collision behavior configurable; that integration is deferred (see Open Questions §6) because there is no obvious-right answer to "what does it mean to merge two `OriginRef` values," and forcing one now would constrain consumers who actually have such an answer.

Notably, promotion does **not** lift relationships, paths, or other structural composites from the lower Pattern into the higher one. Promotion produces crossfolded elements only. Whatever structure the higher stratum acquires — relationships between crossfolded elements, paths through them, annotations on them — is composed at the higher stratum by ordinary Pattern operations, possibly informed by inspecting `stratumOrigin` to see which higher-stratum atoms came from related lower sub-Patterns. This separation keeps `crossfold` focused on a single concern: seeding the higher stratum's atomic elements, one cell at a time.

#### Integration with graph-interpretation machinery

`Stratum` is a navigation-level wrapper over `Pattern Subject`. The library's graph-interpretation machinery — `Pattern.Graph.GraphLens`, `Pattern.PatternGraph`, `Pattern.Graph.GraphQuery`, `Pattern.Graph.Algorithms` — operates on `Pattern Subject` values directly. The integration point is `stratumPattern`: to build a `GraphQuery` over a higher stratum, call `stratumPattern` to get the underlying Pattern, then construct a `GraphLens` or a `PatternGraph` from it as usual; the resulting `GraphQuery` answers questions about the higher stratum's structure. No Stratum-specific graph-query layer is provided.

This design has a consequence worth naming: when graph operations on `stratumPattern` produce a new `Pattern Subject` — a filtered view, a derived subgraph, a transformed Pattern — the origin map of the original `Stratum` is not automatically propagated to the result. The derived Pattern is a Pattern, not a Stratum. Callers who consider the derived Pattern to be "the same stratum, narrowed" can wrap it as a new `Stratum` carrying the original origin map (filtered to the atoms that survived, if appropriate); callers who consider it a new artifact can `stratify` it afresh with an empty origin map. The library takes no position; this is the same "layering is conventional" point applied to graph-derived Patterns.

#### Small-scale and large-scale connections

Two different kinds of connection arise in this framework, at different scales, and the design names both with the same vocabulary.

**Small-scale connections** are *intra-stratum*: they live inside a single stratum and tie its crossfolded elements to the lower-stratum sub-Patterns they were crossfolded from. These are the `stratumOrigin` map's job. They answer "what does this crossfolded element stand for?" and `refine` follows them.

**Large-scale connections** are *inter-stratum*: they relate one stratum to another at the same logical layer. The mechanical-Pattern stratum *informs* the maintenance-Pattern stratum; the supplier-Pattern stratum *supplies-parts-to* the mechanical-Pattern stratum; the procedure-Pattern stratum *operates-on* the electrical-Pattern stratum. These answer "how do strata relate to one another?"

The pleasing observation is that large-scale connections require no new machinery. Because every `Stratum` has a `stratumSubject`, *a Pattern whose elements are stratum subjects is the natural representation of inter-stratum topology*. The mechanical and electrical strata aren't just two values floating in someone's code — they're nodes in a "domains-of-the-airplane" graph, expressed as a `Pattern Subject` whose root subject names the meta-stratum ("airplane domain") and whose elements include atomic patterns whose subjects are `mechanical-graph`, `electrical-graph`, `maintenance-graph`, and so on, plus relationship-shaped patterns expressing how they relate. The same `Pattern.Graph` machinery — `GraphLens`, `GraphQuery`, `Pattern.Graph.Algorithms` — operates on this inter-stratum Pattern exactly as on any other. The same `Pattern.Aspect` combinators filter it. The same `crossfold` can lift it again, producing a meta-meta-stratum whose crossfolded elements are the meta-strata. There is no ceiling.

This is the framework's recursive self-similarity made concrete: Pattern/Subject/Stratum/Aspect work uniformly whether you are modeling people inside a neighborhood, neighborhoods inside a city, or strata inside a graph-of-strata. The shape of `Stratum` (Subject-plus-content, mirroring Pattern Subject) is what makes this work — without `stratumSubject`, strata would not be expressible *as* elements of a higher Pattern, and the inter-stratum graph would need a parallel type, or external bookkeeping, or some other mechanism that the framework itself is trying to dissolve. With `stratumSubject`, the framework's own primitives become first-class values in the framework, and the question "how do I represent a network of strata" has the satisfying answer: the way you represent everything else. The spreadsheet analog is a workbook whose cells reference cells in other sheets, and whose summary sheet aggregates across all of them; everything stays in the same idiom at every layer of indirection.

A consequence of this approach: resolving a stratum subject (in some inter-stratum Pattern) to its actual `Stratum` value requires an external lookup — typically a `Map Symbol Stratum` held by the consumer, or some equivalent registry. The substrate does not provide this. This is consistent with how the framework treats every other cross-reference: Pattern containers hold *values*, not handles, and resolution is a consumer concern. See Open Questions §7.

### Down-references and structural opacity

Crossfolded elements are *down-references*: their `Subject` identifies a higher-stratum entity, and their position in `stratumOrigin` records what lower-stratum sub-Pattern they refer to. The down-reference is non-structural in the traversal sense — it lives in the `Stratum`'s origin map, not in the crossfolded element's `elements`.

This is the load-bearing property of the design: **crossfolded elements are leaves in the Pattern's element-structure, so ordinary traversal stops at them**. `Foldable`, `Functor`, `Traversable`, `Comonad`, and `paraWithScope` all walk a Pattern by descending into its `elements`; a crossfolded element has no elements; descent stops. The structural opacity is achieved not by an active guard but by structural absence — there is no lower-stratum content reachable through the higher Pattern's traversal interface. The secondary metaphor from the glossary applies here precisely: a browser walking a host page's DOM stops at an `<iframe>` element; what's inside is reached through a separate channel, mediated by the surrounding context. Crossfolded elements behave the same way.

Refinement is therefore a deliberate, opt-in operation:

```haskell
refine :: Stratum -> Pattern Subject -> Maybe (Pattern Subject)
-- ^ Given a crossfolded element and the Stratum it belongs to, retrieve
--   the lower-stratum sub-Pattern it was crossfolded from, if available.
```

When `originStratum` is `Just`, the lower Pattern is held inline and refinement is a map lookup. When `originStratum` is `Nothing`, refinement returns the bare `originPattern` and the caller supplies whatever scope is appropriate to use it in. The spreadsheet analog: clicking a cell to open the formula bar (inline case) versus looking up a referenced cell that lives in an external workbook (external case).

A consequence worth naming explicitly: **the stratum boundary is structural, not type-enforced**. Nothing prevents code from reading the origin map, calling `refine`, and mixing higher-stratum and lower-stratum Patterns in the same operation. The decoupling is conventional. Code that respects the layering gets clean layering; code that doesn't can break through, and the type system will not catch it. This is consistent with the library's substrate posture: primitives are provided, policies are built on top.

A specific case worth naming: comonadic computation. `Pattern Subject` has a `Comonad` instance whose `extend` and `duplicate` operate over the Pattern's `elements` structure. Applying them to `stratumPattern` produces intermediate `Pattern Subject` values that are *not* Strata and do not carry origin information; the origin map is not propagated through comonadic traversal. This is fine and follows from the framework's posture — comonadic computation is a Pattern-level operation, and Stratum is one layer above. Callers who want down-references preserved through comonadic computation must re-stratify the result with whatever origin information they want to retain (typically the original Stratum's origin map, possibly filtered to atoms the computation preserved). Callers who do not care can ignore the issue; nothing breaks.

**Export policy.** This RFC commits to exporting `Stratum(..)` and `OriginRef(..)` fully — constructors and all accessors. Anyone with a `Stratum` value can read `stratumOrigin`, call `refine`, and reach into `OriginRef` directly. Stricter discipline (an opaque `Stratified` newtype that hides the origin map and routes all access through `refine`) is a higher-layer concern, not a substrate-layer one. A future module — `Pattern.Stratified` or similar — can provide that discipline by re-exporting a subset of this API behind an abstract type, but `pattern-hs`'s navigation framework itself exposes the primitives.

### `Pattern.Aspect`

Most aspect-shifts do not produce new Patterns; they produce new scopes. The module is primarily a library of scope constructors and combinators.

The framework commits to `ScopeDict Symbol Subject` as the uniform scope representation across all combinators. This is the first-class value form of a scope provider already defined in `Pattern.Core`, parameterized at the identity carrier `Symbol` (= `Id Subject`) and value type `Subject`. Using `ScopeDict` rather than introducing an existential wrapper over `ScopeQuery q Subject` is deliberate: the aspect combinators are *closed operations on scopes* — they consume scopes and produce scopes — and the concrete record representation is sufficient for their semantics. Preserving polymorphism over the underlying `q` through an existential would add machinery without payoff at the framework layer; any consumer holding a `ScopeQuery q Subject` can reify it via `toScopeDict` before combining.

For readability, `Pattern.Aspect` introduces two type aliases over the existing `Subject.Core` types — purely documentation, no new types:

```haskell
type Label      = String                -- the element type of Subject.labels
type Properties = PropertyRecord        -- = Map String Value, from Subject.Core
```

`Set Label` and `Properties -> Bool` then read self-evidently in combinator signatures while remaining straightforwardly the existing `Set String` and `Map String Value -> Bool`. The combinators:

```haskell
-- Base constructors
labelFiltered    :: Set Label -> ScopeDict Symbol Subject -> ScopeDict Symbol Subject
propertyFiltered :: (Properties -> Bool) -> ScopeDict Symbol Subject -> ScopeDict Symbol Subject
kindRestricted   :: PatternKind Subject -> ScopeDict Symbol Subject -> ScopeDict Symbol Subject

-- Algebraic combinators
intersect :: ScopeDict Symbol Subject -> ScopeDict Symbol Subject -> ScopeDict Symbol Subject
union     :: ScopeDict Symbol Subject -> ScopeDict Symbol Subject -> ScopeDict Symbol Subject
restrict  :: (Pattern Subject -> Bool) -> ScopeDict Symbol Subject -> ScopeDict Symbol Subject
```

The combinators are first-class because the day-to-day complexity-management value lives in being able to build composite aspects at query time without materializing intermediate Patterns.

For aspect-shifts that genuinely transform the Pattern (label-renaming projections for external interfaces, for instance), the existing `RepresentationMap` remains the right tool. `Pattern.Aspect` provides helpers for constructing common cases:

```haskell
labelRewrite      :: Map Label Label                           -> RepresentationMap Subject
propertyBijection :: (Properties -> Properties)
                  -> (Properties -> Properties)
                  -> RepresentationMap Subject
```

Aspects are applied to both `Pattern Subject` and `Stratum` uniformly — the latter via `stratumPattern`. Promoting through an aspect-restricted scope is the natural way to get a promotion that only sees the aspect-relevant sub-Patterns of the lower stratum.

### Composition

Stratum-shifts (`Pattern Subject -> Stratum`, or `Stratum -> Stratum`) and aspect-shifts (scope-to-scope functions, with optional Pattern transforms via `RepresentationMap`) compose via ordinary function composition in their respective domains. There is no separate "pivot category" type at the user-facing API. Code that needs to be generic over arbitrary navigation moves can define its own abstraction in an internal module without imposing it on the common case.

### Relationship to existing modules

Nothing in this RFC requires changes to `Pattern.Core`, `Pattern.PatternGraph`, or `Pattern.Reconcile`. `Pattern.RepresentationMap` is reused unchanged for the invertible-aspect case. The proposed modules and the `Stratum` type are additive.

Vocabulary cleanup is worth noting: the design relies on the distinction between a *Pattern* (a decorated-sequence container value) and its *graph interpretation* (what `PatternKind` and `ScopeQuery` make of it). Throughout the RFC, "Pattern" refers to the container; "Stratum" refers to a Pattern Subject wrapped with identity and provenance. The phrase "graph world" used in earlier drafts has been retired: a Stratum can be interpreted as a graph, but need not be, and the framework's machinery operates at the Pattern level regardless. "Sub-Pattern" is preferred over "subgraph" when speaking at the container level.

## Open Questions

1. **Identity scheme for crossfolded elements.** `crossfold` produces a fresh `Subject` for each crossfolded element. Should the library specify any conventions for the produced identity (e.g., deriving deterministically from the source sub-Pattern's identity so that two independent promotions of the same source yield the same higher-stratum identity), or leave this entirely to the caller? Deterministic crossfolding enables incremental computation and stratum-stable identity, much as a spreadsheet's structured references give stable addresses to repeatedly-computed cells; non-deterministic crossfolding is simpler and avoids the question of what "same source" means when scopes differ.

2. **Scope combinator laws.** `intersect`, `union`, and `restrict` should satisfy lattice-like laws (commutativity, associativity, absorption) to be predictable across heterogeneous scope backings. Are there cases where the natural implementation cannot be made commutative without canonicalization? If so, the typeclass needs a normalization step or the combinators need restricted signatures.

3. **Cardinality statistics per aspect.** The query-plan optimization benefit assumes per-aspect cardinality is cheap to maintain. Should the substrate offer a hook for statistics collection in `ScopeQuery` traversals, or is statistics-tracking entirely a consumer concern? The substrate-not-framework principle argues for the latter, but a minimal hook would be cheap and broadly useful.

4. **Formal setting in documentation.** The design is informed by a categorical view (strata as 0-cells, stratum-shifts and aspect-shifts as orthogonal 1-cells in a double-category-like structure) but the user-facing API does not expose this. Should documentation include a "formal model" section that names the categorical structure, or keep it implementation-internal? The benefit is that future contributors inherit the vocabulary; the cost is that the documentation appears to demand category theory of users who do not need it.

5. **Inline vs. external lower-stratum holding.** `OriginRef.originStratum :: Maybe Stratum` lets callers choose whether to retain the lower stratum inline (full refinement is local) or symbolically (caller supplies the scope at refinement time, enabling large lower strata to be persisted separately). Are there cases where a third mode — a lazy thunk or a deferred handle — would be useful enough to warrant complicating the type? Probably not at the substrate layer, but worth confirming.

6. **Reconciliation integration for crossfold collisions.** The current proposal requires `crossfold` injectivity by contract; violations silently lose origin entries. A more disciplined approach would integrate `Pattern.Reconcile`'s `ReconciliationPolicy` so that callers can configure collision behavior. The blocker is semantic: there is no obvious-right merge for two `OriginRef` values (do you union the source Patterns? Pick the first? Pick the second? Keep both as alternatives?), and the right answer is likely consumer-specific. Concrete consumer use cases would help decide whether this should be a fixed policy, a configurable strategy, or remain a caller responsibility. Until then the contract approach is the conservative choice.

7. **Stratum-subject resolution conventions.** Inter-stratum Patterns (Patterns whose elements carry stratum subjects) require an external `Map Symbol Stratum` — or equivalent — to follow a stratum-subject back to the actual `Stratum` value it names. Should the library provide a thin convention for this (a `StratumRegistry` newtype around `Map Symbol Stratum`, with combinators)? Or leave it entirely to consumers, given the substrate posture? Leaning toward the latter, but a minimal convention might reduce the friction of getting started without committing the framework to a particular bookkeeping policy. The decision affects how prominently inter-stratum modeling appears in tutorial documentation.

## Alternatives

**Type-level sorting via GADTs.** Indexing `Pattern` by structural sort (Node, Relationship, Path) at the type level would make some constraints type errors rather than runtime predicate checks. This was rejected because `Pattern Subject`'s uniform-value architecture is load-bearing — many of its instances depend on the type parameter being a plain value type. Sort indexing would either require a parallel `SortedPattern` type or a substantial rewrite. The runtime cost of `PatternKind` predicates is acceptable.

**A single `Pivot` type unifying stratum-shifts and aspect-shifts.** A unified type with optional inverse, optional Pattern-transform, and optional scope-transform was considered. It composes uniformly but makes the common cases carry irrelevant fields. The current proposal keeps the two kinds of move separate at the user-facing API and lets internal code that needs polymorphism define its own abstraction.

**Aggregation rather than promotion.** An earlier draft framed the upward move as "aggregation," with the upward operation producing the entire higher stratum (crossfolded elements *and* lifted relationships) in a single pass via a bundle of specs. This was rejected because it conflated two concerns operating at different timescales: producing the higher stratum's atomic elements (which is a single bounded operation) and composing structure between those elements (which is the modeling work that happens at the higher stratum). Separating them — promotion produces crossfolded elements only, with structure composed by ordinary Pattern operations afterward — gives a cleaner API and an honest inverse (promotion's down-references are the precise dual of refinement's reads).

**Down-references as Subject properties.** Storing the link back to the lower stratum as a reserved property on the crossfolded Subject is mechanically simpler but pollutes user-meaningful property records with library bookkeeping, leaks provenance into serialization, and risks collision with user-chosen property keys. The `Stratum` type localizes provenance information outside the Pattern's content while keeping it structurally bound to the Pattern.

**Predicate-based scopes as the only scope mechanism.** Pure predicate scopes are more expressive (they support virtual / lazy scopes over very large Patterns) but require predicate evaluation at every element, which is expensive. The library's existing `ScopeQuery` is abstract enough to accommodate both predicate-backed and containment-backed instances, and the proposed aspect combinators work against either. The RFC takes no position; it ensures both remain possible.

**A standalone graph-database-style entity registry.** A registry holding canonical entities by identity, with all Patterns referencing them by ID, would centralize cross-Pattern bookkeeping. It was rejected because it pushes the library toward being a database rather than a substrate. Cross-Pattern references are well-handled by Haskell's value semantics — the same `Pattern Subject` can be referenced from any number of containing structures simultaneously — and consumers who want a registry can build one on top without the library imposing it.

**Naming the promotion function `mint`, `embed`, `name`, or `project` rather than `crossfold`.** The promotion function went through several names during the design. The early draft called it `promotionMint` (later, `mint`), which accurately conveys fresh-identity production but is neutral on the structural relationship between the produced atom and its source. `name` undersells what the function does (it produces a whole Subject, not just an identity). `project` conveys lossiness correctly but stretches the spatial metaphor across the stratum boundary. `embed` — webpage-style embedding, where the host page holds a placeholder and the content lives at its source — was a strong contender and matched the recoverability story well, but it papers over the summarization aspect of the function (a webpage embed *references*; promotion *summarizes*). `crossfold` was chosen because it composes two ideas the audience already has (fold from functional programming, cross as a directional preposition) and lands on the spreadsheet model — a cell whose displayed value is a summary of its inputs, with the formula preserved alongside. The spreadsheet framing matches every joint of the design: cell ↔ crossfolded element, formula ↔ the crossfold function, formula bar ↔ refinement, range references ↔ inter-stratum Pattern relationships. The webpage-embed metaphor is retained as a secondary illustration for the structural-opacity property specifically (traversal stops at the cell boundary as it stops at the iframe boundary), but the primary frame is fold-across-stratum-with-preserved-formula. The choice also aligns pattern-hs's primary structural operation with the larger project's tagline — "a spreadsheet for structural composition" — so that users encountering either layer touch the same mental model.
