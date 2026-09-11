---
adrs: []
---

# RFC-0001: Frame and Span — Managed Registry Containers for Pattern Subject

**Status:** draft
**Date:** 2026-05-29
**Updated:** 2026-09-11
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Related modules:** `Pattern.Core`, `Pattern.Reconcile`
**Grounds:** RFC-011 Open Question 3 (scoped identity namespaces) — Frame identity supplies the namespace; completed by RFC-012's anonymous/weak-entity identity model
**Supersedes:** the wrapper/view model of this RFC's earlier draft and [ADR-001](../adr/ADR-001-frame-span-implementation.md) (see [ADRs](#adrs))

## Summary

**Frame**, **Span**, and **FrameSpace** are managed containers for higher-order structure
built from `Pattern Subject`, replacing this RFC's earlier wrapper/view model. A Frame is
not a wrapper around a pre-existing Pattern; it is a
construction context, echoing Pattern's own shape one level up — an identifying Subject
admitting definitions into one referentially closed registry, whose entries are
Subject-plus-ordered-elements rows where the elements are local references rather than
embedded values. A Span relates members of two Frames through a Bundle of pair
relationships, without either Frame carrying a reference to the other — the same
externality property graphs rely on (a node does not know its own relationships), applied
to modules. A Span's validity is relative to a FrameSpace, the aggregate owner of Frame and
Span integrity: FrameSpace is where cross-Frame validation actually happens over time, and
Frame and Span each forgo needing it only where their own invariants are locally decidable.
Query and navigation over a Frame's registry — the "moves an analyst makes" that motivated
this RFC's first draft — remain a real requirement but are not designed here; they are an
explicit open question (Open Question 6), gating implementation planning rather than
implementation itself.

## Glossary

### Structures

**Pattern** — The substrate container: a value paired with an ordered sequence of element
Patterns. Polymorphic in the value type, semantics-free, with no identity or integrity
rules of its own.

**Subject** — The identity-bearing value used throughout this framework: an identity
symbol, a set of labels, and a property record. An anonymous Subject carries no identity
symbol.

**Pattern Subject** — A Pattern whose value type is Subject.

**LocalAddress** — The address of a Frame member: named (for a member with a non-anonymous
Subject identity) or positional (a flat, Frame-scoped ordinal assigned at admission, for an
anonymous member). See §Identity and addresses for the type. Neither form carries a
reference to any containing member — containment is recorded by whichever `PatternRow`
lists the address among its own elements, not by the address itself. This permits an
anonymous member to be shared by more than one container, exactly like a named one.

**PatternRow** — A Frame registry entry, or a Bundle pair: a Subject paired with an ordered
list of `LocalAddress` references — the same Subject-plus-ordered-elements shape `Pattern`
itself has, but with elements as references rather than embedded values.

**Frame** — A container identified by a Subject, owning a registry (`FrameRegistry`) of
`PatternRow`s keyed by `LocalAddress`, and maintaining the invariant that every local
reference in that registry resolves within it (referential closure). A Frame's identity —
its identifying Subject's non-anonymous identity — is stable for its lifetime and unique
within any collection that holds it. A Frame's closure invariant is decidable from the
Frame value alone, so a Frame is independently useful and independently valid before it is
registered anywhere.

**Span** — A container relating two Frame identities and owning the cross-Frame
correspondences between their members, held in a Bundle. Its own identifying Subject's
non-anonymous identity is its `SpanIdentity`, the same relationship a Frame's identifying
Subject has to `FrameIdentity`. A Span stores Frame *identities*, not Frame values, so its
pair-endpoint invariant is not decidable from the Span value alone — it requires resolving
those identities to Frame values, which only a FrameSpace supplies. A `Span` value is the
FrameSpace-confirmed form; a `SpanDraft` is the raw, freely constructible, unvalidated input
to admitting or updating one.

**ClosedSpan** — A Span-shaped value built directly from two Frame values and a Bundle,
validated against those two Frames alone, with no FrameSpace involved. Independently valid
the same way a Frame is, and the export/exchange form for a Span together with its endpoint
Frames.

**FrameSpace** — An immutable collection of Frames (keyed by Frame identity) and Spans
(keyed by Span identity) that provides atomic replacement operations checking a changed
Frame against every incident Span's Bundle before returning the next FrameSpace. The
aggregate owner of cross-container integrity: a Frame or Span may exist and be independently
valid outside a FrameSpace, but only gains cross-Frame integrity enforcement once
registered in one.

**Bundle** — A Span-owned, ordered collection of pair relationship Patterns (`Pair`s), not
an independently serializable entity and not a Pattern of pairs. Canonically an ordered
list, matching Pattern's own convention, because — unlike `FrameRegistry` — no cycle among
pairs forces a keyed form to be canonical.

**Pair** — One Bundle entry: a `PatternRow` conventionally constrained to exactly two
elements, one endpoint reference into each of the Span's two Frames. A pair's own identity,
the `PairLocalIdentity`, is scoped by its containing Span as `PairAddress = (SpanIdentity,
PairLocalIdentity)`.

**PairDispositionPolicy** — A policy, declared once per Span, mapping a threatened pair to
`Veto` (block the Frame edit) or `AutoDrop` (drop the pair, let the edit proceed). Governs
what FrameSpace does when a Frame edit would dangle an incident pair's endpoint.

### Principles

**1. The substrate is untouched; Frame, Span, and FrameSpace are framework types over it.**
`Pattern v` gains no fields and no new constraints. These containers are defined entirely
in the framework layer, in terms of the substrate.

**2. A member's address never names its container.** Containment is recorded by the
containing `PatternRow`, never by the contained member's own address. This is the same
externality principle applied one level below the Frame/Span boundary: just as a Frame does
not carry back-references to its Spans, a member does not carry a back-reference to
whatever contains it — which is exactly what lets one anonymous member be shared by several
containers.

**3. A Frame does not carry back-references to its Spans.** The only way to relate members
across Frames is through a Span, whose Bundle holds the cross-Frame correspondences
externally. A Frame never knows it is spanned.

**4. Reconciliation is a boundary operation, and namespace-aware.** Frame-level
reconciliation applies only between versions of the same Frame identity; it delegates
content-conflict resolution to `Pattern.Reconcile`. Equal named local identities in
different Frames are not duplicate identities — combining differently scoped Frames
requires an explicit import into a chosen destination namespace.

**5. Identity is local to a Frame; a Span's validity is relative to a FrameSpace.** A
`LocalAddress` is meaningful inside one Frame. Outside it, a member is named by
`FrameIdentity x LocalAddress`. A Span carries no standalone validity the way a Frame does:
it is a claim about two Frames it does not possess, confirmed only relative to a
FrameSpace (or, without one, directly against two Frame values as a `ClosedSpan`).

## Motivation

Higher-order graph construction needs a stable way to treat selected Pattern structures as
addressable units without changing Pattern's value-agnostic, semantics-free substrate, and
without forcing every operation to understand a special node role.

`Pattern v` already supports arbitrary recursive composition, useful precisely because
Pattern enforces none of the interpretations placed on it. The difficulty appears when a
higher-order structure must itself participate as an addressable unit in further graph
construction: promoting selected Patterns into a node role inside the existing substrate
would make membership, identity, traversal, and referential-integrity rules depend on a
special context. A wrapper/view over an already-formed Pattern — this RFC's original
approach — retains that ambiguity, attempting to add module, identity, and integrity
semantics after the fact.

A Frame instead establishes a new construction context, echoing Pattern's own shape one
level up: an identifying Subject admitting definitions into one referentially closed
registry, whose entries are again a Subject with ordered elements — now references, not
embedded values. A Span then establishes correspondence between members of two Frames
without placing cross-Frame references inside either Frame — the same externality principle
property graphs rely on (a node does not know its own relationships), applied to modules
instead of nodes; the same principle later governs why a member's own address never names
its container either.

This boundary also gives the in-memory model the ownership shape RFC-011 persistence
already requires: `frame_row(frame_id, id, labels, properties, elements)` uses
`(frame_id, id)` as its member key, and `bundle_pair` enforces both endpoint Frame
membership and endpoint existence through foreign keys. The in-memory model has equivalent
ownership and integrity rules before it is mapped to those rows, rather than requiring a
lossy translation between a recursively nested value and a row-based store.

Pattern, Frame, ClosedSpan, Span, and FrameSpace form one spectrum, not five unrelated
concepts. Pattern is deliberately semantics-free: a value with ordered elements, no
identity or integrity rules at all. FrameSpace is the opposite pole: where identity,
cross-container integrity, and pair disposition policy actually get enforced over time.
Frame and ClosedSpan sit in between — each carries a real, checkable invariant but needs no
ongoing operational context to hold it, unlike Span, which only ever means anything
relative to a FrameSpace.

The motivating use cases carry over from this RFC's original draft, restated against the
managed model:

  - **Clash-free multi-document ingestion.** Two `.gram` files may both contain `alice` or
    an anonymous entity. Each document is admitted into a Frame with a distinct, explicitly
    assigned Frame identity. Local identities remain unchanged, and `(file-a, alice)` and
    `(file-b, alice)` remain distinct registry members.

  - **Disentangling graphs-of-graphs.** A maintained airplane is simultaneously a
    mechanical Frame, an electrical Frame, a maintenance Frame, a supplier Frame, a
    procedure Frame. Correspondences between them are Spans, validated by a shared
    FrameSpace. Queries that cross these Frames become within-Frame lookup (Open Question
    6) plus cross-Frame traversal via Span Bundles, with no special-purpose
    graph-of-graphs machinery.

  - **Aspect-aware partitioning and query-plan scope reduction.** A Span's Bundle names
    exactly the cross-Frame edges that exist; a query or partitioner expressed in terms of
    Frame membership and Span participation carries its own scope information, rather than
    requiring irrelevance to be proven from arbitrary predicates.

The initial Haskell API uses persistent updates: operations return a new Frame, Span, or
FrameSpace instead of mutating a process-local cell. This is database-like in its integrity
rules and data-frame-like in its tabular registry/query model, while remaining a portable
pure reference implementation for Rust and TypeScript.

## Design

### Identity and addresses

Named and anonymous Patterns have different identity properties. A named Pattern carries a
non-anonymous `Subject.identity`; that Symbol identifies a member within one Frame. An
anonymous Pattern carries no Subject identity. Both remain valid Frame members: admission
assigns an anonymous Pattern a flat, Frame-scoped ordinal — the same status a named identity
has, just unnamed — rather than an address relative to whatever Pattern first contained it.
Containment is recorded the other way around: whichever `PatternRow` lists that ordinal
among its elements is one of its containers, and nothing prevents more than one from doing
so. Adding the same anonymous Pattern shape to a Frame multiple times creates distinct
registry entries at distinct ordinals; entries never receive invented Subject identities.

Every Frame member has a local address, while only named members have a local identity:

```text
LocalAddress  = Named LocalIdentity | Positional ElementOrdinal
ScopedAddress = FrameIdentity x LocalAddress
```

The Frame registry is keyed by `LocalAddress`, not by `Subject.identity`. This permits
separately ingested Frames to contain the same named symbols without collision while
preserving anonymous structural vocabulary.

Frame identity must be stable across versions of the same logical Frame and unique among
Frames held by one collection. A Frame's identifying Subject supplies its Frame identity;
the identity cannot change during the Frame's lifetime, although its labels and properties
remain reconcilable metadata. An import boundary supplies the Frame Subject, optionally
derived from a stable source key; Frame and FrameSpace do not infer file paths, generate
hidden global identities, or rewrite source member identities.

This supplies the namespace half of RFC-011 Open Question 3 (scoped identity namespaces):
`FrameIdentity` is the namespace, and `ScopedAddress = FrameIdentity x LocalAddress` is the
qualified key RFC-011's `frame_row(frame_id, id, ...)` persists. It does not supply the
whole of RFC-011's answer. RFC-012 grounds the same Frame-as-namespace primitive but goes
further — weak-entity identity for anonymous members, promotion, and a declared or
store-assigned `namespace` header for a Frame lacking a stable source key — and is the RFC
that actually closes RFC-011 Open Question 3 for anonymous content. This RFC establishes
the Frame identity a scoped address is built from; RFC-012 specifies how that identity is
assigned and how anonymous members acquire one when promoted.

### Defining and reference occurrences

Frame admission preserves the distinction between a defining Pattern occurrence and a local
reference occurrence. A defining occurrence introduces or reconciles one registry entry; a
reference occurrence names a prospective registry entry and resolves to a fuller definition
with the same local identity when one exists, or is promoted to a content-free defining
entry otherwise. An admission batch collects all occurrences before resolving them,
permitting forward references and indirect cycles.

An anonymous defining occurrence retains its anonymous Subject and receives a positional
`LocalAddress`, assigned at admission, not derived from any containing occurrence. It
cannot be targeted by a named reference, a Span pair endpoint, or a stable external key
until a later, separately designed promotion operation assigns it a `LocalIdentity` — a
concern shared with RFC-012, not decided here.

A defining occurrence must not directly reference its own local address; direct
self-reference is rejected at admission. References among distinct registry entries may
form indirect cycles, which are valid registry topology.

Content admitted without a syntactic definition/reference tag (a raw `Pattern Subject`)
recovers the distinction from content instead: identity alone is a reference candidate; any
labels, properties, or elements make it a definition. Two definitions sharing an identity
are a content conflict, deferred to the selected reconciliation policy rather than treated
as an admission error — this raw-import path is a compatibility fallback, more permissive
than the canonical admission format's stricter default. A full Pattern in one Frame and an
atomic Pattern in another Frame are not a Frame-internal reference; a Span pair records
their relationship.

### Frame registry and closure

A new Frame starts with its identifying Subject and no registry entries. Admission
recursively turns each defining occurrence into one independently addressable Frame entry.
Duplicate named local identities reconcile under the selected policy; two fuller
definitions whose content that policy cannot reconcile are errors. Frame admission has no
nested identity namespaces: every defining occurrence at every input depth becomes an entry
in the same registry, while ordered local-address links express its containment
relationships.

Each registry entry is a `PatternRow` (see Glossary); `FrameRegistry`, keyed by
`LocalAddress`, is the canonical membership domain and its address links are the canonical
containment structure. This is canonical, not merely an optimization over some other
ordered shape, precisely because a cycle has no single canonical ordered presentation: a
fully expanded presentation that recursively embeds every target value cannot be finite
when the registry contains a cycle, and duplicates shared members when it does not.

A Frame can still produce many semantically equivalent `Pattern Subject` presentations —
each defining every registry entry once and using atomic local references for further
occurrences, Gram's finite recursive-graph form. Such a presentation and the Frame registry
are logically equivalent when they resolve to the same member Subjects and ordered local
references, though not necessarily the same recursive value shape.

Frame updates are pure and invariant-preserving: admitting one or a batch of top-level
occurrences (resolving forward references across the batch), or removing a registry entry.
Removal succeeds only when no other entry refers to its address — named or positional
alike, since both are now stable, Frame-scoped addresses. A caller must first detach a
containment link before removing an entry nothing else references.

A Frame update preserves local registry closure and produces a candidate replacement Frame.
FrameSpace's Frame-update operation is the cross-Frame commit boundary (see FrameSpace
integrity contract below): a Frame may be locally valid while being ineligible to replace
its registered version in a FrameSpace.

### Span, Bundle, and ownership

One Frame may exist with no Span and may participate in many Spans. A Span must preserve
pair validity as Frame membership changes, but a Frame must not carry back-references to
its Spans — the two therefore need a higher-level integrity owner whenever their lifecycles
are coordinated. FrameSpace is that owner: an immutable collection of Frames keyed by Frame
identity and Spans keyed by Span identity, providing atomic replacement operations that
check a changed Frame against all incident Span Bundles before returning the next
FrameSpace.

A Span is incident to a Frame when that Frame's identity equals the Span's left or right
Frame identity. A Span canonically stores its left and right Frame identities, not Frame
snapshots, and is admitted only when both identities resolve in its FrameSpace; Bundle pair
validation then uses the current endpoint Frames. One Frame can therefore participate in
many Spans without copied state or divergent snapshots.

A Frame's closure invariant is a closed predicate, decidable from the Frame value alone — a
Frame stands alone before it is registered anywhere. A Span's pair-endpoint invariant is
open: undecidable without a `FrameIdentity -> Frame` lookup, which only a FrameSpace
supplies. A Span is better understood as a claim about two Frames it does not possess,
confirmed only relative to a FrameSpace, than as a value with standalone validity the way a
Frame has one. This is also a cardinality argument independent of decidability: Frame-to-
Span is many-to-many, while ownership can only express one-to-many, so a Span owning its
Frames by value would either forbid a Frame from joining a second Span, or embed a
per-Span copy with no shared coordinator to keep synchronized when the source Frame updates.

Because of this asymmetry, only the FrameSpace-confirmed form is named `Span`; holding a
`Span` value is itself evidence that its pairs already resolved. A `SpanDraft` — an
identity, left and right Frame identity, and an unvalidated Bundle of candidate pairs — is
the distinct, freely constructible input to admitting or updating a Span.

### ClosedSpan

A Span's pair-endpoint invariant only appears open because `Span` stores Frame identities
rather than Frame values. Given the two actual Frame values a Span relates, pair-endpoint
resolution is a purely local check against their own registries, needing nothing from a
broader FrameSpace. `ClosedSpan` makes that explicit: a Span-shaped value built directly
from two Frame values and a Bundle of candidate pairs, validated against those two Frames
alone, and independently valid outside any FrameSpace the same way a Frame already is.

A confirmed `Span` can be closed against a FrameSpace as a convenience — resolve its two
Frame identities, then validate as a `ClosedSpan` — but this can fail even though the `Span`
was already confirmed: a `Span` proves validity only against the FrameSpace generation that
confirmed it, and a later generation may have dropped a pair (`AutoDrop`) or rebound one, so
closing revalidates against whichever FrameSpace it is actually given.

`Span` and `ClosedSpan` serve different, non-overlapping roles. `Span` answers "what does
this relationship mean right now," re-resolved against whatever FrameSpace it is given each
time, so it can never go stale because it never freezes anything. `ClosedSpan` answers "what
did this relationship mean at the moment it was closed" — a permanently valid, self-
contained record, useful for serialization or export, unable by construction to observe a
later Frame update.

### Correlating Frames by shared local identity

Two independently ingested Frames may reuse the same named local identity for what is, in
the source domain, the same real-world thing — coincidence, not correlation, under the
Frame model's own namespacing rule. A convenience constructor may turn that coincidence into
an explicit correlation without changing the rule: for each named local identity occurring
in both Frames, it proposes one candidate pair relating them. It produces candidates only —
a caller must assign each one its own `PairLocalIdentity` before a Span or ClosedSpan will
accept it, since matching symbols are a construction hint, never an automatic merge. This is
one convenience constructor among several ways to populate a Bundle, not the general case;
manual pairing remains necessary whenever a correspondence holds between differently named
members.

### FrameSpace integrity contract

FrameSpace's atomic integrity responsibilities, stated behaviorally. Concrete function
signatures, error constructors, and index representation are ADR-level.

| Operation | Contract |
|---|---|
| Add a Frame | Requires a new Frame identity and a locally valid Frame. |
| Update a Frame | Applies a pure local edit, validates the replacement, finds every incident Span, and consults each affected pair's owning Span's `PairDispositionPolicy`. An edit removing a member addressed by a `Veto` pair fails, identifying the affected pair addresses; an edit affecting only `AutoDrop` pairs succeeds, and those pairs are dropped from their Bundle. Never returns a partially updated FrameSpace. |
| Remove a Frame | Fails unconditionally while any incident Span exists — `AutoDrop`-ping a pair cannot repair a Span whose endpoint Frame identity no longer resolves at all. |
| Add / update a Span | Takes a `SpanDraft`, not a `Span`; on success, stores a confirmed `Span` once its pairs resolve against the current FrameSpace. Requires both endpoint Frames and every Bundle pair endpoint to resolve in its designated Frame. Neither operation returns a `Span` directly — the caller retrieves it by lookup against the returned FrameSpace. |
| Rebind a pair | Explicitly replaces one pair's ordered endpoint references, after validating them in the Span's existing left and right Frames. |
| Remove a Span | Removes its relationship entries without changing its endpoint Frames. |
| Look up a Frame or Span | Returns the current value for an identity, or nothing. |

This boundary does not provide cascades, automatic pair rewriting, observers, transactions,
storage backends, or version histories. Those extensions may build on the same atomic pure
replacement model.

### Bundle pair Patterns

A Bundle is a Span-owned, ordered collection of `Pair`s (see Glossary). A pair's Subject
must have a non-anonymous identity, unique within its Span — a pair may be anonymous during
construction, but Span admission rejects it until the caller supplies a
`PairLocalIdentity`; silent identity generation would make a repeated import produce a new
relationship entry instead of a reconcilable revision of the existing one.

A pair's first element identifies a member of the Span's first Frame and its second element
identifies a member of its second Frame; both must resolve in their respective endpoint
Frame. Each endpoint is a named local reference — non-anonymous, with no labels,
properties, or nested elements of its own; a content-free Pattern in this position is
always a reference, never a member definition. The positional order is canonical, but Span
has no core direction field: ordered endpoints are structural positions, not a semantic
arrow. Whether a Span is directed, symmetric, functional, or under some other relationship
law is determined by its Subject's labels and properties, or a higher-level domain
validator — the core exposes counterpart traversal from either endpoint Frame.

The ordered endpoint pair identifies what a relationship entry relates; its `PairAddress`
identifies the relationship entry itself, so a Bundle may contain multiple pairs with the
same endpoints when they have different identities or provenance. An optional Bundle
uniqueness constraint limiting a Span to one pair per ordered endpoint pair is left open
(Open Question 2); endpoint uniqueness is not the default identity rule.

Pair reconciliation preserves exactly-two-endpoint cardinality: two versions with the same
`PairAddress` and the same ordered endpoints reconcile their root Subject's labels and
properties under the selected policy; two versions with the same `PairAddress` but
different endpoints fail with an endpoint conflict. Changing a pair's endpoints requires an
explicit rebind operation rather than generic element merging.

### Pair disposition policy

A pair's Subject may carry data relevant to how important that specific correlation is — a
`Derived` label, a confidence score, a provenance property — but that data has no defined
behavior on its own. `PairDispositionPolicy` (see Glossary) maps it to what FrameSpace does
when the pair's endpoint is threatened by a Frame edit; it is declared once per Span, not
per pair, because pairs sharing one Span typically share one coherent provenance and
confidence model.

`AlwaysVeto` — every pair protects its endpoints, an edit that would dangle one always fails
— is the default when a Span declares no policy, preserving the behavior exercised
throughout the Worked Exercise below. `AlwaysAutoDrop` is its opposite: no pair blocks a
Frame edit, and a pair left dangling by one is dropped. `CustomDisposition` covers graduated
policies between those extremes, mirroring how `Pattern.Reconcile` supplies named policies
alongside custom ones.

A Span carries its `PairDispositionPolicy`; `ClosedSpan` carries it too, inertly, so a
`ClosedSpan` re-admitted into a fresh FrameSpace does not silently revert to the default.
The policy governs a member-level edit inside a Frame, not the disappearance of an entire
endpoint Frame — removing a Frame still fails unconditionally while any incident Span
exists (see FrameSpace integrity contract); cascade-removing Spans when an endpoint Frame is
removed is a separate, undesigned capability.

Named policies (`AlwaysVeto`, `AlwaysAutoDrop`) are serializable; `CustomDisposition` wraps
an opaque function and is not. A `ClosedSpan` built with a named policy round-trips through
export and re-admission intact; one built with `CustomDisposition` is re-admission-only,
its policy resupplied by the caller, since no serializable identifier for an arbitrary
disposition function exists yet. A serializable policy-identifier scheme, if needed, is
ADR-level.

### Reconciliation

Frame-level reconciliation operates only between versions of the same Frame identity. It
registers incoming Patterns into the destination registry and delegates content conflicts,
reference completion, and recursive element merge choices to `Pattern.Reconcile`; Frame
code adds namespace checks, registry construction, and integrity errors, without
duplicating Subject merge policy. Frame reconciliation has two modes: `Replace` validates
the incoming Frame and replaces the existing registry of the same Frame identity — existing
members omitted from the incoming Frame do not survive, and may be removed only when no
local reference or incident Bundle pair addresses them. `Additive` admits incoming
definitions into the existing registry, adds missing members, reconciles matching named
identities under the selected policy, and retains existing members omitted from the
incoming batch. FrameSpace validates incident Span endpoints after either mode.

Frame identity and named member identities remain stable after admission; reconciliation
may change a member's labels, properties, and ordered local references, but never renames
that member or its Frame. Positional addresses are as stable as named ones once assigned;
anonymous members still cannot serve as Span pair endpoints or stable external keys — not
because their address is unstable, but because a pair endpoint needs an author-chosen,
domain-meaningful identity, which an anonymous ordinal is not.

Containment attachment is a distinct, third operation, not a reconciliation mode:
`Attach` admits definitions into the destination Frame namespace, then adds the selected
incoming root addresses to a target member's ordered element sequence, rejecting direct
self-reference, unresolved references, and identity conflicts. Content from a different
Frame must first be imported or rebased into the destination namespace; coincident local
identities never trigger cross-Frame merging. Import copies selected source roots and their
complete transitive local-reference closure into a destination Frame without changing the
source, preserving each named local identity when unoccupied and rejecting collisions by
default; an explicit import plan may map a colliding source identity onto an existing
destination identity, an explicit request to merge those members whose content conflicts
the selected policy then resolves. Anonymous members receive fresh Frame-scoped ordinals in
the destination as their containment is rebuilt.

Reconciliation, import, and attachment operate on distinct axes: reconciliation resolves
temporal versions of one Frame identity; import copies content across Frame namespaces
through an explicit address map while preserving the source; attachment changes containment
among content already in the destination namespace. Because attachment's input references
use only named local identities, it cannot create a cross-Frame reference — cross-Frame
relationships require a Span pair.

Reconciliation is itself a boundary operation: a FrameSpace applies it to an existing Frame
before checking the affected Span Bundles. Ordinary reconciliation cannot change an
address — it preserves existing addresses, adds new ones, or fails before producing an
invalid FrameSpace. A future explicit readdress operation, rewriting all local references
and incident Bundle pair endpoints atomically, is outside the initial model (Open Question
3).

### Deferred Pattern conversion

Conversion between a Frame registry and `Pattern Subject` is not part of the initial
Frame/Span implementation; Frame is not a wrapper, so no lossless conversion pair is
exposed. A later materialization is correct when it is *topologically isomorphic* to the
Frame registry: a shape-preserving correspondence anchored on named identities, respecting
Subject content and ordered containment edges — not structural equality to one chosen
nested `Pattern Subject` shape. Anonymous members' ordinals are labels of one expression of
that shape, not part of it; a faithful round-trip may reassign them, which is safe
precisely because pair endpoints are always named, never positional. The future conversion
design must choose a canonical Gram presentation if one is required, root and definition
placement, shared-member rendering, direct self-reference behavior, and return shape (Open
Question 4).

### Category-theoretic guidance

Frame, Span, and FrameSpace provide syntax and structure in which stronger semantics can
later be expressed. The initial container model does not claim that every Span is an
adjunction or that a Frame collection is a topos, presheaf, or sheaf. The useful guidance is
directional:

- A Frame can become an object in a category of referentially closed Frame snapshots.
  Integrity-preserving transformations can become morphisms.
- A Span can become a relation, correspondence, or categorical span once its endpoint maps
  and laws are defined.
- An adjunction is a separate capability requiring functors, unit, counit, and the
  associated naturality laws.
- Presheaf and sheaf interpretations require a base category of Frames, restriction maps,
  and, for sheaves, a coverage and gluing law.

These concepts should guide extension points and naming without imposing unimplemented
axioms on the initial container API.

### What this RFC does not include

  - **Within-Frame query and navigation.** `find`, `containers`, `siblings`, and a Frame
    paramorphism have no registry-model successors yet (Open Question 6).

  - **Pattern materialization.** Conversion from a Frame registry back to `Pattern Subject`
    is deferred (Open Question 4); until it exists, a managed Frame cannot use
    `Pattern.Graph`, `PatternGraph`, `GraphQuery`, or `Pattern.Graph.Algorithms`.

  - **Explicit readdressing.** Atomically renaming a Frame or member and rewriting every
    dependent reference is deferred (Open Question 3).

  - **Cascade deletion and repair beyond `PairDispositionPolicy`.** Repair ownership for a
    vetoed member deletion, and cascade-removal of Spans when an endpoint Frame is removed,
    remain open (Open Question 1).

  - **Bundle endpoint uniqueness.** Whether a Span may hold more than one pair for the same
    ordered endpoint pair is left unconstrained by default (Open Question 2).

  - **A serializable identifier scheme for `CustomDisposition`.** Named policies serialize;
    arbitrary disposition functions do not, pending a concrete need.

  - **Persistent / lazy / database-backed Frames.** Per the principle that the substrate is
    in-memory, lazy or unmaterialized registries are a database concern, handled at a layer
    above pattern-hs.

  - **A globally unique rewrite of every Subject symbol.** This RFC scopes existing symbols
    contextually via `ScopedAddress`; it does not prefix, hash, or otherwise mutate local
    identities to manufacture process-wide uniqueness.

## Open Questions

1. **Cascade deletion and repair ownership.** The initial model rejects deletion of a member
   addressed by a local reference; whether FrameSpace, callers, or a higher-level service
   repairs those references remains open pending concrete removal workflows. The
   incident-Bundle-pair half is resolved by `PairDispositionPolicy`: `AutoDrop` pairs are
   repaired by FrameSpace itself; `Veto` pairs still block the edit.
2. **Bundle endpoint uniqueness.** A Bundle may optionally limit entries to one PairAddress
   per ordered endpoint pair. Resolve the constraint's opt-in surface and conflict policy
   after evaluating whether ordinary workflows need parallel relationship entries.
3. **Explicit readdressing.** Renaming a Frame or member must atomically rewrite local and
   incident pair addresses. Resolve scope, authorization, and policy when an
   identity-migration use case exists.
4. **Pattern materialization.** A future conversion must choose canonical Gram definition
   placement, shared-member rendering, direct self-reference handling, and return shape.
   Resolve it with a reference-preserving export/import use case. Until then, managed
   Frames cannot use `Pattern.Graph`, `PatternGraph`, `GraphQuery`, or
   `Pattern.Graph.Algorithms` through a `Pattern Subject` materialization. A candidate worth
   evaluating alongside Open Question 6: factor `PatternRow` and `Pattern`'s own node shape
   as one base functor (`Node v a = Node v [a]`, with `Pattern v = Fix (Node v)` and
   `PatternRow v = Node v LocalAddress`), so materialization becomes knot-tying a lazy,
   possibly-cyclic `Pattern v` from the registry — a genuinely cyclic value is constructible
   under laziness, with no special cyclic-Pattern type needed. This does not by itself make
   ordinary Pattern traversal (`Foldable`/`Traversable`) safe to run on the result; see Open
   Question 6.
5. **Incident-Span discovery cost.** Updating a Frame validates every incident Span Bundle,
   so its cost grows with a Frame's Span fan-out. Resolve the index strategy after a
   representative multi-Span workload establishes the required performance profile.
6. **Within-Frame query and navigation.** This RFC's original `find`, `containers`,
   `siblings`, and `framePara` have no registry-model successors yet. This gates
   implementation planning, not merely implementation: `ScopeQuery` (`Pattern.Core`) has
   live generic consumers — `RepresentationMap`, `Graph.Transform`, `Pattern` — so the
   redesign must decide whether it is retained, adapted, or replaced, and must preserve or
   explicitly revise each operation's documented behavior. Traversal order, cycle and
   shared-membership handling, and ambient-context semantics over `FrameRegistry` are the
   ADR's to specify against that contract. Public lookup and counterpart operations return
   a typed error on an unresolved endpoint, never a silent skip. This is paired with Open
   Question 4, not sequenced after it: if `PatternRow` and Pattern's node share one base
   functor (Open Question 4's candidate), the same generic, memoized/visited-set-aware
   recursion scheme could be written once against that functor and interpreted two ways —
   ordinary structural recursion over an in-memory `Pattern` (where memoization is inert),
   and cycle-terminating traversal directly over `FrameRegistry` via `LocalAddress` lookup
   (where memoization is what makes a cyclic Frame like the aircraft/maintenance exercise
   terminate at all). That would make `find`/`containers`/`siblings`/`framePara`'s successors
   one interpretation of a shared algorithm rather than a bespoke design, independent of
   whether Pattern materialization (Open Question 4) is also pursued.
7. **Correlate-by-identity ambiguity.** A shared-identity correlation constructor needs a
   policy for a named identity occurring as a candidate correspondence more than once, or
   for a candidate that conflicts with an existing pair's endpoints. Resolve the conflict
   policy and PairLocalIdentity assignment convention once a concrete correlation workflow
   exists.

## Alternatives

**Frame and Span as typed views (cache-bearing `newtype`s) over `Pattern Subject`.** This
RFC's original position: a Frame wraps a self-contained Pattern, encapsulation is a property
of construction, and cross-Frame correspondence lives in a Span whose elements are the two
Frames by value. Rejected because a recursively embedded wrapper has no finite fully-
expanded presentation under a reference cycle, and duplicates shared members when it does
not — exactly the shape the aircraft/maintenance exercise's indirect cycle requires. It also
gives Frame no place to hold one authoritative, independently addressable entry per member;
every update to a shared member requires updating every embedded copy.

**Keep the recursively embedded wrapper Frame and add cycle detection.** Rejected because
cycle detection controls traversal but does not provide one canonical member definition,
referential-integrity checks for updates, or a stable normalized representation for the
row-based persistence model in RFC-011.

**Coordinate Frame and Span updates with observers.** Rejected because observers receive a
Frame change after its local operation has produced it; they do not make the Frame update
and the Bundle repair one atomic pure replacement.

**Give each Frame back-references to its incident Spans.** Rejected because Frame mutation
would then require cross-Frame relationship knowledge inside the Frame. A FrameSpace can own
Frames and Spans, validate their relationship entries together, and leave each Frame
independently useful outside that aggregate.

**Give each Span ownership of its two Frames by value.** Rejected on two independent
grounds. Ownership is exclusive containment: a Frame owned by one Span cannot also be owned
by a second, directly contradicting one Frame participating in many Spans. Relaxing
exclusivity to let the same Frame be embedded by value in several Spans reintroduces the
divergent-snapshot problem the Observer alternative was also rejected for. A Span that
stores Frame identities and resolves them through FrameSpace satisfies the required
many-to-many relationship the same way RFC-011's `bundle_pair` join table does at the schema
layer, rather than embedding Frames inside a `frame_row`.

**Globally rewriting Subject identities during ingestion.** Prefixing every local symbol
with a file path, UUID, or Frame name would also avoid cross-file clashes. Rejected because
it changes the substrate value, breaks lossless local reference, leaks storage concerns into
gram and Subject semantics, and makes stable local references dependent on an external
naming scheme. Contextual `(frame identity, local address)` qualification preserves source
identities while providing the same disambiguation wherever it is actually needed.

## Worked Exercise: Aircraft and Maintenance

The following exercise tests the managed-container model. It uses two Frames in one
FrameSpace and covers local closure, cross-Frame pair validation, reconciliation,
import/rebase, and anonymous-member addressing.

1. Admit an `aircraft-17` Frame with members `engine`, `fuel-system`, `fuel-pump`, and
   `diagnostic-procedure`. Its local references form the indirect cycle `engine ->
   fuel-system -> fuel-pump -> diagnostic-procedure -> engine`. Frame admission accepts the
   cycle, gives every member one canonical local address, and creates no recursive copies.
2. Admit a separate `maintenance-17` Frame with members `inspect-engine` and
   `inspect-fuel`. The FrameSpace distinguishes any equal local symbols in its two Frames
   through their Frame identities.
3. Admit an `aircraft-maintenance` Span whose Bundle contains:

   ```text
   [pair-engine:Summarizes { confidence: 0.92 } | engine, inspect-engine]
   [pair-fuel:Summarizes { source: "manual" } | fuel-system, inspect-fuel]
   ```

   Pair endpoints resolve through the Span's canonical left/right Frame ordering; pair
   roots are addressable under `aircraft-maintenance`; counterpart traversal works from
   either endpoint Frame.
4. Attempt to remove `fuel-system` from `aircraft-17`. Frame removal initially fails
   because `engine` references it. After detaching that local reference, replacement in
   FrameSpace still fails because `pair-fuel` addresses `(aircraft-17, fuel-system)`.
   Removing or explicitly rebinding `pair-fuel` permits the Frame update.
5. Re-ingest a revised `fuel-pump` definition into `aircraft-17` using `Additive`, adding
   labels `Pump` and `Critical` and `inspectionIntervalDays: 30`. `Pattern.Reconcile`
   applies the selected metadata policy, `(aircraft-17, fuel-pump)` remains unchanged, and
   existing Span pairs remain valid.
6. Import the subgraph reachable from `engine` into a `repair-plan-17` Frame. The import
   copies the complete local-reference closure. A collision with an existing
   `repair-plan-17.engine` fails by default; an explicit import map may deliberately map
   the source `engine` to that destination identity and invoke the selected reconciliation
   policy. `Attach` then adds the imported root address to a selected repair-plan member.
7. Exercise the collision-remapped attachment explicitly. Start with
   `repair-plan-17.work-order` and an existing `repair-plan-17.engine`; map
   `aircraft-17.engine` to `repair-plan-17.engine` and map its remaining closure members to
   unused repair-plan identities. After the selected merge policy reconciles the two
   `engine` definitions, `Attach work-order [engine]` adds the mapped root address to the
   work order. The operation fails without the explicit collision map and returns one
   locally closed repair-plan Frame when the map and merge succeed.
8. Admit an anonymous `[:Note { text: "temporary patch, revisit" }]` into `aircraft-17` as
   an element of `fuel-pump`. It receives its own Frame-scoped ordinal, distinct from
   `fuel-pump`'s own address. Attaching that same ordinal as an element of
   `diagnostic-procedure` too gives the note two containers — valid, since nothing in the
   note's own address ties it to either one. Attempting to use the note's ordinal as a
   Bundle pair endpoint in `aircraft-maintenance` fails: pair endpoints require a named,
   author-chosen identity, which an anonymous ordinal is not.

### Acceptance criteria and demo

The exercise is the acceptance surface. Demonstrated by
[SPIKE-001](../../spikes/SPIKE-001-frame-registry/SPIKE.md): scoped-address collision
handling across two Frames (step 2) and FrameSpace rejection of a Frame update that
invalidates an incident pair (step 4). [SPIKE-002](
../../spikes/SPIKE-002-frame-document-diversity/SPIKE.md) additionally validated Frame
admission and closure — ordinal addressing, forward references, and referential
integrity — against real, independently authored Gram documents rather than only the
hand-designed exercise above. Two criteria remain undemonstrated pending Open Question 6:
within-Frame navigation staying inside the Frame, and counterpart traversal returning only
paired members. `AutoDrop` repair (§Pair disposition policy) is specified but not yet
exercised.

Runnable demos:

```text
cabal build all
cabal exec -- runghc -ilibs/pattern/src -ilibs/subject/src -ilibs/gram/src \
  design/spikes/SPIKE-001-frame-registry/scripts/Main.hs
cabal exec -- runghc -ilibs/pattern/src -ilibs/subject/src -ilibs/gram/src \
  design/spikes/SPIKE-002-frame-document-diversity/scripts/Main.hs
```

SPIKE-001 records 17 PASS assertions against three hand-designed fixtures; SPIKE-002
records 8 PASS assertions against five independently authored documents.

## ADRs

- [ADR-001: Frame and Span Implementation Model](../adr/ADR-001-frame-span-implementation.md)
  — superseded by this RFC's managed-container model; it assumes cache-bearing wrappers and
  a Pattern-shaped Bundle. A replacement ADR, covering the Haskell registry representation,
  error types, FrameSpace ownership API, and the `Pattern.Reconcile` adapter, is not yet
  written.
