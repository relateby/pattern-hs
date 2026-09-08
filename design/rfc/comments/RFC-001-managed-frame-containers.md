---
title: Managed Frame and Span Containers
status: discussion
parent: RFC-001
created: 2026-09-08
---

# RFC-001 Discussion: Managed Frame and Span Containers

## Purpose

This discussion note proposes replacing RFC-001's wrapper/view model with managed,
referentially closed containers. It is a reaction to
[RFC-001](../RFC-001-frames-and-spans.md), not an amendment or an accepted
architecture decision. It records the model to evaluate before RFC-001 and ADR-001 are
rewritten.

## Motivation

ADR-001 models a Frame as a wrapper around a recursively embedded `Pattern Subject`.
That shape has no canonical registry of members or references. A Frame containing the
worked exercise's indirect cycle `engine -> fuel-system -> fuel-pump ->
diagnostic-procedure -> engine` cannot have a finite fully expanded presentation; an
acyclic presentation duplicates shared members. Scoped identity in RFC-001 similarly
qualifies members but has no aggregate owner that validates cross-Frame pair addresses
when a Frame or Span changes.

RFC-011 independently requires the same normalized shape for persistence:
`frame_row(frame_id, id, labels, properties, elements)` uses `(frame_id, id)` as its
member key, and `bundle_pair` enforces both endpoint Frame membership and endpoint
existence through foreign keys. The in-memory model should have equivalent ownership and
integrity rules before it is mapped to those rows.

## Position

A Frame is a container identified by a `Subject`, not a wrapper around one pre-existing
`Pattern Subject`. It owns a registry of locally identified Patterns and maintains the
invariant that every Pattern it contains is referentially closed within that registry.
Frame operations add, combine, and remove Patterns while preserving that invariant.

A Span is a container that relates two Frame identities and owns the cross-Frame
correspondences between their members. Its Bundle is a collection of pair relationship
Patterns. Each pair Pattern has exactly two elements: one member reference from each
endpoint Frame. A Portal is either member of such a pair. Bundle membership, rather than
a label or a type, gives a member its Portal role.

The initial Haskell API uses persistent updates: operations return a new Frame, Span, or
higher-level collection instead of mutating an `IORef` or other process-local cell. This
is database-like in its integrity rules and data-frame-like in its tabular registry/query
model, while remaining a portable pure reference implementation for Rust and TypeScript.

## Alternatives Considered

**Keep the recursively embedded wrapper Frame and add cycle detection.** Rejected because
cycle detection controls traversal but does not provide one canonical member definition,
referential-integrity checks for updates, or a stable normalized representation for the
row-based persistence model in RFC-011. A roster presentation also does not make nested
member updates or shared-member ownership unambiguous.

**Coordinate Frame and Span updates with observers.** Rejected because observers receive a
Frame change after its local operation has produced it; they do not make the Frame update
and the Bundle repair one atomic pure replacement. Subscription ordering, failure, and
observer lifetime would become part of the portable reference model.

**Give each Frame back-references to its incident Spans.** Rejected because Frame mutation
would then require cross-Frame relationship knowledge inside the Frame. A FrameSpace can
own Frames and Spans, validate their relationship entries together, and leave each Frame
independently useful outside that aggregate.

## Working Model

### Identity and addresses

`Subject.identity` is a local identity. It says which member is being discussed inside
one Frame. It is not a global identity.

`ScopedIdentity` is an address:

```text
FrameIdentity x LocalIdentity
```

It says where a locally identified member is found in an enclosing collection. A registry
is a set of such addresses, together with the member Patterns they resolve to. This
distinction permits separately ingested Frames to contain `alice` or generated `#1`
without collision while preserving the Symbols supplied by source data.

Frame identity must be stable across versions of the same logical Frame and unique among
Frames held by one collection. Reconciliation under the same Frame identity updates one
namespace. Equal local identities in different Frame identities remain different members.

A Frame's identifying Subject supplies its Frame identity: `FrameIdentity` is the
Subject's non-anonymous `identity`. A Frame's identity cannot change during its lifetime,
although its labels and properties remain reconcilable metadata. `emptyFrame` requires
such an identifying Subject, and `FrameSpace.addFrame` rejects an identity already
present. An import boundary supplies the Frame Subject, optionally deriving it from a
stable source key; Frame and FrameSpace do not infer file paths, generate hidden global
identities, or rewrite source member identities. Re-ingestion explicitly selects the
destination Frame identity: it uses `Replace` or `Additive` for an existing identity and
`addFrame` for a new one.

### Frame registry and closure

A new Frame starts with its identifying Subject and no members. Adding an external
`PatternLike Subject` recursively registers each definition and nested definition as an
independently addressable Frame member. Duplicate local identities are reconciled by a
selected policy; unresolved identity references and conflicting definitions are errors.
Frame admission has no nested identity namespaces: every Definition at every input depth
becomes a row in the same flat local registry, and only ordered local-address links express
its containment relationships.

Each registered `Member` is an internal row in the Frame registry. It is Pattern-like:
it retains a `Subject` and an ordered sequence of elements, but its elements are local
member addresses rather than recursively embedded Pattern values. The registry is the
canonical membership domain; its address links are the canonical containment structure.
This provides one authoritative row for every local identity, permits cycles, and avoids
updating copied nested values when a member changes.

A Frame can produce many semantically equivalent `Pattern Subject` presentations. Each
complete presentation defines every registered member once and uses atomic local
references for additional occurrences. This is Gram's finite recursive-graph form:

```text
[frame | [a | b], [b | a]]
```

The definitions of `a` and `b` occur once, while the bare identifiers close their
indirect cycle. The Frame registry and any complete, reference-bearing presentation are
logically equivalent when they resolve to the same member Subjects and ordered local
references. They need not have the same recursive value shape: a member may be defined
at a different nesting position, and shared members may be reached by references from
multiple parents.

A fully expanded presentation that recursively embeds every target value is only one
possible view. It cannot be finite when the registry contains a cycle and duplicates
shared members when it does not. A cycle-tolerant roster presentation places every full
member definition once beneath the Frame Subject and retains each member's atomic local
references, preserving both complete membership and containment links. Conversion APIs
remain deferred, but their eventual contract should be reference-preserving graph
equivalence rather than structural equality of one chosen nesting.

### Definitions and local references

The RFC rewrite should retain Definition/Reference admission behavior. The exact
`PatternLike` declaration, module ownership, and conversion adapters are ADR-level
implementation decisions.

Frame admission preserves the distinction between a Pattern definition and a local
reference:

```text
PatternLike Subject
  = Definition Subject [PatternLike Subject]
  | Reference LocalIdentity
```

`Definition` introduces or reconciles one registry member. `Reference` contributes an
ordered local address to its containing definition without introducing another member.
An admission batch first collects all Definitions, then resolves References against the
combined existing and incoming registry. This permits forward references and indirect
cycles while rejecting unresolved local addresses.

An anonymous Definition receives a generated LocalIdentity unique in the combined Frame
registry before Reference resolution. The generated identity remains stable for that
member's lifetime; anonymous source syntax cannot itself refer to that member by name.

A Definition must not directly reference its own local identity. Direct self-reference
does not add useful containment structure and is rejected at Frame admission. References
among distinct members may form indirect cycles; those cycles are valid registry topology.
Semantic self-loops belong in relationship Patterns when a domain requires them.

Gram supplies this distinction directly: bracket syntax maps to `Definition` and a bare
identifier in element position maps to `Reference`. A complete Frame presentation defines
each local identity once and uses References for every further occurrence.

Raw `Pattern Subject` does not preserve definition/reference provenance: an atomic
`Pattern` may be a definition or the in-memory form of a Gram reference. It remains a
convenient compatibility import format. Its importer applies the existing
`Pattern.Reconcile` convention that an atomic Pattern sharing an identity with a fuller
definition is a reference only when that produces one unambiguous interpretation. Any
ambiguous raw import fails with an import-ambiguity error; it never silently selects an
interpretation. `PatternLike Subject` is the canonical admission format for Frame
construction and updates.

A full Pattern in one Frame and an atomic Pattern in another Frame are not a
Frame-internal reference; a Span pair records their relationship.

### Frame updates

The initial update operations are pure and invariant-preserving:

```text
emptyFrame subject
addPatternLike mergePolicy pattern frame
combinePatternLikes mergePolicy patterns frame
removePattern localIdentity frame
```

Adding or combining admits Definitions and resolves their transitive local References.
Matching local identities reconcile according to the selected policy. Removing a member
fails when another registered member refers to it. Cascade deletion, reference rewriting,
and automatic cross-Span cleanup remain caller operations until concrete workflows
establish their semantics.

The API returns explicit errors for duplicate/conflicting identity definitions,
unresolvable local references, and referenced-member deletion. No operation silently
widens a Frame's registry or treats another Frame's local identity as its own.

### Span, Bundle, and ownership

One Frame may exist with no Span and may participate in many Spans. A Span must preserve
pair validity as Frame membership changes, but a Frame must not carry back-references to
its Spans. The two concepts therefore need a higher-level integrity owner whenever their
lifecycles are coordinated.

The recommended owner is a `FrameSpace`: an immutable collection of Frames keyed by
Frame identity and Spans keyed by Span identity. It provides atomic replacement operations
that check a changed Frame against all incident Span Bundles before returning the next
FrameSpace. A Frame remains independently useful outside a FrameSpace; it gains
cross-Frame integrity only once it is registered in one.

A Span is incident to a Frame when that Frame's identity equals the Span's left or right
Frame identity. FrameSpace uses this definition to determine which Bundles must be
validated after a Frame change; the data structure used to locate incident Spans is an
implementation-level indexing decision.

A Span canonically stores its left and right Frame identities, not Frame snapshots. It is
admitted only when both identities resolve in its FrameSpace; Bundle pair validation then
uses the current endpoint Frames. One Frame can therefore participate in many Spans
without copied state or divergent snapshots. A standalone Frame remains valid, but a Span
requires FrameSpace membership because its endpoints and pair addresses must resolve.

A FrameSpace snapshot, rather than a second public Span-by-value type, is the exchange
form for a Span with its endpoint Frames. Replacing a Frame through FrameSpace checks all
incident Spans before returning the replacement collection.

### FrameSpace integrity API

The RFC rewrite should state FrameSpace's atomic integrity responsibilities. The concrete
function signatures, error constructors, and index representation below are ADR-level
implementation decisions. The signatures illustrate the minimum operation boundary:

```text
emptyFrameSpace

lookupFrame FrameIdentity -> FrameSpace -> Maybe Frame
lookupSpan  SpanIdentity  -> FrameSpace -> Maybe Span

addFrame    Frame -> FrameSpace -> Either FrameSpaceError FrameSpace
updateFrame FrameIdentity
            (Frame -> Either FrameError Frame)
            -> FrameSpace
            -> Either FrameSpaceError FrameSpace
removeFrame FrameIdentity -> FrameSpace -> Either FrameSpaceError FrameSpace

addSpan     Span -> FrameSpace -> Either FrameSpaceError FrameSpace
updateSpan  SpanIdentity
            (Span -> Either SpanError Span)
            -> FrameSpace
            -> Either FrameSpaceError FrameSpace
rebindPair  SpanIdentity PairLocalIdentity (LocalIdentity, LocalIdentity)
            -> FrameSpace
            -> Either FrameSpaceError FrameSpace
removeSpan  SpanIdentity -> FrameSpace -> Either FrameSpaceError FrameSpace
```

`updateFrame` is the cross-Frame integrity boundary. It applies a pure local Frame edit,
validates the replacement Frame, finds every incident Span, and validates every affected
Bundle endpoint before returning the next FrameSpace. An edit that removes a member used
by a Bundle pair fails with an error identifying the affected PairAddresses; it never
returns a partially updated FrameSpace.

`addFrame` requires a new Frame identity and a locally valid Frame. `addSpan` requires
both endpoint Frames and all Bundle pair endpoints to resolve in their designated left or
right Frame. `updateSpan` revalidates those constraints. `removeFrame` fails while an
incident Span exists; `removeSpan` removes its relationship entries without changing its
endpoint Frames. A `replaceFrame` convenience function may be defined as `updateFrame`
with a constant replacement. `rebindPair` explicitly replaces one pair's ordered endpoint
references after validating them in the Span's existing left and right Frames.

This boundary does not provide cascades, automatic pair rewriting, observers,
transactions, storage backends, or version histories. Those extensions may build on the
same atomic pure replacement model.

### Bundle pair Patterns

A Bundle is a Span-owned collection of pair relationship Patterns, not a Pattern of
pairs and not an independent serializable entity. Each pair Pattern has a root `Subject`
and exactly two elements. The root Subject supplies the pair's local identity, labels,
and properties. Its identity is scoped by its containing Span:

```text
PairAddress = (SpanIdentity, PairLocalIdentity)
```

Pair Subjects must have a non-anonymous identity that is unique within their Span. A pair
may be anonymous during construction, but Span admission rejects it until the caller
supplies a PairLocalIdentity. Silent identity generation would make a repeated import
produce a new relationship entry instead of a reconcilable revision of the existing one.

The first element identifies a member of the Span's first Frame and the second identifies
a member of its second Frame. The Bundle validates that both endpoints exist in the
endpoint Frames.

Each endpoint is an atomic local reference: it has a non-anonymous local identity and no
labels, properties, or nested elements. The Span supplies its Frame scope by position:

```text
pair.elements[0] = ScopedIdentity(Span.leftFrame, localIdentity)
pair.elements[1] = ScopedIdentity(Span.rightFrame, localIdentity)
```

An endpoint is never a member definition. Unlike a raw Pattern admitted to a Frame, its
position inside a pair is explicitly a reference position, so an atomic Pattern has only
that interpretation. FrameSpace validates the two resulting scoped addresses when the
Span is admitted or updated.

Each Bundle pair is an ordered two-element Pattern. Its first endpoint must resolve in
the Span's left Frame and its second endpoint must resolve in the Span's right Frame.
The positional order is canonical. Traversal may follow a pair in either direction;
symmetry does not require a reversed duplicate pair.

Span has no core `SpanDirection` field. Ordered endpoints provide structural positions,
not a semantic arrow. The core exposes counterpart traversal from either endpoint Frame.
The Span Subject's labels and properties, or a higher-level domain validator, determine
whether a Span is interpreted as directed, symmetric, functional, adjoint-like, or under
other relationship laws.

The ordered endpoint pair identifies what a relationship entry relates; its PairAddress
identifies the relationship entry itself. A Bundle may therefore contain multiple pairs
with the same endpoints when they have different identities or provenance:

```text
[pair-confirmed | left-a, right-b]
[pair-inferred  | left-a, right-b]
```

An optional Bundle uniqueness constraint may limit a Span to one pair for each ordered
endpoint pair. Endpoint uniqueness is not the default identity rule.

Pair reconciliation preserves exactly-two-endpoint cardinality. Two versions with the
same PairAddress and the same ordered endpoints reconcile their root Subject's labels
and properties using the selected `Subject` reconciliation policy. Two versions with the
same PairAddress but different endpoints fail with an endpoint conflict. Changing a
pair's endpoints requires an explicit rebind operation rather than generic Pattern
element merging.

### Reconciliation

Frame-level reconciliation operates only between versions of the same Frame identity.
It registers incoming Patterns into the destination registry and delegates content
conflicts, reference completion, and recursive element merge choices to
`Pattern.Reconcile`. Frame code adds namespace checks, registry construction, and
integrity errors; it does not duplicate Subject merge policy.

Frame reconciliation has two modes. `Replace` validates the incoming Frame and replaces
the existing registry of the same Frame identity; existing members omitted from the
incoming Frame do not survive. `Additive` admits incoming `PatternLike` definitions into
the existing registry, adds missing local identities, reconciles matching identities with
the selected `Pattern.Reconcile` policy, and retains existing members omitted from the
incoming batch. FrameSpace validates incident Span endpoints after either mode.

Frame identity and admitted member local identities are immutable addresses. A FrameSpace
requires each admitted Frame identity to be non-anonymous and unique. Reconciliation may
change a member's labels, properties, and ordered local references, but it does not rename
that member or its Frame. `Replace` may remove a member only when no local reference or
incident Bundle pair addresses it; otherwise FrameSpace rejects the replacement.

Containment attachment is not a reconciliation mode. `Attach targetLocalIdentity
incomingDefinitions` admits definitions into the destination Frame namespace, then adds
the selected incoming root addresses to the target member's ordered element sequence. It
rejects direct self-reference, unresolved references, and identity conflicts. Content
from a different Frame must be imported or rebased into the destination namespace before
attachment; coincident local identities never trigger cross-Frame merging.

`importSubgraph` copies selected source roots and their complete transitive local-reference
closure into a destination Frame without changing the source. It preserves each local
identity when it is unoccupied in the destination and rejects collisions by default. An
import plan maps any colliding source local identity to its destination address and must
cover every member reachable from the selected roots. The mapping is injective for newly
imported members and rewrites every imported local Reference consistently.

Mapping a source identity onto an existing destination identity is an explicit request to
merge those members. That deliberate merge delegates content conflicts to the selected
`Pattern.Reconcile` policy. A caller can attach imported roots only after the resulting
destination Frame passes local closure validation.

Reconciliation, import/rebase, and attachment operate on distinct axes.
`Replace` and `Additive` reconcile temporal versions of one Frame identity. `importSubgraph`
copies content across Frame namespaces through an explicit address map while preserving the
source Frame. `Attach` changes containment among content already in the destination
namespace. Because its input references use only `LocalIdentity`, Attach cannot create a
cross-Frame reference; cross-Frame relationships require a Span pair.

Span-level pair reconciliation delegates pair-root Subject conflicts to the same policy,
while retaining the pair adapter's endpoint cardinality and endpoint-conflict checks.

Reconciliation is a boundary operation. A `FrameSpace` can apply it to an existing Frame
before checking the affected Span Bundles. Ordinary reconciliation cannot change an
address: it preserves existing addresses, adds new ones, or fails before producing an
invalid FrameSpace. A future explicit readdress operation must rewrite all local references
and incident Bundle pair endpoints atomically; readdressing is outside the initial model.

### Deferred Pattern conversion

Conversion between Frame and `Pattern Subject` is not part of the initial Frame and Span
implementation. Frame is not a wrapper, so the initial API does not expose a lossless
`asFrame`/`framePattern` conversion pair. Its admission boundary accepts `PatternLike
Subject`, preserving the distinction between Definitions and References before registry
validation.

A later materialization is correct when it is reference-preserving graph equivalent to a
Frame registry: it defines the same locally identified members, preserves their Subjects
and ordered local references, and resolves to the same containment graph. It does not
promise structural equality to one chosen nested `Pattern Subject` shape. The future
conversion design must choose a canonical Gram presentation if one is required, root and
definition placement, shared-member rendering, direct self-reference behavior, and
whether it returns one Pattern, a collection of Patterns, or a dedicated Frame format.

### Category-theoretic guidance

Frames and Spans provide syntax and structure in which stronger semantics can later be
expressed. The initial container model does not claim that every Span is an adjunction or
that a Frame collection is a topos, presheaf, or sheaf.

The useful guidance is directional:

- A Frame can become an object in a category of referentially closed Frame snapshots.
  Integrity-preserving transformations can become morphisms.
- A Span can become a relation, correspondence, or categorical span once its endpoint
  maps and laws are defined.
- An adjunction is a separate capability requiring functors, unit, counit, and the
  associated naturality laws.
- Presheaf and sheaf interpretations require a base category of Frames, restriction maps,
  and, for sheaves, a coverage and gluing law.

These concepts should guide extension points and naming without imposing unimplemented
axioms on the initial container API.

## Open Questions

1. **Cascade deletion and repair ownership.** The initial model rejects referenced-member
   deletion and does not rewrite local references or incident Bundle pairs. Resolve this
   after concrete removal and repair workflows establish whether FrameSpace, callers, or a
   higher-level service owns the transaction.
2. **Bundle endpoint uniqueness.** A Bundle may optionally limit entries to one PairAddress
   per ordered endpoint pair. Resolve the constraint's opt-in surface and conflict policy
   after evaluating whether ordinary workflows need parallel relationship entries.
3. **Explicit readdressing.** Renaming a Frame or member must atomically rewrite local and
   incident Pair addresses. Resolve the operation's scope, authorization, and policy when
   an identity-migration use case exists.
4. **Pattern materialization.** A future conversion must choose canonical Gram definition
   placement, shared-member rendering, direct self-reference handling, and return shape.
   Resolve it with a reference-preserving export/import use case. Until then, managed
   Frames cannot use `Pattern.Graph`, `PatternGraph`, `GraphQuery`, or
   `Pattern.Graph.Algorithms` through a `Pattern Subject` materialization.
5. **Incident-Span discovery cost.** `updateFrame` validates every incident Span Bundle,
   so its cost grows with a Frame's Span fan-out. Resolve the index strategy after a
   representative multi-Span workload establishes the required performance profile.
6. **Within-Frame query and navigation.** The registry model has not yet designed
   successors for RFC-001's `find`, `containers`, `siblings`, and `framePara` operations.
   Resolve the query API and its ScopeQuery relationship before implementation planning.

## Worked Exercise: Aircraft and Maintenance

The following exercise tests the managed-container model before its implementation-level
API is fixed. It uses two Frames in one FrameSpace and covers local closure, cross-Frame
pair validation, reconciliation, and import/rebase.

1. Admit an `aircraft-17` Frame with members `engine`, `fuel-system`, `fuel-pump`, and
   `diagnostic-procedure`. Its local references form the indirect cycle `engine ->
   fuel-system -> fuel-pump -> diagnostic-procedure -> engine`. Frame admission accepts
   the cycle, gives every member one canonical local address, and creates no recursive
   copies.
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
   applies the selected metadata policy, `(aircraft-17, fuel-pump)` remains unchanged,
   and existing Span pairs remain valid.
6. Import the subgraph reachable from `engine` into a `repair-plan-17` Frame. The import
   copies the complete local-reference closure. A collision with an existing
   `repair-plan-17.engine` fails by default; an explicit import map may deliberately map
   the source `engine` to that destination identity and invoke the selected reconciliation
   policy. `Attach` then adds the imported root address to a selected repair-plan member.
7. Exercise the collision-remapped attachment explicitly. Start with
   `repair-plan-17.work-order` and an existing `repair-plan-17.engine`; map
   `aircraft-17.engine` to `repair-plan-17.engine` and map its remaining closure members
   to unused repair-plan identities. After the selected merge policy reconciles the two
   `engine` definitions, `Attach work-order [engine]` adds the mapped root address to the
   work order. The operation fails without the explicit collision map and returns one
   locally closed repair-plan Frame when the map and merge succeed.

## Proposed Next Steps

1. Exercise the model above against representative source documents before fixing the
   implementation-level API in an ADR.
2. Replace the view/wrapper vocabulary and Pattern-shaped Span/Bundle representation in
   RFC-001 with the managed-container model.
3. Check RFC-011 for terminology alignment after the RFC-001 rewrite, including
   `ScopedIdentity` and PairAddress versus `src_frame`/`src_ref` and `tgt_frame`/`tgt_ref`.
4. Mark ADR-001 superseded or rewrite it against the resulting RFC; it currently assumes
   cache-bearing wrappers and a Pattern-shaped Bundle.
5. Create a new ADR for the Haskell registry representation, error types, FrameSpace
   ownership API, and the `Pattern.Reconcile` adapter.
