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

Higher-order graph construction needs a stable way to treat selected Pattern structures as
addressable units without changing Pattern's value-agnostic, semantics-free substrate or
forcing every operation to understand a special node role.

`Pattern v` already supports arbitrary recursive composition. A Pattern with no elements
can be interpreted as a node; one with an element as an annotation; one with two node-like
elements as a relationship; a connected sequence of relationship Patterns as a path; and
recursively nested Patterns as subgraphs, annotations over subgraphs, or graphs of graphs.
These interpretations are useful precisely because Pattern does not enforce any one of
them. Its values remain polymorphic and its recursive structure remains available to any
consumer.

The difficulty appears when a higher-order structure must itself participate as an
addressable unit in further graph construction. Promoting selected Patterns into a node
role inside the existing Pattern substrate would make membership, identity, traversal, and
referential-integrity rules depend on a special context. A wrapper/view approach retains
that ambiguity: it starts with an already-formed Pattern and attempts to add module,
identity, and integrity semantics after the fact.

A Frame establishes a new construction context instead. It begins with an identifying
Subject and admits Pattern definitions into one referentially closed member registry. Each
admitted definition becomes an addressable member; its ordered local references establish
higher structure inside the Frame. A Span then establishes correspondence between members
of two Frames without placing cross-Frame references inside either Frame.

Given a Pattern `a`, a derived construction proceeds explicitly:

1. Create Frame `af`.
2. Admit `a` into `af`, registering each definition as a Frame member.
3. Create Frame `bf`.
4. Create Span `abs` between `af` and `bf`.
5. Create fresh members in `bf` for the selected members of `af`.
6. Add ordered pair relationship Patterns to `abs`, pairing each selected address in `af`
   with its corresponding address in `bf`.

The source members remain members of `af`; the derived members remain members of `bf`; and
the Span owns the correspondence between them. Frames therefore supply the integrity
boundary, while Spans make higher-order correspondence explicit.

This boundary also gives the in-memory model the ownership shape required by RFC-011
persistence: `frame_row(frame_id, id, labels, properties, elements)` uses `(frame_id, id)`
as its member key, and `bundle_pair` enforces both endpoint Frame membership and endpoint
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
endpoint Frame. A member participates in a cross-Frame correspondence when it appears as
an endpoint of such a pair. Pair membership is a relationship fact, not a label or a
distinct member type.

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

**Give each Span ownership of its two Frames by value.** Rejected on two independent
grounds. Ownership is exclusive containment: a Frame owned by one Span cannot also be
owned by a second, directly contradicting one Frame participating in many Spans.
Relaxing exclusivity to let the same Frame be embedded by value in several Spans instead
reintroduces the divergent-snapshot problem the Observer alternative above was also
rejected for — each embedding Span now holds its own copy, and a later Frame update has
no shared coordinator through which to reach every copy. A Span that stores Frame
identities and resolves them through FrameSpace satisfies the required many-to-many
relationship the same way RFC-011's `bundle_pair` join table does at the schema layer,
rather than embedding Frames inside a `frame_row`.

## Working Model

### Identity and addresses

Named and anonymous Patterns have different identity properties. A named Pattern carries a
non-anonymous `Subject.identity`, such as `engine` or `messages`; that Symbol identifies a
member within one Frame. An anonymous Pattern carries no Subject identity, for example:

```text
[:Greeting { msg: "Hello" }]
```

Anonymous Patterns remain valid Frame members. Their address derives from their containing
address and ordinal element position. In `[messages | [:Greeting { msg: "Hello" }]]`, the
greeting is addressed relative to `messages` and position `0`. Deeper anonymous Patterns
extend that positional path.

An anonymous atomic Pattern is admitted rather than rejected. Adding `()` three times to
one Frame creates three distinct registry entries at three positional addresses, even
though their Subjects and element lists are identical. The entries do not receive invented
Subject identities; their addresses distinguish their Frame-local occurrences.

Every Frame member has a local address, while only named members have a local identity.
A scoped address identifies where either kind of member occurs:

```text
LocalAddress  = Named LocalIdentity | Positional ParentAddress ElementOrdinal
ScopedAddress = FrameIdentity x LocalAddress
```

The Frame registry is keyed by `LocalAddress`, not by `Subject.identity`. Named members
retain their Symbols unchanged; anonymous members remain anonymous and acquire no invented
Subject identity during Frame admission. This permits separately ingested Frames to contain
the same named symbols without collision while preserving anonymous structural vocabulary.

Frame identity must be stable across versions of the same logical Frame and unique among
Frames held by one collection. Reconciliation under the same Frame identity updates one
namespace. Equal named local identities in different Frame identities remain different
members.

A Frame's identifying Subject supplies its Frame identity: `FrameIdentity` is the
Subject's non-anonymous `identity`. A Frame's identity cannot change during its lifetime,
although its labels and properties remain reconcilable metadata. `emptyFrame` requires
such an identifying Subject, and `FrameSpace.addFrame` rejects an identity already
present. An import boundary supplies the Frame Subject, optionally deriving it from a
stable source key; Frame and FrameSpace do not infer file paths, generate hidden global
identities, or rewrite source member identities. Re-ingestion explicitly selects the
destination Frame identity: it uses `Replace` or `Additive` for an existing identity and
`addFrame` for a new one.

### Defining and reference occurrences

The RFC rewrite should retain defining/reference admission behavior. The exact
`PatternLike` declaration, module ownership, and conversion adapters are ADR-level
implementation decisions.

Frame admission preserves the distinction between a defining Pattern occurrence and a
local reference occurrence:

```text
PatternLike Subject
  = Definition Subject [PatternLike Subject]
  | Reference LocalIdentity
```

In this illustrative shape, `Definition` introduces or reconciles one registry entry.
`Reference` is provisional: it contributes an ordered local address to its containing
defining occurrence, then resolves to a fuller definition with the same named local
identity when one exists. An admission batch collects all occurrences before resolving
them, permitting forward references and indirect cycles.

`Reference LocalIdentity` names a prospective named registry entry. If no fuller definition
of that identity exists after admission and reconciliation, the occurrence is promoted to
an atomic defining entry. An anonymous defining occurrence retains its anonymous Subject
and receives a positional LocalAddress from its parent address and ordinal position. It
cannot be targeted by a named Reference, a Span pair endpoint, or a stable external key
until a later explicit promotion operation is defined.

A defining occurrence must not directly reference its own local address. Direct
self-reference does not add useful containment structure and is rejected at Frame
admission. References among distinct registry entries may form indirect cycles; those
cycles are valid registry topology. Semantic self-loops belong in relationship Patterns
when a domain requires them.

Gram supplies this distinction directly: bracket syntax maps to `Definition` and a bare
identifier in element position maps to `Reference`. A complete Frame presentation defines
each named local identity once and uses References for every further named occurrence.

Raw `Pattern Subject` does not preserve definition/reference provenance: an atomic Pattern
may be a definition or the in-memory form of a Gram reference. It remains a convenient
compatibility import format. Its importer applies the existing `Pattern.Reconcile`
convention: an atomic Pattern sharing an identity with a fuller definition is a reference;
an atomic Pattern with no fuller definition is promoted to an atomic defining entry.
`PatternLike Subject` remains the canonical admission format because it retains source
provenance before this fallback is needed.

A full Pattern in one Frame and an atomic Pattern in another Frame are not a
Frame-internal reference; a Span pair records their relationship.

### Frame registry and closure

A new Frame starts with its identifying Subject and no registry entries. Admitting a
`PatternLike Subject` recursively turns each defining occurrence into one independently
addressable Frame entry. Duplicate named local identities reconcile under the selected
policy; conflicting fuller definitions are errors. Anonymous defining occurrences receive
positional addresses from their containing addresses and element positions.

Frame admission has no nested identity namespaces: every defining occurrence at every
input depth becomes an entry in the same registry, while ordered local-address links
express its containment relationships.

Each registry entry is a `PatternRow` — the same Subject-plus-ordered-references shape
`Pattern` itself has, but with elements as local references rather than recursively
embedded Pattern values:

```text
PatternRow    = PatternRow Subject [LocalIdentity]
FrameRegistry = Map LocalAddress PatternRow
```

`FrameRegistry`, keyed by `LocalAddress`, is the canonical membership domain; its address
links are the canonical containment structure. This provides one authoritative entry for
every local address, permits cycles, and avoids updating copied nested values when a
member changes. Unlike Bundle below, this keyed form is not an optimization layered over
some other canonical ordered shape — it is canonical, precisely because a cycle has no
single canonical ordered presentation (next paragraph).

A Frame can produce many semantically equivalent `Pattern Subject` presentations. Each
complete presentation defines every registry entry once and uses atomic local references
for additional occurrences. This is Gram's finite recursive-graph form:

```text
[frame | [a | b], [b | a]]
```

The defining occurrences of `a` and `b` occur once, while the bare identifiers close their
indirect cycle. The Frame registry and any complete, reference-bearing presentation are
logically equivalent when they resolve to the same member Subjects and ordered local
references. They need not have the same recursive value shape: a member may be defined at
a different nesting position, and shared members may be reached by references from multiple
parents.

A fully expanded presentation that recursively embeds every target value is only one
possible view. It cannot be finite when the registry contains a cycle and duplicates shared
members when it does not. A cycle-tolerant roster presentation places every full member
definition once beneath the Frame Subject and retains each member's atomic local references,
preserving both complete membership and containment links. Conversion APIs remain deferred,
but their eventual contract should be reference-preserving graph equivalence rather than
structural equality of one chosen nesting.

### Frame updates

The initial update operations are pure and invariant-preserving:

```text
emptyFrame subject
addPatternLike mergePolicy pattern frame
combinePatternLikes mergePolicy patterns frame
removePattern localAddress frame
```

`addPatternLike` admits one top-level occurrence. `combinePatternLikes` admits a batch of
top-level occurrences atomically and resolves forward references across that batch. Both
operations admit defining occurrences and resolve their transitive named references.
Matching named local identities reconcile according to the selected policy.

`removePattern` removes a registry entry only when no other entry refers to its address. A
caller must first detach a parent-to-child containment link before removing a nested entry.
Detachment is a structural operation distinct from deletion, symmetric with
[Attach](#reconciliation); whether it is a named initial API primitive is an ADR-level
decision. Structural edits recompute
positional addresses for their affected anonymous descendants. A positional address is valid
only against the Frame version that produced it; the replacement Frame supplies the
authoritative new addresses.

Frame updates preserve local registry closure and return a candidate replacement Frame.
`FrameSpace.updateFrame` is the cross-Frame commit boundary: it validates every incident
Span pair before returning a replacement FrameSpace. A Frame may be locally valid while
being ineligible to replace its registered version in a FrameSpace.

The API returns explicit errors for conflicting fuller definitions and referenced-member
deletion. Named atomic occurrences are admitted as references or promoted definitions, not
reported as unresolved references. Admission expands a Frame registry only through supplied
occurrences and their nested defining occurrences. No operation resolves a named reference
by importing a member from another Frame or treats another Frame's local identity as its
own.

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

A `ClosedSpan` (below), rather than a second public Span-by-value type living inside
FrameSpace, is the exchange form for a Span with its endpoint Frames. Replacing a Frame
through FrameSpace checks all incident Spans before returning the replacement collection.

### Span validity is relative to a FrameSpace

A Frame and a Span are not symmetric containers. A Frame's closure invariant — every
local reference resolves inside its own registry — is a closed predicate: it is decidable
from the Frame value alone, which is exactly what lets a Frame stand alone before it is
registered in any FrameSpace. A Span's pair-endpoint invariant is open: a Span stores only
its left and right Frame identities, so whether a pair endpoint exists is undecidable
without a `FrameIdentity -> Frame` lookup, which only a FrameSpace supplies. A Span is
better understood as a claim about two Frames it does not possess, confirmed only relative
to a FrameSpace, rather than as a value with standalone validity the way a Frame has one.

This is also a cardinality argument, independent of the decidability one above. A Frame
participating in many Spans is a many-to-many relationship; ownership is exclusive
containment and can only express one-to-many. A Span that owned its two Frames by value
would either forbid a Frame from joining a second Span or embed a separate Frame copy per
owning Span that a later Frame update has no shared coordinator to keep synchronized.
Storing Frame identities and resolving them through FrameSpace is the same move RFC-011
makes at the schema layer: `bundle_pair` is a join table with two foreign keys into
`frame_row`, not a column embedded in it, because Frame-to-Span is many-to-many and a join
relation is the only structure that represents that without embedding.

The initial API should reflect this asymmetry directly. A raw, freely constructible span
record — identity, left and right Frame identity, and an unvalidated Bundle of candidate
pairs — is distinct from the confirmed Span that `addSpan` or `lookupSpan` returns.
Mirroring Frame's own `PatternLike`/registry-entry distinction, only the FrameSpace-
confirmed form should be named `Span`; holding a `Span` value should itself be evidence
that its pairs already resolved against some FrameSpace, not merely a record shaped like
one.

### ClosedSpan: Span validity without a FrameSpace

Frame's closure invariant is already decidable from a Frame value alone (previous section),
so Frame needs no separate "closed" counterpart — it already is one. A Span's pair-endpoint
invariant only appears open because `Span` stores Frame *identities* rather than Frame
*values*. Given the two actual Frame values a Span relates, pair-endpoint resolution is a
purely local check against their own registries; it needs nothing from a broader FrameSpace.

`ClosedSpan` makes that explicit: a Span-shaped value built directly from two Frame values
and a Bundle of candidate pairs, validated against those two Frames alone.

```text
closedSpan :: Subject -> Frame -> Frame -> Bundle -> Either SpanError ClosedSpan
```

`ClosedSpan` is independently valid outside any FrameSpace, the same way Frame already is.
`closeSpan :: Span -> FrameSpace -> ClosedSpan` remains available as a convenience — resolve
a confirmed Span's two Frame identities through a FrameSpace, then call `closedSpan` — but
FrameSpace involvement is optional plumbing for `ClosedSpan`, not a requirement.

This sharpens what FrameSpace is actually for. It is not required to validate a Span's
pairs — that only ever needed two Frame values. FrameSpace exists to let a Span reference a
Frame by identity rather than by value, so one Frame can be related by many Spans without
duplication, and to track which Spans must be revalidated when a Frame identity's current
value later changes. Neither Frame nor ClosedSpan needs that tracking, because neither one
references anything by identity that could later move: a Frame is a value, and a ClosedSpan
embeds values, not identities.

`Span` and `ClosedSpan` therefore serve different, non-overlapping roles rather than one
superseding the other. `Span` answers "what does this relationship mean right now,"
re-resolved against whatever FrameSpace it is given each time; it can never go stale because
it never freezes anything. `ClosedSpan` answers "what did this relationship mean at the
moment it was closed" — a permanently valid, self-contained record, useful for
serialization, export, or any consumer without a live FrameSpace, but unable by construction
to observe a later Frame update.

### Correlating Frames by shared local identity

Two independently ingested Frames may reuse the same named local identity for what is, in
the source domain, the same real-world thing — while the Frame model's own namespacing rule
treats that as coincidence, not correlation: equal named local identities in different Frame
identities remain different members until something explicit says otherwise. A convenience
constructor can turn that coincidence into an explicit correlation, without changing that
rule.

Graph-algebra combinators are close, but not exact, here. `overlay` (Mokhov, "An Algebra of
Graphs") adds no new edges between the graphs it combines — applied to two Frames it would
only give back two unrelated Frames, which is already true without it. `connect` adds an
edge from every vertex of the first graph to every vertex of the second, which is too
indiscriminate: it would pair every member of one Frame with every member of the other, not
just the ones that actually correspond. The useful operation is a filtered `connect` — a
natural join on matching named local identities:

```text
correlateByIdentity :: Frame -> Frame -> Bundle
```

For each named local identity occurring in both Frames, `correlateByIdentity` proposes one
candidate pair relating them. It produces candidates only: consistent with pair admission
requiring a non-anonymous PairLocalIdentity, and with Attach's rule that coincident local
identities never trigger cross-Frame merging, a caller must assign each candidate its own
PairLocalIdentity before `closedSpan` or `addSpan` will accept it. Matching symbols are a
construction hint, never an automatic merge.

`correlateByIdentity` is one convenience constructor among several ways to populate a
Bundle, not the general case. Manual pairing remains necessary whenever a correspondence
holds between differently named members, as in the aircraft/maintenance exercise's
`engine`/`inspect-engine` pairing.

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

addSpan     SpanDraft -> FrameSpace -> Either FrameSpaceError FrameSpace
updateSpan  SpanIdentity
            (Span -> Either SpanError SpanDraft)
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

`addSpan` and `updateSpan` take a `SpanDraft` — the raw, unvalidated shape described above,
not the confirmed `Span` type — and return a confirmed `Span` only once its pairs resolve
against the current FrameSpace. Holding a `Span` value is therefore itself evidence that
its pairs already resolved; no operation here accepts a bare `Span` as input.

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
pairs and not an independent serializable entity:

```text
Pair   = PatternRow
Bundle = [Pair]
```

A `Pair` is a `PatternRow`: the same Subject-plus-ordered-references shape as a Frame
registry entry, conventionally constrained to exactly two elements — enforced at
admission, not by the type — with the Frame each position resolves against supplied
externally by the owning Span rather than by containment. `Bundle`'s canonical shape is
the ordered list, matching `Pattern`'s own ordered-elements convention; unlike
`FrameRegistry`, there is no cycle among pairs forcing a keyed form to be canonical, so
order is preserved rather than given up. Bundle admission maintains a derived
`Map PairLocalIdentity Pair` index for uniqueness checking and lookup — an optimization
over the canonical ordered list, not a replacement for it. The root Subject supplies the
pair's local identity, labels, and properties. Its identity is the `PairLocalIdentity` —
the same relationship a Frame's identifying Subject has to its `FrameIdentity` — and is
scoped by its containing Span:

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

Each endpoint is an atomic named local reference: it has a non-anonymous local identity and
no labels, properties, or nested elements. The Span supplies its Frame scope by position:

```text
pair.elements[0] = ScopedAddress(Span.leftFrame, Named localIdentity)
pair.elements[1] = ScopedAddress(Span.rightFrame, Named localIdentity)
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
the existing registry, adds missing members, reconciles matching named identities with the
selected `Pattern.Reconcile` policy, and retains existing members omitted from the incoming
batch. FrameSpace validates incident Span endpoints after either mode.

Frame identity and named member identities remain stable after admission. A FrameSpace
requires each admitted Frame identity to be non-anonymous and unique. Reconciliation may
change a member's labels, properties, and ordered local references, but it does not rename
that member or its Frame. Positional addresses are derived structural locations and may
change when their containing structure changes; anonymous members cannot serve as Span
pair endpoints or stable external keys. `Replace` may remove a member only when no local
reference or incident Bundle pair addresses it; otherwise FrameSpace rejects the
replacement.

Containment attachment is not a reconciliation mode. `Attach targetLocalAddress
incomingDefinitions` admits definitions into the destination Frame namespace, then adds
the selected incoming root addresses to the target member's ordered element sequence. It
rejects direct self-reference, unresolved references, and identity conflicts. Content
from a different Frame must be imported or rebased into the destination namespace before
attachment; coincident local identities never trigger cross-Frame merging.

`importSubgraph` copies selected source roots and their complete transitive local-reference
closure into a destination Frame without changing the source. It preserves each named local
identity when it is unoccupied in the destination and rejects collisions by default.
Anonymous members receive destination-relative positional addresses as their containment is
rebuilt. An import plan maps any colliding source named local identity to its destination
address and must cover every reachable named member. The mapping is injective for newly
imported named members and rewrites every imported local Reference consistently.

Mapping a source identity onto an existing destination identity is an explicit request to
merge those members. That deliberate merge delegates content conflicts to the selected
`Pattern.Reconcile` policy. A caller can attach imported roots only after the resulting
destination Frame passes local closure validation.

Reconciliation, import/rebase, and attachment operate on distinct axes.
`Replace` and `Additive` reconcile temporal versions of one Frame identity. `importSubgraph`
copies content across Frame namespaces through an explicit address map while preserving the
source Frame. `Attach` changes containment among content already in the destination
namespace. Because its input references use only named `LocalIdentity` values, Attach cannot
create a cross-Frame reference; cross-Frame relationships require a Span pair.

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
7. **Correlate-by-identity ambiguity.** `correlateByIdentity` needs a policy for a named
   identity occurring as a candidate correspondence more than once, or for a candidate that
   conflicts with an existing pair's endpoints. Resolve the conflict policy and
   PairLocalIdentity assignment convention once a concrete correlation workflow exists.

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
   `ScopedAddress` and PairAddress versus `src_frame`/`src_ref` and `tgt_frame`/`tgt_ref`.
4. Mark ADR-001 superseded or rewrite it against the resulting RFC; it currently assumes
   cache-bearing wrappers and a Pattern-shaped Bundle.
5. Create a new ADR for the Haskell registry representation, error types, FrameSpace
   ownership API, and the `Pattern.Reconcile` adapter.
