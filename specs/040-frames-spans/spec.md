# Feature Specification: Frames and Spans

**Feature Branch**: `040-frames-spans`
**Created**: 2026-06-20
**Status**: Draft
**Input**: User description: "frames & spans as described in design/rfc/RFC-001-frames-and-spans.md"

## Overview

Pattern Subject is a uniform recursive container, but it offers no shared vocabulary
for the two moves analysts repeatedly make over it: treating a Pattern as a
self-contained **module** they can navigate within, and **relating** modules into
larger structures without breaking either module's encapsulation. This feature adds
that vocabulary as two typed views over `Pattern Subject` — **Frame** (the module) and
**Span** (the relationship between modules) — together with a **Bundle** (the
externalized cross-module correspondences) and the **Portal** role (a Subject that
participates in a correspondence).

A Frame also establishes the identity namespace for its contents. Subject identities are
local within a Frame; outside it, a member is identified by the combination of the Frame's
identity and the member's local Subject identity. This lets independently parsed documents
reuse human-chosen or generated identities without collision and without rewriting the
identities stored in either document.

The organizing principle is *externality applied recursively*: just as a property-graph
node does not contain its relationships, a Frame does not contain its cross-Frame
correspondences — the Span does. Crossing from one module to another is always an
explicit Span operation, never a reach through a module's boundary. The substrate
(`Pattern v`) is unchanged; Frame and Span are views defined on top of it, and either
can be unwrapped back to a plain Pattern Subject for substrate-level composition.

## Clarifications

### Session 2026-06-20

- Q: How should the new Frame interface relate to the existing `ScopeQuery` typeclass (RFC-006, used by RepresentationMap)? → A: `ScopeQuery` is re-expressed as an alias over the new Frame operations — one implementation, two names; existing callers keep working unchanged.
- Q: How should Frame reconciliation relate to the existing identity-aware `Pattern.Reconcile` engine (RFC-010)? → A: Frame reconciliation builds on `Pattern.Reconcile`, mapping its Frame modes/policies onto the existing engine rather than duplicating identity-merge logic.
- Q: Should `framePara` (whole-Frame ambient-context paramorphism) reuse the existing scoped paramorphism (`paraWithScope`, RFC-006)? → A: `framePara` is implemented as a new, independent fold primitive distinct from `paraWithScope`.

### Session 2026-09-07

- Q: Does scoped identity belong in Feature 040 or in a separate future feature? → A: It belongs here. A Frame is the identity namespace; local `Subject.identity` values remain unchanged, while identities used outside a Frame are qualified by the Frame identity.
- Q: How are independently parsed `.gram` documents kept distinct when both contain the same explicit or generated identities? → A: Each document is assigned a stable, non-anonymous Frame identity at ingestion. Generated Subject identities need only be unique within that Frame and may recur in another Frame.
- Q: How should Span operations identify a Portal when both endpoint Frames contain the same local Subject identity? → A: Cross-Frame operations use the Portal's scoped identity (Frame identity plus local Subject identity), never the local symbol alone.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Navigate within a self-contained module (Priority: P1)

An analyst holds a Pattern Subject that represents a coherent module — say the
mechanical aspect of an airplane, or a sheet in a workbook — and wants to treat it as a
navigable unit: search it by arbitrary criteria, ask what contains what, list
co-members, and fold over it while always seeing the whole module as context. They wrap
the Pattern as a Frame and run within-module operations that are guaranteed to stay
inside the module.

**Why this priority**: This is the modularization move and the foundation everything
else builds on. A Frame with within-module navigation is independently useful and is the
minimum viable slice — it delivers value (scoped, encapsulated navigation) even if Spans
are never built.

**Independent Test**: Wrap a Pattern Subject as a Frame; run find-by-predicate,
containers, siblings, and the ambient-context paramorphism; confirm results are correct,
in traversal order, computed against the whole module (not a narrowed subtree), and never
include anything outside the module. Unwrap the Frame and confirm the original Pattern is
recovered unchanged.

**Acceptance Scenarios**:

1. **Given** a Pattern Subject with nested members, **When** it is wrapped as a Frame and
   searched with a predicate, **Then** all matching sub-Patterns are returned in
   traversal order and no non-matching ones are.
2. **Given** a Frame and a sub-Pattern within it, **When** containers and siblings are
   requested, **Then** the answers are computed relative to the whole Frame, not the
   sub-Pattern's local subtree.
3. **Given** a Frame, **When** the ambient-context paramorphism runs, **Then** the step
   function sees the same whole Frame at every level of the recursion (the Frame is never
   narrowed as descent proceeds).
4. **Given** a Frame, **When** it is unwrapped to its underlying Pattern Subject, **Then**
   the result equals the Pattern that was wrapped (lossless round-trip).
5. **Given** a Frame, **When** a top-level element is added or removed by Subject
   identity, **Then** a new Frame is produced with exactly that element added/removed and
   all others unchanged.

---

### User Story 2 - Combine independently named documents without collision (Priority: P1)

An analyst ingests several `.gram` documents whose authors chose identities independently.
The files may both define `alice`, and identity assignment may generate `#1` in each. The
analyst assigns each document a stable Frame identity and combines the Frames without
rewriting either document's local Subject identities or accidentally treating equal local
symbols as the same entity.

**Why this priority**: Frame is the identity boundary as well as the navigation boundary.
Without scoped identity, combining real source documents can silently merge unrelated
entities, and identity-keyed persistence cannot safely upsert multi-Frame data.

**Independent Test**: Build two Frames with different Frame identities but the same local
member identities, including generated identities; place them in one enclosing Pattern and
confirm that cross-Frame lookup distinguishes every member by `(Frame identity, local
identity)`, while unwrapping either Frame preserves its local identities exactly.

**Acceptance Scenarios**:

1. **Given** two Frames with different Frame identities and members sharing the local
   identity `alice`, **When** the members are addressed outside their Frames, **Then** their
   scoped identities are distinct.
2. **Given** two independently parsed documents that each generate `#1`, **When** each is
   assigned a distinct Frame identity and the Frames are combined, **Then** neither `#1`
   collides with or reconciles into the other.
3. **Given** a scoped identity and a Frame whose identity does not match its Frame component,
   **When** scoped lookup is attempted, **Then** the lookup reports no match rather than
   comparing only the local identity.
4. **Given** a Frame with local identities, **When** the Frame is wrapped, qualified, and
   unwrapped, **Then** no local Subject identity has been prefixed, rewritten, or otherwise
   changed.
5. **Given** the same stable Frame identity ingested again, **When** its members are
   reconciled, **Then** equal local identities are treated as updates within that namespace;
   a different Frame identity creates a separate namespace.

---

### User Story 3 - Relate modules and cross between them (Priority: P2)

An analyst has two modules — a lower-detail Frame and a higher-level summary Frame, or
two peer Frames — and wants to assert a relationship between them and record
element-level correspondences, then follow those correspondences from a summary entity
(a Portal) back to what it summarizes in the other module. The correspondences must live
in the relationship, not inside either module, so that neither module knows it has been
related.

**Why this priority**: This is the composition move and the heart of the design's
value — disentangling graphs-of-graphs and crossing module boundaries safely. It depends
on Frames (P1) existing but adds the distinctive capability (externalized cross-module
edges, explicit crossing) that motivates the whole feature.

**Independent Test**: Build two Frames; relate them as a bare Span and confirm its two
endpoints resolve without external lookup; relate them with a Bundle of pair-elements;
given a Portal's scoped identity, follow refinement across the Span and confirm exactly its paired
correspondents are returned; confirm neither Frame contains any cross-Frame edge.

**Acceptance Scenarios**:

1. **Given** two Frames, **When** they are related as a bare Span, **Then** the Span
   identifies both endpoints by value and asserts the relationship with no element-level
   detail.
2. **Given** two Frames and a list of paired Patterns, **When** a Bundle is built and
   wrapped with the Frames into a Span, **Then** each pairing appears as a pair-element in
   the Bundle and is reachable only through the Span, not through either Frame.
3. **Given** a bundled Span whose two Frames reuse a local identity and a Portal that appears
   as an endpoint of one or more pair-elements, **When** refinement is requested using that
   Portal's Frame and local identity, **Then** exactly the correspondents paired with that
   scoped endpoint are returned.
4. **Given** a Subject that participates in a Bundle, **When** its Portal-hood is checked,
   **Then** it is recognized as a Portal by its participation in a pair-element, not by any
   label or marker on the Subject itself.
5. **Given** two Frames already related by one Span, **When** a second, differently-named
   Span relates the same two Frames, **Then** both Spans coexist with neither privileged.
6. **Given** a summarization workflow (select members of a lower Frame, summarize them as
   fresh atomic Portals in a higher Frame, record the pairings in a Bundle, wrap as a
   Span), **When** the higher Frame is traversed, **Then** it is self-contained (its
   Portals are atomic) and the correspondence back to the sources lives entirely in the
   Span's Bundle.

---

### User Story 4 - Transform and merge modules at boundaries (Priority: P3)

An analyst maintains modules over time: filtering a Frame to a sub-module, combining two
Frames by element set, rewriting every member through a projection, and — at an I/O
boundary where new data arrives — merging an incoming Frame into an existing one by
Subject identity under a chosen conflict policy.

**Why this priority**: These are the lifecycle operations that make Frames usable in a
real workflow, but they are refinements on top of the core navigate (P1) and relate (P2)
capabilities. They can be delivered after the MVP without blocking it.

**Independent Test**: Apply each Frame-to-Frame transform and confirm the result is a new
Frame with the expected members materialized; reconcile two Frames in each mode under
each basic policy and confirm the merged Frame matches the mode/policy contract.

**Acceptance Scenarios**:

1. **Given** a Frame, **When** it is filtered by a predicate, **Then** a new Frame is
   produced containing exactly the matching elements as real (materialized) members.
2. **Given** two versions of the same Frame namespace, **When** they are intersected or
   unioned, **Then** the resulting Frame's element set is the intersection/union by local
   Subject identity; applying either operation directly to differently scoped Frames reports
   a scope mismatch and requires an explicit import or rebase.
3. **Given** a Frame and an element-wise rewrite function, **When** the rewrite is
   applied, **Then** every member is transformed and the result is a new Frame.
4. **Given** an existing Frame and incoming data targeting its namespace and sharing some
   local Subject identities, **When** reconciliation runs in 1:1 / additive / subsumed mode
   under a conflict policy, **Then** the merged Frame reflects that mode's contract and
   resolves shared-identity conflicts per the policy (prefer-incoming, prefer-existing,
   union, or error), without merging identities from any other Frame namespace.

---

### Edge Cases

- **Non-conforming Span shape**: a relationship-shaped Pattern with other than two or
  three elements has undefined interpretation under the Span convention. The framework
  does not enforce the 2-or-3 shape; behavior on malformed shapes is not a correctness
  guarantee.
- **Non-injective summarization**: when a caller's summarize step maps two different
  sub-Patterns to the same Portal Subject, the Bundle holds two pair-elements pointing at
  the same Portal. The framework records what the caller produced; coherence is the
  caller's responsibility.
- **Refinement with no Bundle**: refining over a bare Span (no Bundle) returns no
  correspondents.
- **Refinement of an unpaired Subject**: a Subject that participates in no pair-element
  has no correspondents and is not a Portal.
- **Empty Frame**: a Frame with no members supports all within-module operations and
  returns empty results where applicable.
- **Self-reference / cycles among members**: within-module traversal terminates and emits
  each member appropriately even when members reference each other.
- **By-reference inter-Frame form**: when Frames or Spans are encoded by reference
  (atomic Patterns whose Subjects identify entities elsewhere), resolving them to full
  Patterns requires an external lookup that may be absent; resolution is a consumer
  concern, not guaranteed by the view.
- **Reconciliation conflict under error policy**: when two Subjects share an identity but
  differ and the policy is "error", reconciliation reports the conflict rather than
  silently merging.
- **Anonymous Frame root**: a Frame without a non-anonymous root identity may still support
  purely local navigation, but cannot supply a stable scoped identity for cross-Frame lookup,
  Span endpoints, or persistence; those operations report the missing namespace rather than
  inventing one.
- **Duplicate Frame identities**: two independently sourced Frames assigned the same Frame
  identity occupy the same namespace and are treated as versions requiring reconciliation,
  not as automatically distinct modules. Callers must assign different Frame identities when
  the sources are independent.
- **Anonymous members**: multiple anonymous Subjects in one Frame are not distinguishable by
  identity. Before identity-bearing lookup, reconciliation, Span pairing, or persistence,
  they must receive identities unique within that Frame.
- **Same local identity on both sides of a Span**: a local symbol alone is ambiguous; Portal
  lookup and refinement use the Frame component to select the intended endpoint.
- **Set operations across namespaces**: intersection and union do not compare bare local
  symbols from differently scoped Frames. The caller must explicitly import/rebase into a
  chosen namespace or operate on Frames representing the same namespace.

## Requirements *(mandatory)*

### Functional Requirements

**Modularization — Frame (within-module)**

- **FR-001**: The framework MUST let a `Pattern Subject` be designated as a self-contained
  module (a Frame) whose root Subject is the module's identifying entity and whose elements
  are the module's members.
- **FR-002**: The framework MUST support both designating a module with optional
  precomputation that speeds later lookups, and designating an existing Pattern as a module
  without recomputation.
- **FR-003**: The framework MUST provide within-module search by an arbitrary predicate
  over Patterns, returning matches in traversal order; by-identity, by-label, by-property,
  and by-shape lookups are compositions of this search, not separate privileged operations.
- **FR-004**: The framework MUST answer "what contains this member" and "what are this
  member's co-members" relative to the whole module, not a narrowed local subtree.
- **FR-005**: The framework MUST provide a paramorphism over a module in which the step
  function receives the whole module as ambient context at every step of the recursion
  (the module is never narrowed during descent). This is a new, independent fold primitive,
  distinct from the existing scoped paramorphism.
- **FR-006**: The framework MUST support adding and removing a top-level member by Subject
  identity, producing a new module.
- **FR-007**: Within-module operations MUST never return or descend into members of other
  modules; encapsulation follows from module self-containment, not from special suppression
  behavior.
- **FR-008**: A Frame MUST unwrap to its underlying `Pattern Subject` losslessly, so the
  wrapped Pattern is recoverable unchanged.

**Composition — Span, Bundle, Portal (cross-module)**

- **FR-009**: The framework MUST let two Frames be related as a bare Span identified by a
  Subject, asserting a relationship with no element-level detail.
- **FR-010**: The framework MUST let two Frames be related with a Bundle providing
  element-level pairwise correspondences (pair-elements) between members of the two Frames.
- **FR-011**: The framework MUST construct a Bundle from a list of paired Patterns, where
  pair-element Subjects are anonymous by default and may optionally carry an explicit
  Subject when a pairing needs identity or properties.
- **FR-012**: A Bundle MUST be a first-class Pattern usable standalone, as the third
  element of a Span, or referenced by multiple Spans.
- **FR-013**: Cross-Frame correspondences MUST live in the Span's Bundle and MUST NOT be
  stored inside either related Frame; neither Frame knows it is spanned.
- **FR-014**: The Frames a Span relates MUST be present by value, so resolving a Span's
  endpoints needs no external lookup.
- **FR-015**: The framework MUST expose a Span's two Frames and its optional Bundle, a
  Bundle's pair-elements and their local endpoint identity pairs, and the corresponding
  Frame-qualified endpoint identity pairs when the Bundle is viewed through a Span.
- **FR-016**: The framework MUST support refinement: given a Span and a Portal identified by
  Frame identity plus local Subject identity, return the correspondents the Portal is paired
  with in the other Frame.
- **FR-017**: Portal-hood MUST be determined by a Subject's participation as an endpoint of
  a Bundle pair-element, not by any label or marker on the Subject; the same Subject may be
  a Portal in multiple Bundles.
- **FR-018**: The framework MUST allow any number of Spans between the same two Frames,
  with none privileged.
- **FR-019**: Frames and Spans MUST remain composable as ordinary Patterns (e.g. a Pattern
  whose elements are Frames and Spans), so inter-Frame structure nests without bound and
  without machinery parallel to ordinary Pattern composition.

**Frame-to-Frame transforms and reconciliation**

- **FR-020**: The framework MUST provide Frame-to-Frame transforms that produce new Frames
  with members materialized: filtering by predicate, element-set intersection and union (by
  Subject identity), and element-wise rewriting. Specialized variants are caller
  compositions, not primitives. Identity-based set operations MUST operate within one Frame
  namespace; differently scoped inputs require an explicit import or rebase into a chosen
  destination namespace and MUST otherwise report a scope mismatch.
- **FR-021**: The framework MUST provide reconciliation that merges two Frames by Subject
  identity in three modes — 1:1, additive, and subsumed — under a conflict-resolution policy
  with at least prefer-incoming, prefer-existing, union, and error policies, and MUST allow
  custom policies. This reconciliation MUST build on the existing identity-aware
  reconciliation engine, mapping the Frame modes and policies onto it rather than duplicating
  identity-merge logic. Equal local identities from different Frame namespaces MUST NOT be
  treated as duplicates unless a boundary operation explicitly imports one Frame into the
  other's namespace.
- **FR-022**: Reconciliation MUST be expressible as a boundary operation, such that internal
  operations may assume their Frames are already reconciled.

**Integration and continuity**

- **FR-023**: Because a Frame and a Span each unwrap to a `Pattern Subject`, the existing
  graph-interpretation capabilities MUST apply to their content directly without conversion.
- **FR-024**: The existing scope-aware-navigation interface MUST be re-expressed as an alias
  over the new Frame operations — a single underlying implementation exposed under both names
  — so that existing callers continue to work unchanged while new code targets the Frame
  interface.
- **FR-025**: The framework MUST allow both by-value and by-reference inter-Frame encodings
  as ordinary Patterns; resolving by-reference encodings to full Patterns is a consumer
  concern requiring an external lookup, not a guarantee of the view.

**Scoped identity and multi-document ingestion**

- **FR-026**: A Frame's root Subject identity MUST define the namespace for every Subject
  contained by that Frame.
- **FR-027**: The framework MUST represent a member's identity outside its Frame as the
  combination of the Frame identity and the member's local `Subject.identity`.
- **FR-028**: Subjects with equal local identities in differently identified Frames MUST be
  treated as distinct; equal local identities in the same Frame namespace identify the same
  entity for reconciliation purposes.
- **FR-029**: Qualifying an identity MUST NOT prefix, rewrite, or otherwise mutate the local
  identity stored in the underlying `Pattern Subject`, and wrapping then unwrapping a Frame
  MUST preserve all local identities exactly.
- **FR-030**: The framework MUST support assigning an explicit, stable, non-anonymous Frame
  identity when an independently parsed document enters a multi-Frame composition or
  persistence boundary.
- **FR-031**: Anonymous Subjects used in identity-bearing operations MUST be assignable
  identities that are unique within their Frame; generated identities MAY recur in other
  Frames without collision.
- **FR-032**: Every cross-Frame member reference, including Span refinement and persistence
  references, MUST carry both Frame and local identity; an operation given a mismatched Frame
  component MUST report no match or an identity-scope error rather than falling back to the
  local identity.
- **FR-033**: Re-ingesting data under the same stable Frame identity MUST target the existing
  namespace and invoke reconciliation, while ingesting under a different Frame identity MUST
  create a distinct namespace regardless of overlapping local identities.

### Key Entities

- **Frame**: A typed view over a self-contained `Pattern Subject` module. Its root Subject
  identifies the module; its elements are the members. Carries within-module operations and
  may carry optional lookup-accelerating precomputation.
- **Span**: A typed view over a relationship-shaped `Pattern Subject` whose first two
  elements are the related Frames (by value) and whose optional third element is a Bundle.
  Identified by its own root Subject; carries cross-module operations.
- **Bundle**: A first-class Pattern whose elements are pair-elements — relationship-shaped
  correspondences each pairing one member of one Frame with one member of the other. Holds
  the cross-Frame edges externally to both Frames.
- **Pair-element**: A single correspondence within a Bundle, of the form (source)→(target),
  with an anonymous or explicit Subject.
- **Portal**: The role a Subject plays by appearing as an endpoint of a pair-element; not a
  type or a label, but a participation fact discoverable by Bundle traversal.
- **Scoped identity**: The external identity of a Frame member, composed from the Frame's root
  Subject identity and the member's local `Subject.identity`. It is contextual metadata, not a
  rewrite of the Subject stored in the Pattern.
- **Pattern Subject / Subject**: The substrate working type (a value with ordered element
  Patterns) and its identity-bearing value (identity symbol, labels, properties). Unchanged
  by this feature.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 100% of within-module operations (search, containers, siblings,
  paramorphism) return only members of the operated-on module — never any element of
  another module — across the full test corpus.
- **SC-002**: Wrapping a Pattern Subject as a Frame and unwrapping it is a verified
  round-trip identity for every pattern in the test corpus.
- **SC-003**: For every bundled Span in the test set, refinement of a Portal returns
  exactly the set of correspondents it is paired with — no more, no fewer.
- **SC-004**: For every Span, no cross-Frame correspondence is found inside either related
  Frame; all cross-Frame edges are located only in the Span's Bundle (verified structurally
  across the test set).
- **SC-005**: Containers, siblings, and the paramorphism produce identical answers whether
  evaluated on a small or a large enclosing module for the same target member — confirming
  answers are scoped to the whole module rather than a local subtree.
- **SC-006**: Each reconciliation mode (1:1, additive, subsumed) under each basic policy
  produces a merged Frame matching its documented contract on a suite of overlapping-Frame
  cases, including correct conflict handling under the error policy.
- **SC-007**: The full existing pattern-hs test suite continues to pass unchanged after the
  feature is introduced (no existing call sites require changes).
- **SC-008**: An analyst can express the motivating graphs-of-graphs query ("the X aspects
  of Y components subject to procedure P owned by Z") purely as within-Frame navigation plus
  across-Span traversal, with no special-purpose graph-of-graphs machinery.
- **SC-009**: Across a corpus of independently parsed document pairs with overlapping explicit
  and generated local identities, 100% of members remain distinct when their Frame identities
  differ, and 100% reconcile as same-namespace candidates when their Frame identities match.
- **SC-010**: For every Frame in the identity test corpus, identity qualification followed by
  unwrapping leaves every local `Subject.identity` byte-for-byte unchanged.
- **SC-011**: Every tested cross-Frame lookup and Span refinement selects the correct endpoint
  when both Frames contain the same local identity, and rejects a mismatched Frame component.

## Assumptions

- **One type per view, optional internal acceleration**: Frame and Span are each a single
  type whose optional lookup acceleration is an internal detail; a view without acceleration
  is the baseline. (Resolves RFC-001 Open Question 3 in favor of same-type.)
- **Bare-Pattern navigation offered**: within-module navigation is also available on an
  unwrapped `Pattern Subject` for convenience, accepting that self-containment is a usage
  property rather than a type-enforced invariant. (RFC-001 Open Question 4.)
- **Narrow Span/Bundle interfaces**: composition and inversion of Spans, and richer Bundle
  operations, are ordinary functions rather than part of the minimal view interfaces.
  (RFC-001 Open Question 5.)
- **Direction and pair-element identity are conventions**: pair-elements may be written
  directionally; symmetric relationships use the directed form with direction treated as
  informational. Whether a pairing gains an explicit identity is the caller's choice, with
  constructors for both anonymous and named forms. (RFC-001 Open Questions 1 and 2.)
- **Existing scope-aware navigation aliases the Frame interface**: the prior scope-query
  capability is re-expressed as an alias over the new Frame operations (one implementation,
  two names) so dependent features are not broken; new code targets the Frame interface. (See
  Clarifications and FR-024.)
- **`framePara` is a distinct primitive**: it is implemented independently of the existing
  scoped paramorphism even though their semantics are expected to coincide. (See
  Clarifications and FR-005.) *Planner note: confirm the two folds are not silently divergent;
  consider sharing a tested core if they prove identical.*
- **Frame identity is the namespace**: the root Subject identity names the Frame and scopes
  every contained Subject identity. A Frame used across module or persistence boundaries has
  a stable, non-anonymous identity unique in that enclosing context.
- **Qualification is contextual and lossless**: scoped identity is the pair `(Frame identity,
  local Subject identity)`. Local symbols are preserved rather than globally prefixed or
  rewritten.
- **Anonymous identity assignment remains local**: existing gram identity assignment may run
  independently for each source document. Generated symbols need only be unique inside the
  resulting Frame.

## Dependencies

- The `Pattern Subject` substrate and its existing traversal, construction, and
  graph-interpretation capabilities (the substrate is used, not modified).
- The existing identity-aware reconciliation engine (`Pattern.Reconcile`), which Frame
  reconciliation builds on directly — Frame modes/policies map onto it (see FR-021).
- The existing scope-aware-navigation interface, which is re-expressed as an alias over the
  Frame operations (see FR-024).
- The existing gram identity-assignment capability, used to assign distinct local identities
  to anonymous Subjects before identity-bearing Frame operations when required.

## Out of Scope

- **Shape validation / schema enforcement** for Spans and Bundles (the 2-or-3-element Span
  shape and pair-element structure are interpretive conventions, pending a broader
  schema-conventions effort).
- **Concrete acceleration/cache designs** for the views (representation, build timing, and
  invalidation are deferred until there is a workload to design against).
- **Persistent, lazy, or database-backed Frames** (a layer above pattern-hs; the substrate
  is in-memory).
- **Global rewriting or process-wide uniqueness of local Subject identities**; this feature
  preserves local symbols and qualifies them contextually with their Frame identity.
- **RepresentationMap and pattern-equivalence integration** (roadmap items the Frame/Span
  model enables later but does not deliver here).
- **Reconciliation across Spans** (re-pointing a Bundle's pair-elements when related Frames
  are reconciled) — a separate boundary concern awaiting use cases.
- **Domain-specific reconciliation policies** beyond the basic four (consumer concerns).
</content>
