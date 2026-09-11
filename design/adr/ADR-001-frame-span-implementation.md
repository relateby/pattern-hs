---
id: ADR-001
title: Frame, Span, and FrameSpace Implementation Model
status: proposed
rfcs: [RFC-001]
created: 2026-09-07
specs: []
---

# ADR-001: Frame, Span, and FrameSpace Implementation Model

This ADR is a full rewrite of its own prior content. The original decision specified
`Frame`/`Span` as cache-bearing `newtype`s over `Pattern Subject`, matching RFC-001's
original wrapper/view model. RFC-001 has since been rewritten around managed, referentially
closed containers; this ADR now specifies that model instead. [SPIKE-001](
../spikes/SPIKE-001-frame-registry/SPIKE.md) and [SPIKE-002](
../spikes/SPIKE-002-frame-document-diversity/SPIKE.md) prototyped and validated the
mechanisms below before this rewrite; both are cited by name where a decision reuses their
validated approach rather than inventing one.

## 1. Context

RFC-001 defines Frame as a registry container — an identifying Subject admitting
`PatternLike` definitions into one referentially closed `FrameRegistry`, keyed by
`LocalAddress` rather than by `Subject.identity` — Span as a container relating two Frame
identities through a Bundle of pair relationships, and FrameSpace as the aggregate owner of
cross-Frame integrity. None of these are wrappers over `Pattern Subject`; none of the prior
ADR's `Frame { framePattern :: Pattern Subject, frameIdentityIndex :: ... }` representation
or `ScopeQuery`-instance strategy applies.

Two things are already validated by spike code, not merely specified in prose:

- SPIKE-002 found and fixed a genuine algorithmic trap: a first attempt at admission that
  recursed into children, then re-walked them to recompute their addresses, cannot recover
  an anonymous child's real assigned ordinal (ordinals are assigned by threading state
  through admission, not derivable from the value alone). The correct shape is a single
  pass that returns the address an occurrence resolves to, threaded directly into its
  parent's element list. This ADR's admission algorithm (§2.2) is that corrected shape.
- SPIKE-001 validated FrameSpace's integrity operations — `addSpan`, Frame replacement
  checked against incident Spans, `rebindPair`, `importSubgraph`, `attach` — against a
  hand-designed aircraft/maintenance workflow (17 PASS assertions), using an earlier
  identity representation (bare `Subject.Symbol`, generated `#spike-N` anonymous
  identities) that RFC-001 has since superseded. This ADR carries SPIKE-001's validated
  operation *shapes* forward, re-typed against `LocalAddress` and the `SpanDraft`/`Span`
  split RFC-001 now requires.

`Pattern.Reconcile.reconcile` normalizes one `Pattern` by identity and accepts a
`ReconciliationPolicy`, implementing first-write, last-write, merge, and strict conflict
handling plus reference completion and element merge. Frame-level reconciliation (§2.9)
adapts this engine rather than reimplementing identity-merge logic, continuing the prior
ADR's decision on this point.

This ADR does not specify within-Frame query or navigation (RFC-001 Open Question 6):
`find`, `containers`, `siblings`, and `framePara` have no registry-model successors here.
The prior ADR's `ScopeQuery Frame Subject` instance and `FrameOps` alias are **not**
carried forward — `Frame` is no longer a `Pattern Subject`-shaped value that instance could
project over, and RFC-001 treats this as a separate, still-open design problem (§4
Consequences discusses the resulting gap for `RepresentationMap` and `Graph.Transform`).

## 2. Decision

### 2.1 Core address and registry types

```haskell
type LocalIdentity   = Subject.Symbol          -- non-anonymous only
newtype ElementOrdinal = ElementOrdinal Int      deriving (Eq, Ord, Show)
type FrameIdentity    = Subject.Symbol          -- non-anonymous only

data LocalAddress
  = Named LocalIdentity
  | Positional ElementOrdinal
  deriving (Eq, Ord, Show)

data ScopedAddress = ScopedAddress
  { scopedFrame :: FrameIdentity
  , scopedLocal :: LocalAddress
  } deriving (Eq, Ord, Show)

data PatternRow = PatternRow
  { rowSubject  :: Subject.Subject
  , rowElements :: [LocalAddress]
  } deriving (Eq, Show)

type FrameRegistry = Map LocalAddress PatternRow

data Frame = Frame
  { frameSubject     :: Subject.Subject   -- frameSubject's identity is the FrameIdentity
  , frameRegistry    :: FrameRegistry
  , frameNextOrdinal :: Int               -- internal: next Positional ordinal to assign
  }
```

`frameNextOrdinal` is not part of the public API; it exists so admission can assign a
strictly increasing ordinal per Frame without rescanning the registry. `emptyFrame subject`
constructs `Frame subject Map.empty 1`. `frameIdentity :: Frame -> Maybe FrameIdentity`
returns `Just` only when `frameSubject` is non-anonymous; every operation that requires a
`FrameIdentity` (registration in a FrameSpace, Span endpoints, persistence) fails against an
anonymous-rooted Frame rather than inventing one, continuing the prior ADR's rule.

`Map LocalAddress PatternRow` (rather than, say, two separate maps for `Named` and
`Positional` keys) keeps admission, closure validation, and removal single-pathed: every
operation that needs "the row at this address" or "does this address resolve" has one
lookup, regardless of which `LocalAddress` constructor it holds.

### 2.2 `PatternLike` admission

```haskell
data PatternLike
  = Definition Subject.Subject [PatternLike]
  | Reference LocalIdentity
  deriving (Eq, Show)

data FrameError
  = ConflictingDefinition LocalAddress
  | DirectSelfReference LocalAddress
  | UnresolvedReference LocalIdentity
  deriving (Eq, Show)

admitPatternLike
  :: PatternLike
  -> Frame
  -> Either FrameError Frame
```

Admission is single-pass, per SPIKE-002's corrected algorithm: a `Definition` recurses into
its children first, obtaining each child's real assigned `LocalAddress`, then registers its
own `PatternRow` using those addresses — the address a call resolves to is returned
directly to its caller, never recomputed by a later walk. An anonymous `Definition`
receives `Positional (frameNextOrdinal frame)`, incrementing the counter; a named
`Definition` receives `Named identity`, and a second fuller `Definition` at that identity
is `ConflictingDefinition` (deferred to reconciliation, §2.9, not raised here) unless the
existing entry is content-free, in which case it is promoted in place. A `Reference
identity` contributes `Named identity` to its containing row without registering anything
itself; if no `Definition` at that identity exists anywhere in the batch once the whole
batch is admitted, it is promoted to a content-free defining entry (a `PatternRow` with an
empty label/property Subject and no elements) rather than left dangling.

`combinePatternLikes :: [PatternLike] -> Frame -> Either FrameError Frame` admits a batch
of top-level occurrences atomically: every occurrence in the batch is registered before any
`Reference` in the batch is resolved, so forward references and mutual (indirect) cycles
within one batch admit correctly — this is the behavior SPIKE-001's aircraft/maintenance
fixture exercises (`engine -> fuel-system -> fuel-pump -> diagnostic-procedure -> engine`).
`admitPatternLike` is `combinePatternLikes` applied to a singleton list.

Closure validation runs once per admission call, after every occurrence in the batch is
registered: every `LocalAddress` appearing in any `rowElements` must be a key in the
resulting registry (`UnresolvedReference`, though this should not occur once promotion has
run), and no row's `rowElements` may contain its own address (`DirectSelfReference`).
Indirect cycles among distinct addresses are valid registry topology and are not checked
against.

`removePattern :: LocalAddress -> Frame -> Either FrameError Frame` removes a registry
entry only when no other entry's `rowElements` contains its address; otherwise it fails
(the caller detaches the containment link first, an ordinary registry update removing the
address from the containing row's element list — detachment is not a named primitive in
this decision, since it is exactly `Frame { frameRegistry = Map.adjust (remove address) ...
}`, not a separate algorithm).

### 2.3 Raw `Pattern Subject` compatibility import

```haskell
importRawPattern
  :: ReconciliationPolicy SubjectMergeStrategy
  -> Pattern Subject.Subject
  -> Frame
  -> Either FrameError Frame
```

A raw `Pattern Subject` carries no `Definition`/`Reference` tag. `importRawPattern`
recovers the distinction from content, per RFC-001 §Defining and reference occurrences: an
occurrence with an identity and no labels, properties, or elements is a reference
candidate; any content makes it a definition. Two definitions sharing an identity are
deferred to the supplied `Pattern.Reconcile` policy rather than raised as
`ConflictingDefinition` — this path is a more permissive compatibility fallback than
`PatternLike` admission's stricter default, not a second definition of admission. This
decision does not fix the recovery algorithm's exact batch semantics beyond that contract;
an implementation may reuse `admitPatternLike`'s registry-building shape with the
content-based tag recovered per occurrence instead of read from a `PatternLike` value.

### 2.4 Span, SpanDraft, and ClosedSpan

```haskell
type SpanIdentity = Subject.Symbol

data SpanDraft = SpanDraft
  { draftSubject    :: Subject.Subject
  , draftLeftFrame  :: FrameIdentity
  , draftRightFrame :: FrameIdentity
  , draftBundle     :: Bundle
  , draftPolicy     :: PairDispositionPolicy
  }

data Span = Span
  { spanSubject    :: Subject.Subject
  , spanLeftFrame  :: FrameIdentity
  , spanRightFrame :: FrameIdentity
  , spanBundle     :: Bundle
  , spanPolicy     :: PairDispositionPolicy
  }
  deriving (Eq, Show)

data ClosedSpan = ClosedSpan
  { closedSubject    :: Subject.Subject
  , closedLeftFrame  :: Frame
  , closedRightFrame :: Frame
  , closedBundle     :: Bundle
  , closedPolicy     :: PairDispositionPolicy
  }

data SpanError
  = UnresolvedFrame FrameIdentity
  | UnresolvedEndpoint ScopedAddress
  | AnonymousPairIdentity
  | DuplicatePairIdentity PairLocalIdentity
  | EndpointConflict PairLocalIdentity
  deriving (Eq, Show)

closedSpan :: Subject.Subject -> Frame -> Frame -> Bundle -> PairDispositionPolicy -> Either SpanError ClosedSpan
closeSpan  :: Span -> FrameSpace -> Either SpanError ClosedSpan
```

Only `Span` is confirmed; `SpanDraft` is structurally identical but is never returned by any
operation in this decision, only accepted as input, matching RFC-001's rule that holding a
`Span` value is itself evidence its pairs already resolved. `closedSpan` validates a
`Bundle` directly against two `Frame` values with no `FrameSpace` involved — checking each
pair's two endpoints resolve in their respective Frame's registry, and that every pair
identity is non-anonymous and distinct within the Bundle. `closeSpan` is a convenience that
resolves a confirmed `Span`'s two `FrameIdentity`s through a `FrameSpace` and then calls
`closedSpan`; it can fail even for an already-confirmed `Span`, since the `FrameSpace`
passed in may be a later generation that dropped or rebound a pair.

### 2.5 Bundle, Pair, and pair disposition

```haskell
type PairLocalIdentity = Subject.Symbol       -- non-anonymous only
data PairAddress = PairAddress SpanIdentity PairLocalIdentity deriving (Eq, Ord, Show)

type Pair   = PatternRow          -- conventionally exactly two elements
type Bundle = [Pair]              -- canonical ordered list

data PairDisposition = Veto | AutoDrop deriving (Eq, Show)

data PairDispositionPolicy
  = AlwaysVeto
  | AlwaysAutoDrop
  | CustomDisposition (Pair -> PairDisposition)

disposition :: PairDispositionPolicy -> Pair -> PairDisposition
disposition AlwaysVeto           _   = Veto
disposition AlwaysAutoDrop       _   = AutoDrop
disposition (CustomDisposition f) p  = f p
```

A `Pair` reuses `PatternRow` rather than introducing a distinct type: its `rowSubject`
carries the `PairLocalIdentity`, labels, and properties; its `rowElements` are exactly two
`LocalAddress` values, conventionally constrained (not type-enforced) to resolve as
`ScopedAddress (leftFrame span) (rowElements !! 0)` and `ScopedAddress (rightFrame span)
(rowElements !! 1)`. A derived `Map PairLocalIdentity Pair` index, built from the canonical
`Bundle` list, is maintained by whichever operation admits or updates a Bundle, for
uniqueness checking and `PairAddress` lookup — it is not the canonical representation and
is never persisted or passed as an argument in place of the list.

`AlwaysVeto` is the default `PairDispositionPolicy` when a `SpanDraft` does not specify one
(matching every behavior described before `PairDispositionPolicy` existed and exercised
throughout the Worked Exercise). `CustomDisposition` is not serializable; a `ClosedSpan`
built with one is re-admission-only, per RFC-001 §Pair disposition policy — this decision
does not introduce a serializable policy-identifier scheme.

### 2.6 FrameSpace and its integrity operations

```haskell
data FrameSpace = FrameSpace
  { spaceFrames :: Map FrameIdentity Frame
  , spaceSpans  :: Map SpanIdentity Span
  }

data FrameSpaceError
  = FrameIdentityExists FrameIdentity
  | FrameIdentityAbsent FrameIdentity
  | SpanIdentityExists SpanIdentity
  | SpanIdentityAbsent SpanIdentity
  | IncidentSpanExists SpanIdentity
  | FrameEditRejected FrameError
  | VetoedByPairs [PairAddress]
  | SpanRejected SpanError

emptyFrameSpace :: FrameSpace
lookupFrame :: FrameIdentity -> FrameSpace -> Maybe Frame
lookupSpan  :: SpanIdentity  -> FrameSpace -> Maybe Span

addFrame    :: Frame -> FrameSpace -> Either FrameSpaceError FrameSpace
updateFrame :: FrameIdentity -> (Frame -> Either FrameError Frame) -> FrameSpace -> Either FrameSpaceError FrameSpace
removeFrame :: FrameIdentity -> FrameSpace -> Either FrameSpaceError FrameSpace

addSpan     :: SpanDraft -> FrameSpace -> Either FrameSpaceError FrameSpace
updateSpan  :: SpanIdentity -> (Span -> Either SpanError SpanDraft) -> FrameSpace -> Either FrameSpaceError FrameSpace
rebindPair  :: SpanIdentity -> PairLocalIdentity -> (LocalAddress, LocalAddress) -> FrameSpace -> Either FrameSpaceError FrameSpace
removeSpan  :: SpanIdentity -> FrameSpace -> Either FrameSpaceError FrameSpace
```

`addFrame` requires `frameIdentity frame` to be `Just` and absent from `spaceFrames`.
`addSpan`/`updateSpan` resolve `draftLeftFrame`/`draftRightFrame` in `spaceFrames`, then
validate `draftBundle` against those two Frame values exactly as `closedSpan` does (§2.4),
before storing a confirmed `Span`; neither returns the `Span` directly, matching RFC-001's
rule that a caller retrieves it via `lookupSpan` against the returned `FrameSpace`.

`updateFrame` is the cross-Frame integrity boundary, re-typed from SPIKE-001's
`replaceFrame` to consult `PairDispositionPolicy`:

1. Look up the existing Frame at `fid`; apply `edit` to it, producing a candidate Frame
   (`FrameEditRejected` on failure — `edit` already ran §2.2's closure validation).
2. Find every Span incident to `fid` (left or right `FrameIdentity` equals `fid`) — the
   index used to make this lookup fast (RFC-001 Open Question 5) is not decided here; a
   correct but unindexed implementation scans `spaceSpans`.
3. For each incident Span's Bundle, find every pair whose endpoint into `fid` no longer
   resolves in the candidate Frame's registry.
4. Partition those pairs by `disposition (spanPolicy span) pair`. If any is `Veto`, fail
   with `VetoedByPairs` naming every affected `PairAddress`, and return the `FrameSpace`
   unchanged. Otherwise, remove every `AutoDrop` pair from its Span's Bundle and commit the
   candidate Frame — one atomic replacement, never a partially updated `FrameSpace`.

`removeFrame` fails with `IncidentSpanExists` while any Span references `fid` as either
endpoint — `AutoDrop` cannot repair a Span whose endpoint Frame identity no longer resolves
at all, only a dangling member reference within a Frame that still exists. `removeSpan`
removes a Span's own entry without touching its endpoint Frames. `rebindPair` replaces one
pair's two `LocalAddress` endpoints after validating them against the Span's current left
and right Frames, then re-runs the same incident-pair check `updateFrame` does (a rebind
can itself introduce a dangling endpoint if given one).

### 2.7 Reconciliation, import, and attach

```haskell
data ReconcileMode = Replace | Additive

reconcileFrame
  :: ReconcileMode
  -> ReconciliationPolicy SubjectMergeStrategy
  -> [PatternLike]           -- incoming top-level occurrences
  -> Frame                   -- existing
  -> Either FrameError Frame

importSubgraph
  :: Frame                              -- source
  -> [LocalAddress]                     -- roots to copy
  -> Map LocalIdentity LocalIdentity     -- explicit collision-avoiding map
  -> Frame                              -- destination
  -> Either ImportError Frame

attach :: LocalAddress -> [LocalAddress] -> Frame -> Either FrameError Frame
```

`reconcileFrame` operates only within one Frame identity (the caller is responsible for
confirming `existing`'s and the incoming batch's Frame identity match before calling this;
a mismatch is not a `FrameError` this function raises, since it has no incoming Frame value
to compare against — only a `[PatternLike]` batch). `Replace` admits the incoming batch into
a fresh registry and, per RFC-001, removes existing members omitted from it, only when no
local reference or incident Bundle pair still addresses them — the incident-Bundle-pair
half of that check happens at the `FrameSpace.updateFrame` boundary (§2.6), not here;
`reconcileFrame Replace` alone can produce a Frame that a subsequent `updateFrame` call
rejects. `Additive` admits the incoming batch into the *existing* registry (rather than a
fresh one), reconciling matching `Named` addresses via the supplied
`ReconciliationPolicy`-driven call into `Pattern.Reconcile.reconcile`, and retaining every
existing member the incoming batch omits. Both modes delegate Subject-level conflict
resolution (label/property merge, reference completion) to `Pattern.Reconcile`, per the
prior ADR's decision to not reimplement it; Frame code owns only registry construction,
address assignment for incoming anonymous occurrences, and the mode-specific
retain/replace rule.

`importSubgraph`, re-typed from SPIKE-001's validated shape onto `LocalAddress`, computes
the transitive closure of `roots` in `source`'s registry, maps each reached `Named` address
through the caller-supplied map (identity if absent from the map — an omitted `Named`
address is preserved unchanged, which is a collision unless the destination happens not to
have it), rejects a non-injective resulting map or any collision with an existing
destination address, and otherwise copies every reached row into the destination with its
`rowElements` rewritten through the same map. `Positional` addresses in the closure are
never looked up in the map; each receives a fresh ordinal in the destination as its
containment is rebuilt, since a positional address has no portable meaning outside the
Frame that assigned it.

`attach` admits no new content itself — its `[LocalAddress]` roots must already resolve in
`frame`'s own registry (typically, immediately after an `importSubgraph` call into that
same Frame) — and appends them to the target address's `rowElements`, then re-runs closure
validation on the modified row. This is the successor to the prior ADR's `Subsumed target`
reconciliation mode, split out as its own operation per RFC-001: reconciliation now
resolves temporal versions of one Frame identity (`Replace`/`Additive`), `importSubgraph`
crosses Frame namespaces, and `attach` changes containment within one already-populated
namespace — three distinct axes, not three cases of one mode.

### 2.8 Module layout

- `Pattern.Frame` — `LocalAddress`, `ScopedAddress`, `PatternRow`, `FrameRegistry`, `Frame`,
  `PatternLike`, `FrameError`, admission (§2.2), raw-Pattern import (§2.3), reconciliation,
  import, and attach (§2.7).
- `Pattern.Span` — `SpanIdentity`, `SpanDraft`, `Span`, `ClosedSpan`, `SpanError`, `Bundle`,
  `Pair`, `PairAddress`, `PairDispositionPolicy` (§2.4, §2.5).
- `Pattern.FrameSpace` — `FrameSpace`, `FrameSpaceError`, and its integrity operations
  (§2.6).

Three modules rather than two (the prior ADR's `Pattern.Frame`/`Pattern.Span`), because
`FrameSpace` is a distinct aggregate with its own error type and its own dependency on both
`Frame` and `Span` — folding it into `Pattern.Span` would make `Pattern.Span` depend on
`Pattern.Frame` in one direction for Span's own types and in the reverse direction for
FrameSpace's Frame-replacement logic.

## 3. Alternatives Considered

**Two-pass admission (compute rows, then resolve addresses in a second walk).** Rejected:
this is exactly the bug SPIKE-002's first draft hit and fixed. An anonymous child's ordinal
is assigned during admission by threading state; a later walk over the same `PatternLike`
value cannot recover which ordinal a specific occurrence received, since nothing in the
value itself records it.

**Separate `Map LocalIdentity PatternRow` and `Map ElementOrdinal PatternRow` registries.**
Rejected: every operation that needs "resolve this address" or "does this address exist"
would need to dispatch on the `LocalAddress` constructor first, doubling the lookup sites
for no behavioral gain — a single `Map LocalAddress PatternRow` already discriminates by
key.

**`PairDispositionPolicy` as a typeclass instead of a closed sum with a function case.**
Rejected: RFC-001 explicitly mirrors `Pattern.Reconcile`'s convention of named policies
plus one custom-function case (`LastWriteWins`, `Strict`, ... alongside a caller-supplied
function). A typeclass would need an instance per policy, blocking a `Span` from storing a
policy value it selects at runtime rather than at compile time — Spans admitted from parsed
input need a runtime-selected policy.

**Reintroducing `ScopeQuery Frame Subject` against the registry.** Rejected for this
decision: `ScopeQuery`'s existing methods (`containers`, `siblings`, `byIdentity`,
`allElements`) assume tree-shaped, cycle-free scope navigation over a wrapped Pattern.
`FrameRegistry` permits cycles and shared membership by construction (§2.1), so a
`ScopeQuery` instance would need cycle-aware traversal semantics `ScopeQuery`'s other
instances (`TrivialScope`, `ScopeDict`, graph-backed scopes) do not require and were not
designed against. RFC-001 Open Question 6 leaves this as a genuinely open design problem,
not a compatibility shim this ADR can supply.

**Cascading `removeFrame` through incident Spans.** Rejected as the default: automatically
removing every Span incident to a removed Frame would silently discard relationship data a
caller may not expect to lose. `removeFrame` fails instead, requiring the caller to
`removeSpan` each incident Span explicitly first — consistent with `PairDispositionPolicy`
governing only member-level edits, never a whole Frame's disappearance (RFC-001 Open
Question 1).

## 4. Consequences

Frame construction is admission-shaped, not wrap-shaped: there is no `asFrame`/`framePattern`
lossless-conversion pair, because Frame is not a wrapper (RFC-001 Open Question 4). A
caller building a Frame from an existing `Pattern Subject` value uses `importRawPattern`
(§2.3) or converts it to `PatternLike` first; neither is free the way the prior ADR's
`asFrame` was.

Existing `ScopeQuery`-based generic consumers — `RepresentationMap`, `Graph.Transform`, and
`Pattern`'s own generic scope operations — do **not** gain Frame support from this ADR. The
prior ADR's compatibility story (`ScopeQuery Frame Subject` instance, `FrameOps` alias) is
retired along with the wrapper representation it depended on. This is a real, temporary
capability gap relative to the prior ADR, tracked as RFC-001 Open Question 6, and this ADR
takes no position on when or how it closes.

`updateFrame`'s cost is proportional to a Frame's Span fan-out (RFC-001 Open Question 5);
this decision specifies the correct unindexed scan and does not commit to an index
structure. A representative multi-Span workload is needed before that tradeoff can be
made concretely.

Reconciliation, import, and attach gain the three-axis separation §2.7 describes, replacing
the prior ADR's single `FrameReconciliationMode` (`OneToOne | Additive | Subsumed Symbol`)
with two reconciliation modes plus two standalone operations. Any code written against the
prior ADR's `reconcileFrame` signature requires a rewrite, not a compatibility shim — the
prior ADR was never implemented (status remained `proposed`), so no such code exists yet.

This decision introduces `Pattern.Frame`, `Pattern.Span`, and `Pattern.FrameSpace` (§2.8),
exports their public types from the umbrella `Pattern` module, and requires tests for:
admission (batch forward-references, indirect cycles, anonymous-ordinal stability across
edits, conflicting-definition and self-reference rejection), FrameSpace integrity
(incident-pair veto/auto-drop, `removeFrame` while a Span exists, `rebindPair`), Bundle
pair-identity uniqueness, `ClosedSpan` independent of any `FrameSpace`, and all four
Frame/registry mutation operations (`reconcileFrame` both modes, `importSubgraph` collision
and collision-remapped cases, `attach`). SPIKE-001's and SPIKE-002's fixtures and assertions
are a direct starting point for this suite, re-typed onto `LocalAddress`.

## 5. Related

- RFCs: RFC-001
- Spikes: [SPIKE-001](../spikes/SPIKE-001-frame-registry/SPIKE.md),
  [SPIKE-002](../spikes/SPIKE-002-frame-document-diversity/SPIKE.md)
- Specs: _(populated automatically by the speckit ADR-link hook once `/speckit-specify`
  references this ADR)_ — `specs/040-frames-spans/spec.md` currently references the
  superseded wrapper/view model and needs its own rewrite against this ADR (see the
  discussion note's Proposed Next Steps, item 7).
