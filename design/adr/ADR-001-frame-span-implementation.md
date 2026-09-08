---
id: ADR-001
title: Frame and Span Implementation Model
status: proposed
rfcs: [RFC-001]
created: 2026-09-07
specs: []
---

# ADR-001: Frame and Span Implementation Model

## 1. Context

`Pattern Subject` holds recursive structure and local `Subject.identity` values, but it
does not retain the module boundary needed to qualify identities outside one document or
to accelerate repeated navigation. A literal `newtype Frame = Frame (Pattern Subject)`
can be losslessly unwrapped, but it cannot hold an optional identity index. Adding a
global prefix to every Subject identity would make separate documents distinct, but it
would alter the source Pattern and break lossless Frame unwrapping.

`ScopeQuery` in `Pattern.Core` is a generic typeclass used by `paraWithScope`,
`PatternKind`, and the rank-n operations in `Pattern.RepresentationMap`. Its existing
implementations include `TrivialScope`, `ScopeDict`, and graph-backed scopes. Replacing it
with a Frame-specific class would force existing generic callers to change their
constraints.

`Pattern.Reconcile.reconcile` normalizes one `Pattern` by identity and accepts a
`ReconciliationPolicy`. It already implements first-write, last-write, merge, strict
conflict handling, reference completion, and element merge strategies. Frame-level
reconciliation must combine separately received Frame versions without reproducing that
identity-merge algorithm.

RFC-001 defines Frames as self-contained modules, Spans as external cross-Frame
relationships, and Frame identity as the namespace for local Subject identities. These
requirements need concrete types, error boundaries, and compatibility rules before the
feature can be implemented.

## 2. Decision

### 2.1 Frame and Span representations

`Pattern.Frame` defines `Frame` as a strict data type with a lossless Pattern projection
and optional derived lookup state:

```haskell
data Frame = Frame
  { framePattern :: Pattern Subject
  , frameIdentityIndex :: Maybe (Map Symbol (Pattern Subject))
  }

newtype Span = Span { spanPattern :: Pattern Subject }
```

`frame` constructs a Frame from a root Subject and top-level elements, validates the
Frame namespace when required, and builds the index. `asFrame` wraps an existing Pattern
without indexing. Every operation that changes the wrapped Pattern returns an unindexed
Frame unless it explicitly rebuilds the index. `indexFrame` is the opt-in operation that
adds derived lookup state to an existing Frame.

The index maps each reconciled local `Symbol` to its Pattern in traversal scope. It is an
optimization for identity lookup only; `find`, `containers`, `siblings`, and
`framePara` retain their specified traversal semantics regardless of index presence.
Duplicate local identities prevent index construction and are reported as a Frame
reconciliation error rather than silently selecting one occurrence.

`span`, `spanBundled`, and `spanPattern` retain the relationship-shaped Pattern form from
RFC-001. Span does not cache pair lookups in the initial implementation. Bundle pair
elements continue to store source and target Patterns by value; the enclosing Span
qualifies their local endpoint identities from its first and second Frame respectively.

### 2.2 Scoped identities and namespace boundaries

`Pattern.Frame` defines the external member key as:

```haskell
data ScopedIdentity = ScopedIdentity
  { scopedFrameIdentity :: Symbol
  , scopedLocalIdentity :: Symbol
  }
  deriving (Eq, Ord, Show)
```

`frameIdentity :: Frame -> Maybe Symbol` returns the root identity when it is
non-anonymous. `scopedIdentity :: Frame -> Symbol -> Maybe ScopedIdentity` qualifies a
local symbol only for such a Frame. `findScoped :: ScopedIdentity -> Frame -> Maybe
(Pattern Subject)` returns `Nothing` when the Frame component differs and never falls
back to an unqualified lookup.

The root identity is assigned by the caller at the ingestion boundary, normally from a
stable document key. Wrapping parsed document elements in a Frame root preserves every
explicit local identity. `Gram.fromGramWithIds` or `Gram.Transform.assignIdentities`
assigns identities to anonymous Subjects only when later identity-bearing operations need
them; generated names need only be unique within that Frame. A Frame with an anonymous
root supports local navigation but cannot be used for scoped lookup, Span endpoint
qualification, or identity-keyed persistence.

Frames that have different root identities define different namespaces even when their
members share local symbols. A Frame with the same root identity denotes a later version
of that namespace and is eligible for Frame reconciliation. Storage and cross-Frame
references use `ScopedIdentity`, which maps directly to RFC-011's `(frame_id, id)` key.

### 2.3 ScopeQuery continuity and Frame operations

`ScopeQuery` remains in `Pattern.Core` unchanged. `Frame` provides the specialized
instance:

```haskell
instance ScopeQuery Frame Subject where
  type ScopeId Frame Subject = Symbol
```

`containers`, `siblings`, `byIdentity`, and `allElements` answer within the Frame's
wrapped Pattern. `byIdentity` uses `frameIdentityIndex` when present and otherwise uses a
linear traversal. Existing `ScopeQuery` users, including `RepresentationMap`, continue to
compile and retain their generic scope choices.

`Pattern.Frame` exports `type FrameOps f = ScopeQuery f Subject` with
`ConstraintKinds`. It is a naming alias, not a second typeclass or a duplicate method
set. New Frame-oriented APIs use `Frame` directly or this alias; compatibility APIs keep
their existing `ScopeQuery q v` constraints.

`framePara` is a Frame-named wrapper around `paraWithScope frame`. The fixed Frame value
is passed to every algebra step, so it supplies the required whole-Frame ambient context
and cannot diverge from `paraWithScope` semantics. `Frame` does not alter the generic
`para` or `TrivialScope` behavior.

### 2.4 Frame reconciliation adapter

`Pattern.Frame` defines:

```haskell
data FrameReconciliationMode
  = OneToOne
  | Additive
  | Subsumed Symbol

reconcileFrame
  :: FrameReconciliationMode
  -> ReconciliationPolicy SubjectMergeStrategy
  -> Frame
  -> Frame
  -> Either FrameReconcileError Frame
```

All modes first require equal, non-anonymous Frame identities. Different identities
produce `FrameScopeMismatch`; importing foreign content into a namespace is an explicit
caller transformation and is not implicit reconciliation.

`OneToOne` normalizes and returns the incoming Frame version. `Additive` combines the
existing and incoming Frame roots and their top-level elements into an adapter Pattern,
then delegates duplicate detection, reference completion, Subject merging, and element
merging to `Pattern.Reconcile.reconcile`; its result is reassembled under the shared
Frame root. `Subsumed target` locates `target` by local identity in the existing Frame,
combines the target subtree with the incoming Frame contents in the same way, and replaces
only that target subtree. Missing targets and reconciliation failures are returned as
`FrameReconcileError`.

The policy mapping is direct: prefer incoming uses `LastWriteWins`, prefer existing uses
`FirstWriteWins`, union uses `Merge UnionElements defaultSubjectMergeStrategy`, error uses
`Strict`, and custom policies pass the caller's `ReconciliationPolicy` through unchanged.
The adapter owns only namespace validation, mode-specific Pattern preparation, root
selection, and error translation. `Pattern.Reconcile` remains the sole implementation of
identity conflict resolution.

## 3. Alternatives Considered

**Single-field `newtype Frame`.** Rejected: it cannot retain an optional identity index;
an external cache would add lifetime and invalidation coordination without improving the
public API.

**Always-indexed Frame.** Rejected: structural composition and one-off operations would
pay index construction cost, and wrapping an existing Pattern would no longer have the
cheap `asFrame` path described by RFC-001.

**Globally rewritten Subject identities.** Rejected: prefixing or hashing local symbols
changes source Pattern values and makes a local identity depend on its ingestion context.

**A replacement FrameOps typeclass.** Rejected: `ScopeQuery` is already used by generic
library APIs. A parallel class would duplicate methods and require adapters at every
existing call site.

**A new Frame-specific merge engine.** Rejected: `Pattern.Reconcile` already defines the
conflict policies, reference rules, reports, and recursive merge behavior that Frame
reconciliation needs.

**Permitting direct reconciliation across Frame identities.** Rejected: equal local
symbols in independently ingested documents must remain distinct until a caller chooses a
destination namespace and import semantics.

## 4. Consequences

Frame construction has two performance profiles: `asFrame` is cheap and performs linear
identity lookup, while `frame` and `indexFrame` pay one traversal to build a reusable
index. Pattern-changing operations invalidate derived index state, preventing stale
lookup results.

Frame roots become operationally significant at multi-document, Span, and persistence
boundaries. Callers must supply stable non-anonymous root identities for those uses. Pure
single-Pattern navigation remains available for anonymous-root Frames and existing
generic scopes.

`ScopeQuery` remains broadly extensible, while Frame gains a first-class scope value and
specialized Subject operations. `framePara` has one tested implementation path through
`paraWithScope`; the prior independent-fold expectation in Feature 040 must be revised
during planning to match this decision.

Frame reconciliation gains explicit failures for mismatched namespaces, anonymous Frame
roots, missing subsumption targets, and underlying `Pattern.Reconcile` conflicts. It does
not update Bundle pair-elements after a Frame version changes; reconciliation across
Spans remains RFC-001 Open Question 6.

The decision introduces `Pattern.Frame` and `Pattern.Span` modules, exports their public
types from the umbrella `Pattern` module, and adds tests for indexed/unindexed equivalence,
scoped identity collisions, ScopeQuery compatibility, and all reconciliation modes.

## 5. Related

- RFCs: RFC-001
- Specs: _(populated automatically by the speckit ADR-link hook once `/speckit-specify` references this ADR)_
