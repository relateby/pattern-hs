---
id: SPIKE-001
title: Frame Registry Model
status: done
rfcs: [RFC-001]
created: 2026-09-08
---

# SPIKE-001: Frame Registry Model

## 1. Question

Can a normalized, referentially closed Frame registry with `PatternLike` admission,
Frame-local identity, and FrameSpace cross-Frame integrity reproduce the
aircraft/maintenance workflow with the existing Pattern, Gram, and Pattern.Reconcile
primitives? The investigation informs RFC-001's managed-container discussion and its
open questions about cascading repair, pair uniqueness, readdressing, Pattern
materialization, incident-Span indexing, and registry-based navigation.

## 2. Method

`scripts/Main.hs` defines a deliberately non-production experiment model. It parses the
fixtures with `Gram.fromGramWithIds`, admits Definition/Reference inputs into a flat local
registry, and validates FrameSpace/Span behavior. It calls the real
`Pattern.Reconcile.reconcile` to observe the adapter requirements for Additive updates.

Run it from the repository root:

```text
cabal exec -- runghc -ilibs/pattern/src -ilibs/subject/src -ilibs/gram/src \
  design/spikes/SPIKE-001-frame-registry/scripts/Main.hs
```

## 3. Time-box

One day.

## 4. Notes

### 2026-09-08

- The spike keeps all experimental types, scripts, fixtures, and findings under this
  directory. It does not add production modules or alter RFC-001 or ADR-001.
- The experiment treats raw `Pattern Subject` reference inference as a compatibility path
  and makes ambiguous atomic/full occurrences a hard failure.
- A fresh worktree needs `cabal build all` before `cabal exec -- runghc` can see the local
  package database. `runghc` include paths use attached `-i` flags such as
  `-ilibs/pattern/src`.
- The completed driver reports 17 PASS assertions. Its result is recorded in
  `expected/findings.md`.
- `cabal test all` passes. Five Gram corpus checks are pending because the nested
  `tree-sitter-gram` test-data submodule is not initialized in this worktree.

## 5. Findings

All 17 assertions pass. The managed model admits a flat local registry from recursive
Definition/Reference input, accepts the aircraft workflow's indirect cycle, gives an
anonymous Definition a generated Frame-unique identity, rejects direct self-reference,
and rejects ambiguous raw Pattern import.

**2026-09-11 addendum:** the discussion note's anonymous-member model changed after this
spike concluded — anonymous members now acquire no invented Subject identity at all and
are addressed instead by a flat, Frame-scoped positional ordinal (see
`design/rfc/comments/RFC-001-managed-frame-containers.md`, §Identity and addresses). The
"generated Frame-unique identity" assertion above validates the superseded model that
preceded that change, not the current one. This spike is frozen and not re-run against
later revisions; it answers the question posed in §1 as of 2026-09-08.

FrameSpace validation rejects both local referenced-member deletion and deletion that
would leave a Span pair endpoint dangling. Explicit `rebindPair` permits the latter
replacement. Import/rebase rejects a local identity collision by default, succeeds with
an explicit collision-avoiding map, and permits Attach after closure validation.

The real Gram parser accepts all three fixtures. The post-parse `Pattern Subject` form
does not preserve the CST's Definition/Reference distinction, confirming that
`PatternLike` or an equivalent CST-derived admission representation is necessary.

The real `Pattern.Reconcile.reconcile` handles the duplicate `fuel-pump` Pattern under a
Merge policy. Inspection and the experiment confirm that its unary input is an adapter
constraint for Frame reconciliation. In particular, `LastWriteWins` and
`FirstWriteWins` choose the Subject value but union the duplicate element lists, so neither
is a direct implementation of Frame `Replace`.

## 6. Conclusion

The normalized Frame registry and FrameSpace ownership model are viable for the exercised
workflow. RFC-001 should retain Frame `Replace` as a registry replacement operation and
Frame `Additive` as an adapter over `Pattern.Reconcile`, rather than map both modes to
generic reconciliation policies. A replacement ADR must specify the PatternLike-to-registry
admission adapter, the Additive adapter input, FrameSpace's incident-Span index, and the
registry-backed successors to ScopeQuery navigation.
