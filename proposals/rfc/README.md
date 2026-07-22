# Design RFCs — pattern-hs

This directory contains the authoritative design documents for pattern-hs, organized
as numbered RFCs. Each RFC follows a standard format: Status, Summary, Motivation,
Design, Open Questions, Alternatives.

## How this directory is organized

- **`rfc/RFC-NNN-*.md`** — authoritative, numbered design documents. **Start here.**
- **`proposals/research/`** — supporting notes that are not RFCs: implementation
  references, CLI plans, and exploratory designs. Optional context, not load-bearing spec.
- **`proposals/` (top level)** — pre-RFC stubs: motivating sketches awaiting a full RFC.

## RFC Index

### Foundation (Accepted — Implemented)

| RFC | Title | Status | Key Modules |
|-----|-------|--------|-------------|
| [RFC-002](RFC-002-pattern-container-substrate.md) | Pattern as Container and Graph Substrate | accepted | `Pattern.Core`, `Pattern.Graph.GraphLens` |
| [RFC-003](RFC-003-gram-notation-semantics.md) | Gram Notation Semantics | accepted | `Gram.Core`, `Gram.Parser` |
| [RFC-010](RFC-010-pattern-reconciliation.md) | Pattern Reconciliation | accepted | `Pattern.Reconcile`, `Subject.Core` |

### Active Design (Draft — In Progress)

| RFC | Title | Status | Key Modules |
|-----|-------|--------|-------------|
| [RFC-001](RFC-001-frames-and-spans.md) | Frames and Spans | draft | `Pattern.Core`, `Pattern.RepresentationMap` |

### Graph Interface Layer (Draft — Design)

| RFC | Title | Status | Depends on |
|-----|-------|--------|------------|
| [RFC-004](RFC-004-graph-classifier.md) | GraphClassifier — Unified Graph View | draft | — |
| [RFC-005](RFC-005-graph-query.md) | GraphQuery — Portable Query Interface | draft | RFC-004 |
| [RFC-006](RFC-006-scope-unification.md) | Scope Unification — ScopeQuery and paraWithScope | draft | RFC-005 |
| [RFC-007](RFC-007-representation-map.md) | RepresentationMap — Invertible Shape Isomorphisms | draft | RFC-006, RFC-008 |
| [RFC-008](RFC-008-graph-transform.md) | GraphTransform — Construction, Transformation, Pipeline | draft | RFC-004, RFC-005 |
| [RFC-009](RFC-009-graph-mutation.md) | GraphMutation — Coherent In-Memory Graph Mutations | draft | RFC-004, RFC-005, RFC-008 |

### Persistence Layer (Draft — Design)

| RFC | Title | Status | Depends on |
|-----|-------|--------|------------|
| [RFC-011](RFC-011-codec-persistence.md) | Codec — Pluggable Persistence Adapters | draft | RFC-001, RFC-007, RFC-004 |
| [RFC-012](RFC-012-scoped-identity.md) | Scoped Identity Namespaces | draft | RFC-001, RFC-003, RFC-010 |

## Implementation Order

The graph interface RFCs form a dependency chain:

```
RFC-004 (GraphClassifier)
  └── RFC-005 (GraphQuery)
        ├── RFC-006 (ScopeQuery)  ──→  RFC-007 (RepresentationMap)  ──→  RFC-011 (Codec)
        └── RFC-008 (GraphTransform)
              └── RFC-009 (GraphMutation)

RFC-001 (Frames) ──→ RFC-012 (Scoped Identity) ──→ RFC-011 (Codec) implementation
```

RFC-011 (Codec) builds on RFC-007: a `RepresentationMap` normalizes shape within
Pattern-space, then a `Codec` crosses the boundary into an external store.

RFC-012 (Scoped Identity) is a prerequisite for RFC-011's *implementation* (not its
acceptance as a design): identity-keyed upsert, seed-then-own, and clash-free ingest
cannot be built until scoped identity lands. RFC-012 in turn rests on RFC-001, since
the Frame is the primitive that grounds an element's identity chain.

RFC-007 (RepresentationMap) additionally depends on RFC-008 (GraphTransform) being
settled first, since it builds on `paraWithScope` and the GraphTransform primitives.

RFC-010 (Pattern Reconciliation) is accepted and implemented independently of the
graph interface chain.

## Superseded Documents

The following standalone proposals were folded into RFCs and have been **removed**. Their
provenance is recorded in each RFC's **Supersedes:** header:

| Superseding RFC | Absorbed documents |
|-----------------|--------------------|
| RFC-002 | `DESIGN.md` (deferred sketches → RFC-002 Appendix B), `pattern-category.md` (categorical detail → RFC-002 Appendix A) |
| RFC-003 | `SEMANTICS.md`, `EXTENDED-SEMANTICS.md` |
| RFC-004 | `proposals/graph-classifier.md`, `proposals/pattern-graph.md` (PatternGraph design → RFC-004 Appendix A) |
| RFC-005 | `proposals/graph-query.md` |
| RFC-006 | `proposals/scope-unification-proposal.md` |
| RFC-007 | `proposals/representation-map-proposal.md` |
| RFC-008 | `proposals/graph-transform.md`, `proposals/pipeline-scenarios.md` |
| RFC-009 | `proposals/graph-mutation.md` |
| RFC-010 | `proposals/pattern-reconciliation.md` |

## Research & Supporting Notes

Non-RFC documents live in [`proposals/research/`](../research/). They are reference material,
not authoritative specifications:

| File | Purpose |
|------|---------|
| [pattern-basic-aspects-review.md](../research/pattern-basic-aspects-review.md) | Implementation reference: `length`, `size`, `depth` query functions |
| [graph-lens.md](../research/graph-lens.md) | Implementation notes for the `GraphLens` feature |
| [gram-hs-cli-improvements.md](../research/gram-hs-cli-improvements.md) | CLI tool improvement proposals |
| [gram-hs-cli-plan.md](../research/gram-hs-cli-plan.md) | CLI tool implementation plan |
| [pattern-matching-dsl-design.md](../research/pattern-matching-dsl-design.md) | Pattern matching DSL exploration |

## Pre-RFC Stubs

Motivating sketches in `proposals/` awaiting development into full RFCs:

| File | Status |
|------|--------|
| [pattern-equivalence.md](../pattern-equivalence.md) | Gram path vs. pattern notation equivalence; motivating examples only |
| [graph-value-instances.md](../graph-value-instances.md) | Simple graphs over `String` + canonical `Subject` conversion; design sketch |
