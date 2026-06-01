# Design RFCs — pattern-hs

This directory contains the authoritative design documents for pattern-hs, organized
as numbered RFCs. Each RFC follows a standard format: Status, Summary, Motivation,
Design, Open Questions, Alternatives.

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
| [RFC-001](RFC-001-strata-and-aspects.md) | Strata and Aspects | draft | `Pattern.Stratum`, `Pattern.Aspect` |

### Graph Interface Layer (Draft — Design)

| RFC | Title | Status | Depends on |
|-----|-------|--------|------------|
| [RFC-004](RFC-004-graph-classifier.md) | GraphClassifier — Unified Graph View | draft | — |
| [RFC-005](RFC-005-graph-query.md) | GraphQuery — Portable Query Interface | draft | RFC-004 |
| [RFC-006](RFC-006-scope-unification.md) | Scope Unification — ScopeQuery and paraWithScope | draft | RFC-005 |
| [RFC-007](RFC-007-representation-map.md) | RepresentationMap — Invertible Shape Isomorphisms | draft | RFC-006, RFC-008 |
| [RFC-008](RFC-008-graph-transform.md) | GraphTransform — Construction, Transformation, Pipeline | draft | RFC-004, RFC-005 |
| [RFC-009](RFC-009-graph-mutation.md) | GraphMutation — Coherent In-Memory Graph Mutations | draft | RFC-004, RFC-005, RFC-008 |

## Implementation Order

The graph interface RFCs form a dependency chain:

```
RFC-004 (GraphClassifier)
  └── RFC-005 (GraphQuery)
        ├── RFC-006 (ScopeQuery)  ──→  RFC-007 (RepresentationMap)
        └── RFC-008 (GraphTransform)
              └── RFC-009 (GraphMutation)
```

RFC-007 (RepresentationMap) additionally depends on RFC-008 (GraphTransform) being
settled first, since it builds on `paraWithScope` and the GraphTransform primitives.

RFC-010 (Pattern Reconciliation) is accepted and implemented independently of the
graph interface chain.

## Superseded Documents

The following documents in this directory have been superseded by RFCs and are retained
for historical reference only:

| File | Superseded by |
|------|---------------|
| [DESIGN.md](DESIGN.md) | RFC-002 |
| [SEMANTICS.md](SEMANTICS.md) | RFC-003 |
| [EXTENDED-SEMANTICS.md](EXTENDED-SEMANTICS.md) | RFC-003 |
| [pattern-category.md](pattern-category.md) | RFC-002 |

The following documents in `proposals/` (parent directory) are similarly superseded:

| File | Superseded by |
|------|---------------|
| `proposals/graph-classifier.md` | RFC-004 |
| `proposals/graph-query.md` | RFC-005 |
| `proposals/scope-unification-proposal.md` | RFC-006 |
| `proposals/representation-map-proposal.md` | RFC-006 + RFC-007 |
| `proposals/graph-transform.md` | RFC-008 |
| `proposals/pipeline-scenarios.md` | RFC-008 |
| `proposals/graph-mutation.md` | RFC-009 |
| `proposals/pattern-reconciliation.md` | RFC-010 |

## Non-RFC Documents

The following documents in this directory serve specific purposes and are not RFCs:

| File | Purpose |
|------|---------|
| [pattern-basic-aspects-review.md](pattern-basic-aspects-review.md) | Implementation reference: `length`, `size`, `depth` query functions |
| [graph-lens.md](graph-lens.md) | Implementation notes for the `GraphLens` feature |
| [gram-hs-cli-improvements.md](gram-hs-cli-improvements.md) | CLI tool improvement proposals |
| [gram-hs-cli-plan.md](gram-hs-cli-plan.md) | CLI tool implementation plan |
| [pattern-matching-dsl-design.md](pattern-matching-dsl-design.md) | Pattern matching DSL exploration |

The following document in `proposals/` is a pre-RFC stub pending full design:

| File | Status |
|------|--------|
| `proposals/pattern-equivalence.md` | Pre-RFC: motivating examples only; needs full design |
