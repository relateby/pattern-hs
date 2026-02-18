# Research: PatternGraph (033-pattern-graph)

**Phase**: 0 – Outline & Research  
**Date**: 2026-02-18

## 1. Merge API: total (result + warnings) vs partial (Either on Unrecognized)

**Decision**: Provide a **total** API that returns the updated graph plus a collection of unrecognized patterns (or warnings). Do not require `Either` at the call site for the common “load and merge” path.

**Rationale**:
- Spec FR-006: “MUST NOT silently drop … MUST signal their presence.” A total function that returns (graph, unrecognized) satisfies this and keeps the common case simple (no short-circuit).
- Callers who want to fail on any unrecognized can check the second component and error if non-empty.
- Aligns with “Writer-style accumulation” suggested in the proposal and with existing reconciliation reports (ReconcileReport) in the codebase.

**Alternatives considered**:
- **Either-based merge**: Reject—forces every caller to handle Left before getting a graph, even when they intend to ignore or log unrecognized.
- **Catch-all pgOther**: Reject—proposal and spec explicitly avoid obscuring unexpected input.

**Concrete shape**: e.g. `merge :: GraphValue v => Pattern v -> PatternGraph v -> (PatternGraph v, [Pattern v])` where the list is unrecognized patterns, or a small wrapper type `MergeResult v` with `mergedGraph` and `unrecognized`. Batch: `fromPatterns` returns `(PatternGraph v, [Pattern v])`.

---

## 2. Monoid / overlay (combining two graphs)

**Decision**: **Defer** a `Monoid` instance (or explicit `overlay`) to a follow-up change. Not in scope for this feature.

**Rationale**:
- Spec and user stories focus on round-trip, merge-on-insert, and conversion to GraphLens. Combining two full graphs is a natural extension but not required for SC-001–SC-005.
- Keeps initial API surface smaller and avoids defining merge policy when overlaying two graphs (e.g. how to reconcile same identity across the two).

**Alternatives considered**:
- **Implement Monoid now**: Rejected—adds design work (overlay semantics with reconciliation) and is not needed for the documented success criteria.
- **Document as future work**: Accepted—note in data-model or plan that overlay/Monoid can be added later with a defined reconciliation strategy for cross-graph identity clashes.

---

## 3. GraphValue: classify in typeclass vs separate

**Decision**: Keep **classification in the GraphValue typeclass** (single place for identity + classification), as in the proposal.

**Rationale**:
- One constraint (`GraphValue v`) for both `identify` and `classify` keeps the API simple and ensures every type used as graph value has a consistent classification scheme.
- Subject is the primary instance; having `classify` in the same class avoids a second typeclass or ad-hoc functions that might get out of sync.

**Alternatives considered**:
- **Separate typeclass or standalone function for classify**: Would allow overriding classification independently of identity, but adds API surface and the proposal does not require multiple classification schemes for the same value type in scope. Can be revisited if we need alternative classifiers for the same `v`.

---

## 4. Best practices: merge semantics and reconciliation

**Decision**: Reuse existing **Pattern.Reconcile** machinery for reconciling duplicate identities within each category (nodes, relationships, walks, annotations). PatternGraph does not define new reconciliation policies.

**Rationale**:
- Spec and proposal assume a “defined reconciliation policy” (e.g. from existing pattern-reconciliation work). Pattern.Reconcile already provides LastWriteWins, FirstWriteWins, Merge, Strict.
- PatternGraph’s merge will call into the same policies when inserting into a category that already has an entry for that identity. No new policy types in this feature.

**Alternatives considered**:
- **PatternGraph-specific policy**: Rejected—would duplicate concepts and fragment configuration.

---

## 5. Integration: PatternGraph → GraphLens

**Decision**: Implement conversion by **building a scope pattern** from the contents of the PatternGraph (e.g. all nodes, relationships, walks as direct elements of one root pattern) and constructing a GraphLens with that scope and the same atomic-node predicate used for classification. No duplication of graph algorithms; reuse existing GraphLens API.

**Rationale**:
- Proposal: “A PatternGraph can always be converted to a GraphLens by constructing a scope pattern from its contents and providing the atomic predicate.”
- Existing GraphLens is scope-bounded and predicate-based; PatternGraph’s categories already align (nodes = atomic, relationships = 2 nodes, walks = sequence of relationships). Conversion is a well-defined construction.

---

## 6. Documentation: .graph.gram and PatternGraph

**Decision**:
- **`.graph.gram` reference**: Add a dedicated reference that (a) defines the subset of gram notation allowed in `.graph.gram` files: **annotations, nodes, relationships, and paths only**—no square-bracket pattern notation in the file syntax; (b) describes the **resulting data structures inside PatternGraph** and may use **pattern notation** to explain them (flow: gram → parse → PatternGraph → explain with pattern notation).
- **PatternGraph usage**: Add or update docs (guide + reference) so users can use PatternGraph for construction, merge, fromPatterns, round-trip with gram, and conversion to GraphLens.

**Rationale**:
- User planning input explicitly requested this: “reference for writing a .graph.gram file which only uses annotation, nodes, relationships and paths but no square-bracket pattern notation,” with “the resulting data structures within PatternGraph” explained and “can use pattern notation to describe those structures.”
- Clear separation: file format is restricted (readable, graph-only); explanation of what ends up in PatternGraph can use full pattern notation for precision.

**Alternatives considered**:
- **Single doc for both gram syntax and Pattern**: Kept as one reference that first defines .graph.gram syntax, then explains PatternGraph structures with pattern notation, to preserve the flow gram → parse → PatternGraph → pattern notation.

---

## Summary

| Topic | Decision |
|-------|----------|
| Merge API | Total: return (graph, unrecognized); no Either in core path |
| Monoid/overlay | Defer to follow-up |
| GraphValue | Keep classify in GraphValue typeclass |
| Reconciliation | Use existing Pattern.Reconcile policies |
| PatternGraph → GraphLens | Build scope pattern + atomic predicate; reuse GraphLens |
| Docs | PatternGraph usage in docs/; .graph.gram reference (graph-only syntax, PatternGraph structures explained with pattern notation) |

All NEEDS CLARIFICATION from Technical Context are resolved; no open blockers for Phase 1.
