# Research: Scope Unification for Structure-Aware Operations

**Branch**: `038-scope-unification` | **Date**: 2026-03-17  
**Purpose**: Resolve implementation-design unknowns before coding.

---

## Decision 1: Represent Generic Scope as a Typeclass Plus First-Class Dictionary

**Decision**: Introduce generic scope behavior in `Pattern.Core` as a typeclass (`ScopeQuery`) with a first-class value form (`ScopeDict`) and a reification helper (`toScopeDict`).

**Rationale**: This follows the reference-design pattern already emerging in the repo: use static dispatch for the common case, but provide a stored/dynamic form when higher-order utilities need to pass scope behavior around as data. It also aligns with the proposal's intent and the constitution's cross-language requirement by making the Haskell design easy to map to traits, protocols, or interfaces elsewhere.

**Alternatives considered**:
- Record-of-functions only — rejected because the generic case reads better as a typeclass in Haskell and should not force all callers into dynamic dispatch.
- Typeclass only — rejected because the spec explicitly requires first-class scope behavior for storage and composition use cases.

---

## Decision 2: Keep `paraWithScope` in `Pattern.Core` as the Canonical Tree Fold

**Decision**: Add `paraWithScope` in `Pattern.Core` as the canonical scope-aware fold for recursive `Pattern` traversal, and redefine `para` in terms of it using `TrivialScope`.

**Rationale**: `para` already has a precise and well-tested recursive contract over a single `Pattern`. Making the scope explicit there is straightforward and preserves the conceptual relationship between the original paramorphism and the new abstraction. This gives a stable generic fold contract without disturbing the existing `para` API.

**Alternatives considered**:
- Replace `para` outright with a differently shaped public API — rejected because the spec requires zero call-site changes.
- Generalize the fold first around a flat list scheduler — rejected because the tree paramorphism is already correct and should remain the conceptual baseline.

---

## Decision 3: Adapt Graph-Wide Scope Through `GraphView`, Not by Expanding `GraphQuery`

**Decision**: Provide graph-wide generic scope answers through a `GraphView`-backed adapter rather than by adding new fields to `GraphQuery`.

**Rationale**: The current public `GraphQuery(..)` record enumerates nodes and relationships and exposes node/relationship lookups plus direct-container queries. It does not provide complete enumeration or lookup across walks, annotations, and other classified elements. Adding new record fields would be a breaking API change for downstream record construction and pattern matching. A `GraphView`-backed adapter preserves additive-only change while giving the unified scope layer access to all in-scope graph elements.

**Alternatives considered**:
- Add generic-scope fields directly to `GraphQuery` — rejected because adding public record fields is breaking.
- Treat `GraphQuery` as if it already represented full graph scope — rejected because it would make `allElements` and generic identity lookup incomplete for classified graph elements beyond nodes and relationships.

---

## Decision 4: Preserve `paraGraph` as a Wrapper With Existing Map Output and Scheduling

**Decision**: Keep `paraGraph` and `paraGraphFixed` as graph-specific wrappers that preserve `Map (Id v) r` output, `topoShapeSort` scheduling, and current best-effort cycle behavior.

**Rationale**: The current `GraphView` is a flat classified element list, not a rooted recursive structure. A direct substitution of `paraGraph` with the tree-shaped `paraWithScope` would either require a synthetic root or duplicate element visits, both of which would distort current semantics. The correct additive design is to unify scope semantics while retaining the graph-specific accumulation strategy that existing tests already lock in.

**Alternatives considered**:
- Convert `GraphView` into a rooted pattern and run `paraWithScope` directly — rejected because the current graph fold is defined over a flat view and returns one result per element keyed by identity.
- Change `paraGraph` to return a single fold result — rejected because it would break the public contract.

---

## Decision 5: Subtree Scope Returns Empty Answers for Missing Parent/Sibling Context

**Decision**: The subtree-only scope used for `para` returns empty results for parent- and sibling-oriented queries when that information is unavailable.

**Rationale**: This matches the feature spec and keeps the generic scope contract total rather than partial. It also makes the subtree scope easy to reason about across languages: unavailable context means empty answer, not exception or sentinel failure.

**Alternatives considered**:
- Raise errors for unsupported queries — rejected because it would make generic scope consumers brittle and violate the spec's edge-case expectations.
- Return `Maybe [Pattern v]` for some queries — rejected because it complicates the shared contract and leaks implementation availability into every caller.

---

## Decision 6: Add Explicit Tests for the New Public Abstractions

**Decision**: Expand test coverage beyond compatibility reruns to include unit tests for `TrivialScope`, `ScopeDict`, and `paraWithScope`, plus at least one property-level equivalence check between `para` and `paraWithScope (trivialScope p)`.

**Rationale**: The constitution requires tests for every public function/type, plus edge cases and mathematically meaningful properties. Rerunning only the old suites would prove backward compatibility but would not specify the behavior of the new API surface itself.

**Alternatives considered**:
- Only rely on existing `para` / `paraGraph` suites — rejected because the new abstractions would remain under-specified.
- Add only unit tests and skip property tests — rejected because observational equivalence and scope-boundary laws are natural property-level requirements here.

---

## Decision 7: Update Existing Reference Docs Instead of Adding a New Feature Doc

**Decision**: Update `docs/reference/features/paramorphism.md`, `docs/reference/features/para-graph.md`, and `docs/reference/PORTING-GUIDE.md` instead of introducing a separate third feature reference file.

**Rationale**: The user-facing concepts already live in those docs. Updating them keeps the reference narrative coherent: `para` and `paraGraph` remain the entry points readers know, and the new scope abstraction explains their shared mental model rather than fragmenting the docs.

**Alternatives considered**:
- Create a brand-new `scope-query.md` feature doc — rejected for this feature because it would duplicate explanations already required in the paramorphism and graph-fold docs.
- Leave the docs untouched and rely on Haddocks — rejected because the constitution requires clear conceptual and cross-language documentation for reference features.

---

## Decision 8: Cross-Language Mapping Must Be Documented as Typeclass ↔ Dictionary ↔ Trait/Protocol

**Decision**: Document the cross-language mapping explicitly in the porting guide: Haskell typeclass + dictionary/value form corresponds to trait/object/protocol plus a concrete adapter in other target languages.

**Rationale**: This feature is a reference-design abstraction, not a Haskell-only convenience. Without the mapping note, ports may copy syntax rather than semantics and drift on what "scope" means.

**Alternatives considered**:
- Leave cross-language implications implicit — rejected because the constitution makes multi-language alignment a first-class requirement.

---

## Resolved Questions

| Question | Resolution |
|----------|------------|
| Should graph-wide scope be represented directly by `GraphQuery`? | No. Use a `GraphView`-backed adapter so the change stays additive and covers all classified elements. |
| Does the unified fold replace `paraGraph` outright? | No. `paraWithScope` becomes the canonical scope-aware tree fold; `paraGraph` remains a graph-specific wrapper preserving current output and scheduling. |
| How should unsupported subtree queries behave? | Return empty results, not errors. |
| Are compatibility tests alone sufficient? | No. New public abstractions require unit and property-level tests in addition to compatibility checks. |
