# Feature Specification: Scope Unification for Structure-Aware Operations

**Feature Branch**: `038-scope-unification`  
**Created**: 2026-03-17  
**Status**: Draft  
**Input**: User description: "Unify the use of scope for stucture+scope aware operations as described in @proposals/scope-unification-proposal.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Preserve existing fold behavior through one unified scope model (Priority: P1)

A developer already using `para` or `paraGraph` wants the library to expose one shared model for scope-aware folding without changing the behavior of their existing code. They should be able to keep using the current entry points and get the same results as before, while the library internally treats both operations as the same kind of fold with different scope boundaries.

**Why this priority**: Preserving existing behavior while removing duplication is the core value of the feature. If current callers must change code or receive different results, the unification fails its primary purpose.

**Independent Test**: Run the existing `para` and `paraGraph` test suites without modifying call sites or expected outputs. Confirm all tests still pass and the same observable results are produced.

**Acceptance Scenarios**:

1. **Given** existing code that uses `para`, **When** the unified scope feature is introduced, **Then** the code continues to work without call-site changes and returns the same results as before.
2. **Given** existing code that uses `paraGraph`, **When** the unified scope feature is introduced, **Then** the code continues to work without call-site changes and returns the same results as before.
3. **Given** a developer uses the new unified fold directly with an explicit scope, **When** the fold runs, **Then** each step receives the current element, the already-computed child results, and access to the same containing scope for the entire fold.

---

### User Story 2 - Add new scope-aware behavior without inventing new fold primitives (Priority: P2)

A developer introducing a new kind of containing context, such as a document-wide or subgraph-wide view, wants to reuse the same fold primitive instead of adding another specialized fold API. They should be able to describe what is in scope once and then use the unified fold for structure-aware operations in that context.

**Why this priority**: The main strategic value of scope unification is extensibility. Once the shared scope contract exists, new scope kinds can participate without duplicating fold logic.

**Independent Test**: Define a non-graph scope provider that can answer the generic scope questions, run the unified fold against it, and confirm the fold can read scope information without requiring a new specialized fold entry point.

**Acceptance Scenarios**:

1. **Given** a new scope provider that can report containment, siblings, identity lookup, and all in-scope elements, **When** a developer uses it with the unified fold, **Then** the fold operates correctly without introducing a new fold primitive.
2. **Given** a structure-aware operation that only needs generic scope information, **When** it is written against the unified scope contract, **Then** it works across both subtree-only and graph-wide scopes.

---

### User Story 3 - Pass scope behavior around as data when needed (Priority: P3)

A developer maintaining higher-order utilities wants to store, pass, or compose scope behavior in places where a direct polymorphic scope value is inconvenient. They need a first-class representation of scope that behaves the same way as the direct scope provider for the core scope operations.

**Why this priority**: This is a supporting capability rather than the main user-visible outcome, but it removes friction for composition and dynamic use cases that would otherwise be blocked by the new abstraction.

**Independent Test**: Reify a scope into a first-class value, pass it to a helper that expects stored scope behavior, and verify that containment queries, sibling queries, identity lookup, and element enumeration produce the same results as the original scope provider.

**Acceptance Scenarios**:

1. **Given** a scope provider and its first-class representation of the same scope, **When** a developer queries either one for containment, siblings, lookup by identity, or all elements, **Then** both produce the same observable results.
2. **Given** a helper that accepts a stored scope value rather than a direct scope provider, **When** the developer passes the first-class representation into that helper, **Then** the helper can perform the same scope-aware behavior without special-case code.

---

### Edge Cases

- What happens when the chosen scope does not include parent or sibling information, as with a subtree-only scope? Scope queries that depend on unavailable information must return empty results rather than fail.
- What happens when a lookup asks for an element identity that is not present in the current scope? The lookup must return no match and the operation must continue safely.
- What happens when the caller chooses a scope boundary narrower than the full graph or document? The fold must respect that boundary and not infer a broader scope.
- What happens when existing callers continue using `para` and `paraGraph` instead of the unified primitive? They must still receive the same behavior as before.
- What happens when scope behavior is passed as a first-class value instead of a direct provider? The observable answers for the core scope queries must remain equivalent.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The system MUST provide one unified scope contract for structure-aware operations that defines containment queries, sibling queries, lookup by identity, and enumeration of all elements in scope.
- **FR-002**: The system MUST provide one unified fold primitive for structure-aware operations that accepts an explicit scope and uses that scope consistently for the full duration of the fold.
- **FR-003**: The unified fold MUST provide each fold step with the current element and the bottom-up results of that element's direct children.
- **FR-004**: The scope boundary used by the unified fold MUST be determined by the containing context supplied by the caller, not inferred from the fold target alone.
- **FR-005**: The existing `para` operation MUST remain available as a derived operation over the unified fold and MUST preserve its current observable behavior.
- **FR-006**: The existing `paraGraph` operation MUST remain available as a derived operation over the unified fold and MUST preserve its current observable behavior.
- **FR-007**: The system MUST provide a subtree-only scope suitable for `para`, limited to the folded subtree.
- **FR-008**: In the subtree-only scope, queries that require unavailable parent or sibling context MUST return empty results rather than raise errors.
- **FR-009**: The system MUST allow a scope provider to extend the unified scope model with additional context-specific queries without changing the shared fold contract.
- **FR-010**: A developer MUST be able to use a new scope provider for a different containing context with the shared fold without introducing a new specialized fold primitive.
- **FR-011**: The system MUST provide a first-class representation of scope behavior for cases where scope needs to be stored, passed dynamically, or composed outside direct polymorphic use.
- **FR-012**: The first-class representation of scope MUST return the same observable results as its corresponding direct scope provider for containment, sibling lookup, identity lookup, and scope enumeration for at least the subtree-only scope and any graph-backed scope adapter introduced by this feature.
- **FR-013**: The feature MUST be additive only: no existing public behavior may be removed or broken as part of this change.
- **FR-014**: This feature MUST exclude broader cleanup work for other structure-aware operations unless they can be derived from the unified scope model without expanding the agreed scope of this feature.

### Key Entities

- **Unified Scope**: The definition of what elements are visible to a structure-aware operation. It supports containment queries, sibling queries, identity lookup, and complete enumeration of in-scope elements.
- **Unified Scope-Aware Fold**: The shared fold behavior that processes a pattern bottom-up while consulting a single containing scope throughout the traversal.
- **Subtree Scope**: The limited scope used for `para`, where the fold can inspect only the folded subtree and cannot rely on parent or sibling information.
- **Graph-Wide Scope**: A richer scope used for graph-aware operations, where the fold can access graph-level context in addition to generic scope information.
- **First-Class Scope Value**: A stored representation of scope behavior used when direct scope providers are inconvenient to pass or compose.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All existing tests for `para` pass without modification after scope unification is introduced.
- **SC-002**: All existing tests for `paraGraph` pass without modification after scope unification is introduced.
- **SC-003**: Existing `para` and `paraGraph` call sites require zero code changes to preserve their prior observable behavior.
- **SC-004**: At least one structure-aware fold written against the unified fold produces the same result as `para` on subtree-only input and the same result as `paraGraph` on graph-scoped input.
- **SC-005**: A scope provider that lacks parent or sibling information returns empty results for those queries in 100% of tested cases, with no runtime failures.
- **SC-006**: A first-class scope value and its corresponding direct scope provider return identical results for containment, sibling lookup, identity lookup, and all-elements enumeration in 100% of comparison tests.
- **SC-007**: A developer can define and use an additional scope provider beyond the current graph-wide scope without adding another specialized fold primitive.

## Assumptions

- The current observable semantics of `para` and `paraGraph` are already correct and should be preserved exactly by this feature.
- A subtree-only scope does not have enough information to answer parent or sibling questions, so empty results are the correct default behavior.
- Broader unification of other structure-aware operations may follow later, but this feature is complete once the shared scope model and unified fold are in place.
- Future scope providers may offer richer context than the generic scope contract, but the generic contract is the minimum shared behavior required for participation in the unified fold.
