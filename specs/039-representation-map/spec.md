# Feature Specification: RepresentationMap

**Feature Branch**: `039-representation-map`
**Created**: 2026-03-17
**Status**: Draft
**Input**: User description: "representation-map as described above and detailed in proposals/representation-map-proposal.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Recognize Named Shape Kinds (Priority: P1)

A library developer defines a named shape kind — a description of what structural constraints a pattern must satisfy to be considered an instance of that kind. They then check whether any given pattern belongs to that kind, within a scope that provides context for cross-referencing.

This is the foundational concept: before you can map between two shape kinds, you must be able to name them and recognize them.

**Why this priority**: All other stories depend on `PatternKind` existing. Without the ability to name and recognize shape kinds, there is no domain or codomain for a `RepresentationMap`, and no way to verify that a transformation produces the expected output kind.

**Independent Test**: Can be fully tested by defining two distinct shape kinds, generating example patterns, and asserting that kind membership predicates correctly classify patterns as belonging or not belonging to each kind. Delivers named, recognizable shape constraints usable for validation and property-test generation.

**Acceptance Scenarios**:

1. **Given** a `PatternKind` with a predicate that matches patterns having a `Location` label directly containing a `Diagnostic` label, **When** a pattern matching that structure is checked, **Then** the kind predicate returns true.
2. **Given** a `PatternKind` with the above predicate, **When** a flat pattern with a `Diagnostic` label but no enclosing `Location` is checked, **Then** the kind predicate returns false.
3. **Given** a `PatternKind` with a canonical example, **When** the example is checked against the kind predicate, **Then** the predicate returns true (the example is always a member of its own kind).
4. **Given** a scope-relative predicate (e.g., "a pattern that is referenced by another element in scope"), **When** the predicate is evaluated with a scope that contains the referencing element, **Then** it returns true; when evaluated without that context, it returns false.

---

### User Story 2 - Transform Patterns Between Shape Kinds (Priority: P2)

A library developer declares a named `RepresentationMap` — an invertible mapping between a source kind and a target kind. They apply the forward transform to convert a source pattern into the target kind, and apply the inverse to convert back. The map records its name, the two kinds it connects, and a round-trip witness.

**Why this priority**: The forward and inverse transforms are the core capability. Without them, declaring shape kinds has no practical consequence beyond validation. The map is the mechanism that makes two representations interchangeable.

**Independent Test**: Can be fully tested by defining a `RepresentationMap` between two concrete shape kinds (e.g., nested diagnostic structure → flat node-relationship form), applying the forward transform to a known pattern, and verifying that the result satisfies the codomain kind predicate. Delivers the ability to convert between two named structural forms.

**Acceptance Scenarios**:

1. **Given** a `RepresentationMap` whose domain is `DiagnosticPattern` (nested location/diagnostic/remediation), **When** the forward transform is applied to a pattern satisfying `DiagnosticPattern`, **Then** the result satisfies the `DiagnosticGraph` kind predicate.
2. **Given** a `RepresentationMap` and a pattern of the codomain kind, **When** the inverse transform is applied, **Then** the result satisfies the domain kind predicate.
3. **Given** a scope that contains cross-references needed by the transform, **When** the forward transform is applied with that scope, **Then** cross-references are resolved correctly within the transform.

---

### User Story 3 - Verify Round-Trip Correctness (Priority: P3)

A library developer verifies that a `RepresentationMap` is a true isomorphism: applying the forward transform followed by the inverse recovers the original pattern, for all patterns of the domain kind. This property is machine-checkable via property-based tests using generated examples.

**Why this priority**: The round-trip property is the formal witness that the map is an isomorphism, not just a one-way transform. Without it, the map has no guarantee of information preservation. This is what distinguishes a `RepresentationMap` from an arbitrary conversion function.

**Independent Test**: Can be fully tested by running the round-trip predicate against the canonical example and a suite of generated domain-kind patterns. Delivers machine-checked confidence that forward and inverse are each other's inverse.

**Acceptance Scenarios**:

1. **Given** any pattern `p` satisfying the domain kind predicate, **When** the inverse transform is applied to `forward(p)`, **Then** the result equals `p` structurally.
2. **Given** a pattern that does not satisfy the domain kind predicate, **When** the round-trip predicate is applied, **Then** the result is undefined (the map makes no guarantees outside its domain).
3. **Given** a `RepresentationMap` and a property-test generator producing domain-kind patterns, **When** the round-trip predicate is run over the generated suite, **Then** all cases pass.

---

### User Story 4 - Compose Compatible Maps (Priority: P4)

A library developer composes two `RepresentationMap`s where the codomain of the first matches the domain of the second, producing a third map that connects the first domain to the second codomain and preserves the round-trip property end-to-end.

**Why this priority**: Composability is what makes the library extensible. Without it, every representation conversion requires a new hand-written map. With it, intermediate forms can be named and chained, and the space of recognized shapes is navigable step-by-step.

**Independent Test**: Can be fully tested by composing two known maps and verifying that the composed map's round-trip predicate holds for patterns of the first map's domain kind. Delivers the ability to chain transformations without writing new maps from scratch.

**Acceptance Scenarios**:

1. **Given** two maps `m1` (A → B) and `m2` (B → C), **When** they are composed, **Then** the result is a map from A to C whose forward applies `m1.forward` then `m2.forward`.
2. **Given** two maps where the codomain of `m1` does not match the domain of `m2`, **When** composition is attempted, **Then** the operation fails with a clear message identifying the mismatch.
3. **Given** a composed map, **When** the round-trip predicate is applied to a domain-kind pattern, **Then** it holds (composition preserves the isomorphism).

---

### Edge Cases

- What happens when a transform produces a pattern that does not satisfy the declared codomain kind? The map is incorrectly specified — this should be detectable by checking `kindPred (codomain map) q (forward q p)`.
- What happens when the forward transform references elements by identity but the scope has duplicate identities? Duplicate identities within a scope are an error and should be rejected, not silently collapsed.
- What happens when a pattern satisfies multiple shape kinds? Each kind is independent; a pattern may simultaneously satisfy many kinds. Kind membership is not exclusive.
- What happens when a concrete map depends on encoding choices such as `_arity` or `_depth`? Those choices are documented alongside the map implementation and validated through kind checks and round-trip tests; a first-class declarative claims model is deferred.

---

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The library MUST provide a `PatternKind` abstraction: a named, scope-aware predicate that classifies patterns as belonging or not belonging to a named shape kind.
- **FR-002**: A `PatternKind` MUST include a canonical example pattern that always satisfies its own predicate.
- **FR-003**: The `PatternKind` predicate MUST be evaluable at any scope level — structural predicates (self-contained) and scope-relative predicates (cross-referencing) MUST use the same interface.
- **FR-004**: The library MUST provide a `RepresentationMap` abstraction: a named record pairing a domain kind, a codomain kind, a forward transform, an inverse transform, and a round-trip predicate.
- **FR-005**: The forward and inverse transforms in a `RepresentationMap` MUST be polymorphic over scope — they MUST operate correctly given any scope that satisfies the generic scope contract, not only a graph-backed scope.
- **FR-006**: The library MUST provide a `compose` operation that takes two compatible `RepresentationMap`s (codomain of first = domain of second) and produces a third map connecting the first domain to the second codomain.
- **FR-007**: `compose` MUST fail with a clear error when the codomain kind of the first map does not match the domain kind of the second.
- **FR-008**: The round-trip predicate of the composed map MUST hold whenever the round-trip predicates of both constituent maps hold.
- **FR-009**: The feature MUST include a concrete `diagnosticMap` test/example instance: a `RepresentationMap` between the nested `DiagnosticPattern` shape kind and the flat `DiagnosticGraph` shape kind, using `_arity` and `_depth` in its implementation and documentation.
- **FR-010**: The `diagnosticMap` test/example MUST pass a property-based round-trip test suite that generates arbitrary `DiagnosticPattern`-satisfying inputs and verifies the round-trip predicate for each.
- **FR-011**: All existing operations that fold, map, or filter over patterns MUST continue to work without change after this feature is introduced.

### Key Entities

- **PatternKind**: A named, recognizable class of pattern shapes. Defined by a scope-aware predicate and a canonical example. Not an individual pattern — the description of a family of patterns.
- **RepresentationMap**: A named, invertible, composable isomorphism between two `PatternKind`s, with a round-trip predicate as the machine-checkable isomorphism witness.
- **Encoding choice**: A concrete map-level decision such as `_arity` (element count encoding) or `_depth` (nesting depth encoding). These choices are currently documented beside the map implementation rather than stored on `RepresentationMap` values.
- **Scope**: The containing context that defines what elements are visible during a transform. Provided by the caller; may be a subtree scope, a graph-backed scope, or any caller-defined scope satisfying the generic scope contract.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Any pattern that satisfies a `PatternKind` predicate is classified as that kind in zero additional steps — kind membership is a single predicate evaluation, not a multi-step process.
- **SC-002**: Applying the forward transform to any domain-kind pattern produces a result that satisfies the codomain kind predicate, verifiable by passing `kindPred (codomain map) q (forward q p)`.
- **SC-003**: The round-trip predicate holds for all generated `DiagnosticPattern`-kind inputs in the property-based test suite — zero failures across the full generated suite.
- **SC-004**: A composed map's round-trip predicate passes for all domain-kind inputs that pass the round-trip predicates of both constituent maps — composition does not weaken correctness.
- **SC-005**: All tests that passed before this feature is introduced continue to pass after it is introduced — zero regressions in existing fold, map, and filter operations.
- **SC-006**: The `diagnosticMap` test/example canonical input satisfies `DiagnosticPattern` before `forward`, and its mapped result satisfies `DiagnosticGraph`, verifiable without property-based generation.

## Assumptions

- Kind compatibility in `compose` is checked by comparing kind names at runtime. Phantom-type-level kind checking is deferred until the abstraction is stable.
- Inline `RepresentationMap` metadata for encoding conventions is deferred. Later work may introduce a declarative, machine-checkable claims model once there is more than one concrete map to generalize from.
- `GraphScope` (graph-topology-specific scope operations: source, target, incidents) is deferred. The `diagnosticMap` and other initial maps require only the generic scope contract. `GraphScope` will be introduced when a concrete map requires graph topology.
- The question of whether a `RepresentationMap` can itself be serialized as a `Pattern` is explicitly deferred until at least two concrete maps are implemented and their structure is well understood.
- `pattern-equivalence` (syntactic equivalence within gram notation) is out of scope — it addresses a different concern than isomorphism between structurally distinct shape kinds.
