# Feature Specification: topoShapeSort — Rename and Clarify Graph Element Ordering

**Feature Branch**: `037-topo-shape-sort`
**Created**: 2026-02-24
**Status**: Draft
**Input**: Revise the structure-aware transform operations on a graph, notably `paraGraph` to use `topoShapeSort` as described above, but reviewing other functions and documentation.

## User Scenarios & Testing *(mandatory)*

### User Story 1 — Correct fold results for within-bucket dependencies (Priority: P1)

A developer writes a fold function for `paraGraph` over a graph that contains annotation-of-annotation (annotation A references annotation B). They expect that when `f` is called for annotation A, the result for annotation B is already present in `subResults`. With the old bucket-sort approach, there was no ordering guarantee within the annotation bucket, so B might not yet be in the accumulator when A is processed, silently producing an empty `subResults`. After this change, `topoShapeSort` ensures B is processed before A, so `f` receives B's result correctly.

**Why this priority**: This is the primary behavioral correctness guarantee the feature delivers. Without it, `paraGraph` silently produces wrong results for valid graphs — the rename and documentation changes have no value if the fold is still incorrect.

**Independent Test**: Construct a `GraphView` with two annotations where A references B. Call `paraGraph` with a fold function that records the length of `subResults`. Verify that when A is processed, `subResults` contains exactly one entry (B's result), not zero.

**Acceptance Scenarios**:

1. **Given** a `GraphView` containing annotation B and annotation A where A references B, **When** `paraGraph` processes annotation A, **Then** the `subResults` list passed to the fold function contains B's already-computed result.
2. **Given** a `GraphView` containing `GOther` element X that references `GOther` element Y, **When** `paraGraph` processes X, **Then** Y's result is present in `subResults`.
3. **Given** a `GraphView` containing a cycle (annotation A references annotation B, B references A), **When** `paraGraph` processes the cycle members, **Then** no error is raised and each cycle member receives whatever results are available at the time of processing (possibly empty).

---

### User Story 2 — Reading `paraGraph` and understanding its processing order (Priority: P2)

A developer using `paraGraph` reads the function signature, its helper, and its documentation to understand how elements will be processed. The name `sortByArity` currently suggests elements are ordered by their number of connections, but annotations (arity 1) are sorted after walks (arity ≥ 2), contradicting that expectation. After this change, the helper is named `topoShapeSort` and its documentation explains the ordering as dependency-respecting: each element is processed after the shapes it can contain or reference.

**Why this priority**: `paraGraph` is the primary entry point for structure-aware folding. Misunderstanding its processing order leads to incorrect fold implementations. Fixing the name and docs delivers the most value.

**Independent Test**: Can be validated by reading the renamed function, its comment, and the `paraGraph` documentation to confirm that a developer unfamiliar with the codebase would correctly predict that annotations are processed last, and understand why.

**Acceptance Scenarios**:

1. **Given** a developer reads the `topoShapeSort` comment, **When** they see that annotations come after walks, **Then** the comment explains that annotations can reference any shape type (node, relationship, or walk) and therefore must be processed last.
2. **Given** a developer reads the `paraGraph` documentation, **When** they look up which helper determines the fold order, **Then** the docs name `topoShapeSort` and describe the shape-containment rationale, not arity.
3. **Given** the old comment "Sort elements by structural arity", **When** the feature is complete, **Then** no comment or documentation in this module uses "arity" to describe the sort order.

---

### User Story 3 — Extending or debugging `paraGraphWithSeed` (Priority: P3)

A developer maintaining the iterative fixed-point algorithm (`paraGraphFixed` / `paraGraphWithSeed`) needs to understand why each round applies the same element ordering. After this change, `paraGraphWithSeed` consistently references `topoShapeSort` and the shared ordering rationale is easy to locate.

**Why this priority**: `paraGraphWithSeed` is an internal helper that also calls the sort. Consistent naming across both call sites prevents confusion when reading or modifying the fixed-point algorithm.

**Independent Test**: Verify that `paraGraphWithSeed` calls `topoShapeSort` (not `sortByArity`) and that any associated documentation describes the same dependency-based ordering rationale as `paraGraph`.

**Acceptance Scenarios**:

1. **Given** a developer searches the module for the ordering logic, **When** they look at `paraGraphWithSeed`, **Then** they see a call to `topoShapeSort`, not `sortByArity`.
2. **Given** a developer reads the `paraGraphFixed` or `paraGraphWithSeed` documentation, **When** they want to understand the element ordering, **Then** the documentation is consistent with the ordering described in `paraGraph`.

---

### User Story 4 — Reviewing the module-level documentation for consistency (Priority: P4)

A developer reads the module header to orient themselves before using the transform functions. After this change, the module documentation uses consistent vocabulary aligned with `classifyByShape` (the existing classification function) and the new `topoShapeSort` name.

**Why this priority**: The module header is a first-read surface. Inconsistency between the header vocabulary and function names creates friction but does not block correctness.

**Independent Test**: Read the module documentation block and confirm that "shape" is used as the unifying concept for element classification and ordering, with no stray references to "arity" as a sort criterion.

**Acceptance Scenarios**:

1. **Given** the module-level documentation, **When** a developer reads it, **Then** the vocabulary aligns with `classifyByShape` and `topoShapeSort` — shape is the organizing concept.
2. **Given** any inline comments describing the fold ordering, **When** reviewed, **Then** none describe the ordering as "by arity."

---

### Edge Cases

- What if a `GraphView` contains only one shape type (e.g., only nodes)? `topoShapeSort` must handle empty buckets — no behavioral change expected.
- What if `GOther` elements have implicit dependencies on known shapes? The ordering places them last (after annotations); within the `GOther` bucket, topological ordering MUST be applied using `Pattern v` sub-element inspection — the same mechanism used for annotations. `GOther`-of-`GOther` nesting degrades gracefully to soft failure (cycle handling).
- What if an annotation references another annotation (annotation-of-annotation)? The within-bucket toposort MUST order the referenced annotation before the referencing one.
- What if a dependency cycle exists within a bucket (e.g., annotation A references annotation B which references A)? Cycle members MUST be sorted in arbitrary (encountered) order; the missing sub-results will yield empty lists in `paraGraph`, consistent with its existing `maybe [] (:[])` semantics. Cycles are caller's responsibility to avoid; no error is raised.
- Are there any other modules that import or re-export `sortByArity`? If so, those call sites must also be updated.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The function `sortByArity` MUST be renamed to `topoShapeSort` at its definition and at all call sites within the module.
- **FR-002**: The comment describing `topoShapeSort` MUST explain that the ordering is dependency-respecting — each shape is processed after the shapes it can contain or reference — not arity-based.
- **FR-003**: The comment MUST explicitly account for annotations coming after walks, noting that annotations can reference nodes, relationships, and walks.
- **FR-004**: The `paraGraph` documentation MUST reference `topoShapeSort` (by name or description) as the mechanism determining fold order, and describe the containment-based rationale.
- **FR-005**: The `paraGraphWithSeed` call site MUST use `topoShapeSort`.
- **FR-006**: The module-level documentation MUST be reviewed and updated where it describes element ordering, ensuring vocabulary is consistent with "shape" rather than "arity."
- **FR-007**: `topoShapeSort` MUST implement correct within-bucket topological ordering — elements within the same shape class (e.g., annotation-of-annotation) MUST be ordered so that contained elements are processed before their containers.
- **FR-008**: Any other modules in the library that import `sortByArity` MUST be updated to use `topoShapeSort`.
- **FR-009**: `topoShapeSort` MUST apply within-bucket topological ordering to both `GAnnotation` and `GOther` elements using `Pattern v` sub-element inspection to determine dependencies.
- **FR-010**: When `topoShapeSort` encounters a dependency cycle within a bucket, it MUST produce a valid (non-crashing) ordering — cycle members may appear in arbitrary order. No error or exception MUST be raised.
- **FR-011**: The documentation for `topoShapeSort` MUST state that cycles within a bucket produce arbitrary ordering and that callers are responsible for providing cycle-free data if correct fold results are required.
- **FR-012**: The documentation for `paraGraph` MUST explicitly state that if a sub-element result is not yet available when an element is processed (e.g., due to a cycle), that sub-element's result is omitted from the `subResults` list passed to the fold function.

### Key Entities

- **`topoShapeSort`**: The sort helper. Orders `GraphClass` elements first by shape class (nodes → relationships → walks → annotations → other), then within each class by containment dependency (contained elements before their containers). Implements true topological ordering, consistent with the existing `classifyByShape` vocabulary.
- **`paraGraph`**: The primary structure-aware fold. Uses `topoShapeSort` to ensure each element is processed after the elements it contains.
- **`paraGraphWithSeed`**: Internal helper for the fixed-point iteration; applies the same ordering per round.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Zero remaining references to `sortByArity` anywhere in the codebase after the change.
- **SC-002**: Zero comments or documentation strings in the module describe the sort ordering as "by arity."
- **SC-003**: The description of `topoShapeSort` and the `paraGraph` fold-order documentation are consistent — a developer reading either one reaches the same mental model of the processing sequence.
- **SC-004**: For any `GraphView` where annotation A references annotation B (or any within-bucket dependency), `topoShapeSort` produces an ordering in which B appears before A.
- **SC-005**: A developer unfamiliar with the prior discussion can read the module and correctly explain why annotations are processed after walks, based solely on the updated names and documentation.
- **SC-006**: A test with annotation-of-annotation data verifies that the referenced annotation's result is present in `subResults` when the referencing annotation is processed by `paraGraph`.

## Clarifications

### Session 2026-02-24

- Q: Should this feature include actual within-bucket topological ordering, or remain a rename + doc update that documents the current limitation? → A: Expand scope — implement correct within-bucket topological ordering for all shape classes, including cycles.
- Q: When a dependency cycle exists within a bucket (e.g., annotation A references annotation B which references A), how should `topoShapeSort` handle it? → A: Soft failure — cycle members sorted in arbitrary order; missing sub-results yield empty lists, consistent with existing `paraGraph` semantics.
- Q: Should `GOther` elements be topologically ordered within their bucket using `Pattern v` sub-element inspection (same as annotations), or treated as unordered? → A: Inspect `Pattern v` sub-elements for `GOther` ordering, same approach as annotations; cycles fall through to soft failure.
- Q: Should the `paraGraph` silent-miss behavior (`maybe [] (:[])` — missing sub-results silently become empty lists) be explicitly documented as a contract? → A: Yes — document as explicit contract: "If a sub-element has not yet been processed (e.g., due to a cycle), its result is omitted from `subResults`."
- Q: Should a behavioral user story be added for a developer whose graph contains annotation-of-annotation (or `GOther`-of-`GOther`) and expects correct fold results? → A: Yes — add as P1 behavioral user story.

## Assumptions

- The element ordering (nodes → relationships → walks → annotations → other) is correct and is not being changed — only the name and rationale description are updated.
- Annotations can annotate any shape type (node, relationship, walk), which is why they come last; this is assumed correct based on the existing `filterGraph` logic and `paraGraph` documentation.
- `sortByArity` is not part of the module's public export list, so the rename has no external API impact. (Verify before implementation.)
