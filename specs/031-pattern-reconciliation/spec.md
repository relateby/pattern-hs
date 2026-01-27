# Feature Specification: Pattern Subject Reconciliation

**Feature Branch**: `031-pattern-reconciliation`
**Created**: 2026-01-23
**Status**: Draft
**Input**: User description: "Pattern Subject reconciliation as described in proposals/pattern-reconciliation.md"

## Clarifications

### Session 2026-01-23

- Q: Does the planned approach work only on single Patterns (and nested elements) or can it also work on a collection of patterns (or a stream)? → A: Single pattern only - users combine patterns into one root pattern before reconciling
- Q: What is the specific latency target for reconciling patterns with 10,000 subjects? → A: <100ms for 10,000 subjects (fast enough for synchronous use)

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Normalize Parsed Patterns (Priority: P1)

A developer parses gram notation that contains the same identity multiple times with different or evolving content. They need to transform this raw parsed pattern into a coherent pattern where each identity appears exactly once.

**Why this priority**: This is the most common use case - parsing is the primary way patterns enter the system. Without reconciliation, parsed patterns cannot be reliably stored or queried.

**Independent Test**: Can be fully tested by parsing a gram text with duplicate IDs, calling reconcile with LastWriteWins policy, and verifying the result contains each identity once with expected content.

**Acceptance Scenarios**:

1. **Given** a pattern with the same identity appearing twice with different property values, **When** reconciled with LastWriteWins policy, **Then** the result contains one instance with the later property values
2. **Given** a pattern with the same identity appearing twice with different labels, **When** reconciled with Merge policy using UnionLabels, **Then** the result contains one instance with all labels combined
3. **Given** a pattern with no duplicate identities, **When** reconciled, **Then** the result is structurally identical to the input

---

### User Story 2 - Merge Patterns from Multiple Sources (Priority: P2)

A developer combines patterns from different sources (files, API responses, databases) into a single pattern by wrapping them as elements of a root pattern. The combined pattern may have the same entities defined differently across sources. They need to merge these definitions according to business rules.

**Why this priority**: Essential for building applications that aggregate data from multiple sources. Common in data integration and ETL scenarios.

**Independent Test**: Can be fully tested by creating patterns from different "sources" with overlapping identities, combining them into a single root pattern (e.g., `pattern root [p1, p2, p3]`), reconciling with a specific merge strategy, and verifying the merge follows the configured strategy.

**Acceptance Scenarios**:

1. **Given** two patterns with the same identity having different properties, **When** combined into a root pattern and merged using ShallowMerge strategy, **Then** the result contains all properties with later values overwriting earlier ones for conflicts
2. **Given** two patterns with the same identity having different element lists, **When** combined and merged using UnionElements strategy, **Then** the result contains deduplicated elements by identity
3. **Given** patterns from three sources with an identity defined in all three, **When** combined into a root and reconciled, **Then** the merge strategy is applied to all three definitions progressively

---

### User Story 3 - Validate Pattern Coherence (Priority: P3)

A developer receives or generates a pattern and wants to verify it is coherent (no duplicate identities with conflicting content) before storing or transmitting it. If conflicts exist, they need detailed information about each conflict.

**Why this priority**: Important for data quality validation and debugging, but not required for basic reconciliation workflows. Provides diagnostic capabilities.

**Independent Test**: Can be fully tested by creating a pattern with known conflicts, calling reconcile with Strict policy, and verifying it returns detailed conflict information including identities, differing content, and locations.

**Acceptance Scenarios**:

1. **Given** a pattern with duplicate identities having different content, **When** reconciled with Strict policy, **Then** an error is returned listing all conflicts with identity, differing subjects, and their locations
2. **Given** a coherent pattern, **When** reconciled with Strict policy, **Then** the pattern is returned unchanged without errors
3. **Given** a pattern, **When** needsReconciliation is called, **Then** it returns true if duplicates exist, false otherwise

---

### User Story 4 - Complete Partial References (Priority: P3)

A developer has a pattern where some subjects are atomic references (minimal content) while the same identity appears elsewhere with full content. They need references replaced with their full definitions.

**Why this priority**: Enables efficient streaming and incremental pattern construction. Less common than basic deduplication, but valuable for advanced use cases.

**Independent Test**: Can be fully tested by creating a pattern with an atomic subject (identity only) and another occurrence with full content (properties, elements), reconciling, and verifying the atomic reference is replaced with the full definition.

**Acceptance Scenarios**:

1. **Given** a pattern with an atomic reference (no elements) and a full definition elsewhere, **When** reconciled, **Then** the reference is replaced with the full definition
2. **Given** a pattern where all occurrences of an identity are atomic, **When** reconciled, **Then** the atomic pattern is preserved (not treated as a reference to itself)
3. **Given** a pattern with circular references (A references B, B references A), **When** reconciled, **Then** the cycle is resolved and each identity appears once

---

### User Story 5 - Track Reconciliation Actions (Priority: P4)

A developer performing reconciliation wants detailed reporting on what changes were made: how many duplicates were found, how many references resolved, how many merges performed, and occurrence counts per identity.

**Why this priority**: Useful for debugging, logging, and monitoring, but not essential for core reconciliation functionality. Can be added after basic operations work.

**Independent Test**: Can be fully tested by reconciling a pattern with known characteristics (e.g., 3 duplicates, 2 references) using reconcileWithReport and verifying the report contains accurate counts.

**Acceptance Scenarios**:

1. **Given** a pattern with 5 duplicate identities, **When** reconciled with reporting enabled, **Then** the report shows duplicatesFound = 5
2. **Given** a pattern with 3 atomic references, **When** reconciled with reporting enabled, **Then** the report shows referencesResolved = 3
3. **Given** a pattern, **When** reconciled with reporting, **Then** the report includes a map showing occurrence count for each identity

---

### Edge Cases

- What happens when a pattern has self-referential structure (subject's elements include itself)?
  - **Resolution**: Track visited identities during rebuild, emit each identity exactly once

- What happens when an atomic reference has no full definition elsewhere (orphan reference)?
  - **Resolution**: Keep the atomic pattern as-is (pass through)

- What happens when reconciling an empty pattern (no subjects)?
  - **Resolution**: Return unchanged

- What happens when labels/properties conflict during merge with incompatible types?
  - **Resolution**: Merge strategy determines behavior (typically last-write-wins for properties, union for labels)

- What happens when elements in a merge contain the same identity with different content?
  - **Resolution**: Recursively reconcile nested patterns using same policy

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a reconcile operation that accepts a reconciliation policy and a single Pattern Subject, returning either a normalized Pattern Subject or an error
- **FR-002**: System MUST support LastWriteWins policy that keeps the last occurrence of each identity
- **FR-003**: System MUST support FirstWriteWins policy that keeps the first occurrence of each identity
- **FR-004**: System MUST support Merge policy with configurable strategies for labels, properties, and elements
- **FR-005**: System MUST support Strict policy that fails if any duplicate identities have different content
- **FR-006**: System MUST detect all duplicate identities in a pattern by comparing identity symbols
- **FR-007**: System MUST produce a normalized pattern where each identity appears exactly once at its first occurrence location
- **FR-008**: System MUST preserve all unique identities present in the input pattern
- **FR-009**: System MUST provide error information for Strict policy including conflicting subjects, identities, and their locations in the pattern structure
- **FR-010**: System MUST support three label merge strategies: UnionLabels (combine all), IntersectLabels (keep common only), ReplaceLabels (later replaces earlier)
- **FR-011**: System MUST support three property merge strategies: ReplaceProperties (later replaces earlier), ShallowMerge (merge top-level keys), DeepMerge (recursive merge)
- **FR-012**: System MUST support three element merge strategies: ReplaceElements (later replaces earlier), AppendElements (concatenate lists), UnionElements (deduplicate by identity)
- **FR-013**: System MUST provide a default merge strategy (UnionLabels, ShallowMerge, UnionElements)
- **FR-014**: System MUST provide reconcileWithReport operation that returns both the result and a detailed report
- **FR-015**: Reconcile report MUST include count of duplicates found, references resolved, merges performed, and occurrence count per identity
- **FR-016**: System MUST provide needsReconciliation operation that returns true if pattern contains duplicate identities
- **FR-017**: System MUST provide findConflicts operation that returns all conflicts without performing reconciliation
- **FR-018**: System MUST detect references as atomic patterns (no elements) with same identity defined more fully elsewhere
- **FR-019**: System MUST complete references by replacing them with their full definitions when reconciling
- **FR-020**: System MUST be idempotent: reconciling an already-reconciled pattern produces the same result
- **FR-021**: System MUST handle self-referential patterns by tracking visited identities and emitting each once
- **FR-022**: System MUST preserve orphan references (atomic patterns with no full definition) as-is
- **FR-023**: System MUST recursively apply reconciliation policy to nested pattern elements

### Key Entities

- **Pattern Subject**: A recursive structure where Subject values carry identity (Symbol), and may contain nested Pattern Subject elements. This is the primary data structure being reconciled.

- **ReconciliationPolicy**: A parameterized strategy specification that determines how duplicate identities are resolved. Four variants: LastWriteWins, FirstWriteWins, Merge (with element and value-specific strategies), and Strict.

- **ElementMergeStrategy**: Configuration for how element lists are merged (replace/append/union).

- **SubjectMergeStrategy**: Configuration for Subject content merging composed of two sub-strategies for labels (set union/intersection/replace) and properties (shallow/deep merge/replace).

- **Conflict**: Information about a duplicate identity including the identity symbol, the existing and incoming Subject definitions, and the structural locations (paths) where duplicates were found.

- **ReconcileReport**: Summary of reconciliation actions including counts of duplicates, resolved references, merges performed, and per-identity occurrence statistics.

- **Path**: A sequence of indices representing the location of a pattern within the nested structure, used for conflict reporting and debugging.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Reconciliation is idempotent: applying reconcile twice produces the same result as applying it once (testable via property-based testing)
- **SC-002**: All unique identities are preserved: the set of distinct identity symbols in the output equals the set in the input (testable via set comparison)
- **SC-003**: Patterns with 10,000 subjects can be reconciled in less than 100ms (testable via benchmark)
- **SC-004**: Strict mode is accurate: it reports an error if and only if duplicate identities with different content exist (testable via positive and negative test cases)
- **SC-005**: Reference resolution is complete: all atomic patterns with full definitions elsewhere are replaced with full content (testable by counting atomic patterns before and after)
- **SC-006**: Each reconciliation policy produces deterministic results: reconciling the same pattern twice with same policy gives identical output (testable via repeated execution)
- **SC-007**: Merge strategies correctly combine content: UnionLabels includes all labels, ShallowMerge combines all top-level properties, UnionElements deduplicates by identity (testable per strategy)
- **SC-008**: Reconciliation reports are accurate: report counts match actual reconciliation actions taken (testable by manual verification)

## Assumptions

- The gram-hs Pattern.Core and Subject.Core modules already provide the foundational Pattern and Subject types with identity, labels, properties, and elements
- Developers using this feature understand that reconciliation is necessary when patterns come from parsing, streaming, or merging operations
- The default merge strategy (UnionLabels, ShallowMerge, UnionElements) covers the most common use cases
- Orphan references (atomic patterns with no full definition) are intentional and should be preserved rather than treated as errors
- Performance for patterns with fewer than 10,000 subjects is acceptable without optimization
- The order of elements after reconciliation preserves the first occurrence of each identity
- Deep merge of properties will handle nested maps recursively, but property values that are not maps will use last-write-wins
- Circular references are rare but must be handled gracefully by detecting cycles during rebuild
- When reconciling multiple separate patterns, users will first combine them into a single root pattern using the pattern constructor (e.g., `pattern root [p1, p2, p3]`)

## Dependencies

- Existing Pattern.Core module providing the Pattern type and basic operations
- Existing Subject.Core module providing the Subject type with identity, labels, properties
- Data.Map and Data.Set from base containers for efficient identity tracking
- No external library dependencies required beyond standard Haskell base libraries

## Out of Scope

- Real-time streaming reconciliation API (future consideration)
- Batch reconciliation API for collections/lists of patterns (users must combine patterns into single root first)
- Custom reconciliation policies beyond the four provided (LastWriteWins, FirstWriteWins, Merge, Strict)
- Reconciliation of other pattern value types (feature is specific to Pattern Subject)
- Versioning or history tracking of reconciliation changes
- Conflict resolution UI or interactive conflict resolution
- Schema validation during reconciliation
- Type-specific property merging (treats all properties as generic JSON-like values)
- Automatic detection of semantic equivalence (e.g., "Alice" vs "alice" as same person)
- Performance optimization for patterns larger than 10,000 subjects (can be added if needed)
- Integration with specific parsing, storage, or transmission libraries
