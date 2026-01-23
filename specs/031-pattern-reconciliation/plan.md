# Implementation Plan: Pattern Subject Reconciliation

**Branch**: `031-pattern-reconciliation` | **Date**: 2026-01-23 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/031-pattern-reconciliation/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

This feature adds a reconciliation operation to normalize `Pattern Subject` structures by resolving duplicate identities and completing partial references. The primary requirement is to provide a `reconcile` function that takes a `Pattern Subject` with potentially duplicate identities (from parsing, streaming, or merging) and produces a coherent pattern where each identity appears exactly once, with configurable policies for resolving conflicts (LastWriteWins, FirstWriteWins, Merge, Strict).

The technical approach involves:
1. Creating a new `Pattern.Reconcile` module with reconciliation-specific types and operations
2. Implementing identity tracking using `Data.Map` and `Data.Set` for efficient deduplication
3. Providing four reconciliation policies with configurable merge strategies
4. Supporting detailed conflict reporting and optional action tracking

## Technical Context

**Language/Version**: Haskell (GHC 9.10.3)
**Primary Dependencies**:
  - `pattern` library (v0.3.0.0) - provides Pattern type and operations
  - `subject` library (v0.1.0.0) - provides Subject type with identity, labels, properties
  - `containers` (^0.7) - Map and Set for identity tracking
  - `comonad` (^5) - already used by Pattern.Core

**Storage**: N/A (pure in-memory data transformation)
**Testing**:
  - `hspec` (^2.11) for behavior-driven tests
  - `QuickCheck` (^2.14) for property-based testing (idempotence, identity preservation)

**Target Platform**: Cross-platform Haskell library (Linux, macOS, Windows)
**Project Type**: Multi-library mono-repo (adding module to existing `pattern` library)
**Performance Goals**:
  - Handle patterns with 10,000+ subjects efficiently (O(n) time complexity)
  - Maintain low memory overhead (O(k) where k = unique identities)

**Constraints**:
  - Must preserve all unique identities (no data loss)
  - Must be idempotent (reconciling twice = reconciling once)
  - Must handle deep nesting (100+ levels) and circular references without stack overflow

**Scale/Scope**:
  - New module `Pattern.Reconcile` in existing `pattern` library
  - ~500-800 lines of implementation code
  - ~1000-1500 lines of test code (comprehensive property and unit tests)
  - 4 reconciliation policies, 9 merge strategy variants, 5 main operations

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Status**: ✅ PASS (Constitution template not yet customized for this project)

The constitution file is currently a template with placeholder content. Once customized, this section will evaluate:
- Whether reconciliation follows library-first principles (if applicable)
- Test coverage requirements compliance
- Integration testing needs
- Documentation standards
- Versioning implications (this is a new feature, requires minor version bump to 0.4.0.0)

**Initial Assessment**:
- ✅ Library-first: Adding to existing `pattern` library, not creating new organizational structure
- ✅ Test coverage: Property-based tests planned for core invariants (idempotence, preservation)
- ✅ No breaking changes: New module, existing API unchanged
- ✅ Documentation: Haddock comments required for all exported functions

## Project Structure

### Documentation (this feature)

```text
specs/031-pattern-reconciliation/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
│   └── Pattern-Reconcile.hs  # Type signatures and module structure
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
libs/pattern/
├── src/
│   ├── Pattern.hs                 # Re-export Reconcile types/functions
│   └── Pattern/
│       ├── Core.hs               # Existing (unchanged)
│       ├── Graph.hs              # Existing (unchanged)
│       └── Reconcile.hs          # NEW: Reconciliation module
│
└── tests/
    ├── Test.hs                    # Existing test runner (add Reconcile spec)
    └── Spec/
        ├── Pattern/
        │   ├── CoreSpec.hs       # Existing (unchanged)
        │   ├── GraphSpec.hs      # Existing (unchanged)
        │   ├── ReconcileSpec.hs  # NEW: Unit tests for reconciliation
        │   └── ReconcileProperties.hs  # NEW: Property-based tests
        └── ...

libs/subject/
└── (no changes - reconciliation is Pattern-specific but uses Subject types)
```

**Structure Decision**: Single project structure (Option 1) applies. This is a library addition to the existing `libs/pattern/` library within the gram-hs mono-repo. The pattern library already uses a modular structure with `Pattern/Core.hs`, `Pattern/Graph.hs`, so adding `Pattern/Reconcile.hs` follows established conventions. Tests follow the existing `tests/Spec/Pattern/` hierarchy.

## Complexity Tracking

No constitution violations requiring justification. This is a straightforward library extension following existing patterns.
