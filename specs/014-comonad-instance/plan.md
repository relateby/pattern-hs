# Implementation Plan: Comonad Instance for Pattern

**Branch**: `014-comonad-instance` | **Date**: 2025-01-28 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/014-comonad-instance/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Implement a Comonad instance for the Pattern type that enables context-aware computations where functions have access to the full structural context (parent, siblings, depth, indices) around each value, not just the value itself. The implementation must provide `extract` (extract decoration value), `duplicate` (create pattern of contexts), and `extend` (context-aware transformation), satisfying all Comonad laws (extract-extend, extend-extract, extend composition). The instance will follow standard comonad patterns for tree structures, similar to `Data.Tree`, enabling context-aware operations like depth, size, and indices computation at each position.

## Technical Context

**Language/Version**: Haskell (GHC 9.8.4, 9.10.3)  
**Primary Dependencies**: base >=4.17.0.0, comonad (for Comonad typeclass), QuickCheck (for property-based testing), Hspec (for test framework)  
**Storage**: N/A (in-memory data structure)  
**Testing**: Hspec for unit tests, QuickCheck for property-based testing of Comonad laws  
**Target Platform**: Cross-platform (Haskell/GHC)
**Project Type**: Single library project  
**Performance Goals**: Basic implementation sufficient (no specific performance targets for MVP)  
**Constraints**: Must satisfy Comonad laws, must handle all pattern structures (atomic, with elements, nested), must enable context-aware computations  
**Scale/Scope**: Single typeclass instance implementation with comprehensive test coverage, optional helper functions (depthAt, sizeAt, indicesAt)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: ✅ PASS - Design provides clear, self-documenting code structure. All public APIs (Comonad instance methods `extract`, `duplicate`, `extend`) will be documented with Haddock examples. Implementation follows standard Haskell Comonad patterns for recursive structures, similar to `Data.Tree`.
- **Testing Standards (NON-NEGOTIABLE)**: ✅ PASS - Testing strategy is comprehensive: property-based tests for all Comonad laws (extract-extend, extend-extract, extend composition), unit tests for edge cases, integration tests with existing Pattern operations. Category-theoretic properties (Comonad laws) are explicitly testable via property-based testing.
- **Conceptual Consistency**: ✅ PASS - Design aligns with category theory formalisms. Comonad is correctly identified as a categorical structure (dual to Monad). Comonad laws are mathematical requirements that must be satisfied. The instance extends beyond Foldable to enable context-aware computations with full structural context.
- **Mathematical Clarity**: ✅ PASS - Formal definitions provided in specification (Comonad laws stated explicitly). Notation consistent with standard Haskell and category theory conventions. Research phase will document mathematical foundations and relationship to zippers.
- **Multi-Language Reference Alignment**: ✅ PASS - Core design (Comonad instance semantics) is language-agnostic. Context-aware computation semantics can be translated to other languages. Language-specific concerns (Haskell typeclass syntax) separated from core concepts (comonadic semantics).

**Violations must be documented in Complexity Tracking section below.**

## Project Structure

### Documentation (this feature)

```text
specs/014-comonad-instance/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
src/
└── Pattern/
    └── Core.hs          # Comonad instance implementation

tests/
└── Spec/
    └── Pattern/
        ├── CoreSpec.hs  # Unit tests for Comonad instance
        └── Properties.hs # Property-based tests for Comonad laws
```

**Structure Decision**: Single library project structure. The Comonad instance will be added to `src/Pattern/Core.hs` alongside existing Functor, Foldable, Traversable, and Applicative instances. Tests will be added to existing test files in `tests/Spec/Pattern/`. Optional helper functions (depthAt, sizeAt, indicesAt) will be added to `Core.hs` if implemented.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | N/A | N/A |
