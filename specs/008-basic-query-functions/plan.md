# Implementation Plan: Basic Query Functions

**Branch**: `008-basic-query-functions` | **Date**: 2025-01-27 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/008-basic-query-functions/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Implement basic query functions for the Pattern type to enable pattern introspection. This includes functions to query pattern sequence length, total node count, maximum nesting depth, and extract all values. These functions provide essential structural information about patterns without modifying them, enabling developers to understand pattern complexity, validate patterns, and perform analysis operations.

## Technical Context

**Language/Version**: Haskell (GHC 9.8.4, 9.10.3)  
**Primary Dependencies**: base >=4.17.0.0, containers ^>=0.6  
**Storage**: N/A (pure functional data structure)  
**Testing**: hspec ^>=2.11, QuickCheck ^>=2.14  
**Target Platform**: GHC-compatible platforms (Linux, macOS, Windows)  
**Project Type**: Library (Haskell package)  
**Performance Goals**: Query functions complete in <10ms for patterns with up to 1000 nodes  
**Constraints**: Pure functions (no side effects), must work with patterns of any value type, must handle deeply nested structures without stack overflow  
**Scale/Scope**: Library functions for pattern introspection, used by developers working with Pattern structures

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: ✅ Design provides clear, self-documenting code structure. All query functions will have explicit type signatures, comprehensive Haddock documentation with examples, and clear naming (`length`, `size`, `depth`, `values`). Public APIs will be documented with usage examples showing various pattern structures.

- **Testing Standards (NON-NEGOTIABLE)**: ✅ Testing strategy is comprehensive. Each query function will have unit tests covering atomic patterns, patterns with elements, nested patterns, and edge cases. Property-based tests will verify correctness properties (e.g., `size p >= length p`, `depth p >= 0`, `length (values p) == size p`). Tests will be written alongside implementation following TDD principles.

- **Conceptual Consistency**: ✅ Design aligns with category theory formalisms. Query functions are pure functions that preserve pattern structure (they don't modify patterns, only extract information). The functions respect the recursive nature of Pattern and work correctly with all pattern structures. No category-theoretic violations.

- **Mathematical Clarity**: ✅ Formal definitions will be provided in documentation. Depth counting convention is explicitly documented (depth 0 for atomic patterns). Function semantics are clearly defined (e.g., `length` counts direct elements, `size` counts all nodes, `depth` returns maximum nesting depth).

- **Multi-Language Reference Alignment**: ✅ Core design is language-agnostic. Query functions are pure mathematical operations that can be translated to any language. The concepts (length, size, depth, value extraction) are universal and don't depend on Haskell-specific features. Language-specific implementation details (recursion, pattern matching) are standard across functional languages.

**Violations must be documented in Complexity Tracking section below.**

*No violations identified. All constitution checks pass.*

### Post-Phase 1 Design Review

After completing Phase 1 design (data-model.md, contracts, quickstart.md):

- **Code Quality (NON-NEGOTIABLE)**: ✅ All query functions have clear type signatures documented in contracts/type-signatures.md. Comprehensive Haddock documentation with examples provided. Function names are clear and follow Haskell conventions.

- **Testing Standards (NON-NEGOTIABLE)**: ✅ Testing requirements clearly defined in contracts/type-signatures.md. Unit tests, property-based tests, and integration tests specified. Test coverage requirements documented.

- **Conceptual Consistency**: ✅ Query functions are pure mathematical operations. No category-theoretic violations. Functions respect Pattern's recursive structure.

- **Mathematical Clarity**: ✅ Formal definitions provided in data-model.md. Depth counting convention explicitly documented. Function semantics clearly defined in contracts.

- **Multi-Language Reference Alignment**: ✅ Core design is language-agnostic. Query functions are pure operations that translate to any language. Implementation details (recursion) are standard across functional languages.

*All constitution checks pass after Phase 1 design. Ready to proceed to Phase 2 (task generation).*

## Project Structure

### Documentation (this feature)

```text
specs/008-basic-query-functions/
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
    └── Core.hs          # Add query functions: length, size, depth, values

tests/
└── Spec/
    └── Pattern/
        └── CoreSpec.hs   # Add tests for query functions
```

**Structure Decision**: Single Haskell library project. Query functions will be added to `Pattern.Core` module alongside existing Pattern type and operations. Tests will be added to existing `CoreSpec.hs` test file. This maintains consistency with existing codebase structure and follows the established pattern of adding functionality to the Core module.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

*No violations. All constitution checks pass without justification needed.*
