# Implementation Plan: Construction Functions

**Branch**: `004-construction-functions` | **Date**: 2025-01-28 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/004-construction-functions/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Implement three constructor functions to simplify pattern creation: `pattern` for atomic patterns (0 elements), `patternWith` for patterns with elements (1, 2, or more), and `fromList` for creating patterns from lists of values. These functions will provide a more convenient API than the verbose record syntax currently required. The implementation will include comprehensive tests covering all edge cases (0, 1, 2, many elements), Haddock documentation with examples, and verification that constructor-created patterns are functionally identical to record-syntax patterns. This feature reduces code verbosity and improves developer experience while maintaining full compatibility with existing pattern creation methods.

## Technical Context

**Language/Version**: Haskell / GHC 9.10.3 (from cabal.project, matches existing features)  
**Primary Dependencies**: `base` ^>=4.17.0.0, `containers` ^>=0.6 (from existing pattern.cabal)  
**Storage**: N/A (in-memory data structure)  
**Testing**: QuickCheck ^>=2.14, hspec ^>=2.11 (from existing pattern.cabal)  
**Target Platform**: Cross-platform (Linux, macOS, Windows via GHC)  
**Project Type**: single (Haskell library)  
**Performance Goals**: Not primary concern - constructor functions are simple wrappers, performance should be equivalent to record syntax  
**Constraints**: Must maintain type safety, must preserve element order, must work with all value types, must be functionally identical to record syntax  
**Scale/Scope**: Three pure functions with comprehensive tests and documentation

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: ✅ The design provides clear, self-documenting function names (`pattern`, `patternWith`, `fromList`). All functions will be pure and simple, making their behavior obvious. All public functions will be documented with Haddock including usage examples for atomic patterns, singular patterns, pairs, and extended patterns. Code organization follows existing module structure (`Pattern.Core`).

- **Testing Standards (NON-NEGOTIABLE)**: ✅ Testing strategy includes unit tests for all element counts (0, 1, 2, many), tests verifying functional equivalence with record syntax, tests for all value types (strings, integers, custom types), and property-based tests where applicable. Tests will verify element order preservation and type safety. Edge cases (empty lists, nested patterns) will be explicitly tested.

- **Conceptual Consistency**: ✅ The constructor functions are simple wrappers that don't change the underlying Pattern structure. They maintain the category-theoretic foundation - patterns remain decorated sequences. The functions preserve all structural properties and don't introduce any new categorical concepts.

- **Mathematical Clarity**: ✅ The functions will be documented with clear type signatures and mathematical descriptions. Documentation will explain that these are convenience functions that produce patterns identical to record syntax. No new mathematical concepts are introduced.

- **Multi-Language Reference Alignment**: ✅ Constructor functions are a common pattern in functional languages and can be easily translated. The concept of "create X from Y" is universal. Language-specific concerns (Haskell function syntax) are minimal and well-documented, making translation straightforward.

**Violations must be documented in Complexity Tracking section below.**

## Project Structure

### Documentation (this feature)

```text
specs/004-construction-functions/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
│   └── type-signatures.md
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
src/
└── Pattern/
    └── Core.hs          # Add pattern, patternWith, and fromList functions (existing file)

tests/
└── Spec/
    └── Pattern/
        └── CoreSpec.hs  # Add tests for constructor functions (existing file)
```

**Structure Decision**: Using existing project structure. The constructor functions will be added to `src/Pattern/Core.hs` which already contains the Pattern data type. Tests will be added to `tests/Spec/Pattern/CoreSpec.hs` which already exists. This maintains consistency with the existing project organization and keeps related functionality together.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

No violations - these are simple, pure constructor functions that align with all constitutional requirements.

---

## Phase Completion Status

### Phase 0: Research ✅

**Status**: Complete  
**Output**: `research.md`

**Decisions Made**:
- Use function names: `pattern`, `patternWith`, and `fromList` (standard Haskell naming)
- Type signatures: 
  - `pattern :: v -> Pattern v`
  - `patternWith :: v -> [Pattern v] -> Pattern v`
  - `fromList :: v -> [v] -> Pattern v`
- Implementation: Simple pure functions wrapping record syntax (or combinations thereof)
- `fromList` implemented as: `fromList decoration values = patternWith decoration (map pattern values)`
- Testing: Comprehensive unit tests + property-based tests for equivalence
- Documentation: Haddock with examples for all use cases
- Module: Add to `Pattern.Core`, export from both `Pattern.Core` and main `Pattern` module

**All NEEDS CLARIFICATION items resolved**: ✅

---

### Phase 1: Design & Contracts ✅

**Status**: Complete  
**Output**: `data-model.md`, `contracts/type-signatures.md`, `quickstart.md`

**Artifacts Created**:
- **data-model.md**: Documents all three constructor functions, their behavior, edge cases, and functional equivalence
- **contracts/type-signatures.md**: Complete type signatures with Haddock documentation and examples
- **quickstart.md**: Practical guide showing how to use constructor functions for all common scenarios

**Design Decisions**:
- Functions are pure wrappers around record syntax (or combinations thereof)
- No validation needed (all inputs are valid)
- Preserve element order in `patternWith` and `fromList`
- Empty list in `patternWith` and `fromList` produces atomic pattern
- All functions work with all value types
- `fromList` follows standard Haskell convention (`Set.fromList`, `Map.fromList`, etc.)

**Constitution Check (Post-Phase 1)**:
- ✅ **Code Quality**: Design provides clear, self-documenting function names. Haddock documentation with examples defined in contracts.
- ✅ **Testing Standards**: Comprehensive testing strategy defined (unit tests for all cases + property-based tests for equivalence)
- ✅ **Conceptual Consistency**: Functions maintain category-theoretic foundation, no new concepts introduced
- ✅ **Mathematical Clarity**: Type signatures and behavior clearly documented, functional equivalence explicitly stated
- ✅ **Multi-Language Reference**: Simple constructor pattern is universal, easily translatable

All constitutional requirements satisfied. ✅
