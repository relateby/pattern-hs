# Implementation Plan: Pattern and Path Semantics

**Branch**: `018-pattern-path-semantics` | **Date**: 2025-11-29 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/018-pattern-path-semantics/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Implement a semantic validator for the Gram language that enforces core rules: single definition, referential integrity, immutability, and consistency between pattern and path notations. This will be implemented as a separate pass `Gram.Validate` working on the `Gram.CST` before transformation to the core `Pattern` type.

## Technical Context

**Language/Version**: Haskell (GHC 9.8+)
**Primary Dependencies**: `gram`, `pattern`, `megaparsec` (for error bundle types), `containers`, `mtl`
**Storage**: N/A (In-memory validation)
**Testing**: `hspec` for behavior verification
**Target Platform**: Cross-platform library
**Project Type**: single (library)
**Performance Goals**: Fast enough for real-time validation in editor tooling (future)
**Constraints**: Strict adherence to `SEMANTICS.md` rules
**Scale/Scope**: Core validation logic for the Gram language

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: Yes. The design isolates validation logic in a dedicated module with clear types.
- **Testing Standards (NON-NEGOTIABLE)**: Yes. `hspec` will be used to verify acceptance criteria and edge cases.
- **Conceptual Consistency**: Yes. The validator enforces the mathematical and logical consistency of pattern definitions.
- **Mathematical Clarity**: Yes. Definitions align with graph theory and the specific pattern calculus of Gram.
- **Multi-Language Reference Alignment**: Yes. The semantic rules are language-agnostic and clearly documented for porting.

**Violations must be documented in Complexity Tracking section below.**

## Project Structure

### Documentation (this feature)

```text
specs/018-pattern-path-semantics/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
libs/gram/src/
├── Gram/
│   ├── Validate.hs      # NEW: Validation logic
│   ├── CST.hs           # Existing: AST definition
│   ├── Parse.hs         # Existing: Parser
│   └── Transform.hs     # Existing: Transformation
├── Gram.hs              # Existing: Re-exports

libs/gram/tests/
├── SemanticsSpec.hs     # NEW: Validation tests
```

**Structure Decision**: Introduce `Gram.Validate` as a new module to encapsulate the validation logic, keeping it separate from parsing (syntactic) and transformation (structural).

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| None | | |
