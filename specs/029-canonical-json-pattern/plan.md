# Implementation Plan: Canonical JSON Pattern Representation

**Branch**: `029-canonical-json-pattern` | **Date**: 2026-01-10 | **Spec**: [spec.md](./spec.md)  
**Input**: Feature specification from `/specs/029-canonical-json-pattern/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Establish a canonical JSON representation of Pattern<Subject> as a first-class serialization format with formal specification, roundtrip testing, and schema generation capabilities. Enhance the gramref CLI tool to generate JSON Schema and type definitions in multiple target languages (TypeScript, Rust), enabling downstream projects to implement compatible Pattern structures without reverse-engineering the Haskell code. Implement comprehensive roundtrip testing (gram notation ↔ JSON) across the entire tree-sitter-gram test corpus to ensure lossless conversion and data integrity.

## Technical Context

**Language/Version**: Haskell with GHC 9.12.2  
**Primary Dependencies**: aeson (JSON), aeson-pretty (formatting), optparse-applicative (CLI), pattern (core), subject (values), gram (serialization)  
**Storage**: N/A (serialization library feature)  
**Testing**: hspec (unit tests), QuickCheck (property-based tests), existing tree-sitter-gram test corpus  
**Target Platform**: Multi-platform CLI tool (Linux, macOS, Windows)  
**Project Type**: Multi-package library and CLI application  
**Performance Goals**: Handle patterns with thousands of nested elements without stack overflow; schema generation completes in under 1 second for complete schema  
**Constraints**: Must maintain backward compatibility with existing gramref JSON output format; must not break existing CLI commands; roundtrip tests must pass 100% of tree-sitter-gram corpus  
**Scale/Scope**: ~130 existing test corpus files, 15+ value types, arbitrary pattern nesting depth

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: ✅ PASS (Verified Post-Phase 1) - Design leverages existing `Gramref.CLI.JSON` module as basis for canonical format, refactored to `Gram.JSON` for library-level access. Schema generation modules (`Gram.Schema.*`) will be clearly documented with Haddock comments explaining categorical structure. Public APIs include usage examples in quickstart.md. Clear separation maintained: core serialization in libs/gram, CLI interface in apps/gramref-cli. FromJSON instances provide bijective mapping preserving functor structure.

- **Testing Standards (NON-NEGOTIABLE)**: ✅ PASS (Verified Post-Phase 1) - Comprehensive test strategy defined: (1) Roundtrip property tests using QuickCheck verify `fromJSON . toJSON ≡ id`, (2) Unit tests for each value type serialization/deserialization, (3) Integration tests against tree-sitter-gram corpus (130+ files), (4) CLI command tests for schema generation, (5) Contract tests validating generated JSON against schema. Property-based tests will verify functor laws hold through JSON transformation. Tests serve as executable specification for ports.

- **Conceptual Consistency**: ✅ PASS (Verified Post-Phase 1) - Pattern<Subject> functor structure explicitly preserved in JSON representation: `{ value: Subject, elements: [Pattern] }` maintains recursive covariant functor. JSON Schema formally specifies this structure using recursive `$ref`. The bijective mapping (toJSON/fromJSON) forms an isomorphism in the category of data structures. No violations of compositional properties - element-wise transformation commutes with JSON serialization. Documented in data-model.md with categorical interpretation.

- **Mathematical Clarity**: ✅ PASS (Verified Post-Phase 1) - Formal definitions provided in data-model.md. Roundtrip property formally stated: `∀ p : Pattern Subject. fromJSON (toJSON p) ≡ p` (using Eq for semantic equivalence). Value type representations clearly specified with JSON Schema using `oneOf` for sum types. Semantic equivalence rules explicitly defined (structural equality, format-independent). JSON Schema Draft 2020-12 provides formal mathematical foundation. Type generation preserves algebraic structure (products → records, sums → discriminated unions).

- **Multi-Language Reference Alignment**: ✅ PASS (Verified Post-Phase 1) - JSON is inherently language-agnostic. Schema serves as formal specification independent of Haskell. Generated TypeScript and Rust types demonstrate cross-language applicability with concrete examples in contracts/. Data model documentation explicitly maps Haskell types to JSON structures: Set → sorted array, Map → object with sorted keys. Type discriminators for sum types follow standard JSON patterns. Quickstart.md provides language-specific integration examples. Core Pattern structure translates cleanly to any language with records and arrays.

**Violations**: None

**Phase 1 Verification**: All constitutional principles maintained after detailed design. The canonical JSON format, schema generation, and roundtrip testing align perfectly with reference implementation requirements. Type generation for TypeScript and Rust demonstrates multi-language alignment. Test strategy ensures mathematical correctness through property-based testing of categorical laws.

## Project Structure

### Documentation (this feature)

```text
specs/029-canonical-json-pattern/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command) ✅ COMPLETE
├── data-model.md        # Phase 1 output (/speckit.plan command) ✅ COMPLETE
├── quickstart.md        # Phase 1 output (/speckit.plan command) ✅ COMPLETE
├── contracts/           # Phase 1 output (/speckit.plan command) ✅ COMPLETE
│   ├── json-schema.json       # JSON Schema definition for Pattern<Subject>
│   ├── typescript-types.ts     # TypeScript type definitions
│   └── rust-types.rs          # Rust struct definitions with serde
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
libs/gram/
├── src/
│   └── Gram/
│       ├── Serialize.hs           # Existing: toGram
│       ├── Parse.hs               # Existing: fromGram
│       ├── JSON.hs                # NEW: Canonical JSON serialization (moved from gramref-cli)
│       ├── Schema.hs              # NEW: JSON Schema generation
│       └── Schema/
│           ├── JSONSchema.hs     # NEW: JSON Schema format output
│           ├── TypeScript.hs     # NEW: TypeScript type generation
│           └── Rust.hs           # NEW: Rust type generation
└── tests/
    └── Spec/
        └── Gram/
            ├── JSONSpec.hs        # NEW: JSON serialization tests
            ├── RoundtripSpec.hs   # NEW: Roundtrip tests
            └── SchemaSpec.hs      # NEW: Schema generation tests

apps/gramref-cli/
├── src/
│   └── Gramref/
│       └── CLI/
│           ├── JSON.hs                    # REMOVE: Move to libs/gram/src/Gram/JSON.hs
│           ├── Commands/
│           │   └── Schema.hs              # NEW: Schema command implementation
│           └── Output.hs                  # UPDATE: Support schema output
└── tests/
    └── Spec/
        └── Gramref/
            └── CLI/
                └── SchemaCommandSpec.hs   # NEW: CLI schema command tests

test-data/
└── roundtrip/                             # NEW: Roundtrip test data
    ├── corpus/                            # Symlink to tree-sitter-gram corpus
    └── custom/                            # Additional roundtrip test cases
```

**Structure Decision**: The feature extends existing libs/gram with JSON serialization as a core capability (moving it from gramref-cli to the library). Schema generation is added as a new module within gram. The gramref CLI gets a new `schema` command that uses these library functions. This maintains the separation where libs/gram handles core functionality and apps/gramref-cli provides the user interface.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

No violations to justify. The design aligns with all constitutional principles.

## Proposed Changes

### Phase 0: Research & Technical Design ✅ COMPLETE

**Objective**: Resolve technical unknowns and establish implementation approach for schema generation and roundtrip testing.

**Output**: `research.md` documenting decisions, rationale, and alternatives considered

**Key Decisions Made**:
1. JSON Schema Generation: Manual construction using aeson Value builders
2. JSON Schema Version: Draft 2020-12
3. TypeScript Generation: Template-based in Haskell + external tool recommendation
4. Rust Generation: Template-based in Haskell
5. Roundtrip Testing: Semantic equivalence via Eq, integrated with tree-sitter-gram corpus
6. Value Type Representation: Keep existing format (confirmed optimal)
7. Deserialization: Implement full FromJSON instances

---

### Phase 1: Data Model & API Contracts ✅ COMPLETE

**Objective**: Define the canonical JSON format structure, schema output formats, and API contracts.

**Outputs**:
- ✅ `data-model.md` - Complete formal definition of all entities
- ✅ `contracts/json-schema.json` - JSON Schema Draft 2020-12 specification
- ✅ `contracts/typescript-types.ts` - TypeScript interfaces with type guards
- ✅ `contracts/rust-types.rs` - Rust structs with serde, includes tests
- ✅ `quickstart.md` - Usage examples and common patterns
- ✅ Agent context updated with new dependencies

**Data Model Entities Defined**:
1. Canonical JSON Pattern - Structure and validation rules
2. Subject - Identity, labels, properties mapping
3. Value - Discriminated union for all value types
4. JSON Schema Document - Formal specification structure
5. TypeScript Type Definitions - Generated interface structure
6. Rust Type Definitions - Generated struct structure with serde
7. Semantic Equivalence Rules - Roundtrip testing criteria
8. Schema Versioning - Versioning and compatibility rules

**Contract Examples Created**:
- Complete JSON Schema with all definitions and recursive references
- TypeScript types with discriminated unions and type guards
- Rust types with serde derives and convenience methods including unit tests

---

### Phase 2: Implementation Plan (Detailed in tasks.md)

**Note**: Phase 2 is executed via `/speckit.tasks` command, which generates the detailed task breakdown in `tasks.md`.

**High-Level Implementation Sequence**:

1. **Move and Enhance JSON Serialization** (User Story 2, US3)
   - Refactor `Gramref.CLI.JSON` → `Gram.JSON` for library-level access
   - Ensure deterministic serialization (canonical form)
   - Add deserialization (fromJSON functions)

2. **Implement Roundtrip Testing** (User Story 3)
   - Create roundtrip test suite against tree-sitter-gram corpus
   - Implement semantic equivalence checking
   - Verify 100% corpus coverage

3. **Implement Schema Generation Core** (User Story 1)
   - Create `Gram.Schema` module
   - Implement JSON Schema generation
   - Add version information and metadata

4. **Add CLI Schema Command** (User Story 1)
   - Create `Gramref.CLI.Commands.Schema`
   - Wire into gramref command parser
   - Support `--format` flag (json-schema, typescript, rust)

5. **Implement Type Generation** (User Story 4)
   - Add TypeScript type generation
   - Add Rust type generation with serde
   - Test compilation in target languages

6. **Validation Capabilities** (User Story 5)
   - Add JSON validation function against schema
   - Provide clear error messages
   - CLI integration for validation command

7. **Documentation and Polish** (All User Stories)
   - Update docs/reference/features/gram-serialization.md
   - Add JSON format specification document
   - CLI manpage updates for schema command
   - Examples and usage guide

**Dependencies**:
- Phase 1 (data model) must complete before implementation ✅
- Roundtrip testing depends on JSON serialization/deserialization
- Schema generation can proceed in parallel with roundtrip testing
- Type generation depends on JSON Schema being complete
- Validation depends on schema generation

**Testing Strategy**:
- Unit tests for each serialization/deserialization function
- Property tests for roundtrip properties
- Integration tests for CLI commands
- Contract tests against generated schema
- Corpus-based tests against tree-sitter-gram test data

**Success Gates**:
- All existing gramref tests continue to pass
- 100% roundtrip success on tree-sitter-gram corpus
- Generated schema validates all test outputs
- Generated TypeScript/Rust types compile successfully
- All constitutional principles maintained

---

## Phase 3: Review & Polish (Executed via /speckit.review)

**Note**: Phase 3 is a separate command, not executed by `/speckit.plan`.

Post-implementation review will verify:
- All user stories delivered with passing acceptance scenarios
- Success criteria met (SC-001 through SC-008)
- Constitution compliance maintained
- Documentation complete and accurate
- No regressions in existing functionality
