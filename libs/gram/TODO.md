# TODO: Gram Library Implementation

**Approach**: Incremental, careful progression. Build solid foundations before advanced features.

**⚠️ IMPORTANT: Development Workflow**

**All work MUST be done in feature branches using the Speckit workflow. Never commit directly to `main`.**

1. **Create Feature Branch**: Use `/speckit.specify` to create a numbered feature branch (e.g., `015-gram-validation`)
2. **Plan**: Use `/speckit.plan` to generate implementation plan with constitution checks
3. **Tasks**: Use `/speckit.tasks` to generate dependency-ordered task list
4. **Implement**: Follow tasks, ensuring compliance with [Constitution](../../.specify/memory/constitution.md)
5. **Test**: All code must have comprehensive tests
6. **Merge**: After review and all tests pass, merge to `main`

See [README.md](../../README.md#development-workflow) for complete workflow details.

---

## Feature 1: Gram Serialization ✅

### 1.1 Serialization Implementation
- [x] Implement `toGram :: Pattern Subject -> String` in `src/Gram/Serialize.hs`
- [x] Handle all value types (standard and extended)
- [x] Support nested patterns
- [x] Support relationship patterns
- [x] Handle anonymous subjects (empty Symbol)
- [x] Write comprehensive tests for all serialization cases
- [x] Write tests for edge cases (empty patterns, deeply nested, special characters)

**Goal**: Serialize Pattern Subject structures to gram notation. ✅ **COMPLETE**

---

## Feature 2: Gram Parsing ⚠️ (Under Review)

### 2.1 Parsing Implementation
- [x] Implement `fromGram :: String -> Either ParseError (Pattern Subject)` in `src/Gram/Parse.hs`
- [x] Parse all value types (standard and extended)
- [x] Parse nested patterns
- [x] Parse relationship patterns
- [x] Handle anonymous subjects (parse to empty Symbol)
- [x] Handle comments (strip during parsing)
- [x] Write comprehensive tests for all parsing cases
- [x] Write tests for error handling
- [x] Write tests for edge cases

**Goal**: Parse gram notation strings into Pattern Subject structures. ✅ **Initial Implementation Complete**

### 2.2 Parsing Conformance Re-assessment
- [x] Integrate tree-sitter-gram corpus tests (T063)
- [ ] Run corpus tests and analyze failures (T064)
- [ ] Gap Analysis: Identify missing syntax support (e.g., Path syntax `(a)-->(b)`)
- [ ] Implement missing syntax support in `src/Gram/Parse.hs`
- [ ] Verify all corpus files parse successfully

**Goal**: Ensure parser supports ALL syntax features defined in tree-sitter-gram corpus.

---

## Feature 3: Gram Validation

### 3.1 Research Phase
- [ ] **STOP and RESEARCH**: Identify semantic validation requirements
- [ ] Review gram notation specification for semantic constraints
- [ ] Identify validation categories:
  - Identity conflicts in relationships (e.g., `(a)-[a]->(a)`)
  - Undefined references (references to non-existent identities)
  - Invalid relationship structures
  - Type mismatches
  - Scope and binding issues
- [ ] Research validation approaches in similar parsing libraries
- [ ] Document validation requirements and use cases
- [ ] Evaluate: should validation be optional or required?
- [ ] Evaluate: should validation happen during parsing or post-processing?

**Goal**: Understand what semantic validations are needed and how they should work.

### 3.2 Design Phase
- [ ] **STOP and DESIGN**: Design validation architecture
- [ ] Design `ValidationError` type and error categories
- [ ] Design `validatePattern :: Pattern Subject -> Either [ValidationError] (Pattern Subject)` function
- [ ] Design validation module structure (`Gram.Validate`)
- [ ] Design error reporting format (clear, actionable messages)
- [ ] Design integration strategy (separate function, optional validation, etc.)
- [ ] Consider: how to preserve relationship metadata for validation (currently lost in parsing)
- [ ] Document design decisions and rationale
- [ ] Create validation examples and test cases

**Goal**: Design a clean, extensible validation system.

### 3.3 User Confirmation Phase
- [ ] **STOP and CONFIRM**: Get user confirmation on design
- [ ] Present validation design to user
- [ ] Confirm validation categories and error types
- [ ] Confirm integration strategy (optional vs required, separate vs integrated)
- [ ] Confirm error reporting format
- [ ] Confirm relationship metadata preservation approach (if needed)
- [ ] Get approval to proceed with implementation

**Goal**: Ensure validation design aligns with user requirements and expectations.

### 3.4 Implementation Phase
- [ ] Implement `Gram.Validate` module
- [ ] Implement `ValidationError` type and error categories
- [ ] Implement identity conflict detection in relationships
- [ ] Implement undefined reference detection
- [ ] Implement relationship structure validation
- [ ] Implement other validation rules as designed
- [ ] Write comprehensive tests for all validation rules
- [ ] Write tests for error reporting
- [ ] Write tests for edge cases
- [ ] Add Haddock documentation with examples
- [ ] Export validation functions from `Gram` module

**Goal**: Provide semantic validation for parsed gram notation patterns.

---

## Principles

1. **Branch-Based Development**: All work done in feature branches following Speckit workflow
2. **Constitution Compliance**: All code must adhere to [Constitution](../../.specify/memory/constitution.md) principles
3. **Incremental**: Each phase builds on previous phases
4. **Testable**: Every feature has tests before moving on
5. **Review Points**: Stop and review before advanced features
6. **Round-Trip Compatibility**: Serialization and parsing must preserve structure and values
7. **Clear Error Messages**: Parse errors and validation errors must be actionable

---

## Current Status

**Current Branch**: WIP (014-gram-serialization) - Pending PR to main before addressing parsing gaps.

**Current Phase**: Feature 2.2 (Parsing Conformance) / Feature 3 (Gram Validation)

**Completed**:
- ✅ Feature 1: Gram Serialization - Complete with comprehensive tests
- ⚠️ Feature 2: Gram Parsing - Initial implementation complete, but corpus tests reveal missing syntax support (Path syntax)

**Next Steps**: 
1. Merge current WIP to main
2. Create new branch for parsing conformance (addressing path syntax gaps)
3. Complete corpus integration verification
4. Proceed to Feature 3 (Validation)

---

## Notes

- **Workflow**: Always use feature branches and Speckit commands (`/speckit.specify`, `/speckit.plan`, `/speckit.tasks`)
- **Constitution**: Review [Constitution](../../.specify/memory/constitution.md) before starting any work
- **Round-Trip**: Serialization and parsing must preserve structure (formatting may vary)
- **Anonymous Subjects**: Use empty Symbol ("") to represent anonymous subjects, serialize as anonymous syntax
- **Comments**: Comments are stripped during parsing, not preserved in Pattern Subject structure
- **Validation**: Semantic validation is separate from syntax parsing - parser handles syntax, validator handles semantics

