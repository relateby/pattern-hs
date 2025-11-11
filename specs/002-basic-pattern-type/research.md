# Research: Basic Pattern Type

**Feature**: 002-basic-pattern-type  
**Date**: 2025-01-27

## Decisions

### Technical Context

**Decision**: Use existing project infrastructure from feature 001 (GHC 9.10.3, Cabal, QuickCheck/HSpec).

**Rationale**: 
- Feature 001 already established the project structure and tooling
- No need to revisit build tools, testing frameworks, or GHC version
- Consistent with existing project setup
- All technical decisions from feature 001 apply to this feature

**Action**: Use existing `pattern.cabal` configuration and project structure.

---

### Pattern Type Definition Approach

**Decision**: Define Pattern as a standard Haskell algebraic data type with record syntax.

**Rationale**:
- Record syntax provides automatic field accessors (`value`, `elements`)
- Standard Haskell pattern for recursive data structures
- Clear and self-documenting
- Compatible with future typeclass instances (Functor, Foldable, Traversable)
- Easy to translate to other languages

**Alternatives considered**:
- GADT (Generalized Algebraic Data Types): More powerful but unnecessary complexity for basic type
- Newtype wrapper: Not appropriate - Pattern is a new type, not a wrapper
- Type synonym: Not sufficient - need actual data constructor

**Action**: Use standard data type with record syntax:
```haskell
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  }
```

---

### Field Accessor Naming

**Decision**: Use `value` and `elements` as field names.

**Rationale**:
- `value` clearly indicates the value stored in the pattern
- `elements` clearly indicates the pattern elements (more descriptive than `children`)
- Consistent with existing design documents (data-model.md from feature 001)
- Generic enough to work for all pattern interpretations

**Alternatives considered**:
- `node` and `children`: More graph-specific, but patterns are more general
- `content` and `subpatterns`: Less clear, more verbose
- `val` and `kids`: Too informal, less descriptive

**Action**: Use `value` and `elements` as specified in feature 001 design documents.

---

### Documentation Style

**Decision**: Use Haddock documentation with mathematical explanations.

**Rationale**:
- Haddock is the standard Haskell documentation tool
- Supports mathematical notation through LaTeX-style comments
- Integrates with Haskell tooling (IDE support, documentation generation)
- Required by Constitution for all public APIs
- Must explain recursive tree structure clearly

**Alternatives considered**:
- External documentation only: Less discoverable, harder to maintain
- Minimal comments: Insufficient for reference implementation

**Action**: Provide comprehensive Haddock documentation explaining:
- The sequence-based conceptual model
- How values are stored
- How elements form the pattern sequence
- Examples of atomic patterns and patterns with elements

---

### Testing Approach

**Decision**: Start with unit tests for construction and inspection; property tests in future phases.

**Rationale**:
- This phase only defines the data type - no typeclass instances yet
- Unit tests verify basic construction and field accessors
- Property-based tests for Functor/Foldable/Traversable laws will come in Phase 4-6
- Follows incremental approach from TODO.md
- Tests serve as executable specifications

**Alternatives considered**:
- Property tests from start: Premature - no typeclass instances to test yet
- No tests: Violates Constitution testing standards

**Action**: Write unit tests for:
- Creating atomic patterns (patterns with no elements)
- Creating patterns with elements
- Verifying field accessors return correct values
- Testing edge cases (empty elements, singular pattern, many elements)

---

## Summary

All technical decisions align with existing project infrastructure from feature 001. The Pattern type will be defined as a standard Haskell algebraic data type with record syntax, providing automatic field accessors. Comprehensive Haddock documentation will explain the recursive tree structure. Testing will focus on unit tests for construction and inspection, with property-based tests deferred to future phases when typeclass instances are implemented.

---

### Semantic Clarification: Pattern Length

**Decision**: A Pattern is conceptually a sequence of elements with associated metadata (the value), not a tree structure. While the implementation uses a recursive tree structure, semantically patterns are sequences.

**Terminology**:
- `elements` - the sequence of pattern elements (not "children")
- `value` - information/metadata about the sequence
- `length (elements p)` - current way to get sequence length (number of elements)

**Future Addition (Phase 7)**: 
- `length :: Pattern v -> Int` - will be added alongside `depth` to provide idiomatic access to pattern sequence length
- This will be equivalent to `length (elements p)` but more semantically appropriate

**Rationale**: 
- Patterns are sequences: "3 1 4 1 9 5" is a pattern of length 6
- The recursive structure enables representing complex sequences, but the primary semantic is sequence-based
- Adding `length` as a Pattern function makes the API more intuitive

**Action**: Continue using `length (elements p)` in current code. Plan to add `length :: Pattern v -> Int` in Phase 7 alongside `depth :: Pattern v -> Int`.

---

The feature is ready to proceed with Phase 1 design artifacts.

