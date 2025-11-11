# Research: Construction Functions

**Feature**: 004-construction-functions  
**Date**: 2025-01-28  
**Status**: Complete

## Research Questions

### 1. Function Naming Conventions

**Question**: What are standard Haskell naming conventions for constructor functions?

**Decision**: Use lowercase camelCase names: `pattern` and `patternWith`

**Rationale**: 
- Standard Haskell convention for functions is lowercase with camelCase
- `pattern` is concise and clear for the atomic pattern case
- `patternWith` follows the common Haskell pattern of "baseNameWith" for functions that take additional parameters
- These names are self-documenting and align with existing Haskell library conventions

**Alternatives Considered**:
- `mkPattern` / `mkPatternWith`: More explicit but verbose, not commonly used in modern Haskell
- `atomic` / `withElements`: Less clear about what they create
- `newPattern` / `newPatternWith`: Implies object construction, not idiomatic for functional data types

**References**: 
- Haskell Style Guide: Function names should be lowercase
- Common patterns: `fromList`, `withDefault`, `fromMaybe` - the "with" suffix is idiomatic

---

### 2. Function Type Signatures

**Question**: What should the exact type signatures be?

**Decision**: 
- `pattern :: v -> Pattern v`
- `patternWith :: v -> [Pattern v] -> Pattern v`

**Rationale**:
- `pattern` takes a value and returns an atomic pattern (no elements)
- `patternWith` takes a value and a list of pattern elements, returns a pattern with those elements
- Type signatures are explicit and preserve the type parameter `v`
- Order of parameters (value first, then elements) matches the Pattern data type structure

**Alternatives Considered**:
- `patternWith :: [Pattern v] -> v -> Pattern v`: Reversed order, but value-first is more natural
- Curried versions: Not needed, current signatures are clear

**References**:
- Pattern data type: `Pattern { value :: v, elements :: [Pattern v] }`
- Standard Haskell practice: Match data type field order when possible

---

### 3. Implementation Approach

**Question**: Should these be simple wrappers or include validation?

**Decision**: Simple pure functions that directly construct patterns using record syntax

**Rationale**:
- Pattern data type already enforces type safety through Haskell's type system
- No validation needed - any value and any list of patterns is valid
- Empty lists are valid (produce atomic patterns)
- Simplicity aligns with functional programming principles
- Performance: Direct construction, no overhead

**Alternatives Considered**:
- Add validation for empty lists in `patternWith`: Not needed, empty list is valid input
- Add smart constructors with Maybe types: Unnecessary complexity, all inputs are valid
- Add logging or debugging: Not appropriate for pure functions

**References**:
- Pattern data type definition: No constraints on values or elements
- Functional programming principle: Prefer simple, pure functions

---

### 4. Testing Strategy

**Question**: What testing approach should be used?

**Decision**: Comprehensive unit tests covering all element counts (0, 1, 2, many) plus property-based tests for equivalence

**Rationale**:
- Unit tests verify specific cases: atomic patterns, singular patterns, pairs, extended patterns
- Property-based tests verify functional equivalence with record syntax
- Tests for all value types (strings, integers, custom types)
- Tests for edge cases (empty lists, nested patterns, element order)
- Tests verify type safety and element order preservation

**Alternatives Considered**:
- Only unit tests: Missing property-based verification of equivalence
- Only property tests: Missing explicit coverage of specific cases
- Integration tests: Not needed for simple constructor functions

**References**:
- Constitution: Testing Standards require comprehensive coverage
- Feature spec: Success criteria require 100% test coverage and behavioral equivalence

---

### 5. Documentation Approach

**Question**: What documentation style should be used?

**Decision**: Comprehensive Haddock documentation with examples for each function

**Rationale**:
- Haddock is standard for Haskell libraries
- Examples should demonstrate: atomic patterns, singular patterns, pairs, extended patterns
- Documentation should explain relationship to record syntax
- Include examples with different value types
- Show usage in context (building graph structures)

**Alternatives Considered**:
- Minimal documentation: Violates Code Quality principle
- Separate tutorial: Not needed for simple functions, examples in Haddock sufficient

**References**:
- Constitution: Code Quality requires all public APIs documented with examples
- Existing Pattern.Core.hs: Comprehensive Haddock documentation already present

---

### 6. Module Organization

**Question**: Where should these functions be placed?

**Decision**: Add to `Pattern.Core` module alongside the Pattern data type

**Rationale**:
- Constructor functions are core functionality
- Keeps related code together (data type and constructors)
- Matches existing module structure
- No need for separate module for simple functions

**Alternatives Considered**:
- Separate `Pattern.Constructors` module: Unnecessary complexity for two functions
- Add to main `Pattern` module: Core functionality belongs in Core module

**References**:
- Existing structure: Pattern data type in `Pattern.Core`
- Module organization: Keep related functionality together

---

### 7. Export Strategy

**Question**: Should these be exported from the main Pattern module?

**Decision**: Export from both `Pattern.Core` and re-export from main `Pattern` module

**Rationale**:
- `Pattern.Core` exports for direct access
- Main `Pattern` module re-exports for convenience (matches existing pattern)
- Allows both `Pattern.Core.pattern` and `Pattern.pattern` usage
- Matches existing export strategy for Pattern data type

**Alternatives Considered**:
- Only export from Core: Less convenient for users
- Only export from main: Hides module structure

**References**:
- Existing exports: Pattern data type exported from both modules
- User convenience: Main module should provide easy access to common functions

---

---

### 8. List Conversion Function

**Question**: Should we provide a function to convert a list of values into a pattern?

**Decision**: Add `fromList :: v -> [v] -> Pattern v` function

**Rationale**:
- Follows standard Haskell convention (`Set.fromList`, `Map.fromList`, etc.)
- Provides convenient way to create patterns from lists of values
- Each value in the list is automatically converted to an atomic pattern
- Reduces verbosity when creating patterns from raw data
- Complements `patternWith` which takes existing patterns

**Type Signature**: `fromList :: v -> [v] -> Pattern v`

**Behavior**:
- Takes a decoration value and a list of values
- Converts each value in the list to an atomic pattern using `pattern`
- Creates a pattern with those atomic patterns as elements using `patternWith`
- Functionally equivalent to: `fromList decoration values = patternWith decoration (map pattern values)`

**Example**:
```haskell
-- Convert list of strings to pattern
p = fromList "graph" ["Alice", "Bob", "Charlie"]
-- Equivalent to:
p' = patternWith "graph" [pattern "Alice", pattern "Bob", pattern "Charlie"]
```

**Alternatives Considered**:
- `fromValues`: More explicit but less conventional
- `patternFromList`: Consistent with `pattern*` naming but more verbose
- `fromListWith`: Combines conventions but `With` suffix usually indicates optional parameters

**References**:
- Standard Haskell libraries: `Set.fromList`, `Map.fromList`, `Vector.fromList`
- Common pattern: `from*` prefix indicates conversion from another structure

---

## Summary

All research questions resolved. The implementation will:

1. Add three simple pure functions: `pattern`, `patternWith`, and `fromList`
2. Use standard Haskell naming conventions
3. Implement as direct wrappers around record syntax (or combinations thereof)
4. Include comprehensive tests (unit + property-based)
5. Provide Haddock documentation with examples
6. Place in `Pattern.Core` module
7. Export from both `Pattern.Core` and main `Pattern` module

No unresolved questions or NEEDS CLARIFICATION items remain.

