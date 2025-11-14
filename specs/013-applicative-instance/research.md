# Research: Applicative Instance for Pattern

**Feature**: 013-applicative-instance  
**Date**: 2025-01-28  
**Status**: Complete

## Research Questions

### RQ-001: How should the Applicative instance be implemented for a recursive data structure?

**Decision**: Implement `pure` and `<*>` recursively using structure-preserving/zip-like semantics. The `pure` function creates an atomic pattern (pattern with empty elements list) containing the provided value. The `<*>` operator applies functions to values at corresponding positions (root to root, element to element), recursively applying to nested patterns.

**Rationale**: 
- Structure-preserving semantics align with the decorated sequence model of Pattern
- Zip-like behavior matches the recursive nature of the data structure
- Consistent with how Functor and Traversable instances work for Pattern
- Standard pattern for Applicative instances on recursive tree structures

**Implementation Pattern**:
```haskell
instance Applicative Pattern where
  pure :: a -> Pattern a
  pure x = Pattern x []
  
  (<*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
  Pattern f fs <*> Pattern x xs = 
    Pattern (f x) (zipWith (<*>) fs xs)
```

**Breakdown**:
- `pure x`: Creates atomic pattern with value `x` and empty elements list
- `Pattern f fs <*> Pattern x xs`: Applies root function `f` to root value `x`, then recursively applies element functions to element values using `zipWith (<*>)`
- `zipWith (<*>) fs xs`: Applies functions to values at corresponding positions, truncating to minimum length (zip-like truncation for mismatched structures)

**Alternatives Considered**:
- **Cartesian product semantics**: Apply each function to each value, producing all combinations. Rejected because it doesn't align with the decorated sequence model and would create exponential growth in pattern size.
- **Root-only application**: Apply only root function to root value, ignore elements. Rejected because it doesn't utilize the full pattern structure and doesn't match the recursive nature of Pattern.
- **Non-recursive implementation**: Manual traversal and reconstruction. Rejected because recursive implementation is cleaner and leverages Haskell's type system.

**References**:
- Haskell Applicative typeclass definition in `base` package
- Standard recursive Applicative patterns in Haskell ecosystem
- Existing Functor and Traversable instances for Pattern (similar recursive structure)

---

### RQ-002: How should mismatched structures be handled?

**Decision**: Use zip-like truncation - apply functions to values up to the minimum element count, ignoring extra elements in the longer pattern.

**Rationale**:
- Permissive handling allows partial matches without errors
- Zip-like behavior is intuitive and matches standard Haskell patterns (e.g., `zipWith`)
- Aligns with structure-preserving semantics (preserves structure of the shorter pattern)
- Enables graceful degradation when structures don't match exactly

**Behavior**:
- If function pattern has 3 elements and value pattern has 5 elements: apply functions to first 3 values, ignore remaining 2 values
- If function pattern has 5 elements and value pattern has 3 elements: apply first 3 functions to values, ignore remaining 2 functions
- Root values are always applied (both patterns have exactly one root value)

**Alternatives Considered**:
- **Fail with error/exception**: Rejected because it's too restrictive and doesn't allow partial matches
- **Use default/identity**: Rejected because it's less predictable and may produce unexpected results
- **Require exact match**: Rejected because it's too restrictive and doesn't handle common use cases

**References**:
- Standard `zipWith` behavior in Haskell
- Zip-like truncation patterns in functional programming

---

### RQ-003: How should Applicative laws be verified?

**Decision**: Use property-based testing with QuickCheck to verify all Applicative laws across many pattern structures.

**Rationale**:
- Property-based testing is the standard approach for verifying mathematical laws in Haskell
- QuickCheck can generate patterns of various structures (atomic, with elements, nested) automatically
- Property tests serve as executable specifications that implementations in other languages must satisfy
- This aligns with the constitution requirement for property-based testing of category-theoretic properties

**Test Strategy**:
1. **Identity Law**: `pure id <*> v = v` - Test that applying identity function produces identical pattern
2. **Composition Law**: `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)` - Test that composition of functions equals sequential application
3. **Homomorphism Law**: `pure f <*> pure x = pure (f x)` - Test that applying pure function to pure value equals pure application
4. **Interchange Law**: `u <*> pure y = pure ($ y) <*> u` - Test that applying function pattern to pure value equals applying pure function to value pattern

**Implementation Approach**:
- Generate arbitrary patterns using QuickCheck's `Arbitrary` typeclass
- Generate arbitrary functions for function patterns
- Test laws for patterns of various structures (atomic, with elements, nested)
- Test laws for various value types (String, Int, custom types)

**Alternatives Considered**:
- Manual unit tests only: Rejected because property-based testing provides better coverage and confidence
- Formal verification: Considered but rejected as overkill for this feature; property-based testing provides sufficient confidence

**References**:
- QuickCheck documentation for property-based testing
- Applicative law verification patterns in Haskell libraries
- Existing property-based tests for Functor and Traversable instances

---

### RQ-004: How should consistency with Functor instance be verified?

**Decision**: Use property-based testing to verify that `fmap f x = pure f <*> x` for all functions `f` and patterns `x`.

**Rationale**:
- Consistency with Functor is a fundamental requirement for Applicative
- Property-based testing ensures consistency across all pattern structures and value types
- This relationship is a mathematical requirement that must hold for all patterns

**Test Strategy**:
- Generate arbitrary functions and patterns
- Verify `fmap f x = pure f <*> x` for all generated functions and patterns
- Test across various pattern structures (atomic, with elements, nested)
- Test with various value types

**References**:
- Standard relationship between Functor and Applicative in Haskell
- Existing Functor instance implementation

---

### RQ-005: What edge cases need explicit testing?

**Decision**: Test atomic patterns (no elements), patterns with empty elements list, singular patterns (one element), pair patterns (two elements), extended patterns (many elements), deeply nested patterns (3+ levels), mismatched element counts, and various value types.

**Rationale**:
- Edge cases ensure the Applicative instance works correctly for all pattern structures
- Explicit testing of edge cases provides confidence and serves as documentation
- Edge cases align with the pattern structure taxonomy established in previous features
- Mismatched structures are a key edge case for zip-like truncation semantics

**Edge Cases to Test**:
1. Atomic pattern: `Pattern f [] <*> Pattern x []` - apply root function to root value only
2. Pattern with empty elements: Same as atomic (redundant but explicit)
3. Singular pattern: `Pattern f [Pattern f1 []] <*> Pattern x [Pattern x1 []]` - apply root and single element
4. Pair pattern: `Pattern f [Pattern f1 [], Pattern f2 []] <*> Pattern x [Pattern x1 [], Pattern x2 []]` - apply root and both elements
5. Extended pattern: Multiple elements - apply root and all elements
6. Nested pattern: Deep nesting - apply recursively at all levels
7. Mismatched element counts: Function pattern with 3 elements, value pattern with 5 elements (truncate to 3)
8. Different value types: String, Int, custom types

**Alternatives Considered**:
- Relying only on property-based tests: Rejected because explicit edge case tests provide clear documentation and catch specific issues
- Testing only happy path: Rejected because edge cases are critical for a reference implementation

**References**:
- Pattern structure taxonomy from Feature 1 (Pattern Data Structure)
- Testing best practices for recursive data structures
- Zip-like truncation behavior patterns

---

### RQ-006: How should the Applicative instance be documented?

**Decision**: Provide Haddock documentation that explains: (1) the categorical interpretation (Pattern is an applicative functor), (2) structure-preserving/zip-like semantics, (3) examples demonstrating function application, (4) references to Applicative laws, and (5) consistency with Functor instance.

**Rationale**:
- Documentation must explain the mathematical meaning, not just the implementation (constitution requirement)
- Examples help developers understand how to use the Applicative instance
- Categorical interpretation aligns with the project's category theory foundations
- Clear documentation enables accurate translation to other languages

**Documentation Structure**:
1. Module-level documentation explaining Pattern as an applicative functor
2. Instance documentation with categorical interpretation
3. `pure` documentation with examples
4. `<*>` documentation with examples showing structure-preserving semantics
5. References to Applicative laws and property tests
6. Documentation of consistency with Functor instance

**Alternatives Considered**:
- Minimal documentation: Rejected because this is a reference implementation that must be clear for translation to other languages
- Implementation-only documentation: Rejected because it violates the constitution requirement for mathematical clarity

**References**:
- Haddock documentation standards
- Category theory Applicative documentation patterns
- Existing documentation for Functor and Traversable instances

---

## Technical Decisions Summary

| Decision | Rationale | Impact |
|----------|-----------|--------|
| Structure-preserving/zip-like semantics | Aligns with decorated sequence model, consistent with existing instances | Low risk, well-understood approach |
| Zip-like truncation for mismatched structures | Permissive handling, intuitive behavior | Handles edge cases gracefully |
| Property-based testing for laws | Standard approach for mathematical properties | High confidence in correctness |
| Explicit edge case testing | Ensures coverage and documentation | Clear test coverage |
| Comprehensive Haddock documentation | Reference implementation requirement | Enables accurate translation |

## Dependencies and Prerequisites

- **Pattern data type**: ✅ Complete (Feature 1)
- **Functor instance**: ✅ Complete (Feature 4) - Required for consistency verification
- **Eq instance**: ✅ Complete (Feature 2) - Required for testing equality
- **Show instance**: ✅ Complete (Feature 2) - Required for test diagnostics
- **QuickCheck**: ✅ Available in test dependencies
- **Hspec**: ✅ Available in test dependencies

## Open Questions

None. All research questions have been resolved. The implementation approach is standard and well-understood, following patterns established by existing Functor and Traversable instances.

## Next Steps

Proceed to Phase 1: Design & Contracts to generate:
- `data-model.md`: Data model for Applicative instance
- `contracts/type-signatures.md`: Type signatures and API contracts
- `quickstart.md`: Usage examples and quickstart guide

