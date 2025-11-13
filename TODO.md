# TODO: Pattern Implementation

**Approach**: Incremental, careful progression. Build solid foundations before advanced features.

**⚠️ IMPORTANT: Development Workflow**

**All work MUST be done in feature branches using the Speckit workflow. Never commit directly to `main`.**

1. **Create Feature Branch**: Use `/speckit.specify` to create a numbered feature branch (e.g., `003-construction-functions`)
2. **Plan**: Use `/speckit.plan` to generate implementation plan with constitution checks
3. **Tasks**: Use `/speckit.tasks` to generate dependency-ordered task list
4. **Implement**: Follow tasks, ensuring compliance with [Constitution](.specify/memory/constitution.md)
5. **Test**: All code must have comprehensive tests
6. **Merge**: After review and all tests pass, merge to `main`

See [README.md](README.md#development-workflow) for complete workflow details.

---

## Feature 1: Core Data Type (Foundation) ✅

### 1.1 Basic Pattern Type
- [x] Define `Pattern v` data type with `value :: v` and `elements :: [Pattern v]` in `src/Pattern/Core.hs`
- [x] Add basic Haddock documentation explaining the recursive tree structure
- [x] Write simple test: create a pattern with no member elements and verify structure
- [x] Write simple test: create a pattern with member elements and verify structure

**Goal**: Have a working Pattern type that can be constructed and inspected. ✅ **COMPLETE**

---

## Feature 2: Basic Typeclasses ✅

### 2.1 Show Instance
- [x] Implement `Show` instance for `Pattern v` (requires `Show v`)
- [x] Write test: `show` of a pattern produces readable output
- [x] Write test: nested patterns display correctly

### 2.2 Eq Instance
- [x] Implement `Eq` instance for `Pattern v` (requires `Eq v`)
- [x] Write test: two identical patterns are equal
- [x] Write test: two different patterns are not equal
- [x] Write test: patterns with different structures are not equal

**Goal**: Patterns can be compared and displayed. ✅ **COMPLETE**

---

## Feature 3: Construction Functions ✅

### 3.1 Basic Constructors
- [x] Implement `pattern :: v -> Pattern v` (creates atomic pattern)
- [x] Implement `patternWith :: v -> [Pattern v] -> Pattern v` (creates pattern with member elements)
- [x] Implement `fromList :: v -> [v] -> Pattern v` (creates pattern from list of values by converting each to atomic pattern)
- [x] Write tests: construct various patterns and verify structure
- [x] Write tests: edge cases -- empty sequence (atomic pattern), one element (singular pattern), two elements (a pair), many elements (an extended pattern)
- [x] Add comprehensive Haddock documentation with examples
- [x] Export all constructor functions from `Pattern.Core` and re-export from main `Pattern` module

**Constructor Functions Added**:
1. **`pattern :: v -> Pattern v`** - Creates an atomic pattern (pattern with no elements) from a single value. Provides a convenient alternative to `Pattern { value = v, elements = [] }`.
2. **`patternWith :: v -> [Pattern v] -> Pattern v`** - Creates a pattern with elements from a value and a list of pattern elements. Provides a convenient alternative to `Pattern { value = v, elements = ps }`.
3. **`fromList :: v -> [v] -> Pattern v`** - Creates a pattern from a list of values by converting each value to an atomic pattern. Implemented as `patternWith decoration (map pattern values)`.

**Goal**: Convenient ways to create patterns. ✅ **COMPLETE**

---

## Feature 4: Functor Instance ✅

### 4.1 Functor Implementation
- [x] Implement `Functor` instance for `Pattern`
- [x] Write property test: `fmap id = id` (functor identity law)
- [x] Write property test: `fmap (f . g) = fmap f . fmap g` (functor composition law)
- [x] Write unit tests: transform values in simple patterns
- [x] Write unit tests: transform values in nested patterns

**Goal**: Patterns can have their values transformed while preserving structure. ✅ **COMPLETE**

---

## Feature 5: Foldable Instance ✅

### 5.1 Foldable Implementation
- [x] Implement `Foldable` instance for `Pattern`
- [x] Write test: `foldr` collects all values
- [x] Write test: `foldl` collects all values (verify order)
- [x] Write test: `foldMap` works correctly
- [x] Write test: `toList` produces all values

**Goal**: Patterns can be folded/traversed for aggregation. ✅ **COMPLETE**

**Foldable Instance Added**:
1. **`Foldable` instance** - Enables folding over all values in pattern structures with `foldr`, `foldl`, `foldMap`, and `toList`
2. **`flatten :: Pattern a -> [a]`** - Explicit function for extracting all values as a flat list (equivalent to `toList`)
3. **`toTuple :: Pattern v -> (v, [Pattern v])`** - Extracts pattern as tuple preserving structure
4. Comprehensive tests for all foldable operations (231 test cases, all passing)
5. Property-based tests verifying foldable laws and properties
6. Complete Haddock documentation with examples

---

## Feature 6: Traversable Instance ✅

### 6.1 Traversable Implementation
- [x] Implement `Traversable` instance for `Pattern`
- [x] Write test: `traverse` with `Identity` preserves structure
- [x] Write test: `traverse` with `Maybe` handles failures
- [x] Write test: `sequenceA` works correctly

**Goal**: Patterns can be traversed with effects. ✅ **COMPLETE**

**Traversable Instance Added**:
1. **`Traversable` instance** - Enables effectful traversal over pattern values with `traverse` and `sequenceA`
2. **`traverse`** - Applies effectful functions to all values while preserving pattern structure
3. **`sequenceA`** - Sequences applicative effects from patterns containing applicative values
4. Comprehensive tests for all traversable operations (293 test cases, all passing)
5. Property-based tests verifying all Traversable laws (Naturality, Identity, Composition)
6. Support for Identity, Maybe, Either, IO, State, and other applicative functors
7. Complete Haddock documentation with examples
8. Validation use cases with error handling
9. All 80 tasks (T001-T080) completed across 6 phases

---

## Feature 7: Basic Query Functions ✅

### 7.1 Size and Depth
- [x] Implement `length :: Pattern v -> Int` (number of elements in the sequence - equivalent to `length (elements p)`)
- [x] Implement `size :: Pattern v -> Int` (total number of nodes)
- [x] Implement `depth :: Pattern v -> Int` (maximum nesting depth)
- [x] Write tests: length, size and depth for various patterns
- [x] Write tests: edge cases (empty, single node, deep nesting)

### 7.2 Value Access
- [x] Verify `value` field accessor works (already from data type)
- [x] Implement `values :: Pattern v -> [v]` (all values in pattern)
- [x] Write tests: extract all values from patterns

**Goal**: Basic introspection of pattern structure. ✅ **COMPLETE**

**Basic Query Functions Added**:
1. **`length :: Pattern v -> Int`** - Returns the number of direct elements in a pattern's sequence (O(1))
2. **`size :: Pattern v -> Int`** - Returns the total number of nodes in a pattern structure, including all nested patterns (O(n))
3. **`depth :: Pattern v -> Int`** - Returns the maximum nesting depth of a pattern structure (O(n))
4. **`values :: Pattern v -> [v]`** - Extracts all values from a pattern structure as a flat list (O(n), equivalent to `toList`)
5. **`value` field accessor** - Direct access to a pattern's decoration value (O(1), already from data type, now documented)
6. Comprehensive tests for all query functions (333 test cases, all passing)
7. Property-based tests verifying query function properties
8. Integration tests with pattern constructors and type class instances
9. Edge case tests (100+ levels nesting, 100+ elements, duplicate values)
10. Complete Haddock documentation with examples for all functions
11. Module-level documentation updated to include query functions
12. All 67 tasks (T001-T067) completed across 6 phases (5 user stories + integration)

---

## Feature 8: Additional Typeclasses (As Needed)

### 8.1 Ord Instance ✅
- [x] Consider if `Ord` instance is needed
- [x] If yes, implement `Ord` instance with clear ordering semantics
- [x] Write tests: ordering of patterns

**Ord Instance Added**:
1. **`Ord` instance** - Provides lexicographic ordering for patterns based on structure (value first, then elements recursively)
2. **Lexicographic ordering** - Patterns are ordered by comparing their value first, then their elements recursively
3. **Consistency with Eq** - Ord instance is consistent with Eq instance (equal patterns compare as equal)
4. **Standard library integration** - Works with `Data.Set` and `Data.Map` for pattern organization and indexing
5. **Comprehensive tests** - All tests passing (370 examples, 0 failures):
   - Unit tests for compare, operators, min/max (T001-T007)
   - Property-based tests for ordering properties (T008-T011)
   - Integration tests for Data.Set and Data.Map (T016-T024)
   - Consistency tests with Eq instance (T028-T034)
   - Edge case tests (T037-T041)
   - Integration tests with existing functions and type classes (T042-T044)
6. **Complete Haddock documentation** - Module-level and instance-level documentation with examples
7. **Type constraint** - Requires `Ord v` constraint, ensuring type safety
8. **All 47 tasks (T001-T047) completed** across 4 phases (3 user stories + edge cases)

**Goal**: Patterns can be ordered and used in standard library collections. ✅ **COMPLETE**

### 8.2 Other Instances
- [ ] Evaluate if other standard typeclasses are needed (e.g., `Semigroup`, `Monoid`)
- [ ] Implement only if clearly needed for core functionality

**Goal**: Complete basic typeclass coverage.

**Status**: Ord instance complete ✅. Other instances (Semigroup, Monoid) can be evaluated as needed.

---

## Feature 9: Graph Views (Underspecified)

### 9.1 GraphView Typeclass Design
- [ ] **STOP and REVIEW**: Is GraphView needed yet?
- [ ] Design `GraphView` typeclass interface (minimal)
- [ ] Consider: can we defer this entirely?

### 9.2 DirectedView (If Proceeding)
- [ ] Implement minimal `DirectedView` if needed
- [ ] Keep implementation simple

**Goal**: Basic view support if needed.

**Note**: Advanced views and view composition deferred until core is solid.

---

## Feature 10: Pattern Morphisms (Underspecified)

### 10.1 Morphism Design
- [ ] **STOP and REVIEW**: Are morphisms needed yet?
- [ ] Define `PatternMorphism` type synonym
- [ ] Implement `homomorphism` if clearly needed
- [ ] Implement `forget` if clearly needed

**Goal**: Basic morphisms if needed for core.

**Note**: Advanced morphism theory deferred until core is solid.

---

## Feature 11: Integration and Polish

### 17.1 Module Exports
- [ ] Review and finalize exports from `Pattern.Core`
- [ ] Update `Pattern.hs` main module exports
- [ ] Ensure clean public API

### 17.2 Documentation
- [ ] Add comprehensive Haddock documentation
- [ ] Include usage examples
- [ ] Document mathematical properties where applicable

### 17.3 Testing
- [ ] Review test coverage
- [ ] Add property-based tests for laws (functor, etc.)
- [ ] Add edge case tests

**Goal**: Polished, well-documented core implementation.

---

## Principles

1. **Branch-Based Development**: All work done in feature branches following Speckit workflow
2. **Constitution Compliance**: All code must adhere to [Constitution](.specify/memory/constitution.md) principles
3. **Incremental**: Each phase builds on previous phases
4. **Testable**: Every feature has tests before moving on
5. **Review Points**: Stop and review before advanced features (Phases 14-16)
6. **Underspecified**: Advanced features (graphs, views, morphisms) are minimal until core is solid
7. **Solid Core First**: Ensure core Pattern type is well-understood before adding complexity

---

## Current Status

**Current Phase**: Phase 8.1 (Ord Instance) - Complete ✅

**Completed**:
- ✅ Phase 1: Core Pattern type fully implemented with comprehensive tests (25 test cases)
- ✅ Phase 2: Show and Eq instances implemented with comprehensive tests (13 additional test cases)
- ✅ Phase 3: Construction Functions implemented with comprehensive tests:
  - `pattern` function for atomic patterns
  - `patternWith` function for patterns with elements
  - `fromList` function for patterns from lists of values
  - All functions fully tested (81 total test cases including property-based tests)
  - Comprehensive edge case coverage (0, 1, 2, many elements)
  - All functions exported and documented
- ✅ Phase 4: Functor Instance implemented with comprehensive tests:
  - `Functor` instance for `Pattern` with recursive `fmap` implementation
  - Property-based tests for functor laws (identity and composition)
  - Unit tests for all pattern structures (atomic, with elements, nested)
  - Edge case tests (singular, pair, extended patterns, type transformations)
  - All tests passing (81 examples, 0 failures)
  - Comprehensive Haddock documentation with examples
  - Performance optimizations for property-based tests (~6ms total runtime)
- ✅ Phase 5: Foldable Instance implemented with comprehensive tests:
  - `Foldable` instance for `Pattern` with `foldr`, `foldl`, `foldMap`, and `toList`
  - `flatten` function for explicit value extraction
  - `toTuple` function for structure-preserving extraction
  - Property-based tests for foldable laws and properties
  - Unit tests for all pattern structures and edge cases
  - All tests passing (231 examples, 0 failures)
  - Comprehensive Haddock documentation with examples
  - Test performance verified (<10ms for property-based tests)
- ✅ Phase 6: Traversable Instance implemented with comprehensive tests:
  - `Traversable` instance for `Pattern` with `traverse` and `sequenceA`
  - Support for Identity, Maybe, Either, IO, State, and other applicative functors
  - Property-based tests for all Traversable laws (Naturality, Identity, Composition)
  - Unit tests for effectful traversal, validation, and error handling
  - Tests for IO and State applicative functors
  - All tests passing (293 examples, 0 failures)
  - Comprehensive Haddock documentation with examples
  - Test performance verified (<10ms for property-based tests)
  - All 80 tasks (T001-T080) completed across 6 phases
- ✅ Phase 7: Basic Query Functions implemented with comprehensive tests:
  - `length` function for querying direct element count (O(1))
  - `size` function for querying total node count (O(n))
  - `depth` function for querying maximum nesting depth (O(n))
  - `values` function for extracting all values as flat list (O(n))
  - `value` field accessor documented and verified (O(1))
  - Property-based tests for all query functions
  - Integration tests with pattern constructors and type class instances
  - Edge case tests (100+ levels nesting, 100+ elements, duplicate values)
  - All tests passing (333 examples, 0 failures)
  - Comprehensive Haddock documentation with examples
  - Module-level documentation updated
  - Performance targets verified (length <1ms, size <10ms for 1000 nodes, depth <5ms for 100 levels, values <10ms for 1000 nodes)
  - All 67 tasks (T001-T067) completed across 6 phases (5 user stories + integration)
- ✅ Phase 8.1: Ord Instance implemented with comprehensive tests:
  - `Ord` instance for `Pattern` with lexicographic ordering (value first, then elements recursively)
  - Integration with `Data.Set` and `Data.Map` for pattern organization
  - Consistency verification with `Eq` instance
  - Property-based tests for ordering properties (transitivity, antisymmetry, reflexivity)
  - Unit tests for compare, operators, min/max functions
  - Integration tests for standard library collections
  - Edge case tests (atomic patterns, different element counts, deep nesting, type constraints)
  - Integration tests with existing functions and type class instances
  - All tests passing (370 examples, 0 failures)
  - Comprehensive Haddock documentation with examples
  - Module-level documentation updated
  - All 47 tasks (T001-T047) completed across 4 phases (3 user stories + edge cases)

**Next Steps**: 
1. Move to Phase 8.2 (Other Instances) if needed (e.g., `Semigroup`, `Monoid`)
2. Or proceed to Feature 9 (Graph Views) or Feature 10 (Pattern Morphisms) if needed
3. Or proceed to Feature 11 (Integration and Polish)

---

## Notes

- **Workflow**: Always use feature branches and Speckit commands (`/speckit.specify`, `/speckit.plan`, `/speckit.tasks`)
- **Constitution**: Review [Constitution](.specify/memory/constitution.md) before starting any work
- Don't rush ahead to advanced features
- Each phase should feel complete and tested
- If something feels unclear, stop and clarify before proceeding
- The core Pattern type is the foundation - get it right first

