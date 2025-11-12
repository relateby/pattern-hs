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

## Feaure 1: Core Data Type (Foundation) ✅

### 1.1 Basic Pattern Type
- [x] Define `Pattern v` data type with `value :: v` and `elements :: [Pattern v]` in `src/Pattern/Core.hs`
- [x] Add basic Haddock documentation explaining the recursive tree structure
- [x] Write simple test: create a pattern with no member elements and verify structure
- [x] Write simple test: create a pattern with member elements and verify structure

**Goal**: Have a working Pattern type that can be constructed and inspected. ✅ **COMPLETE**

---

## Feaure 2: Basic Typeclasses ✅

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

## Feaure 3: Construction Functions ✅

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

## Feaure 5: Foldable Instance

### 5.1 Foldable Implementation
- [ ] Implement `Foldable` instance for `Pattern`
- [ ] Write test: `foldr` collects all values
- [ ] Write test: `foldl` collects all values (verify order)
- [ ] Write test: `foldMap` works correctly
- [ ] Write test: `toList` produces all values

**Goal**: Patterns can be folded/traversed for aggregation.

---

## Feaure 6: Traversable Instance

### 6.1 Traversable Implementation
- [ ] Implement `Traversable` instance for `Pattern`
- [ ] Write test: `traverse` with `Identity` preserves structure
- [ ] Write test: `traverse` with `Maybe` handles failures
- [ ] Write test: `sequenceA` works correctly

**Goal**: Patterns can be traversed with effects.

---

## Feaure 7: Basic Query Functions

### 7.1 Size and Depth
- [ ] Implement `length :: Pattern v -> Int` (number of elements in the sequence - equivalent to `length (elements p)`)
- [ ] Implement `size :: Pattern v -> Int` (total number of nodes)
- [ ] Implement `depth :: Pattern v -> Int` (maximum nesting depth)
- [ ] Write tests: length, size and depth for various patterns
- [ ] Write tests: edge cases (empty, single node, deep nesting)

### 7.2 Value Access
- [ ] Verify `value` field accessor works (already from data type)
- [ ] Implement `values :: Pattern v -> [v]` (all values in pattern)
- [ ] Write tests: extract all values from patterns

**Goal**: Basic introspection of pattern structure.

---

## Feaure 8: Additional Typeclasses (As Needed)

### 8.1 Ord Instance (If Needed)
- [ ] Consider if `Ord` instance is needed
- [ ] If yes, implement `Ord` instance with clear ordering semantics
- [ ] Write tests: ordering of patterns

### 8.2 Other Instances
- [ ] Evaluate if other standard typeclasses are needed (e.g., `Semigroup`, `Monoid`)
- [ ] Implement only if clearly needed for core functionality

**Goal**: Complete basic typeclass coverage.

---

## Feaure 9: Graph Views (Underspecified)

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

## Feaure 10: Pattern Morphisms (Underspecified)

### 10.1 Morphism Design
- [ ] **STOP and REVIEW**: Are morphisms needed yet?
- [ ] Define `PatternMorphism` type synonym
- [ ] Implement `homomorphism` if clearly needed
- [ ] Implement `forget` if clearly needed

**Goal**: Basic morphisms if needed for core.

**Note**: Advanced morphism theory deferred until core is solid.

---

## Feaure 11: Integration and Polish

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

**Current Phase**: Phase 4 (Functor Instance) - Complete ✅

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

**Next Steps**: 
1. Move to Phase 5 (Foldable Instance)
2. Implement `Foldable` instance for `Pattern`
3. Write tests for folding operations

---

## Notes

- **Workflow**: Always use feature branches and Speckit commands (`/speckit.specify`, `/speckit.plan`, `/speckit.tasks`)
- **Constitution**: Review [Constitution](.specify/memory/constitution.md) before starting any work
- Don't rush ahead to advanced features
- Each phase should feel complete and tested
- If something feels unclear, stop and clarify before proceeding
- The core Pattern type is the foundation - get it right first

