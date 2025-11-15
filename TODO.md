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
- [x] Evaluate if other standard typeclasses are needed (e.g., `Semigroup`, `Monoid`)
- [x] Implement only if clearly needed for core functionality

**Goal**: Complete basic typeclass coverage.

**Status**: Ord instance complete ✅. Semigroup instance complete ✅. Monoid instance complete ✅. Hashable instance complete ✅. Applicative instance complete ✅. All planned typeclass instances complete.

### 8.3 Semigroup Instance ✅
- [x] **STOP and REVIEW**: Identify clear use cases for combining patterns
- [x] Evaluate semantics: should `p1 <> p2` combine values and concatenate elements?
- [x] Consider: does this align with the decorated sequence model?
- [x] Document use cases before proceeding
- [x] If proceeding: implement `Semigroup` instance (requires `Semigroup v` constraint)
- [x] Write tests: verify combination semantics
- [x] Write tests: verify associativity law
- [x] Write tests: edge cases (empty patterns, single elements, nested patterns)

**Goal**: Enable incremental pattern construction through combination, if use cases are clear. ✅ **COMPLETE**

**Semigroup Instance Added**:
1. **`Semigroup` instance** - Enables combining patterns by concatenating their elements and combining their values using the value type's Semigroup instance
2. **Combination semantics** - Values combine using `v1 <> v2`, elements concatenate using `els1 ++ els2`
3. **Associativity law** - Verified through property-based testing: `(p1 <> p2) <> p3 = p1 <> (p2 <> p3)`
4. **Standard library integration** - Works with `sconcat` and `stimes` for pattern combination operations
5. **Comprehensive tests** - All tests passing (comprehensive test coverage):
   - Unit tests for combination operations (T001-T010)
   - Property-based tests for associativity law (T015-T019)
   - Edge case tests (T020-T023)
   - Integration tests for standard combinators (T026-T030)
6. **Complete Haddock documentation** - Module-level and instance-level documentation with examples
7. **Type constraint** - Requires `Semigroup v` constraint, ensuring type safety
8. **All 33 tasks (T001-T033) completed** across 4 phases (3 user stories + integration)

### 8.4 Monoid Instance ✅
- [x] **STOP and REVIEW**: Identify clear use cases for identity pattern
- [x] Evaluate semantics: what should `mempty` be? (pattern with `mempty` value and empty elements?)
- [x] Consider: does this naturally extend Semigroup semantics?
- [x] Document use cases before proceeding
- [x] If proceeding: implement `Monoid` instance (requires `Monoid v` constraint)
- [x] Write tests: verify identity laws (`mempty <> p = p`, `p <> mempty = p`)
- [x] Write tests: verify consistency with Semigroup
- [x] Write tests: edge cases (combining with identity, nested patterns)

**Goal**: Provide identity pattern for Semigroup operations, if use cases are clear. ✅ **COMPLETE**

**Monoid Instance Added**:
1. **`Monoid` instance** - Extends the Semigroup instance by providing an identity element (`mempty`)
2. **Identity pattern** - `mempty = pattern mempty` (pattern with `mempty` value and empty elements list)
3. **Identity laws** - Verified through property-based testing: `mempty <> p = p` and `p <> mempty = p`
4. **Consistency with Semigroup** - Uses same `<>` implementation, naturally extending Semigroup semantics
5. **Standard library integration** - Works with `mconcat` for combining lists of patterns (returns `mempty` for empty list)
6. **Comprehensive tests** - All tests passing (437 examples, 0 failures):
   - Unit tests for identity pattern structure and laws (T007-T011, T021-T026)
   - Property-based tests for identity laws across various value types (T017-T020)
   - Edge case tests (various pattern structures, value types, mconcat behavior)
   - Consistency tests with Semigroup instance
7. **Complete Haddock documentation** - Module-level and instance-level documentation with examples
8. **Type constraint** - Requires `Monoid v` constraint, ensuring type safety
9. **All 32 tasks (T001-T032) completed** across 7 phases (setup, foundational, user stories, polish)
10. **Documentation examples updated** - All examples use constructor functions (`pattern`, `patternWith`) instead of record syntax

### 8.5 Hashable Instance ✅
- [x] **STOP and REVIEW**: Identify clear use cases for hash-based containers
- [x] Evaluate: do we need `HashMap`/`HashSet` beyond `Data.Set`/`Data.Map` (which use Ord)?
- [x] Consider: performance requirements for lookups and deduplication
- [x] Document use cases before proceeding
- [x] If proceeding: implement `Hashable` instance (requires `Hashable v` constraint)
- [x] Write tests: verify hash consistency with `Eq` (equal patterns have same hash)
- [x] Write tests: verify hash distribution (avoid collisions)
- [x] Write tests: integration with `HashMap` and `HashSet`
- [x] Write tests: edge cases (nested patterns, large structures)

**Goal**: Enable efficient hash-based lookups and deduplication, if use cases are clear. ✅ **COMPLETE**

**Hashable Instance Added**:
1. **`Hashable` instance** - Enables using patterns as keys in `HashMap` and elements in `HashSet` for O(1) average-case lookups
2. **Structure-preserving hashing** - Patterns are hashed based on their structure (value and elements recursively), distinguishing different structures even if they have the same flattened values
3. **Hash consistency with Eq** - Verified through property-based testing: equal patterns (according to `Eq`) produce the same hash value
4. **Good hash distribution** - Statistical testing shows collision rate < 1% for random patterns
5. **Comprehensive tests** - All tests passing (477 examples, 0 failures):
   - Unit tests for basic hashing operations (T001-T010)
   - Property-based tests for hash consistency with Eq (T017-T019)
   - Hash distribution tests (T020-T021)
   - Edge case tests (T022-T026)
   - Integration tests with HashMap and HashSet (T029-T038)
   - Integration tests with other typeclass instances (T045-T047)
6. **Complete Haddock documentation** - Module-level and instance-level documentation with examples
7. **Type constraint** - Requires `Hashable v` constraint, ensuring type safety
8. **All 50 tasks (T001-T050) completed** across 5 phases (MVP, verification, integration, polish)

### 8.6 Applicative Instance
- [x] **STOP and REVIEW**: Identify clear use cases for applying patterns of functions to patterns of values
- [x] Evaluate semantics: how should `pure` and `<*>` work for Pattern?
- [x] Consider: does this fit the decorated sequence model, or is Functor sufficient?
- [x] Document use cases before proceeding
- [x] If proceeding: design and implement `Applicative` instance
- [x] Write tests: verify applicative laws (identity, composition, homomorphism, interchange)
- [x] Write tests: verify consistency with Functor (`fmap f x = pure f <*> x`)
- [x] Write tests: edge cases (empty patterns, nested patterns, function application)

**Goal**: Enable applicative-style pattern operations, if use cases are clear and semantics are well-defined. ✅ **COMPLETE**

**Applicative Instance Added**:
1. **`Applicative` instance** - Enables applying functions stored in patterns to values stored in patterns using structure-preserving/zip-like semantics
2. **`pure :: a -> Pattern a`** - Wraps a value in an atomic pattern (pattern with empty elements list)
3. **`<*> :: Pattern (a -> b) -> Pattern a -> Pattern b`** - Applies pattern of functions to pattern of values using structure-preserving/zip-like semantics (root function to root value, element functions to corresponding element values)
4. **Structure-preserving semantics** - Functions are applied at matching positions (root to root, element to element), preserving pattern structure
5. **Zip-like truncation** - When function pattern and value pattern have different element counts, functions are applied to values up to the minimum element count, ignoring extra elements
6. **Comprehensive tests** - All tests passing (comprehensive test coverage):
   - Unit tests for `pure` and `<*>` operations (T005-T012)
   - Property-based tests for all Applicative laws (identity, composition, homomorphism, interchange) (T022-T025)
   - Property-based tests for Functor consistency (`fmap f x = pure f <*> x`) (T036-T040)
   - Edge case tests (empty elements, mismatched structures, deep nesting) (T049-T055)
7. **Complete Haddock documentation** - Module-level and instance-level documentation with examples
8. **Type constraint** - Requires no additional constraints beyond standard Applicative requirements
9. **All 74 tasks (T001-T074) completed** across 6 phases (setup, 4 user stories, polish)
10. **See `specs/013-applicative-instance/`** for full specification and implementation details

---

## Feature 9: Predicate-Based Pattern Matching ✅

### 9.1 Value Predicate Functions
- [x] **STOP and REVIEW**: Identify clear use cases for value-based predicates
- [x] Evaluate semantics: should predicates match on values only, or also consider structure?
- [x] Consider: how do predicates relate to existing `Eq` and `Ord` instances?
- [x] Document use cases before proceeding
- [x] If proceeding: implement `anyValue :: (v -> Bool) -> Pattern v -> Bool`
- [x] If proceeding: implement `allValues :: (v -> Bool) -> Pattern v -> Bool`
- [x] Write tests: verify predicate matching on atomic patterns
- [x] Write tests: verify predicate matching on nested patterns
- [x] Write tests: edge cases (empty patterns, all match, none match)

**Goal**: Enable value-based predicate matching for pattern queries. ✅ **COMPLETE**

### 9.2 Pattern Predicate Functions
- [x] **STOP and REVIEW**: Identify clear use cases for pattern-based predicates
- [x] Evaluate semantics: should predicates match on structure, values, or both?
- [x] Consider: how do pattern predicates relate to structural pattern matching?
- [x] Document use cases before proceeding
- [x] If proceeding: implement `filterPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]`
- [x] If proceeding: implement `findPattern :: (Pattern v -> Bool) -> Pattern v -> Maybe (Pattern v)`
- [x] If proceeding: implement `findAllPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]`
- [x] Write tests: verify pattern matching on various structures
- [x] Write tests: verify matching includes root pattern and all nested subpatterns
- [x] Write tests: edge cases (no matches, all match, deeply nested)

**Goal**: Enable pattern-based predicate matching for finding subpatterns. ✅ **COMPLETE**

### 9.3 Structural Pattern Matching
- [x] **STOP and REVIEW**: Identify clear use cases for structural pattern matching
- [x] Evaluate semantics: should matching be exact structure, or allow partial matching?
- [x] Consider: how does structural matching relate to `Eq` instance?
- [x] Document use cases before proceeding
- [x] If proceeding: implement `matches :: Pattern v -> Pattern v -> Bool` (structural matching)
- [x] If proceeding: implement `contains :: Pattern v -> Pattern v -> Bool` (subpattern containment)
- [x] Write tests: verify structural matching semantics
- [x] Write tests: verify subpattern containment detection
- [x] Write tests: edge cases (empty patterns, self-matching, nested matching)

**Goal**: Enable structural pattern matching beyond exact equality. ✅ **COMPLETE**

**Note**: Predicate-based matching extends beyond `Eq` to support flexible querying and filtering of patterns based on value properties or structural characteristics.

**Predicate-Based Pattern Matching Added**:
1. **Value Predicate Functions** - Operate on flattened values extracted via `Foldable.toList`:
   - **`anyValue :: (v -> Bool) -> Pattern v -> Bool`** - Checks if any value in a pattern satisfies a predicate (O(n))
   - **`allValues :: (v -> Bool) -> Pattern v -> Bool`** - Checks if all values in a pattern satisfy a predicate (O(n))
2. **Pattern Predicate Functions** - Operate on pattern structures (including element sequences):
   - **`filterPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]`** - Filters all subpatterns (including root) matching a pattern predicate (O(n))
   - **`findPattern :: (Pattern v -> Bool) -> Pattern v -> Maybe (Pattern v)`** - Finds the first subpattern (including root) matching a pattern predicate (O(n))
   - **`findAllPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]`** - Finds all subpatterns (including root) matching a pattern predicate (O(n), equivalent to `filterPatterns`)
3. **Structural Pattern Matching Functions** - Perform structural pattern matching:
   - **`matches :: (Eq v) => Pattern v -> Pattern v -> Bool`** - Checks if two patterns match structurally (O(n), equivalent to `==`)
   - **`contains :: (Eq v) => Pattern v -> Pattern v -> Bool`** - Checks if a pattern contains a subpattern (O(n))
4. **Key Distinction** - Value predicates operate on flattened values (all values at all nesting levels), while pattern predicates operate on pattern structures and can match on element sequences, repetition patterns, and structural arrangements
5. **Comprehensive tests** - All tests passing (comprehensive test coverage):
   - Unit tests for all functions (T001-T050)
   - Property-based tests for predicate relationships and structural matching properties (T009-T011, T029-T031, T051-T054)
   - Integration tests with existing Pattern operations and typeclass instances (T063-T064)
   - Edge case tests (100+ nesting levels, 1000+ nodes) (T065)
6. **Complete Haddock documentation** - Module-level and function-level documentation with examples
7. **Type constraints** - Value predicates require no constraints; structural matching requires `Eq v`
8. **All 69 tasks (T001-T069) completed** across 4 phases (3 user stories + polish)
9. **See `specs/012-predicate-matching/`** for full specification and implementation details

---

## Feature 10: Comonad Instance ✅

### 10.1 Comonad Design
- [x] **STOP and REVIEW**: Identify clear use cases for context-aware computations
- [x] Evaluate semantics: what does "context" mean for Pattern? (full structure, position, depth?)
- [x] Consider: how does Comonad relate to existing `Foldable` and `Traversable` instances?
- [x] Research: review comonad implementations for tree structures (e.g., `Data.Tree`)
- [x] Document use cases before proceeding
- [x] Design `extract :: Pattern v -> v` semantics (extract decoration value)
- [x] Design `extend :: (Pattern v -> w) -> Pattern v -> Pattern w` semantics (context-aware computation)
- [x] Design `duplicate :: Pattern v -> Pattern (Pattern v)` semantics (create context at each position)

**Goal**: Enable context-aware computations that have access to full structural context. ✅ **COMPLETE**

### 10.2 Comonad Implementation
- [x] Implement `extract :: Pattern v -> v` (extract value at focus)
- [x] Implement `duplicate :: Pattern v -> Pattern (Pattern v)` (create pattern of contexts)
- [x] Implement `extend :: (Pattern v -> w) -> Pattern v -> Pattern w` (context-aware transformation)
- [x] Verify: `extract . extend f = f` (comonad law 1)
- [x] Verify: `extend extract = id` (comonad law 2)
- [x] Verify: `extend f . extend g = extend (f . extend g)` (comonad law 3)
- [x] Write tests: verify all comonad laws with property-based testing
- [x] Write tests: verify extract on atomic patterns
- [x] Write tests: verify extract on nested patterns
- [x] Write tests: verify extend with context-aware functions
- [x] Write tests: verify duplicate creates correct context structures
- [x] Write tests: edge cases (empty patterns, deeply nested, single element)

**Goal**: Implement Comonad instance with verified laws and comprehensive tests. ✅ **COMPLETE**

### 10.3 Context-Aware Operations
- [x] **STOP and REVIEW**: Identify useful context-aware operations enabled by Comonad
- [x] Evaluate: what context information is most useful? (depth, path, parent, siblings, size?)
- [x] Consider: should we provide helper functions for common context queries?
- [x] Document use cases before proceeding
- [x] If proceeding: implement `depthAt :: Pattern v -> Pattern Int` (depth at each position)
- [x] If proceeding: implement `sizeAt :: Pattern v -> Pattern Int` (size of subtree at each position)
- [x] If proceeding: implement `indicesAt :: Pattern v -> Pattern [Int]` (indices from root at each position)
- [x] Write tests: verify context-aware operations produce correct results
- [x] Write tests: verify context operations on various structures
- [x] Write tests: edge cases (root position, leaf positions, nested structures)

**Goal**: Provide useful context-aware operations enabled by Comonad instance. ✅ **COMPLETE**

**Note**: Comonad enables context-aware folding where computations have access to the full structural context (parent, siblings, depth, indices) around each value, not just the value itself. This extends beyond `Foldable` which only provides values in sequence.

**Comonad Laws**:
- **Law 1**: `extract . extend f = f` - Extracting from an extended computation gives the original result ✅ Verified
- **Law 2**: `extend extract = id` - Extending with extract is identity ✅ Verified
- **Law 3**: `extend f . extend g = extend (f . extend g)` - Extend is associative ✅ Verified

**Use Cases**:
- Depth-aware aggregations (compute based on nesting level)
- Position-aware transformations (transform based on location in structure)
- Context-sensitive queries (find patterns based on structural context)
- Structural metadata computation (compute depth, size, indices at each position)

**Comonad Instance Added**:
1. **`Comonad` instance** - Enables context-aware computations where functions have access to the full structural context (parent, siblings, depth, indices) around each value, not just the value itself
2. **`extract :: Pattern v -> v`** - Extracts the decoration value from a pattern (root value)
3. **`duplicate :: Pattern v -> Pattern (Pattern v)`** - Creates a pattern where each position contains the full pattern structure focused at that position
4. **`extend :: (Pattern v -> w) -> Pattern v -> Pattern w`** - Applies a context-aware function to each position in a pattern
5. **Helper functions** - Convenience functions for common context-aware operations:
   - **`depthAt :: Pattern v -> Pattern Int`** - Computes depth at each position
   - **`sizeAt :: Pattern v -> Pattern Int`** - Computes size of subtree at each position
   - **`indicesAt :: (Eq v) => Pattern v -> Pattern [Int]`** - Computes indices from root at each position
6. **Comprehensive tests** - All tests passing (579 examples, 0 failures):
   - Unit tests for extract, extend, duplicate (T009-T038)
   - Property-based tests for all Comonad laws (T046-T048)
   - Unit tests for helper functions (T058-T063)
7. **Complete Haddock documentation** - Module-level and instance-level documentation with examples
8. **Type constraint** - `indicesAt` requires `Eq v` constraint for structural equality
9. **All core tasks (T001-T071) completed** across 6 phases (5 user stories + helper functions)
10. **See `specs/014-comonad-instance/`** for full specification and implementation details

---

## Feature 11: Integration and Polish

### 11.1 Module Exports
- [ ] Review and finalize exports from `Pattern.Core`
- [ ] Update `Pattern.hs` main module exports
- [ ] Ensure clean public API

### 11.2 Documentation
- [ ] Add comprehensive Haddock documentation
- [ ] Include usage examples
- [ ] Document mathematical properties where applicable

### 11.3 Testing
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
5. **Review Points**: Stop and review before advanced features
6. **Deferred Features**: Advanced features (graphs, views, morphisms, pattern matching DSL) are deferred to `TODO-later.md` until core is solid
7. **Solid Core First**: Ensure core Pattern type is well-understood before adding complexity

---

## Current Status

**Current Phase**: Feature 10 (Comonad Instance) - Complete ✅. All planned typeclass instances complete.

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
- ✅ Phase 8.3: Semigroup Instance implemented with comprehensive tests:
  - `Semigroup` instance for `Pattern` with value combination and element concatenation
  - Property-based tests for associativity law (all passing)
  - Unit tests for all pattern structures and edge cases
  - Integration tests for standard Semigroup combinators (`sconcat`, `stimes`)
  - All tests passing (comprehensive test coverage)
  - Complete Haddock documentation with examples
  - Module-level documentation updated
  - All 33 tasks (T001-T033) completed across 4 phases (3 user stories + integration)
- ✅ Phase 8.4: Monoid Instance implemented with comprehensive tests:
  - `Monoid` instance for `Pattern` extending Semigroup with identity element (`mempty`)
  - Identity pattern: `mempty = pattern mempty` (pattern with `mempty` value and empty elements)
  - Property-based tests for identity laws (`mempty <> p = p`, `p <> mempty = p`)
  - Unit tests for identity pattern structure, laws, and edge cases
  - Integration tests for standard Monoid combinators (`mconcat`)
  - Consistency tests with Semigroup instance
  - All tests passing (437 examples, 0 failures)
  - Complete Haddock documentation with examples
  - Module-level documentation updated
  - All examples use constructor functions (`pattern`, `patternWith`) instead of record syntax
  - All 32 tasks (T001-T032) completed across 7 phases (setup, foundational, user stories, polish)
- ✅ Phase 8.5: Hashable Instance implemented with comprehensive tests:
  - `Hashable` instance for `Pattern` with structure-preserving hashing (value and elements recursively)
  - Integration with `HashMap` and `HashSet` for O(1) average-case lookups
  - Hash consistency with `Eq` verified through property-based testing
  - Good hash distribution (collision rate < 1% for random patterns)
  - Property-based tests for hash consistency with Eq (T017-T019)
  - Hash distribution tests (T020-T021)
  - Edge case tests (atomic patterns, many elements, deep nesting, duplicate values)
  - Integration tests with HashMap and HashSet (T029-T038)
  - Integration tests with other typeclass instances (T045-T047)
  - All tests passing (477 examples, 0 failures)
  - Complete Haddock documentation with examples
  - Module-level documentation updated
  - All 50 tasks (T001-T050) completed across 5 phases (MVP, verification, integration, polish)
- ✅ Phase 8.6: Applicative Instance implemented with comprehensive tests:
  - `Applicative` instance for `Pattern` with `pure` and `<*>` operations
  - Structure-preserving/zip-like semantics for applying functions to values
  - Property-based tests for all Applicative laws (identity, composition, homomorphism, interchange)
  - Property-based tests for Functor consistency (`fmap f x = pure f <*> x`)
  - Edge case tests (empty elements, mismatched structures, deep nesting)
  - All tests passing (comprehensive test coverage)
  - Complete Haddock documentation with examples
  - All 74 tasks (T001-T074) completed across 6 phases (setup, 4 user stories, polish)
  - See `specs/013-applicative-instance/` for full specification and implementation details
- ✅ Phase 9: Predicate-Based Pattern Matching implemented with comprehensive tests:
  - Value predicate functions (`anyValue`, `allValues`) operating on flattened values via `Foldable.toList`
  - Pattern predicate functions (`filterPatterns`, `findPattern`, `findAllPatterns`) operating on pattern structures
  - Structural matching functions (`matches`, `contains`) for pattern comparison and containment
  - Key distinction: value predicates operate on flattened values, pattern predicates operate on structures
  - Property-based tests for predicate relationships and structural matching properties
  - Integration tests with existing Pattern operations and typeclass instances
  - Edge case tests (100+ nesting levels, 1000+ nodes)
  - All tests passing (comprehensive test coverage)
  - Complete Haddock documentation with examples
  - All 69 tasks (T001-T069) completed across 4 phases (3 user stories + polish)
  - See `specs/012-predicate-matching/` for full specification and implementation details
- ✅ Phase 10: Comonad Instance implemented with comprehensive tests:
  - `Comonad` instance for `Pattern` with `extract`, `duplicate`, and `extend` operations
  - Context-aware computations with access to full structural context (parent, siblings, depth, indices)
  - Property-based tests for all Comonad laws (extract-extend, extend-extract, extend composition)
  - Helper functions (`depthAt`, `sizeAt`, `indicesAt`) for common context-aware operations
  - Unit tests for all Comonad operations and helper functions
  - All tests passing (579 examples, 0 failures)
  - Complete Haddock documentation with examples
  - Module-level documentation updated
  - All core tasks (T001-T071) completed across 6 phases (5 user stories + helper functions)
  - See `specs/014-comonad-instance/` for full specification and implementation details

**Next Steps**: 
1. Feature 11 (Integration and Polish) - finalize exports, documentation, and testing
2. See `TODO-later.md` for deferred features (Graph Views, Pattern Morphisms, Pattern Matching DSL)

---

## Notes

- **Workflow**: Always use feature branches and Speckit commands (`/speckit.specify`, `/speckit.plan`, `/speckit.tasks`)
- **Constitution**: Review [Constitution](.specify/memory/constitution.md) before starting any work
- Don't rush ahead to advanced features
- Each phase should feel complete and tested
- If something feels unclear, stop and clarify before proceeding
- The core Pattern type is the foundation - get it right first

