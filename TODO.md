# TODO: Project Roadmap & History

**Approach**: Incremental, rigorous, foundation-first.
**Workflow**: Feature branches -> Speckit (`specify`, `plan`, `tasks`) -> Implementation -> Test -> Merge.
**Constitution**: All code must adhere to [Constitution](.specify/memory/constitution.md).

---

## 📜 History & Completed Work

### Core Pattern Library (`libs/pattern`)
Foundational implementation of the generic `Pattern v` recursive data structure.

- **Feature 1: Core Data Type**: Defined `Pattern v` with `value` and `elements`. Recursive tree structure verified.
- **Feature 2: Basic Typeclasses**: Implemented `Show` and `Eq` instances.
- **Feature 3: Construction Functions**: Added `pattern`, `patternWith`, `fromList`.
- **Feature 4: Functor**: `fmap` preserves structure while transforming values.
- **Feature 5: Foldable**: `foldr`, `foldl`, `toList` (flatten), `toTuple`.
- **Feature 6: Traversable**: `traverse`, `sequenceA` for effectful traversals.
- **Feature 7: Query Functions**: `length`, `size`, `depth`, `values`. Performance constraints met.
- **Feature 8: Advanced Typeclasses**:
  - `Ord`: Lexicographic ordering.
  - `Semigroup` / `Monoid`: Combination and Identity patterns.
  - `Hashable`: Structure-preserving hashing.
  - `Applicative`: Zip-like application of pattern functions to pattern values.
- **Feature 9: Predicate Matching**: `anyValue`, `allValues` (values); `filterPatterns`, `findPattern` (structure); `matches`, `contains`.
- **Feature 10: Comonad**: `extract` (focus), `duplicate` (context at every position), `extend`. Added `depthAt`, `sizeAt`, `indicesAt`.
- **Feature 19: Integration & Polish**: Explicit export lists, 100% Haddock documentation, property-based law verification, and final cleanup.

### Gram Library (`libs/gram`)
Serialization and parsing for `Pattern Subject`.

- **Serialization**: `toGram` handles all value types, nested patterns, relationships, and anonymous subjects.
- **Parsing**: `fromGram` implements initial support for standard and extended values, relationships, and nesting.
- **Parsing Conformance**: Verified 100% pass rate against `tree-sitter-gram` corpus (Feature 16).
- **Validation**: `Gram.Validate` module implemented with duplicate definition, undefined reference, and arity checking.

---

## 🗺️ Roadmap

### 1. Subject Identity & Serialization (Feature 20)
**Priority**: High / Next Up
**Goal**: Serialize Subject instances to gram notation and parse gram notation to Subject instances, handling the identity requirement (Subject requires identity, but gram allows anonymous subjects).

#### 10.1 Serialization Design
- [ ] Design serialization format for Subject to gram notation
- [ ] Handle anonymous subjects: gram syntax allows anonymous (unidentified) subjects, but Subject data type requires identity
- [ ] Design strategy for assigning identity to anonymous subjects during serialization
  - [ ] Option: Generate unique identifiers (e.g., UUIDs, sequential IDs)
  - [ ] Option: Use placeholder identifiers that can be omitted in output
  - [ ] Option: Track identity mapping for round-trip serialization
- [ ] Implement `toGram :: Subject -> String` (serialize Subject to gram notation)
- [ ] Implement `fromGram :: String -> Either ParseError Subject` (parse gram notation to Subject)
- [ ] Write tests: verify serialization of subjects with all components
- [ ] Write tests: verify parsing of gram notation with anonymous subjects
- [ ] Write tests: verify round-trip serialization (parse . serialize = id, with identity handling)

---

### 2. Graph Views (Feature 21)
**Priority**: High
**Goal**: Interpret `Pattern` structures as different graph elements (nodes, relationships, walks) through categorical functors.

#### 11.1 GraphView Typeclass Design
- [ ] **STOP and REVIEW**: Is GraphView needed yet?
- [ ] Design `GraphView` typeclass interface (minimal)
- [ ] Consider: can we defer this entirely?

#### 11.2 DirectedView (If Proceeding)
- [ ] Implement minimal `DirectedView` if needed
- [ ] Keep implementation simple

*(Note: This feature was previously "Feature 11" in TODO-later.md and is now the active priority)*

---

### 3. Gram Validation Tests
**Priority**: Medium
**Goal**: Add comprehensive tests for the implemented `Gram.Validate` module.

- [ ] Create `libs/gram/tests/Spec/Gram/ValidateSpec.hs`
- [ ] Write tests for `DuplicateDefinition` detection
- [ ] Write tests for `UndefinedReference` detection
- [ ] Write tests for relationship arity/consistency validation
- [ ] Verify validation on corpus files

---

### 4. Pattern Morphisms
**Priority**: Low / Deferred
**Goal**: Basic morphisms if needed for core.

#### 12.1 Morphism Design
- [ ] **STOP and REVIEW**: Are morphisms needed yet?
- [ ] Define `PatternMorphism` type synonym
- [ ] Implement `homomorphism` if clearly needed
- [ ] Implement `forget` if clearly needed

---

### 5. Zipper for Interactive Navigation and Editing
**Priority**: Low / Deferred
**Goal**: Efficient interactive navigation and editing (Up/Down/Left/Right).
**Reference**: See `design/DESIGN.md` (section "Zipper for Focus").

#### 13.1 Core Zipper Data Structure
- [ ] Review design document `design/DESIGN.md` (Zipper section)
- [ ] Implement `Zipper v` type with `focus` and `context` fields
- [ ] Implement `Context v` type with `parent`, `left`, `right`, and `above` fields
- [ ] Implement `fromPattern :: Pattern v -> Zipper v` (create zipper at root)
- [ ] Implement `toPattern :: Zipper v -> Pattern v` (reconstruct pattern from zipper)
- [ ] Write tests: verify zipper creation from patterns
- [ ] Write tests: verify pattern reconstruction from zipper
- [ ] Write tests: verify zipper preserves pattern structure and values

#### 13.2 Navigation Operations
- [ ] Implement `moveDown :: Int -> Zipper v -> Maybe (Zipper v)` (move to nth child)
- [ ] Implement `moveUp :: Zipper v -> Maybe (Zipper v)` (move to parent)
- [ ] Implement `moveLeft :: Zipper v -> Maybe (Zipper v)` (move to previous sibling)
- [ ] Implement `moveRight :: Zipper v -> Maybe (Zipper v)` (move to next sibling)
- [ ] Implement `moveToFirst :: Zipper v -> Maybe (Zipper v)` (move to first child)
- [ ] Implement `moveToLast :: Zipper v -> Maybe (Zipper v)` (move to last child)
- [ ] Implement `moveToRoot :: Zipper v -> Zipper v` (move to root)
- [ ] Implement `canMoveDown`, `canMoveUp`, `canMoveLeft`, `canMoveRight` predicates
- [ ] Write tests: verify all navigation operations work correctly
- [ ] Write tests: verify navigation handles edge cases (root, atomic patterns, boundaries)
- [ ] Write tests: verify navigation preserves pattern structure

#### 13.3 Context Access Operations
- [ ] Implement `parents :: Zipper v -> [v]` (list of parent values from immediate parent to root)
- [ ] Implement `ancestors :: Zipper v -> [Pattern v]` (list of parent Patterns from immediate parent to root)
- [ ] Implement `siblings :: Zipper v -> ([Pattern v], Pattern v, [Pattern v])` (left siblings, focus, right siblings)
- [ ] Implement `path :: Zipper v -> [Int]` (indices from root to focus)
- [ ] Implement `depth :: Zipper v -> Int` (depth of focus from root)
- [ ] Write tests: verify context access operations return correct values
- [ ] Write tests: verify `parents` and `ancestors` handle root correctly (empty lists)
- [ ] Write tests: verify context operations work at all nesting levels

#### 13.4 Editing Operations
- [ ] Implement `modifyFocus :: (Pattern v -> Pattern v) -> Zipper v -> Zipper v` (modify focused pattern)
- [ ] Implement `replaceFocus :: Pattern v -> Zipper v -> Zipper v` (replace focused pattern)
- [ ] Implement `insertBefore :: Pattern v -> Zipper v -> Zipper v` (insert before focus)
- [ ] Implement `insertAfter :: Pattern v -> Zipper v -> Zipper v` (insert after focus)
- [ ] Implement `insertChild :: Pattern v -> Zipper v -> Zipper v` (insert as first child)
- [ ] Implement `appendChild :: Pattern v -> Zipper v -> Zipper v` (append as last child)
- [ ] Implement `deleteFocus :: Zipper v -> Maybe (Zipper v)` (delete focus, move to parent or sibling)
- [ ] Implement `deleteChild :: Int -> Zipper v -> Maybe (Zipper v)` (delete nth child)
- [ ] Write tests: verify all editing operations work correctly
- [ ] Write tests: verify editing operations preserve pattern structure
- [ ] Write tests: verify editing operations handle edge cases (root deletion, empty patterns)
- [ ] Write tests: verify editing operations maintain zipper validity

#### 13.5 Zipper Query Operations
- [ ] Implement `isRoot :: Zipper v -> Bool` (check if at root)
- [ ] Implement `isAtomic :: Zipper v -> Bool` (check if focus is atomic)
- [ ] Implement `hasChildren :: Zipper v -> Bool` (check if focus has elements)
- [ ] Implement `childCount :: Zipper v -> Int` (number of children at focus)
- [ ] Implement `position :: Zipper v -> Maybe Int` (position among siblings, Nothing if root)
- [ ] Write tests: verify all query operations return correct values
- [ ] Write tests: verify query operations handle edge cases

#### 13.6 Integration and Utilities
- [ ] Implement `findFocus :: (Pattern v -> Bool) -> Pattern v -> Maybe (Zipper v)` (find first matching pattern)
- [ ] Implement `findAllFocus :: (Pattern v -> Bool) -> Pattern v -> [Zipper v]` (find all matching patterns)
- [ ] Implement `focusAt :: [Int] -> Pattern v -> Maybe (Zipper v)` (navigate to position by path)
- [ ] Consider: `Zipper` instances for common typeclasses (Functor, Foldable, Traversable)
- [ ] Consider: Conversion utilities between Zipper and Comonad operations
- [ ] Write tests: verify find operations work correctly
- [ ] Write tests: verify path-based navigation works correctly
- [ ] Write documentation: comprehensive examples showing zipper usage patterns
- [ ] Write documentation: relationship between Zipper and Comonad instances

---

### 6. Pattern Matching DSL (PatternExpr Library)
**Priority**: Low / Deferred
**Goal**: Enable regex-like pattern matching expressions for Pattern structures.
**Reference**: See `design/pattern-matching-dsl-design.md`.

#### 14.0 Design Validation and Exploration
- [ ] **STOP and REVIEW**: Review design document `design/pattern-matching-dsl-design.md` thoroughly
- [ ] **Validate Design Approach**: Analyze the layered architecture (PatternExpr → PathPattern → GraphPattern)
- [ ] **Consider Alternatives**: Research and evaluate alternative design approaches
- [ ] **Explore Toy Examples**: Create expressive toy examples to validate the design
- [ ] **Compare with Feature 9**: Evaluate relationship with predicate-based matching
- [ ] **Design Simplifications**: Identify what can be deferred or simplified
- [ ] **Create Design Summary**: Document the validated design approach
- [ ] **MANDATORY CHECKPOINT**: Present design validation to user for approval

#### 14.1 Pattern Expressions (Layer 1)
- [ ] Design `PatternExpr v` type with basic constructors (`PAny`, `PAtom`, `PSequence`)
- [ ] Design `Quantifier` type for repetition (`ZeroOrMore`, `OneOrMore`, `Exactly`, `Between`, etc.)
- [ ] Design combinators (`PThen`, `POr`, `PWhere`, `PBind`)
- [ ] Implement basic `PatternExpr` type and constructors
- [ ] Implement basic matching engine (`match`, `matchAll`, `matches`)
- [ ] Write tests: verify basic pattern matching on atomic patterns
- [ ] Write tests: verify basic pattern matching on sequences
- [ ] Write tests: verify combinators (sequential, alternative, predicates)

#### 14.2 Quantification and Advanced Matching
- [ ] Implement `PRepeat` with `Quantifier` support
- [ ] Implement backtracking for alternatives and quantifiers
- [ ] Implement variable binding (`PBind`) and capture groups
- [ ] Design `MatchResult` type with bindings and position information
- [ ] Implement `find` and `findAll` for finding patterns within larger patterns
- [ ] Write tests: verify quantifiers (`*`, `+`, `{m,n}`) work correctly
- [ ] Write tests: verify backtracking behavior
- [ ] Write tests: verify variable binding and capture groups
- [ ] Write tests: verify `find` and `findAll` operations
- [ ] Write tests: edge cases (empty patterns, deeply nested, complex quantifiers)

#### 14.3 Path Patterns (Layer 2)
- [ ] **STOP and REVIEW**: Evaluate need for path-level patterns beyond basic PatternExpr
- [ ] Design `PathPattern v` type for path-based matching
- [ ] Implement path-level predicates and constraints
- [ ] Implement intermediate node capturing in paths
- [ ] Implement path composition (`Seq`)
- [ ] Write tests: verify path pattern matching
- [ ] Write tests: verify path-level constraints
- [ ] Write tests: verify intermediate node capturing

#### 14.4 Graph Patterns (Layer 3)
- [ ] **STOP and REVIEW**: Evaluate need for non-linear graph patterns
- [ ] Design `GraphPattern v` type for multi-path patterns
- [ ] Implement equijoins (shared variables across paths)
- [ ] Implement logical operators (`AndGraph`, `OrGraph`)
- [ ] Write tests: verify graph pattern matching
- [ ] Write tests: verify equijoins work correctly
- [ ] Write tests: verify logical combinations

#### 14.5 Matching Engine and Optimization
- [ ] Implement pattern compilation (`compile :: PatternExpr v -> CompiledPattern v`)
- [ ] Implement `replace` and `replaceAll` for pattern transformation
- [ ] Add memoization for efficient matching
- [ ] Optimize common cases (early termination, indexing)
- [ ] Write tests: verify compilation improves performance
- [ ] Write tests: verify replace operations
- [ ] Write tests: verify performance targets (large patterns, complex expressions)

#### 14.6 Combinator Library and Surface Syntax
- [ ] Implement combinator library (`atom`, `sequence`, `zeroOrMore`, `oneOrMore`, etc.)
- [ ] Implement infix operators (`<~>`, `<|>`, `satisfying`, `as`)
- [ ] Implement derived combinators (`optional`, `listOf`, `atLeast`, `atMost`)
- [ ] Consider: QuasiQuoter for embedded pattern syntax (optional)
- [ ] Consider: Parser for Cypher-like syntax (optional)
- [ ] Write tests: verify all combinators work correctly
- [ ] Write tests: verify infix operators have correct precedence
- [ ] Write documentation: comprehensive examples using combinator library
