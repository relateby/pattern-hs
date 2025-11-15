# TODO-later: Deferred Pattern Features

**Purpose**: This file contains features that are not foundational to the core Pattern type itself. These features may be implemented later when the core is solid and use cases are clearer.

**Approach**: These features are deferred until the foundational Pattern implementation is complete and well-tested.

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

## Feature 11: Graph Views (Underspecified)

### 11.1 GraphView Typeclass Design
- [ ] **STOP and REVIEW**: Is GraphView needed yet?
- [ ] Design `GraphView` typeclass interface (minimal)
- [ ] Consider: can we defer this entirely?

### 11.2 DirectedView (If Proceeding)
- [ ] Implement minimal `DirectedView` if needed
- [ ] Keep implementation simple

**Goal**: Basic view support if needed.

**Note**: Advanced views and view composition deferred until core is solid.

---

## Feature 12: Pattern Morphisms (Underspecified)

### 12.1 Morphism Design
- [ ] **STOP and REVIEW**: Are morphisms needed yet?
- [ ] Define `PatternMorphism` type synonym
- [ ] Implement `homomorphism` if clearly needed
- [ ] Implement `forget` if clearly needed

**Goal**: Basic morphisms if needed for core.

**Note**: Advanced morphism theory deferred until core is solid.

---

## Feature 13: Zipper for Interactive Navigation and Editing

**Reference**: See `design/DESIGN.md` (section "Zipper for Focus") for initial design specification.

**Use Case**: The Zipper type enables efficient interactive navigation and editing of Pattern structures, particularly useful for building user interfaces where users need to:
- Navigate through pattern structures (up/down/left/right)
- Focus on specific pattern elements
- Edit patterns at the focus point efficiently
- Access parent/sibling context for UI features (breadcrumbs, context menus)
- Support undo/redo operations through zipper state history

**Relationship to Comonad**: The Comonad instance (Feature 10) provides context-aware computations across all positions, while the Zipper provides explicit focus management and efficient navigation/editing operations. They complement each other:
- **Comonad**: Batch context-aware transformations (e.g., "compute depth at every position")
- **Zipper**: Interactive editing with explicit focus and efficient navigation

### 13.1 Core Zipper Data Structure

- [ ] Review design document `design/DESIGN.md` (Zipper section)
- [ ] Implement `Zipper v` type with `focus` and `context` fields
- [ ] Implement `Context v` type with `parent`, `left`, `right`, and `above` fields
- [ ] Implement `fromPattern :: Pattern v -> Zipper v` (create zipper at root)
- [ ] Implement `toPattern :: Zipper v -> Pattern v` (reconstruct pattern from zipper)
- [ ] Write tests: verify zipper creation from patterns
- [ ] Write tests: verify pattern reconstruction from zipper
- [ ] Write tests: verify zipper preserves pattern structure and values

**Goal**: Core zipper data structure that maintains focus point and full context for efficient navigation and editing.

### 13.2 Navigation Operations

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

**Goal**: Complete set of navigation operations for moving focus through pattern structures.

### 13.3 Context Access Operations

- [ ] Implement `parents :: Zipper v -> [v]` (list of parent values from immediate parent to root)
- [ ] Implement `ancestors :: Zipper v -> [Pattern v]` (list of parent Patterns from immediate parent to root)
- [ ] Implement `siblings :: Zipper v -> ([Pattern v], Pattern v, [Pattern v])` (left siblings, focus, right siblings)
- [ ] Implement `path :: Zipper v -> [Int]` (indices from root to focus)
- [ ] Implement `depth :: Zipper v -> Int` (depth of focus from root)
- [ ] Write tests: verify context access operations return correct values
- [ ] Write tests: verify `parents` and `ancestors` handle root correctly (empty lists)
- [ ] Write tests: verify context operations work at all nesting levels

**Goal**: Operations for accessing parent, sibling, and path context information useful for UI features.

### 13.4 Editing Operations

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

**Goal**: Complete set of editing operations for modifying patterns at the focus point efficiently.

### 13.5 Zipper Query Operations

- [ ] Implement `isRoot :: Zipper v -> Bool` (check if at root)
- [ ] Implement `isAtomic :: Zipper v -> Bool` (check if focus is atomic)
- [ ] Implement `hasChildren :: Zipper v -> Bool` (check if focus has elements)
- [ ] Implement `childCount :: Zipper v -> Int` (number of children at focus)
- [ ] Implement `position :: Zipper v -> Maybe Int` (position among siblings, Nothing if root)
- [ ] Write tests: verify all query operations return correct values
- [ ] Write tests: verify query operations handle edge cases

**Goal**: Query operations for inspecting zipper state and focus properties.

### 13.6 Integration and Utilities

- [ ] Implement `findFocus :: (Pattern v -> Bool) -> Pattern v -> Maybe (Zipper v)` (find first matching pattern)
- [ ] Implement `findAllFocus :: (Pattern v -> Bool) -> Pattern v -> [Zipper v]` (find all matching patterns)
- [ ] Implement `focusAt :: [Int] -> Pattern v -> Maybe (Zipper v)` (navigate to position by path)
- [ ] Consider: `Zipper` instances for common typeclasses (Functor, Foldable, Traversable)
- [ ] Consider: Conversion utilities between Zipper and Comonad operations
- [ ] Write tests: verify find operations work correctly
- [ ] Write tests: verify path-based navigation works correctly
- [ ] Write documentation: comprehensive examples showing zipper usage patterns
- [ ] Write documentation: relationship between Zipper and Comonad instances

**Goal**: Utility functions and integrations for common zipper use cases.

**Note**: The Zipper type provides explicit focus management and efficient navigation/editing operations, making it ideal for interactive user interfaces. Unlike the Comonad instance which provides context-aware computations across all positions, the Zipper maintains a single focus point with full context, enabling efficient O(1) to O(depth) editing operations and natural UI navigation patterns.

**Implementation Strategy**:
1. **Phase 1**: Core data structure and basic navigation (moveDown, moveUp, moveLeft, moveRight)
2. **Phase 2**: Context access operations (parents, ancestors, siblings, path)
3. **Phase 3**: Editing operations (modify, insert, delete)
4. **Phase 4**: Query operations and utilities
5. **Phase 5**: Integration with existing Pattern operations and typeclass instances

**See `design/DESIGN.md`** (section "Zipper for Focus") for initial design specification and type definitions.

---

## Feature 14: Pattern Matching DSL (PatternExpr Library)

**Reference**: See `design/pattern-matching-dsl-design.md` for complete design specification.

### 14.0 Design Validation and Exploration
- [ ] **STOP and REVIEW**: Review design document `design/pattern-matching-dsl-design.md` thoroughly
- [ ] **Validate Design Approach**: Analyze the layered architecture (PatternExpr → PathPattern → GraphPattern)
  - [ ] Evaluate if the three-layer approach is necessary or if a simpler approach would suffice
  - [ ] Consider: can we start with just PatternExpr and add layers incrementally?
  - [ ] Document pros and cons of the layered approach vs. a single unified type
- [ ] **Consider Alternatives**: Research and evaluate alternative design approaches
  - [ ] Compare with existing pattern matching libraries (e.g., regex libraries, tree pattern matching)
  - [ ] Consider: monadic vs. applicative vs. direct matching approaches
  - [ ] Evaluate: GADT-based vs. ADT-based pattern expressions
  - [ ] Consider: embedded DSL vs. external DSL vs. combinator library
  - [ ] Document alternative approaches and their trade-offs
- [ ] **Explore Toy Examples**: Create expressive toy examples to validate the design
  - [ ] Example 1: Match atomic patterns with specific values
  - [ ] Example 2: Match sequences with exact structure (e.g., pattern with 3 elements)
  - [ ] Example 3: Match patterns with quantifiers (e.g., one or more repetitions)
  - [ ] Example 4: Match patterns with alternatives (e.g., pattern A or pattern B)
  - [ ] Example 5: Match patterns with predicates (e.g., pattern where value > 5)
  - [ ] Example 6: Match patterns with variable binding and capture groups
  - [ ] Example 7: Match nested patterns with complex structure
  - [ ] Example 8: Find patterns within larger patterns (subpattern matching)
  - [ ] Example 9: Replace matched patterns with transformations
  - [ ] Example 10: Express real-world use case (e.g., matching AST patterns, data structure patterns)
  - [ ] Verify: each example is expressible and readable with the proposed design
  - [ ] Verify: examples demonstrate the value over existing predicate functions (Feature 9)
- [ ] **Compare with Feature 9**: Evaluate relationship with predicate-based matching
  - [ ] Document: when to use PatternExpr DSL vs. predicate functions
  - [ ] Document: can PatternExpr be implemented using predicate functions, or vice versa?
  - [ ] Document: performance implications of each approach
  - [ ] Verify: PatternExpr provides clear value beyond predicate functions
- [ ] **Design Simplifications**: Identify what can be deferred or simplified
  - [ ] Evaluate: are PathPatterns necessary for MVP, or can they be deferred?
  - [ ] Evaluate: are GraphPatterns necessary for MVP, or can they be deferred?
  - [ ] Evaluate: is pattern compilation necessary for MVP, or can it be added later?
  - [ ] Document: minimal viable PatternExpr that provides value
- [ ] **Create Design Summary**: Document the validated design approach
  - [ ] Write: design rationale document explaining chosen approach
  - [ ] Write: comparison with alternatives and why chosen approach is preferred
  - [ ] Write: toy examples document showing expressive power
  - [ ] Write: relationship document explaining PatternExpr vs. predicate functions
  - [ ] Write: implementation roadmap with phases and dependencies
- [ ] **MANDATORY CHECKPOINT**: Present design validation to user for approval
  - [ ] Present: validated design approach with rationale
  - [ ] Present: toy examples demonstrating expressiveness
  - [ ] Present: comparison with alternatives
  - [ ] Present: relationship with Feature 9 (predicate functions)
  - [ ] Present: proposed implementation phases and MVP scope
  - [ ] **STOP**: Do not proceed to implementation until user approves the design approach
  - [ ] Document: user feedback and any design adjustments needed

**Goal**: Validate the PatternExpr design approach through exploration, alternative analysis, and expressive examples. Ensure the design provides clear value and is the right approach before implementation begins.

**Checkpoint Requirement**: Implementation cannot begin until user approves the validated design approach.

### 14.1 Pattern Expressions (Layer 1)

**Prerequisite**: Section 14.0 (Design Validation) must be completed and approved before proceeding.

- [ ] Design `PatternExpr v` type with basic constructors (`PAny`, `PAtom`, `PSequence`)
- [ ] Design `Quantifier` type for repetition (`ZeroOrMore`, `OneOrMore`, `Exactly`, `Between`, etc.)
- [ ] Design combinators (`PThen`, `POr`, `PWhere`, `PBind`)
- [ ] Implement basic `PatternExpr` type and constructors
- [ ] Implement basic matching engine (`match`, `matchAll`, `matches`)
- [ ] Write tests: verify basic pattern matching on atomic patterns
- [ ] Write tests: verify basic pattern matching on sequences
- [ ] Write tests: verify combinators (sequential, alternative, predicates)

**Goal**: Enable regex-like pattern matching expressions for Pattern structures. This provides a declarative DSL for pattern matching, complementing the imperative predicate functions from Feature 9.

### 14.2 Quantification and Advanced Matching
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

**Goal**: Support quantified repetition and variable binding for complex pattern matching scenarios.

### 14.3 Path Patterns (Layer 2)
- [ ] **STOP and REVIEW**: Evaluate need for path-level patterns beyond basic PatternExpr
- [ ] Design `PathPattern v` type for path-based matching
- [ ] Implement path-level predicates and constraints
- [ ] Implement intermediate node capturing in paths
- [ ] Implement path composition (`Seq`)
- [ ] Write tests: verify path pattern matching
- [ ] Write tests: verify path-level constraints
- [ ] Write tests: verify intermediate node capturing

**Goal**: Enable path-based pattern matching for traversing pattern structures along specific paths.

### 14.4 Graph Patterns (Layer 3)
- [ ] **STOP and REVIEW**: Evaluate need for non-linear graph patterns
- [ ] Design `GraphPattern v` type for multi-path patterns
- [ ] Implement equijoins (shared variables across paths)
- [ ] Implement logical operators (`AndGraph`, `OrGraph`)
- [ ] Write tests: verify graph pattern matching
- [ ] Write tests: verify equijoins work correctly
- [ ] Write tests: verify logical combinations

**Goal**: Support non-linear graph patterns with multiple paths and shared variables (if needed).

### 14.5 Matching Engine and Optimization
- [ ] Implement pattern compilation (`compile :: PatternExpr v -> CompiledPattern v`)
- [ ] Implement `replace` and `replaceAll` for pattern transformation
- [ ] Add memoization for efficient matching
- [ ] Optimize common cases (early termination, indexing)
- [ ] Write tests: verify compilation improves performance
- [ ] Write tests: verify replace operations
- [ ] Write tests: verify performance targets (large patterns, complex expressions)

**Goal**: Provide efficient pattern matching engine with compilation and optimization.

### 14.6 Combinator Library and Surface Syntax
- [ ] Implement combinator library (`atom`, `sequence`, `zeroOrMore`, `oneOrMore`, etc.)
- [ ] Implement infix operators (`<~>`, `<|>`, `satisfying`, `as`)
- [ ] Implement derived combinators (`optional`, `listOf`, `atLeast`, `atMost`)
- [ ] Consider: QuasiQuoter for embedded pattern syntax (optional)
- [ ] Consider: Parser for Cypher-like syntax (optional)
- [ ] Write tests: verify all combinators work correctly
- [ ] Write tests: verify infix operators have correct precedence
- [ ] Write documentation: comprehensive examples using combinator library

**Goal**: Provide ergonomic combinator library for constructing pattern expressions.

**Note**: This feature provides a declarative DSL for pattern matching, conceptualized as "regex for recursive patterns." It complements the imperative predicate functions from Feature 9 by providing a more expressive, composable way to match patterns. The design follows a layered architecture (Pattern Expressions → Path Patterns → Graph Patterns) allowing incremental implementation.

**Implementation Strategy** (from design document):
1. **Phase 1**: Basic PatternExpr matching without quantifiers
2. **Phase 2**: Add quantification and backtracking
3. **Phase 3**: Path patterns (if needed)
4. **Phase 4**: Graph patterns (if needed)
5. **Phase 5**: Optimization and compilation
6. **Phase 6**: Surface syntax (optional)

**See `design/pattern-matching-dsl-design.md`** for complete design specification, type definitions, examples, and implementation strategy.

---

## Principles

1. **Branch-Based Development**: All work done in feature branches following Speckit workflow
2. **Constitution Compliance**: All code must adhere to [Constitution](.specify/memory/constitution.md) principles
3. **Incremental**: Each phase builds on previous phases
4. **Testable**: Every feature has tests before moving on
5. **Review Points**: Stop and review before advanced features
6. **Deferred Until Core Solid**: These features are deferred until the foundational Pattern implementation is complete and well-tested
7. **Use Case Driven**: Implement only when clear use cases emerge

---

## Notes

- **Workflow**: Always use feature branches and Speckit commands (`/speckit.specify`, `/speckit.plan`, `/speckit.tasks`)
- **Constitution**: Review [Constitution](.specify/memory/constitution.md) before starting any work
- These features are not foundational to Pattern itself
- Each feature should feel complete and tested before moving on
- If something feels unclear, stop and clarify before proceeding
- The core Pattern type is the foundation - get it right first before implementing these advanced features

