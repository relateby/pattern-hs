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

## Phase 1: Core Data Type (Foundation) ✅

### 1.1 Basic Pattern Type
- [x] Define `Pattern v` data type with `value :: v` and `elements :: [Pattern v]` in `src/Pattern/Core.hs`
- [x] Add basic Haddock documentation explaining the recursive tree structure
- [x] Write simple test: create a leaf pattern and verify structure
- [x] Write simple test: create a pattern with children and verify structure

**Goal**: Have a working Pattern type that can be constructed and inspected. ✅ **COMPLETE**

---

## Phase 2: Basic Typeclasses ✅

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

## Phase 3: Construction Functions

### 3.1 Basic Constructors
- [ ] Implement `pattern :: v -> Pattern v` (creates leaf node)
- [ ] Implement `patternWith :: v -> [Pattern v] -> Pattern v` (creates pattern with children)
- [ ] Write tests: construct various patterns and verify structure
- [ ] Write tests: edge cases (empty list, single child, many children)

**Goal**: Convenient ways to create patterns.

---

## Phase 4: Functor Instance

### 4.1 Functor Implementation
- [ ] Implement `Functor` instance for `Pattern`
- [ ] Write property test: `fmap id = id` (functor identity law)
- [ ] Write property test: `fmap (f . g) = fmap f . fmap g` (functor composition law)
- [ ] Write unit tests: transform values in simple patterns
- [ ] Write unit tests: transform values in nested patterns

**Goal**: Patterns can have their values transformed while preserving structure.

---

## Phase 5: Foldable Instance

### 5.1 Foldable Implementation
- [ ] Implement `Foldable` instance for `Pattern`
- [ ] Write test: `foldr` collects all values
- [ ] Write test: `foldl` collects all values (verify order)
- [ ] Write test: `foldMap` works correctly
- [ ] Write test: `toList` produces all values

**Goal**: Patterns can be folded/traversed for aggregation.

---

## Phase 6: Traversable Instance

### 6.1 Traversable Implementation
- [ ] Implement `Traversable` instance for `Pattern`
- [ ] Write test: `traverse` with `Identity` preserves structure
- [ ] Write test: `traverse` with `Maybe` handles failures
- [ ] Write test: `sequenceA` works correctly

**Goal**: Patterns can be traversed with effects.

---

## Phase 7: Basic Query Functions

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

## Phase 8: Simple Classification (Node Only)

### 8.1 Node Detection
- [ ] Implement `isNode :: Pattern v -> Bool` (pattern has no children)
- [ ] Write test: leaf pattern is a node
- [ ] Write test: pattern with children is not a node
- [ ] Write test: nested patterns correctly identify nodes

**Goal**: Can identify the simplest graph element (node).

---

## Phase 9: Relationship Classification

### 9.1 Relationship Detection
- [ ] Implement `isRelationship :: Pattern v -> Bool` (exactly 2 child nodes)
- [ ] Write test: pattern with 2 child nodes is a relationship
- [ ] Write test: pattern with 1 child is not a relationship
- [ ] Write test: pattern with 3+ children is not a relationship
- [ ] Write test: pattern with 2 children that aren't nodes is not a relationship

### 9.2 Relationship Navigation
- [ ] Implement `source :: Pattern v -> Pattern v` (first child of relationship)
- [ ] Implement `target :: Pattern v -> Pattern v` (second child of relationship)
- [ ] Write tests: extract source and target from relationships
- [ ] Write tests: error handling for non-relationship patterns (consider `Maybe` return type)

**Goal**: Can identify and navigate relationships.

---

## Phase 10: Graph Element Classification

### 10.1 Graph Element Detection
- [ ] Implement `isGraphElement :: Pattern v -> Bool` (is node, relationship, subgraph, or path)
- [ ] Write test: node is a graph element
- [ ] Write test: relationship is a graph element
- [ ] Write test: simple non-graph structure is not a graph element

### 10.2 Subgraph Detection
- [ ] Implement `isSubgraph :: Pattern v -> Bool` (all children are graph elements)
- [ ] Write test: pattern with graph element children is subgraph
- [ ] Write test: pattern with mixed children is not subgraph
- [ ] Write test: nested subgraphs work correctly

**Goal**: Can classify patterns as graph elements.

---

## Phase 11: Path Detection

### 11.1 Path Classification
- [ ] Implement helper `chainsCorrectly :: [Pattern v] -> Bool`
- [ ] Implement `isPath :: Pattern v -> Bool` (subgraph where relationships chain)
- [ ] Write test: simple path of 2 relationships
- [ ] Write test: longer path chains correctly
- [ ] Write test: broken chain is not a path
- [ ] Write test: path with branching is not a path (or is it? clarify semantics)

**Goal**: Can identify paths in patterns.

---

## Phase 12: Pattern Navigation and Extraction

### 12.1 Node Extraction
- [ ] Implement `nodes :: Pattern v -> [Pattern v]` (all nodes in pattern)
- [ ] Write test: extract all nodes from simple pattern
- [ ] Write test: extract all nodes from nested pattern
- [ ] Write test: nodes appear in correct order (or specify order semantics)

### 12.2 Relationship Extraction
- [ ] Implement `relationships :: Pattern v -> [Pattern v]` (all relationships in pattern)
- [ ] Write test: extract all relationships from pattern
- [ ] Write test: nested relationships are included

**Goal**: Can extract graph elements from patterns.

---

## Phase 13: Additional Typeclasses (As Needed)

### 13.1 Ord Instance (If Needed)
- [ ] Consider if `Ord` instance is needed
- [ ] If yes, implement `Ord` instance with clear ordering semantics
- [ ] Write tests: ordering of patterns

### 13.2 Other Instances
- [ ] Evaluate if other standard typeclasses are needed (e.g., `Semigroup`, `Monoid`)
- [ ] Implement only if clearly needed for core functionality

**Goal**: Complete basic typeclass coverage.

---

## Phase 14: Graph Representation (Underspecified)

### 14.1 Graph Type Design
- [ ] **STOP and REVIEW**: Is Graph representation needed yet?
- [ ] Define `Graph dir v` type (if proceeding)
- [ ] Define `Edge dir v` type (if proceeding)
- [ ] Keep this minimal - defer complex graph operations

### 14.2 Basic Graph Operations (If Needed)
- [ ] Implement `emptyGraph :: Graph dir v`
- [ ] Implement basic graph construction from patterns
- [ ] Write minimal tests

**Goal**: Basic graph representation if needed for core functionality.

**Note**: Advanced graph operations deferred until core is solid.

---

## Phase 15: Graph Views (Underspecified)

### 15.1 GraphView Typeclass Design
- [ ] **STOP and REVIEW**: Is GraphView needed yet?
- [ ] Design `GraphView` typeclass interface (minimal)
- [ ] Consider: can we defer this entirely?

### 15.2 DirectedView (If Proceeding)
- [ ] Implement minimal `DirectedView` if needed
- [ ] Keep implementation simple

**Goal**: Basic view support if needed.

**Note**: Advanced views and view composition deferred until core is solid.

---

## Phase 16: Pattern Morphisms (Underspecified)

### 16.1 Morphism Design
- [ ] **STOP and REVIEW**: Are morphisms needed yet?
- [ ] Define `PatternMorphism` type synonym
- [ ] Implement `homomorphism` if clearly needed
- [ ] Implement `forget` if clearly needed

**Goal**: Basic morphisms if needed for core.

**Note**: Advanced morphism theory deferred until core is solid.

---

## Phase 17: Integration and Polish

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

**Current Phase**: Phase 2 (Basic Typeclasses) - Complete ✅

**Completed**:
- ✅ Phase 1: Core Pattern type fully implemented with comprehensive tests (25 test cases)
- ✅ Phase 2: Show and Eq instances implemented with comprehensive tests (13 additional test cases)

**Next Steps**: 
1. Move to Phase 3 (Construction Functions)
2. Implement `pattern` and `patternWith` constructor functions
3. Write tests for constructor functions

---

## Notes

- **Workflow**: Always use feature branches and Speckit commands (`/speckit.specify`, `/speckit.plan`, `/speckit.tasks`)
- **Constitution**: Review [Constitution](.specify/memory/constitution.md) before starting any work
- Don't rush ahead to advanced features
- Each phase should feel complete and tested
- If something feels unclear, stop and clarify before proceeding
- The core Pattern type is the foundation - get it right first

