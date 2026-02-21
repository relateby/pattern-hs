# Implementation Plan: Paramorphism for Structure-Aware Folding

**Branch**: `028-paramorphism-folding` | **Date**: 2025-01-28 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/028-paramorphism-folding/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Implement paramorphism operations for the `Pattern` data type that enable structure-aware folding where the folding function receives both the current pattern subtree and recursively computed results from children. The implementation must provide `para :: (Pattern v -> [r] -> r) -> Pattern v -> r` as the core operation, enabling structure-aware aggregations that consider structural properties (depth, element count, nesting level) in addition to values. This extends `Foldable` (value-only folding) to provide structure-aware folding, just as `Comonad` extends `Functor` to provide structure-aware transformation. The implementation must work with patterns of any structure (atomic, with elements, nested), handle all value types, and enable operations like depth-weighted sums, structure-preserving transformations during fold, and context-dependent aggregations. Additionally, provide comprehensive documentation (user guide, reference, and porting notes) explaining the relationship between paramorphism, `Foldable`, and `Comonad`.

## Technical Context

**Language/Version**: Haskell (GHC 9.12.2)  
**Primary Dependencies**: `base >=4.17.0.0 && <5`, `QuickCheck ^>=2.14`, `hspec ^>=2.11`  
**Storage**: N/A (in-memory data structure)  
**Testing**: Hspec for unit tests, QuickCheck for property-based testing  
**Target Platform**: Cross-platform (GHC-supported platforms)  
**Project Type**: Single library project (Haskell package)  
**Performance Goals**: Standard recursive folding performance (O(n) where n is total number of nodes in pattern structure). Tests must complete quickly (<10ms for property-based tests using `quickProperty` helper with 20 test cases).  
**Constraints**: Must provide structure-aware folding that complements `Foldable` (value-only) and `Comonad` (structure-aware transformation), must handle all pattern structures correctly, must preserve element order semantics  
**Scale/Scope**: Library feature for structure-aware folding over patterns of arbitrary size and nesting depth, plus comprehensive documentation (user guide, reference, porting notes)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: ✅ PASS - Paramorphism function will be self-documenting through its type signature and standard paramorphism semantics. Haddock documentation will include examples demonstrating structure-aware aggregations, depth-weighted sums, and relationship to `Foldable` and `Comonad`. The implementation follows standard paramorphism patterns for recursive tree structures.

- **Testing Standards (NON-NEGOTIABLE)**: ✅ PASS - Comprehensive testing strategy includes: (1) Unit tests for paramorphism operations in various pattern structures (atomic, with elements, nested), (2) Unit tests demonstrating structure-aware aggregations (depth-weighted sums, nesting-level statistics, element-count-based aggregations), (3) Property-based tests verifying paramorphism provides access to pattern structure (using `quickProperty` helper for performance - 20 test cases max), (4) Edge case tests for empty elements, single elements, many elements, deep nesting, and various value types, (5) Comparison tests demonstrating differences between `Foldable`, paramorphism, and `Comonad`. All tests must complete quickly to avoid long-running test suites.

- **Conceptual Consistency**: ✅ PASS - Paramorphism is a fundamental category theory concept (recursion scheme). The implementation explicitly represents the paramorphism structure: `Pattern` is a recursive structure that can be folded with access to the original structure at each position. The relationship to `Foldable` (value-only folding) and `Comonad` (structure-aware transformation) is mathematically sound: paramorphism extends `Foldable` to see structure, just as `Comonad` extends `Functor` to see structure.

- **Mathematical Clarity**: ✅ PASS - Paramorphism signature and semantics will be formally stated in documentation. Examples will illustrate both mathematical structure and implementation. The relationship between `Foldable` (value-only), paramorphism (structure-aware folding), and `Comonad` (structure-aware transformation) will be clearly documented with examples showing when to use each operation.

- **Multi-Language Reference Alignment**: ✅ PASS - Paramorphism is a standard concept in functional programming languages. The core design (fold with access to original structure) is language-agnostic. Haskell-specific concerns (function signatures, type system) are clearly separated from the conceptual model. The implementation serves as a reference for how paramorphism semantics apply to the Pattern data structure, and porting notes will document how to implement paramorphism in other languages.

**Violations must be documented in Complexity Tracking section below.**

## Project Structure

### Documentation (this feature)

```text
specs/028-paramorphism-folding/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
│   └── type-signatures.md
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
libs/pattern/
├── src/
│   └── Pattern/
│       └── Core.hs          # Paramorphism function implementation
└── tests/
    └── Spec/
        └── Pattern/
            ├── CoreSpec.hs  # Unit tests for paramorphism
            └── Properties.hs # Property-based tests for paramorphism
```

### User Documentation (repository root)

```text
docs/
├── guide/
│   └── 06-advanced-morphisms.md  # Add paramorphism section
├── reference/
│   └── features/
│       └── paramorphism.md        # Reference documentation
└── reference/
    └── PORTING-GUIDE.md            # Add paramorphism porting notes
```

**Structure Decision**: Single Haskell library project. The paramorphism function will be added to `libs/pattern/src/Pattern/Core.hs` where the `Pattern` data type is defined. Tests will be added to existing test files: unit tests in `tests/Spec/Pattern/CoreSpec.hs` and property-based tests in `tests/Spec/Pattern/Properties.hs`. Documentation will be added to existing user guide (`docs/guide/06-advanced-morphisms.md`), reference documentation (`docs/reference/features/paramorphism.md`), and porting guide (`docs/reference/PORTING-GUIDE.md`).

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

No violations. All constitution checks pass. Paramorphism is a standard, well-understood recursion scheme that aligns with category theory foundations and Haskell best practices. The relationship to `Foldable` and `Comonad` is mathematically sound and well-documented in functional programming literature.

## Implementation Strategy

### Core Implementation Approach

The paramorphism function will be implemented using the standard recursive pattern for paramorphism on tree-like structures:

1. **Function Signature**: `para :: (Pattern v -> [r] -> r) -> Pattern v -> r`
   - The folding function receives: (1) the current pattern subtree, (2) the list of recursively computed results from children
   - Returns: the aggregated result of type `r`

2. **Implementation Pattern**: 
   ```haskell
   para :: (Pattern v -> [r] -> r) -> Pattern v -> r
   para f (Pattern v els) = 
     f (Pattern v els) (map (para f) els)
   ```
   This provides the folding function with both the current pattern subtree and recursively computed results from children.

3. **Optional Variants**: Consider implementing `paraMap` (structure-preserving paramorphism) and `paraWith` (with explicit accumulator) if they provide additional value, but start with core `para` function.

### Decision: Use Standard Paramorphism Pattern

**Rationale**: 
- Standard paramorphism pattern is well-understood and proven
- Provides explicit access to pattern structure at each position
- Enables structure-aware aggregations naturally
- Aligns with standard functional programming patterns
- Relationship to `Foldable` and `Comonad` is clear

### Test Strategy

**Performance Considerations**:
- Use `quickProperty` helper (20 test cases max) for all property-based tests
- Limit pattern generator size to keep tests fast (<10ms total)
- Reuse existing `Arbitrary` instances from `Properties.hs`
- Monitor test execution time and adjust if needed

**Test Coverage**:
1. **Unit Tests**: Cover all user stories with concrete examples
   - Atomic patterns (no elements)
   - Singular patterns (one element)
   - Patterns with multiple elements
   - Nested patterns (2+ levels deep)
   - Structure-aware aggregations (depth-weighted sums, nesting-level statistics, element-count-based aggregations)
   - Comparison with `Foldable` and `Comonad` operations
   - Various value types (String, Int, custom types)
   - Edge cases (empty, single, many elements)

2. **Property-Based Tests**: Verify paramorphism properties
   - Paramorphism provides access to pattern structure at each position
   - Structure-aware aggregations produce correct results
   - Relationship to `Foldable` (paramorphism can simulate `foldr` when ignoring structure)
   - Order preservation properties

**Examples Alignment**:
- Ensure test examples align with patterns shown in user guide
- Use consistent terminology (atomic patterns, elements, values, structure)
- Demonstrate real-world usage patterns (depth-weighted sums, structure-aware statistics)

## Research Questions

### RQ-001: What is the standard paramorphism pattern for recursive tree structures?

**Decision**: Use standard paramorphism pattern where the folding function receives both the current pattern subtree and recursively computed results from children.

**Rationale**:
- Standard paramorphism pattern is well-established in functional programming literature
- Provides natural access to pattern structure at each position
- Enables structure-aware aggregations without manual traversal
- Aligns with standard recursion schemes (catamorphism, paramorphism, anamorphism)

**Implementation**:
```haskell
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
para f (Pattern v els) = 
  f (Pattern v els) (map (para f) els)
```

**Alternatives Considered**:
- Post-order processing: Rejected because standard paramorphism uses the pattern as-is
- Pre-order processing: Already included in standard pattern (pattern subtree is provided)
- Manual traversal: Rejected because paramorphism provides the abstraction we need

### RQ-002: Should we implement additional paramorphism variants (paraMap, paraWith)?

**Decision**: Start with core `para` function. Consider `paraMap` and `paraWith` if concrete use cases emerge, but defer to future enhancement.

**Rationale**:
- Core `para` function provides essential structure-aware folding capability
- Additional variants can be added later if needed
- Keeps initial implementation focused and simple
- Standard `para` function is sufficient for documented use cases

**Implementation**:
- Core: `para :: (Pattern v -> [r] -> r) -> Pattern v -> r`
- Deferred: `paraMap :: (Pattern v -> [r] -> r) -> Pattern v -> Pattern r` (structure-preserving paramorphism)
- Deferred: `paraWith :: (Pattern v -> [r] -> r) -> Pattern v -> r` (with explicit accumulator - may be equivalent to `para`)

**Alternatives Considered**:
- Implement all variants immediately: Rejected because it increases complexity without clear use cases
- Only implement `paraMap`: Rejected because `para` is more fundamental and `paraMap` can be derived from it

### RQ-003: How should we document the relationship between paramorphism, Foldable, and Comonad?

**Decision**: Provide comprehensive documentation in user guide, reference documentation, and porting notes explaining:
1. How paramorphism extends `Foldable` to provide structure access (like `Comonad` extends `Functor`)
2. When to use each operation (`Foldable` for value-only, paramorphism for structure-aware folding, `Comonad` for structure-aware transformation)
3. Examples comparing all three operations on the same pattern

**Rationale**:
- Understanding relationships helps developers choose the right operation
- Clear documentation reduces confusion and improves developer experience
- Examples demonstrate practical differences and use cases
- Porting notes help implementers in other languages understand the concepts

**Documentation Structure**:
- User guide (`docs/guide/06-advanced-morphisms.md`): Add paramorphism section with intuitive explanation, examples, and relationship to `Foldable` and `Comonad`
- Reference (`docs/reference/features/paramorphism.md`): Complete reference documentation with type signatures, laws, properties, and implementation details
- Porting guide (`docs/reference/PORTING-GUIDE.md`): Add paramorphism porting notes explaining how to implement in other languages

**Alternatives Considered**:
- Only user guide: Rejected because reference documentation and porting notes are essential for completeness
- Only reference documentation: Rejected because user guide provides intuitive introduction needed for learning

### RQ-004: How should property-based tests verify structure access?

**Decision**: Use property-based tests that verify the folding function receives the correct pattern structure at each position, and that structure-aware aggregations produce correct results.

**Rationale**:
- Property-based tests can verify structure access by checking that folding functions can extract structural properties (depth, element count)
- Structure-aware aggregations (depth-weighted sums) provide concrete verification
- Tests serve as executable specifications for implementations in other languages

**Implementation**:
- Test that `para` provides access to pattern structure: `para (\p _ -> depth p) pattern == depth pattern`
- Test structure-aware aggregations: `para (\p rs -> value p * depth p + sum rs) pattern` produces correct depth-weighted sum
- Test relationship to `Foldable`: `para (\_ rs -> foldr (+) 0 rs)` can simulate `foldr` when ignoring structure

**Alternatives Considered**:
- Only unit tests: Rejected because property-based tests provide better coverage and serve as executable specifications
- Only structure access tests: Rejected because structure-aware aggregation tests provide concrete verification

## Data Model

### Paramorphism Function Structure

```haskell
-- Core paramorphism function
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
para f (Pattern v els) = 
  f (Pattern v els) (map (para f) els)

-- The folding function receives:
-- 1. The current pattern subtree (Pattern v)
-- 2. The list of recursively computed results from children ([r])
-- Returns: aggregated result (r)
```

### Structure-Aware Aggregation Examples

```haskell
-- Depth-weighted sum
depthWeightedSum :: Pattern Int -> Int
depthWeightedSum = para (\p rs -> value p * depth p + sum rs)

-- Element-count-aware aggregation
elementCountSum :: Pattern Int -> Int
elementCountSum = para (\p rs -> value p * length (elements p) + sum rs)

-- Structure-preserving transformation during fold
structureAwareTransform :: Pattern Int -> Pattern Int
structureAwareTransform = para (\p rs -> Pattern (value p * depth p) rs)
```

### Relationship to Foldable and Comonad

```haskell
-- Foldable: value-only folding
foldr (+) 0 pattern  -- Only values, no structure access

-- Paramorphism: structure-aware folding
para (\p rs -> value p * depth p + sum rs) pattern  -- Structure + values

-- Comonad: structure-aware transformation
extend (\p -> value p * depth p) pattern  -- Structure-aware transformation, not folding
```

### Value Processing Order

For a `Pattern v`:
1. Recursively compute results for all children using `para f`
2. Apply folding function `f` to: (1) current pattern subtree, (2) list of child results
3. Return aggregated result

This follows standard paramorphism pattern: process children first, then combine with current pattern.

### Example: Paramorphism on a Pattern

```haskell
-- Pattern structure:
-- Pattern { value = 10, elements = [
--   Pattern { value = 5, elements = [] },
--   Pattern { value = 3, elements = [] }
-- ]}

-- Depth-weighted sum:
-- para (\p rs -> value p * depth p + sum rs)
-- = (\p rs -> value p * depth p + sum rs) 
--     (Pattern 10 [Pattern 5 [], Pattern 3 []])
--     [para f (Pattern 5 []), para f (Pattern 3 [])]
-- = (\p rs -> value p * depth p + sum rs)
--     (Pattern 10 [Pattern 5 [], Pattern 3 []])
--     [(\p rs -> value p * depth p + sum rs) (Pattern 5 []) [],
--      (\p rs -> value p * depth p + sum rs) (Pattern 3 []) []]
-- = (\p rs -> value p * depth p + sum rs)
--     (Pattern 10 [Pattern 5 [], Pattern 3 []])
--     [5 * 0 + 0, 3 * 0 + 0]
-- = 10 * 1 + (5 + 3)
-- = 10 + 8
-- = 18
```

## Quickstart Guide

### Basic Usage

```haskell
import Pattern.Core (Pattern(..), pattern, point, para)

-- Create a pattern with integer values
let p = pattern 10 [point 5, point 3]

-- Depth-weighted sum using paramorphism
depthWeightedSum = para (\pat rs -> value pat * depth pat + sum rs) p
-- Provides access to pattern structure (depth) and values

-- Compare with Foldable (value-only)
valueOnlySum = foldr (+) 0 p  -- Only values, no structure access

-- Structure-aware aggregation considering element count
elementCountSum = para (\pat rs -> value pat * length (elements pat) + sum rs) p
```

### Common Patterns

```haskell
-- Depth-weighted sum
depthWeightedSum :: Pattern Int -> Int
depthWeightedSum = para (\p rs -> value p * depth p + sum rs)

-- Nesting-level statistics
nestingLevelStats :: Pattern Int -> (Int, Int, Int)  -- (sum, count, maxDepth)
nestingLevelStats = para (\p rs -> 
  let (s, c, d) = foldr (\(s', c', d') (s'', c'', d'') -> 
    (s' + s'', c' + c'', max d' d'')) (0, 0, 0) rs
  in (value p + s, 1 + c, max (depth p) d))

-- Element-count-aware aggregation
elementCountSum :: Pattern Int -> Int
elementCountSum = para (\p rs -> value p * length (elements p) + sum rs)

-- Structure-preserving transformation during fold
structureAwareTransform :: Pattern Int -> Pattern Int
structureAwareTransform = para (\p rs -> Pattern (value p * depth p) rs)
```

### When to Use Paramorphism vs. Foldable vs. Comonad

```haskell
-- Use Foldable when you only need values:
sumValues = foldr (+) 0 pattern  -- Value-only aggregation

-- Use Paramorphism when you need structure-aware folding:
depthWeightedSum = para (\p rs -> value p * depth p + sum rs) pattern
-- Structure-aware aggregation considering depth, element count, etc.

-- Use Comonad when you need structure-aware transformation:
depthWeightedValues = extend (\p -> value p * depth p) pattern
-- Structure-aware transformation, not aggregation
```

## Contracts

### Type Signatures

```haskell
-- Core paramorphism function
para :: (Pattern v -> [r] -> r) -> Pattern v -> r

-- The folding function signature:
-- Pattern v -> [r] -> r
--   First argument: current pattern subtree
--   Second argument: list of recursively computed results from children
--   Returns: aggregated result
```

### Laws and Properties

1. **Structure Access**: `para (\p _ -> p) pattern == pattern` (paramorphism can extract structure)
2. **Value Access**: `para (\p rs -> value p : concat rs) pattern == toList pattern` (paramorphism can simulate `Foldable`)
3. **Recursive Structure**: `para f (Pattern v els) = f (Pattern v els) (map (para f) els)` (standard paramorphism pattern)
4. **Order Preservation**: Paramorphism preserves element order when aggregating results
5. **Relationship to Foldable**: `para (\_ rs -> foldr f z rs) pattern` can simulate `foldr f z pattern` when ignoring structure (but loses pattern's own value - need to handle separately)

## Testing Plan

### Unit Tests (CoreSpec.hs)

**User Story 1 - Fold Patterns with Structure Awareness**:
- Atomic pattern: `para` provides access to pattern structure
- Pattern with elements: `para` receives pattern subtree and child results
- Nested pattern: `para` provides structural access at all levels
- Structure-preserving transformation: `para` can access original structure while computing results
- Context-dependent aggregation: `para` enables aggregations that adapt to structural properties

**User Story 2 - Perform Structure-Aware Aggregations**:
- Depth-weighted sum: `para` computes correct depth-weighted aggregation
- Element-count aggregation: `para` considers element count in aggregation
- Nesting-level statistics: `para` computes level-aware statistics
- Context-dependent aggregation: `para` adapts to structural properties

**User Story 3 - Understand Relationship to Foldable and Comonad**:
- Comparison with `Foldable`: Demonstrate value-only vs. structure-aware folding
- Comparison with `Comonad`: Demonstrate structure-aware transformation vs. structure-aware folding
- Examples showing when to use each operation

**Edge Cases**:
- Atomic pattern (no elements): `para` works correctly
- Empty elements list: `para` works correctly
- Singular pattern (one element): `para` works correctly
- Many elements: `para` works correctly
- Deep nesting (3+ levels): `para` works correctly
- Various value types: `para` works with String, Int, custom types

### Property-Based Tests (Properties.hs)

**Using `quickProperty` helper (20 test cases max)**:

1. **Structure Access**: `para (\p _ -> depth p) pattern == depth pattern` (paramorphism provides structure access)
2. **Value Access**: `para (\p rs -> value p : concat rs) pattern == toList pattern` (paramorphism can simulate `Foldable`)
3. **Depth-Weighted Sum**: `para (\p rs -> value p * depth p + sum rs) pattern` produces correct depth-weighted sum
4. **Element-Count Aggregation**: `para (\p rs -> value p * length (elements p) + sum rs) pattern` produces correct element-count-aware aggregation
5. **Order Preservation**: Paramorphism preserves element order when aggregating results
6. **Relationship to Foldable**: `para (\p rs -> value p + sum rs) pattern == foldr (+) 0 pattern` (when ignoring structure, paramorphism can simulate `Foldable`)

**Performance**: All property-based tests must complete in <10ms total using `quickProperty` helper.

## Documentation Plan

### User Guide (`docs/guide/06-advanced-morphisms.md`)

**Add new section: "Paramorphism: Structure-Aware Folding"**

Content:
- Intuitive explanation: Paramorphism extends `Foldable` to provide structure access, just as `Comonad` extends `Functor`
- Examples: Depth-weighted sums, nesting-level statistics, element-count-aware aggregations
- Relationship to `Foldable` and `Comonad`: When to use each operation
- Practical use cases: Structure-aware analyses that are impossible with `Foldable` alone

### Reference Documentation (`docs/reference/features/paramorphism.md`)

**Create new file**

Content:
- Complete reference documentation for paramorphism
- Type signatures and function documentation
- Laws and properties
- Implementation details
- Examples and use cases
- Performance characteristics

### Porting Guide (`docs/reference/PORTING-GUIDE.md`)

**Add new section: "Paramorphism Implementation"**

Content:
- How to implement paramorphism in other languages
- Language-specific considerations (type systems, recursion patterns)
- Examples in multiple languages (Rust, TypeScript, Python)
- Relationship to `Foldable` and `Comonad` in other languages

## Examples Alignment

Ensure test examples align with patterns shown in user guide:
- Use `pattern`, `point` constructor functions
- Follow consistent terminology (atomic patterns, elements, values, structure)
- Demonstrate real-world usage patterns (depth-weighted sums, structure-aware statistics)
- Show nested pattern structures matching user guide patterns

## Success Criteria Verification

- ✅ **SC-001**: Unit tests demonstrate paramorphism over atomic, with elements, and nested patterns with full structural awareness
- ✅ **SC-002**: Property-based tests verify paramorphism provides access to pattern subtree and child results with 100% accuracy
- ✅ **SC-003**: Unit and property-based tests verify structure-aware aggregations produce correct results reflecting both values and structural properties
- ✅ **SC-004**: Tests cover String, Int, and custom type values
- ✅ **SC-005**: Tests include nested patterns (3+ levels deep)
- ✅ **SC-006**: Tests verify order preservation
- ✅ **SC-007**: Documentation clearly explains relationship between paramorphism, `Foldable`, and `Comonad`
- ✅ **SC-008**: Examples demonstrate structure-aware aggregations effectively
- ✅ **SC-009**: Tests demonstrate structure-aware folding operations impossible with `Foldable` alone

## Next Steps

1. Run `/speckit.tasks` to generate dependency-ordered task list
2. Implement paramorphism function following the plan
3. Write tests (unit + property-based) ensuring they fail first
4. Implement function to make tests pass
5. Add documentation (user guide, reference, porting notes)
6. Verify all success criteria are met
7. Ensure test performance is acceptable (<10ms for property-based tests)
