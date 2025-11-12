# Implementation Plan: Foldable Instance for Pattern

**Branch**: `006-foldable-instance` | **Date**: 2025-01-28 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/006-foldable-instance/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Implement a `Foldable` instance for the `Pattern` data type that enables folding over all values in pattern structures (including the pattern's own value and all element values at all nesting levels). The implementation must support `foldr`, `foldl`, `foldMap`, and `toList` operations, work with patterns of any structure (atomic, with elements, nested), and satisfy foldable laws and properties. The `toList()` operation follows standard Foldable behavior, extracting all values as a flat list. Additionally, implement a `flatten()` function that extracts all values as a flat list (which may be equivalent to `toList` or provided for clarity), and a `toTuple()` function that extracts patterns as tuples preserving structure. This provides the foundational capability for aggregating pattern values without manual traversal, enabling statistics computation, value extraction, and data analysis over pattern structures.

## Technical Context

**Language/Version**: Haskell (GHC 9.8.4, 9.10.3)  
**Primary Dependencies**: `base >=4.17.0.0 && <5`, `QuickCheck ^>=2.14`, `hspec ^>=2.11`  
**Storage**: N/A (in-memory data structure)  
**Testing**: Hspec for unit tests, QuickCheck for property-based testing  
**Target Platform**: Cross-platform (GHC-supported platforms)  
**Project Type**: Single library project (Haskell package)  
**Performance Goals**: Standard foldable performance (O(n) where n is total number of values in pattern structure). Tests must complete quickly (<10ms for property-based tests using `quickProperty` helper with 20 test cases).  
**Constraints**: Must satisfy foldable laws, must process all values (pattern's own value + all element values recursively), must preserve element order semantics  
**Scale/Scope**: Library feature for folding over patterns of arbitrary size and nesting depth

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: ✅ PASS - Foldable instance is a standard Haskell typeclass implementation. The `foldr`, `foldl`, `foldMap`, and `toList` functions will be self-documenting through their type signatures and standard foldable semantics. Haddock documentation will include examples demonstrating value aggregation, list extraction, and order preservation.

- **Testing Standards (NON-NEGOTIABLE)**: ✅ PASS - Comprehensive testing strategy includes: (1) Unit tests for folding operations in various pattern structures (atomic, with elements, nested), (2) Unit tests for `toList` flat list extraction (standard Foldable behavior), (3) Unit tests for `flatten` flat list extraction, (4) Unit tests for `foldMap` with monoids, (5) Property-based tests for foldable laws and properties (using `quickProperty` helper for performance - 20 test cases max), (6) Edge case tests for empty elements, single elements, many elements, deep nesting, and various value types. All tests must complete quickly to avoid long-running test suites.

- **Conceptual Consistency**: ✅ PASS - Foldable is a fundamental category theory concept. The implementation explicitly represents the foldable structure: `Pattern` is a foldable that aggregates values while traversing the pattern structure. The foldable laws are mathematical requirements that ensure categorical correctness.

- **Mathematical Clarity**: ✅ PASS - Foldable laws and properties will be formally stated in documentation. Examples will illustrate both mathematical structure and implementation. The relationship between `foldr`, `foldl`, `foldMap`, and `toList` will be clearly documented.

- **Multi-Language Reference Alignment**: ✅ PASS - Foldable is a standard concept in functional programming languages. The core design (aggregate values while traversing structure) is language-agnostic. Haskell-specific concerns (typeclass instance syntax) are clearly separated from the conceptual model. The implementation serves as a reference for how foldable semantics apply to the Pattern data structure.

**Violations must be documented in Complexity Tracking section below.**

## Project Structure

### Documentation (this feature)

```text
specs/006-foldable-instance/
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
src/
└── Pattern/
    └── Core.hs          # Foldable instance implementation

tests/
└── Spec/
    └── Pattern/
        ├── CoreSpec.hs  # Unit tests for foldable instance
        └── Properties.hs # Property-based tests for foldable laws
```

**Structure Decision**: Single Haskell library project. The Foldable instance will be added to `src/Pattern/Core.hs` where the `Pattern` data type is defined. Tests will be added to existing test files: unit tests in `tests/Spec/Pattern/CoreSpec.hs` and property-based tests in `tests/Spec/Pattern/Properties.hs`.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

No violations. All constitution checks pass. The Foldable instance is a standard, well-understood abstraction that aligns with category theory foundations and Haskell best practices.

## Implementation Strategy

### Core Implementation Approach

The Foldable instance will be implemented using the standard recursive pattern for tree-like structures:

1. **Minimal Complete Definition**: Implement `foldr` as the primary method. Other methods (`foldl`, `foldMap`, `toList`) can be derived from `foldr` or implemented directly for efficiency.

2. **Value Processing Order**: For a `Pattern v`, we need to process:
   - The pattern's own value (`value` field)
   - All element values recursively (from `elements` field)

3. **Implementation Pattern**: 
   ```haskell
   instance Foldable Pattern where
     foldr f z (Pattern v els) = 
       f v (foldr (\p acc -> foldr f acc p) z els)
   ```
   This processes the pattern's value first, then recursively folds over all elements.

4. **Alternative Approach (if needed)**: Implement `foldMap` as the primary method if it provides better performance or clarity:
   ```haskell
   instance Foldable Pattern where
     foldMap f (Pattern v els) = 
       f v <> foldMap (foldMap f) els
   ```

### Decision: Use `foldr` as Primary Method

**Rationale**: 
- `foldr` is the standard minimal complete definition for Foldable
- Provides explicit control over processing order
- Other methods can be efficiently derived from `foldr`
- Aligns with standard Haskell Foldable patterns

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
   - Various value types (String, Int, custom types)
   - Edge cases (empty, single, many elements)

2. **Property-Based Tests**: Verify foldable laws and properties
   - `toList` preserves structure correctly
   - `flatten` extracts all values correctly as flat list
   - `foldr` and `foldl` process all values
   - `foldMap` with monoids produces correct results
   - Order preservation properties

**Examples Alignment**:
- Ensure test examples align with patterns shown in `examples/examples.md`
- Use consistent terminology (atomic patterns, elements, values)
- Demonstrate real-world usage patterns

## Research Questions

### RQ-001: What is the correct order for processing values in Pattern?

**Decision**: Process the pattern's own value first, then recursively process all element values. This follows a pre-order traversal pattern.

**Rationale**:
- The pattern's value is decoration about the pattern itself
- Processing it first makes the folding operation intuitive
- Recursive processing of elements naturally handles nested structures
- This order aligns with how `Functor` processes values (pattern's value, then elements)

**Implementation**:
```haskell
foldr f z (Pattern v els) = f v (foldr (\p acc -> foldr f acc p) z els)
```

**Alternatives Considered**:
- Post-order (elements first, then pattern's value): Rejected because it's less intuitive and doesn't align with Functor semantics
- In-order: Not applicable for tree structures

### RQ-002: Should we implement `foldr` or `foldMap` as the primary method?

**Decision**: Implement `foldr` as the primary method (minimal complete definition).

**Rationale**:
- `foldr` is the standard minimal complete definition for Foldable
- Provides explicit control over processing order
- Other methods (`foldl`, `foldMap`, `toList`) can be efficiently derived
- Aligns with standard Haskell Foldable patterns

**Implementation**:
```haskell
instance Foldable Pattern where
  foldr f z (Pattern v els) = 
    f v (foldr (\p acc -> foldr f acc p) z els)
```

**Alternatives Considered**:
- `foldMap` as primary: Considered but rejected because `foldr` is more standard and provides better control over order
- Both implemented: Not necessary - `foldMap` can be derived from `foldr`

### RQ-003: How should we handle edge cases (atomic patterns, empty elements)?

**Decision**: Handle all edge cases explicitly in the implementation:
- Atomic patterns (no elements): Process only the pattern's value
- Empty elements list: Process only the pattern's value
- Nested patterns: Recursively process all levels

**Rationale**:
- Explicit handling ensures correctness for all pattern structures
- Recursive implementation naturally handles edge cases
- No special-case logic needed - the recursive structure handles it

**Implementation**:
The recursive `foldr` implementation naturally handles all cases:
- Atomic pattern: `foldr f z (Pattern v []) = f v z` (processes only value)
- Pattern with elements: Recursively processes value and all elements
- Nested patterns: Recursion handles all nesting levels

### RQ-004: How should property-based tests be structured for performance?

**Decision**: Use `quickProperty` helper with 20 test cases max, reuse existing `Arbitrary` instances, and limit pattern generator size.

**Rationale**:
- `quickProperty` helper already exists in `Properties.hs` and limits to 20 test cases
- Existing `Arbitrary` instances are optimized for fast test execution
- Pattern generators limit size to keep tests fast (<10ms total)
- This aligns with existing test performance standards

**Implementation**:
- Reuse `Arbitrary (Pattern String)` and `Arbitrary (Pattern Int)` from `Properties.hs`
- Use `quickProperty` for all property-based tests
- Monitor test execution time and adjust if needed

## Data Model

### Foldable Instance Structure

```haskell
instance Foldable Pattern where
  -- Minimal complete definition: foldr
  foldr :: (a -> b -> b) -> b -> Pattern a -> b
  foldr f z (Pattern v els) = 
    f v (foldr (\p acc -> foldr f acc p) z els)
  
  -- Derived methods (can be overridden for efficiency if needed)
  foldl :: (b -> a -> b) -> b -> Pattern a -> b
  foldMap :: Monoid m => (a -> m) -> Pattern a -> m
  toList :: Pattern a -> [a]  -- Standard Foldable: returns flat list of all values
```

### flatten() Function

```haskell
-- Explicit flattening function
flatten :: Pattern a -> [a]
flatten = foldr (:) []
-- Extracts all values from all nesting levels as a flat list
```

### toTuple() Function

```haskell
-- Structure-preserving tuple extraction
toTuple :: Pattern v -> (v, [Pattern v])
toTuple (Pattern v els) = (v, els)
-- Extracts pattern as tuple preserving structure: value and elements list
```

### Value Processing Order

For a `Pattern v`:
1. Process the pattern's own value (`value` field)
2. Recursively process all element values (from `elements` field)

This follows a pre-order traversal: pattern value first, then elements.

### Example: Folding a Pattern

```haskell
-- Pattern structure:
-- Pattern { value = "root", elements = [
--   Pattern { value = "a", elements = [] },
--   Pattern { value = "b", elements = [] }
-- ]}

-- foldr (+) 0 processes: "root" + ("a" + ("b" + 0))
-- Note: This is conceptual - actual types must match

-- For integers:
-- Pattern { value = 10, elements = [
--   Pattern { value = 5, elements = [] },
--   Pattern { value = 3, elements = [] }
-- ]}
-- foldr (+) 0 = 10 + (5 + (3 + 0)) = 18
```

## Quickstart Guide

### Basic Usage

```haskell
import Pattern.Core (Pattern(..), pattern, patternWith)
import Data.Monoid (Sum(..))

-- Create a pattern with integer values
let p = patternWith 10 [pattern 5, pattern 3]

-- Sum all values (using flatten for flat list)
sum (flatten p)  -- [10, 5, 3] -> 18

-- Or using foldMap with Sum monoid
getSum (foldMap Sum p)  -- 18

-- Extract values as a flat list (standard Foldable behavior)
toList p  -- [10, 5, 3]  (flat list)

-- Flatten all values (explicit flattening)
flatten p  -- [10, 5, 3]  (flat list)

-- Extract pattern as tuple (preserves structure)
toTuple p  -- (10, [Pattern {value = 5, elements = []}, Pattern {value = 3, elements = []}])

-- Fold with custom function
foldr (+) 0 p  -- 18
```

### Common Patterns

```haskell
-- Sum integers
getSum (foldMap Sum pattern) :: Int

-- Concatenate strings
foldr (++) "" pattern :: String

-- Count values (using flatten for flat list)
length (flatten pattern) :: Int

-- Check if all values satisfy a predicate
all (> 0) (flatten pattern) :: Bool

-- Extract all values as flat list
toList pattern :: [a]  -- Flat list of all values

-- Extract pattern structure as tuple
toTuple pattern :: (a, [Pattern a])  -- Preserves structure
```

## Contracts

### Type Signatures

```haskell
-- Minimal complete definition
foldr :: (a -> b -> b) -> b -> Pattern a -> b

-- Derived methods
foldl :: (b -> a -> b) -> b -> Pattern a -> b
foldMap :: Monoid m => (a -> m) -> Pattern a -> m
toList :: Pattern a -> [a]

-- Other standard Foldable methods are available via default implementations
```

### Laws and Properties

1. **toList extracts all values**: `toList (Pattern v els) = v : concatMap toList els` (flat list, standard Foldable behavior)
2. **flatten extracts all values**: `flatten (Pattern v els) = v : concatMap flatten els` (flat list)
3. **foldr processes all values**: `foldr f z p = foldr f z (toList p)` (conceptually, using toList)
4. **foldMap with monoids**: `foldMap f p = foldMap f (toList p)` (conceptually, using toList)
5. **Relationship**: `toList p = flatten p` (both extract flat lists, flatten may be an alias for clarity)
6. **toTuple preserves structure**: `toTuple (Pattern v els) = (v, els)` (structure-preserving tuple)

## Testing Plan

### Unit Tests (CoreSpec.hs)

**User Story 1 - Aggregate Values**:
- Atomic pattern with integer: `foldr (+) 0` produces correct sum
- Pattern with multiple integers: `foldr (+) 0` sums all values
- Pattern with strings: `foldr (++) ""` concatenates all values
- Nested pattern: `foldr (+) 0` sums values from all levels
- Custom type: `foldr` works with custom aggregation functions

**User Story 2 - Extract Values as Flat List**:
- Atomic pattern: `toList` returns single-element list
- Pattern with elements: `toList` returns flat list with all values (pattern value and all element values)
- Nested pattern: `toList` returns flat list with all values from all nesting levels
- Integer pattern: `toList` returns flat list of integers

**User Story 2a - Flatten All Values**:
- Atomic pattern: `flatten` returns single-element list
- Pattern with elements: `flatten` returns flat list with all values including pattern's value
- Nested pattern: `flatten` returns flat list with values from all levels
- Integer pattern: `flatten` returns flat list of integers

**User Story 2b - Extract Pattern as Tuple**:
- Atomic pattern: `toTuple` returns tuple with value and empty list
- Pattern with elements: `toTuple` returns tuple with value and list of element patterns
- Nested pattern: `toTuple` returns tuple where elements list contains nested Pattern structures
- Integer pattern: `toTuple` returns tuple with integer value and list of Pattern Int

**User Story 3 - Right-Associative Folding**:
- Pattern with values: `foldr` processes in correct order
- String pattern: `foldr` builds list in correct order
- Nested pattern: `foldr` processes all levels correctly

**User Story 4 - Left-Associative Folding**:
- Pattern with values: `foldl` processes in left-to-right order
- Integer pattern: `foldl` computes running total correctly
- Nested pattern: `foldl` processes all levels in left-to-right order

**User Story 5 - Monoid-Based Aggregation**:
- Integer pattern with Sum: `foldMap Sum` produces correct sum
- String pattern: `foldMap` with list monoid concatenates correctly
- Boolean pattern with All: `foldMap All` produces logical AND
- Nested pattern: `foldMap` processes all levels correctly

**Edge Cases**:
- Atomic pattern (no elements): All operations work correctly
- Empty elements list: All operations work correctly
- Singular pattern (one element): All operations work correctly
- Many elements: All operations work correctly
- Deep nesting (3+ levels): All operations work correctly
- Various value types: All operations work with String, Int, custom types

### Property-Based Tests (Properties.hs)

**Using `quickProperty` helper (20 test cases max)**:

1. **toList extracts all values**: `toList p` returns flat list with all values from pattern structure
2. **flatten extracts all values**: `length (flatten p) == countValues p` (where `countValues` manually counts all values)
3. **foldr processes all values**: `foldr (+) 0 p == sum (flatten p)` (for numeric patterns)
4. **foldl processes all values**: `foldl (+) 0 p == sum (flatten p)` (for commutative operations)
5. **foldMap with Sum monoid**: `getSum (foldMap Sum p) == sum (flatten p)` (for numeric patterns)
6. **Order preservation**: Both `toList p` and `flatten p` maintain consistent order across multiple calls
7. **Relationship**: `toList p = flatten p` (both extract flat lists, standard Foldable behavior)

**Performance**: All property-based tests must complete in <10ms total using `quickProperty` helper.

## Examples Alignment

Ensure test examples align with patterns shown in `examples/examples.md`:

- Use `pattern`, `patternWith`, `fromList` constructor functions
- Follow consistent terminology (atomic patterns, elements, values)
- Demonstrate real-world usage patterns (summing, concatenating, extracting)
- Show nested pattern structures matching examples.md patterns

## Success Criteria Verification

- ✅ **SC-001**: Unit tests demonstrate folding over atomic, with elements, and nested patterns
- ✅ **SC-002**: Property-based tests verify `foldr` processes all values with 100% accuracy
- ✅ **SC-003**: Property-based tests verify `foldl` processes all values with 100% accuracy
- ✅ **SC-004**: Unit and property-based tests verify `foldMap` works correctly
- ✅ **SC-005**: Unit and property-based tests verify `toList` extracts all values correctly as a flat list (standard Foldable behavior)
- ✅ **SC-005a**: Unit and property-based tests verify `flatten` extracts all values correctly as flat lists
- ✅ **SC-005b**: Unit and property-based tests verify `toTuple` extracts patterns correctly as tuples preserving structure
- ✅ **SC-006**: Tests cover String, Int, and custom type values
- ✅ **SC-007**: Tests include nested patterns (3+ levels deep)
- ✅ **SC-008**: Tests verify order preservation
- ✅ **SC-009**: Property-based tests verify foldable laws and properties

## Next Steps

1. Run `/speckit.tasks` to generate dependency-ordered task list
2. Implement Foldable instance following the plan
3. Write tests (unit + property-based) ensuring they fail first
4. Implement instance to make tests pass
5. Verify all success criteria are met
6. Ensure test performance is acceptable (<10ms for property-based tests)

