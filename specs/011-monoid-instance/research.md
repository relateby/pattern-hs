# Research: Monoid Instance for Pattern

**Feature**: 011-monoid-instance  
**Date**: 2025-01-27  
**Status**: Complete

## Research Objectives

1. Identify concrete use cases for identity patterns (`mempty`)
2. Design identity semantics that align with decorated sequence model
3. Evaluate whether Monoid provides distinct value beyond Semigroup alone
4. Verify that identity laws hold for the designed semantics
5. Determine best practices for Monoid instances extending Semigroup in Haskell

## Use Case Analysis

### Use Case 1: Pattern Accumulation with Empty Initial State

**Scenario**: Accumulating patterns from a collection starting with an empty/identity state.

**Example**: 
- Using `foldMap` or `foldr` to accumulate patterns from a list, starting with `mempty`
- Building patterns incrementally: `foldr (<>) mempty [p1, p2, p3]` produces combined pattern
- Using `mconcat` to combine a list of patterns: `mconcat [p1, p2, p3]` (handles empty list case)

**Value**: Enables standard Haskell accumulation patterns (folds, concat) to work naturally with Pattern types, including handling empty collections gracefully.

**Alignment with Decorated Sequence Model**: ✅ Identity pattern has empty elements (no pattern sequence) and identity value (no decoration), serving as the natural starting point for accumulation.

### Use Case 2: Optional Pattern Construction

**Scenario**: Building patterns conditionally, where some conditions may result in no pattern contribution.

**Example**:
- Conditional pattern building: `maybe mempty pattern optionalValue`
- Combining optional patterns: `maybePattern1 <> maybePattern2` where either could be `mempty`
- Filtering and combining: `mconcat (mapMaybe f xs)` where `f` may return `Nothing` (represented as `mempty`)

**Value**: Enables graceful handling of optional or conditional pattern contributions without special-casing empty states.

**Alignment with Decorated Sequence Model**: ✅ Identity pattern represents "no pattern" state, which naturally combines with any pattern to produce that pattern unchanged.

### Use Case 3: Pattern Initialization and Default Values

**Scenario**: Initializing pattern structures with default/empty state before populating with actual data.

**Example**:
- Initializing empty pattern: `let p = mempty in ...`
- Building patterns incrementally: `mempty <> p1 <> p2 <> p3`
- Using as default in pattern matching: `case maybePattern of Nothing -> mempty; Just p -> p`

**Value**: Provides a natural default/empty state for patterns, enabling clean initialization patterns and default value handling.

**Alignment with Decorated Sequence Model**: ✅ Identity pattern represents the minimal pattern state (no elements, identity decoration), serving as the natural default.

## Identity Semantics Design

### Decision: Identity Pattern Structure

**Chosen Approach**: `mempty` is a pattern with `mempty` value (from value type's Monoid) and empty elements list: `Pattern { value = mempty, elements = [] }`

**Rationale**: 
- Naturally extends Semigroup semantics: identity for `<>` operation
- Aligns with decorated sequence model: empty elements (no pattern sequence), identity value (no decoration)
- Satisfies identity laws: `mempty <> p = p` and `p <> mempty = p`
- Follows standard Haskell pattern for Monoid instances extending Semigroup

**Verification of Identity Laws**:

For left identity (`mempty <> p = p`):
- `value (mempty <> p) = value mempty <> value p = mempty <> value p = value p` (by Monoid identity for value type)
- `elements (mempty <> p) = elements mempty ++ elements p = [] ++ elements p = elements p` (by list concatenation)
- Therefore: `mempty <> p = Pattern { value = value p, elements = elements p } = p` ✅

For right identity (`p <> mempty = p`):
- `value (p <> mempty) = value p <> value mempty = value p <> mempty = value p` (by Monoid identity for value type)
- `elements (p <> mempty) = elements p ++ elements mempty = elements p ++ [] = elements p` (by list concatenation)
- Therefore: `p <> mempty = Pattern { value = value p, elements = elements p } = p` ✅

**Alternatives Considered**:
- Pattern with arbitrary value and empty elements: Rejected - would not satisfy identity laws (value wouldn't combine correctly)
- Pattern with `mempty` value and non-empty elements: Rejected - would not satisfy identity laws (elements wouldn't concatenate correctly)
- Custom identity function: Rejected - violates Monoid abstraction, requires additional parameter

### Decision: Consistency with Semigroup

**Chosen Approach**: Monoid instance uses the same `<>` implementation as Semigroup instance (inherited from Semigroup)

**Rationale**:
- Monoid extends Semigroup, so `<>` should be identical
- Consistency ensures `p1 <> p2` produces same result whether using Semigroup or Monoid instance
- Standard Haskell pattern: Monoid instance typically just adds `mempty`, inherits `<>` from Semigroup

**Verification**:
- Semigroup: `Pattern v1 els1 <> Pattern v2 els2 = Pattern (v1 <> v2) (els1 ++ els2)`
- Monoid: Same `<>` implementation, plus `mempty = Pattern { value = mempty, elements = [] }`
- Consistency: ✅ `p1 <> p2` produces identical result under both instances

### Decision: Type Constraint

**Chosen**: `Monoid v => Monoid (Pattern v)`

**Rationale**:
- Value type must have Monoid instance to provide `mempty` for identity pattern
- Standard pattern for parameterized types extending Semigroup to Monoid
- Type-safe: compiler enforces constraint
- Natural extension of `Semigroup v => Semigroup (Pattern v)` constraint

## Alignment with Decorated Sequence Model

### Elements Form the Pattern

**Verification**: ✅ Identity pattern has empty elements list `[]`, representing "no pattern sequence". When combining with identity, the pattern sequence remains unchanged (empty elements concatenate to produce original elements).

### Value is Decoration

**Verification**: ✅ Identity pattern has `mempty` value (from value type's Monoid), representing "no decoration" or "identity decoration". When combining with identity, the decoration remains unchanged (identity value combines to produce original value).

### Sequence Order Preservation

**Verification**: ✅ Identity pattern has empty elements, so concatenation with identity preserves order: `elements p ++ [] = elements p` and `[] ++ elements p = elements p`.

## Distinct Value Analysis

### Comparison with Semigroup Alone

**Semigroup (`<>`)**: Combines two patterns. Requires both patterns to exist. Cannot handle empty collections or optional patterns gracefully.

**Monoid (`mempty`, `mconcat`)**: Extends Semigroup with identity element. Enables:
- Handling empty collections: `mconcat [] = mempty`
- Optional pattern construction: `maybe mempty pattern optionalValue`
- Clean initialization: `mempty <> p1 <> p2`
- Standard accumulation patterns: `foldr (<>) mempty patterns`

**Conclusion**: Monoid provides distinct value beyond Semigroup by enabling identity-based operations, empty collection handling, and standard Monoid combinators.

## Haskell Best Practices

### Monoid Instance Pattern (Extending Semigroup)

**Standard Pattern**: For types with Semigroup instance, add `mempty` to create Monoid:

```haskell
instance Semigroup v => Semigroup (Pattern v) where
  Pattern v1 els1 <> Pattern v2 els2 = Pattern (v1 <> v2) (els1 ++ els2)

instance Monoid v => Monoid (Pattern v) where
  mempty = Pattern { value = mempty, elements = [] }
  -- <> inherited from Semigroup
```

**Rationale**: 
- Follows standard Haskell pattern for extending Semigroup to Monoid
- Clear and straightforward implementation
- Easy to verify identity laws
- Inherits `<>` from Semigroup automatically

### Type Constraint

**Chosen**: `Monoid v => Monoid (Pattern v)`

**Rationale**:
- Value type must have Monoid instance to provide `mempty` for identity pattern
- Standard pattern for parameterized types
- Type-safe: compiler enforces constraint
- Natural extension of Semigroup constraint

### Documentation Requirements

**Required**:
- Explain identity semantics clearly (what `mempty` represents)
- Provide examples showing identity patterns in use
- Document identity law satisfaction
- Explain consistency with Semigroup instance
- Show usage with standard Monoid combinators (`mconcat`)

## Implementation Strategy

### Phase 1: Evaluation and Design
- Document use cases (this research)
- Design identity semantics (this research)
- Verify identity laws hold (this research)
- Verify alignment with decorated sequence model (this research)

### Phase 2: Implementation
- Implement Monoid instance in `Pattern.Core` (extends Semigroup)
- Add comprehensive Haddock documentation
- Write unit tests for identity pattern structure and combination

### Phase 3: Verification
- Write property-based tests for identity laws
- Test consistency with Semigroup instance
- Test with different value types (String, Sum, Product, All, Any)
- Verify standard Monoid combinators work (`mconcat`)

## Conclusion

**Decision**: ✅ Proceed with Monoid instance implementation

**Rationale**:
1. Three concrete use cases identified (accumulation with empty state, optional patterns, initialization)
2. Identity semantics clearly defined: `mempty = Pattern { value = mempty, elements = [] }`
3. Identity laws verified to hold for all pattern structures
4. Provides distinct value beyond Semigroup (identity operations, empty collection handling)
5. Follows standard Haskell patterns and best practices
6. Naturally extends existing Semigroup instance
7. Aligns with decorated sequence model

**Next Steps**: Proceed to Phase 1 design (data-model.md, contracts, quickstart.md)

