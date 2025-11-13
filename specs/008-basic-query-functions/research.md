# Research: Basic Query Functions

**Feature**: 008-basic-query-functions  
**Date**: 2025-01-27  
**Status**: Complete

## Research Tasks

### Task 1: Depth Counting Convention

**Question**: How should depth be counted for atomic patterns and nested structures?

**Research**: Standard tree depth conventions in computer science and functional programming.

**Decision**: Depth 0 for atomic patterns (root only, no nesting levels below root). Depth 1 for one level of nesting, depth 2 for two levels, etc.

**Rationale**: 
- Follows standard tree depth conventions where root is at depth 0
- Consistent with mathematical tree terminology
- Makes depth counting intuitive: depth = number of nesting levels below root
- Atomic pattern has no nesting, so depth 0 is natural

**Alternatives Considered**:
- Depth 1 for atomic patterns: Rejected because it's less intuitive and doesn't match standard tree conventions
- Depth counting from leaves: Rejected because it's less useful for understanding nesting structure

**References**: Standard tree data structure conventions, Haskell tree libraries (Data.Tree)

---

### Task 2: Values Function Relationship to Foldable

**Question**: Should `values` function duplicate `toList` from Foldable, or provide different behavior?

**Research**: Existing Foldable instance for Pattern provides `toList` which extracts all values. Reviewing whether `values` should be identical or have different semantics.

**Decision**: `values` function should be equivalent to `toList` from Foldable, but provided explicitly for clarity and intentional value extraction.

**Rationale**:
- `toList` already exists and works correctly (verified in Foldable instance tests)
- Having explicit `values` function makes value extraction intentional and clear
- Follows pattern of providing explicit functions alongside typeclass methods (similar to `flatten` which is equivalent to `toList`)
- Documentation can clarify the relationship: `values = toList`

**Alternatives Considered**:
- Different ordering: Rejected because consistency with Foldable is important
- Different semantics: Rejected because there's no clear need for different behavior
- Remove `values` and use `toList` only: Rejected because explicit functions improve clarity

**References**: Pattern.Core module (Foldable instance), existing `flatten` function

---

### Task 3: Performance Considerations for Recursive Functions

**Question**: How should query functions handle very deeply nested patterns to avoid stack overflow?

**Research**: Haskell recursion patterns, tail recursion, and handling deep structures.

**Decision**: Use standard recursive implementations. Haskell's lazy evaluation and GHC's stack handling should be sufficient for typical use cases. For very deep structures (>1000 levels), consider documenting limitations.

**Rationale**:
- Standard recursion is idiomatic Haskell and clear
- GHC handles deep recursion reasonably well with default stack sizes
- Performance targets specify patterns up to 1000 nodes, which should be manageable
- If stack overflow becomes an issue, can optimize later (not premature optimization)

**Alternatives Considered**:
- Tail recursion: Rejected because query functions need to traverse tree structure, making tail recursion difficult without accumulating state
- Iterative approach: Rejected because it's less idiomatic and adds complexity
- Explicit stack management: Rejected because it's premature optimization for typical use cases

**References**: Haskell performance best practices, GHC documentation on stack handling

---

### Task 4: Function Naming and Consistency

**Question**: Should query functions follow existing naming conventions in the codebase?

**Research**: Review existing Pattern.Core functions and naming patterns.

**Decision**: Use clear, standard names: `length`, `size`, `depth`, `values`. These names are intuitive and follow common conventions.

**Rationale**:
- `length` is standard for counting elements (matches Prelude.length semantics for lists)
- `size` is common for total node count in tree structures
- `depth` is standard for tree depth
- `values` is clear and explicit for value extraction
- All names are short, clear, and follow Haskell naming conventions

**Alternatives Considered**:
- `patternLength`, `patternSize`, etc.: Rejected because too verbose, and context is clear from module
- `elementCount`, `nodeCount`, etc.: Rejected because less standard than `length` and `size`
- `getValues`: Rejected because `values` is more idiomatic (noun form for pure functions)

**References**: Pattern.Core module naming conventions, Haskell naming best practices

---

## Summary

All research tasks completed. Key decisions:
1. Depth counting: 0 for atomic patterns (root only)
2. Values function: Equivalent to `toList`, provided explicitly for clarity
3. Performance: Standard recursion sufficient for target use cases
4. Naming: Use standard, clear names (`length`, `size`, `depth`, `values`)

No blocking issues identified. Ready to proceed to Phase 1 design.

