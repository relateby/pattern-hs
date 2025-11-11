# Terminology Standards: Pattern Library

**Feature**: 003-pattern-structure-review  
**Date**: 2025-01-27  
**Status**: Authoritative Reference

This document establishes the standard terminology for the Pattern library. All documentation and code comments should use these terms consistently.

---

## Core Terms

### Pattern

**Primary Term**: **Pattern**

**Definition**: A sequence of elements with associated metadata. While implemented using a recursive tree structure, the primary semantic is sequence-based.

**Usage**: Always use "Pattern" (capitalized) when referring to the data type. Use "pattern" (lowercase) when referring to an instance.

**Examples**:
- ✅ "A Pattern is a sequence of elements"
- ✅ "Create a pattern with value 'test'"
- ❌ "A pattern type" (use "Pattern type" or "Pattern data type")

---

### Value Field

**Primary Term**: **value**

**Definition**: The value associated with a pattern instance. The value field stores data of any type and is associated with the pattern sequence itself, not with individual elements in the sequence.

**Field Name**: `value :: v`

**Usage**: Always use "value" (not "metadata", "data", or other terms).

**Examples**:
- ✅ "The value field stores the pattern's metadata"
- ✅ "Access the pattern's value using `value pattern`"
- ❌ "The metadata field" (use "value field")
- ❌ "The pattern's data" (use "pattern's value")

**Rationale**: Matches the actual code field name, ensuring consistency between code and documentation.

---

### Elements Field

**Primary Term**: **elements**

**Definition**: The elements contained within a pattern, forming the sequence structure. Each element in the sequence is itself a Pattern.

**Field Name**: `elements :: [Pattern v]`

**Usage**: Always use "elements" (not "children", "child patterns", "sequence elements", or other terms).

**Examples**:
- ✅ "The elements field contains the sequence of patterns"
- ✅ "Access pattern elements using `elements pattern`"
- ✅ "A pattern with three elements"
- ❌ "The children field" (use "elements field")
- ❌ "Child patterns" (use "elements" or "pattern elements")
- ❌ "Sequence elements" (redundant, just use "elements")

**Rationale**: Matches the actual code field name and aligns with the sequence-based conceptual model.

---

## Conceptual Model Terms

### Sequence

**Primary Term**: **sequence**

**Definition**: The conceptual model for patterns. Patterns are conceptually sequences of elements.

**Usage**: Use "sequence" when describing the conceptual model or semantic meaning.

**Examples**:
- ✅ "Patterns are conceptually sequences"
- ✅ "The sequence-based semantic model"
- ✅ "Each pattern represents a sequence of elements"

**Related Terms**: 
- "sequence elements" → just use "elements"
- "sequence structure" → use "sequence" or "sequence of elements"

---

### Tree

**Secondary Term**: **tree** (or "recursive tree structure")

**Definition**: The implementation model for patterns. Patterns are implemented as recursive trees.

**Usage**: Use "tree" when describing implementation details, not as the primary conceptual model.

**Examples**:
- ✅ "Patterns are implemented as recursive trees"
- ✅ "The tree structure supports sequence semantics"
- ✅ "Tree-based implementation"
- ❌ "Patterns are trees" (use "Patterns are sequences, implemented as trees")

**Rationale**: Tree is the implementation detail; sequence is the conceptual model.

---

## Pattern Variant Terms

### Atomic Pattern

**Primary Term**: **atomic pattern**

**Definition**: A pattern with no elements (`elements == []`). Atomic patterns are the fundamental building blocks from which all other patterns are constructed.

**Usage**: Use "atomic pattern" to describe the structure.

**Examples**:
- ✅ "An atomic pattern has no elements"
- ✅ "Create an atomic pattern with `Pattern { value = "x", elements = [] }`"
- ✅ "Atomic patterns are the fundamental building blocks for constructing composite patterns"

---

### Singular Pattern

**Primary Term**: **singular pattern**

**Definition**: A pattern with exactly one element (`length (elements p) == 1`). Singular patterns contain a single element in their sequence.

**Usage**: Use "singular pattern" to describe the structure.

**Examples**:
- ✅ "A singular pattern has exactly one element"
- ✅ "Create a singular pattern with `Pattern { value = "x", elements = [elem] }`"
- ✅ "Singular patterns contain one element in their sequence"

---

### Node

**Primary Term**: **node**

**Definition**: A pattern interpreted as a node (typically an atomic pattern).

**Usage**: Use "node" when describing graph interpretation.

**Examples**:
- ✅ "A node is a pattern with no elements"
- ✅ "Atomic patterns are interpreted as nodes"

**Note**: "Node" is a graph interpretation term. The structural term is "atomic pattern".

---

### Relationship

**Primary Term**: **relationship**

**Definition**: A pattern with exactly 2 elements, both of which are nodes.

**Usage**: Use "relationship" when describing graph interpretation.

**Examples**:
- ✅ "A relationship pattern has exactly two elements"
- ✅ "Both elements of a relationship must be nodes"

---

### Subgraph

**Primary Term**: **subgraph**

**Definition**: A pattern where all elements are graph elements (nodes, relationships, or other subgraphs).

**Usage**: Use "subgraph" when describing graph interpretation.

**Examples**:
- ✅ "A subgraph contains graph elements as its elements"
- ✅ "Patterns can represent subgraphs"

---

### Path

**Primary Term**: **path**

**Definition**: A subgraph where relationships chain correctly (target of one equals source of next).

**Usage**: Use "path" when describing graph interpretation.

**Examples**:
- ✅ "A path is a subgraph with chained relationships"

---

## Implementation Status Terms

### Status Markers

Use these markers consistently when documenting implementation status:

- ✅ **Implemented**: Feature exists in code
- ⏳ **Planned**: Feature is documented but not yet implemented
- ❌ **Not Planned**: Feature is not currently planned

**Examples**:
- ✅ "The Pattern type is implemented"
- ⏳ "Functor instance is planned (TODO.md Feature 4)"
- ⏳ "Classification functions are planned but not yet implemented"

---

## Terms to Avoid

### Deprecated Terms

These terms should not be used in new documentation:

- ❌ **"metadata"** → Use "value"
- ❌ **"children"** → Use "elements"
- ❌ **"child patterns"** → Use "elements" or "pattern elements"
- ❌ **"sequence elements"** → Use "elements" (redundant)
- ❌ **"tree" as primary model** → Use "sequence" as primary, "tree" as implementation detail

### Context-Specific Exceptions

In some contexts, alternative terms may be acceptable if clearly explained:

- "Graph node" vs "atomic pattern": Both acceptable, but clarify that "node" is the graph interpretation
- "Tree node" vs "pattern": Use "pattern" to avoid confusion with graph nodes

---

## Documentation Style Guide

### Capitalization

- **Pattern** (capitalized): The data type name
- **pattern** (lowercase): An instance of Pattern
- **value**, **elements** (lowercase): Field names

### Code References

When referring to code:
- Use backticks for field names: `` `value` ``, `` `elements` ``
- Use backticks for type names: `` `Pattern v` ``
- Use backticks for function names: `` `isNode` ``

### Examples

**Good**:
- "The `value` field stores the pattern's metadata"
- "Access pattern `elements` using the `elements` accessor"
- "A Pattern is a sequence of elements"

**Bad**:
- "The metadata field" (use "value field")
- "The children of a pattern" (use "elements of a pattern")
- "Patterns are trees" (use "Patterns are sequences, implemented as trees")

---

## Checklist for Documentation Updates

When updating documentation, verify:

- [ ] Uses "value" (not "metadata")
- [ ] Uses "elements" (not "children" or "child patterns")
- [ ] Describes patterns as sequences (primary), trees (implementation)
- [ ] Capitalizes "Pattern" when referring to the type
- [ ] Uses status markers (✅/⏳/❌) for implementation status
- [ ] Uses backticks for code references

---

## References

- **Authoritative Data Model**: [data-model.md](../data-model.md)
- **Implementation**: `src/Pattern/Core.hs`
- **Research Findings**: [research.md](../research.md)

