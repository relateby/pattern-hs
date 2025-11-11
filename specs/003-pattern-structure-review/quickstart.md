# Quickstart: Understanding the Pattern Structure

**Feature**: 003-pattern-structure-review  
**Date**: 2025-01-27

This guide helps you understand the Pattern structure with consistent definitions and terminology.

---

## Core Concept

A **Pattern** is a sequence of elements with an associated value. While implemented as a recursive tree, the primary semantic is sequence-based.

**Key Points**:
- Patterns are conceptually **sequences** (primary)
- Patterns are implemented as **trees** (implementation detail)
- Each pattern has a **value** (metadata about the sequence)
- Each pattern has **elements** (the sequence itself)

---

## Basic Pattern Structure

### Pattern Type

```haskell
data Pattern v = Pattern 
  { value    :: v              -- Value (decoration) about the sequence
  , elements :: [Pattern v]    -- The sequence of elements
  }
```

### Creating Patterns

**Atomic Pattern** (sequence with no elements):

```haskell
-- Create an atomic pattern
nodeA :: Pattern String
nodeA = Pattern { value = "A", elements = [] }

-- Access the value
value nodeA  -- Returns: "A"

-- Access the elements (empty for leaf)
elements nodeA  -- Returns: []
```

**Pattern with Elements** (sequence with elements):

```haskell
-- Create elements
nodeB = Pattern { value = "B", elements = [] }
nodeC = Pattern { value = "C", elements = [] }

-- Create pattern with elements
parent :: Pattern String
parent = Pattern 
  { value = "parent"
  , elements = [nodeB, nodeC]
  }

-- Access elements
elements parent  -- Returns: [nodeB, nodeC]
length (elements parent)  -- Returns: 2
```

---

## Terminology Reference

### Standard Terms

| Term | Definition | Usage |
|------|------------|-------|
| **Pattern** | Sequence of elements with an associated value | The data type |
| **value** | Value (decoration) about the sequence | Field name: `value` |
| **elements** | The sequence of pattern elements | Field name: `elements` |
| **sequence** | Conceptual model (primary) | How to think about patterns |
| **tree** | Implementation model | How patterns are stored |

### Terms to Use

✅ **Use these terms**:
- "value" (not "metadata")
- "elements" (not "children" or "child patterns")
- "sequence" as primary model
- "tree" as implementation detail

❌ **Avoid these terms**:
- "metadata" → use "value"
- "children" → use "elements"
- "child patterns" → use "elements"
- "tree" as primary model → use "sequence"

---

## Pattern Variants

Pattern variants are structural classifications based on element structure.

### Atomic Pattern

A pattern with no elements (`elements == []`). Atomic patterns are the fundamental building blocks from which all other patterns are constructed.

```haskell
leaf :: Pattern String
leaf = Pattern { value = "leaf", elements = [] }
```

**Status**: ✅ Implemented (this is the basic structure)

### Singular Pattern

A pattern with exactly one element (`length (elements p) == 1`). Singular patterns contain a single element in their sequence.

```haskell
singular :: Pattern String
singular = Pattern 
  { value = "parent"
  , elements = [ Pattern { value = "child", elements = [] } ]
  }
```

**Status**: ✅ Implemented (this is the basic structure)

### Node

A pattern interpreted as a node (typically an atomic pattern).

**Status**: ⏳ Planned (classification function `isNode` not yet implemented)

### Relationship

A pattern with exactly 2 elements, both of which are nodes.

```haskell
nodeA = Pattern { value = "A", elements = [] }
nodeB = Pattern { value = "B", elements = [] }
relationship = Pattern { value = "knows", elements = [nodeA, nodeB] }
```

**Status**: ⏳ Planned (classification function `isRelationship` not yet implemented)

### Subgraph

A pattern where all elements are graph elements.

**Status**: ⏳ Planned (classification function `isSubgraph` not yet implemented)

### Path

A subgraph with chained relationships.

**Status**: ⏳ Planned (classification function `isPath` not yet implemented)

---

## Implementation Status

### Available Now

✅ **Pattern data type**: Create and inspect patterns
✅ **Eq instance**: Compare patterns for equality
✅ **Show instance**: Display patterns

### Planned (Not Yet Available)

⏳ **Typeclass instances**: Functor, Foldable, Traversable
⏳ **Classification functions**: isNode, isRelationship, isSubgraph, isPath
⏳ **Navigation functions**: source, target, nodes, relationships
⏳ **Graph views**: GraphView typeclass, DirectedView, UndirectedView
⏳ **Construction functions**: pattern, patternWith

**See**: [implementation-status.md](./contracts/implementation-status.md) for complete status

---

## Common Patterns

### Creating a Simple Graph

```haskell
-- Nodes (atomic patterns)
alice = Pattern { value = "Alice", elements = [] }
bob = Pattern { value = "Bob", elements = [] }

-- Relationship
knows = Pattern { value = "knows", elements = [alice, bob] }

-- Graph (subgraph)
graph = Pattern 
  { value = "myGraph"
  , elements = [alice, bob, knows]
  }
```

### Nested Patterns

```haskell
-- Deep nesting
level3 = Pattern { value = "level3", elements = [] }
level2 = Pattern { value = "level2", elements = [level3] }
level1 = Pattern { value = "level1", elements = [level2] }
root = Pattern { value = "root", elements = [level1] }
```

### Different Value Types

```haskell
-- String values
strPattern :: Pattern String
strPattern = Pattern { value = "text", elements = [] }

-- Integer values
intPattern :: Pattern Int
intPattern = Pattern { value = 42, elements = [] }

-- Custom types
data Person = Person { name :: String, age :: Int }
personPattern :: Pattern Person
personPattern = Pattern 
  { value = Person { name = "Alice", age = 30 }
  , elements = []
  }
```

---

## Understanding Sequence vs Tree

### Conceptual Model: Sequence

Think of patterns as **sequences**:

- Each pattern represents a sequence
- The `value` is decoration about the sequence
- The `elements` are the items in the sequence
- Elements maintain their order

**Example**: The pattern "3 1 4 1 5 9" is conceptually a sequence of 6 elements.

### Implementation Model: Tree

Patterns are **implemented** as trees:

- Tree structure supports sequence semantics
- Each level stores a value and contains pattern elements
- Recursive structure enables nesting

**Key Point**: The tree is how sequences are stored. The sequence is what you should think about.

---

## Documentation References

- **Authoritative Definition**: [data-model.md](./data-model.md)
- **Terminology Standards**: [contracts/terminology-standards.md](./contracts/terminology-standards.md)
- **Implementation Status**: [contracts/implementation-status.md](./contracts/implementation-status.md)
- **Research Findings**: [research.md](./research.md)

---

## Next Steps

1. **Read the authoritative data model**: [data-model.md](./data-model.md)
2. **Check implementation status**: [contracts/implementation-status.md](./contracts/implementation-status.md)
3. **Review terminology standards**: [contracts/terminology-standards.md](./contracts/terminology-standards.md)
4. **Explore the code**: `src/Pattern/Core.hs`

---

## Troubleshooting

### "I can't find function X"

Check [implementation-status.md](./contracts/implementation-status.md). Many functions are planned but not yet implemented.

### "What's the difference between value and elements?"

- **value**: Value (decoration) about the pattern sequence
- **elements**: The sequence of pattern elements themselves

### "Should I think of patterns as sequences or trees?"

Think of them as **sequences** (conceptual model). The tree structure is the implementation detail.

### "What terminology should I use?"

Use "value" and "elements" consistently. See [terminology-standards.md](./contracts/terminology-standards.md) for details.

