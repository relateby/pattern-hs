# Gram Notation Reference: Special Cases and Syntactic Sugar

## Introduction

Gram notation provides syntactic sugar for common Pattern structures. These are **notation conveniences** that map to standard Pattern structures. Understanding these special cases helps you read and write gram notation more effectively.

**Important Note**: Some of these constructions may not yet have explicit construction functions in the Haskell Pattern library. You can construct equivalent patterns using the standard `pattern` function, even if gram notation provides syntactic sugar for them.

## Nodes (0 elements)

A Pattern with 0 elements (atomic pattern) can be written as a node using path-like notation:

**Gram notation (syntactic sugar):**
```gram
(n:Person)
```

**Equivalent Pattern notation:**
```gram
[n:Person]
```

Both represent an atomic pattern with value `n:Person` and empty elements (`elements == []`).

**Haskell equivalent:**
```haskell
import Pattern.Core (point)

node = point "n:Person"
-- Pattern { value = "n:Person", elements = [] }
```

**Current library support**: ✅ Fully supported via `point` function.

## Annotations (1 element)

A Pattern with 1 element can be written as an annotation:

**Gram notation (syntactic sugar):**
```gram
@k("v") [:Target]
```

**Equivalent Pattern notation:**
```gram
[{k:"v"} | [:Target]]
```

This represents a pattern with value `{k:"v"}` and one element `[:Target]`.

**Haskell equivalent:**
```haskell
import Pattern.Core (pattern, point)

target = point ":Target"
annotation = pattern "{k:\"v\"}" [target]
-- Pattern { value = "{k:\"v\"}", elements = [Pattern { value = ":Target", elements = [] }] }
```

**Current library support**: ⏳ Notation supported, but explicit annotation construction functions may not yet be available. Use `pattern` function to construct.

## Relationships (2 elements, both nodes)

A Pattern with 2 elements where both are atomic patterns can be written as a relationship using path-like notation:

**Gram notation (syntactic sugar):**
```gram
(a)-[r:KNOWS]->(b)
```

**Equivalent Pattern notation:**
```gram
[r:KNOWS | (a), (b)]
```

This represents a pattern with value `r:KNOWS` and two elements: `(a)` and `(b)`.

**Haskell equivalent:**
```haskell
import Pattern.Core (pattern, point)

nodeA = point "a"
nodeB = point "b"
relationship = pattern "r:KNOWS" [nodeA, nodeB]
-- Pattern { value = "r:KNOWS", elements = [Pattern { value = "a", elements = [] }, Pattern { value = "b", elements = [] }] }
```

**Current library support**: ⏳ Notation supported, but explicit relationship construction functions may not yet be available. Use `pattern` function to construct.

### Directed vs Undirected Relationships

In gram notation, relationships can be directed or undirected:

**Directed relationship:**
```gram
(a)-[r:KNOWS]->(b)    -- Direction: a to b
```

**Undirected relationship:**
```gram
(a)-[r:KNOWS]-(b)     -- No direction
```

Both map to Pattern structures, but the direction information (if any) is encoded in the relationship value or properties.

## References

Gram notation supports references to patterns defined elsewhere. This is a notation feature for reusability:

**Gram notation:**
```gram
:PersonRef              -- Reference to a pattern defined elsewhere
```

**Pattern notation:**
```gram
[:PersonRef]            -- Equivalent when resolved
```

References allow you to define patterns once and reuse them, reducing duplication in gram files.

**Current library support**: ⏳ Reference resolution may not yet be fully supported in the library. References are parsed but may require explicit resolution.

## Summary: Current Library Support

The Haskell Pattern library currently supports:

- ✅ **Creating atomic patterns** with `point` (maps to nodes)
- ✅ **Creating patterns with elements** using `pattern` (maps to annotations, relationships, and general patterns)
- ✅ **All standard Pattern operations** (Functor, Foldable, Traversable, Comonad, etc.)

Future library support may include:

- ⏳ Explicit node construction functions (convenience wrappers around `point`)
- ⏳ Explicit annotation construction functions
- ⏳ Explicit relationship construction functions (with direction support)
- ⏳ Reference resolution utilities

For now, you can construct all these patterns using the standard `pattern` function, even if gram notation provides syntactic sugar for them.

## Understanding the Mapping

The key insight is that gram notation's syntactic sugar is purely a **notation convenience**. Under the hood, all gram notation constructs map to standard Pattern structures:

- **Nodes** → Atomic patterns (`Pattern v { elements = [] }`)
- **Annotations** → Patterns with 1 element (`Pattern v { elements = [elem] }`)
- **Relationships** → Patterns with 2 elements (`Pattern v { elements = [elem1, elem2] }`)
- **References** → Patterns that resolve to other patterns

This means you can always fall back to standard Pattern construction functions (`point`, `pattern`) to create any structure that gram notation can express.

## Next Steps

- Learn more about [Pattern Construction](03-construction.md) to understand how to build these structures programmatically
- Explore [Basic Operations](04-basic-operations.md) to see how to work with patterns regardless of how they were constructed
- Check the [Gram library documentation](../../../dist-newstyle/doc/html/gram/index.html) for parsing and serialization details

