# Gram Pattern Semantics

**Status**: ✅ Implemented  
**Implementation**: Gram serialization and parsing  
**Reference**: See `docs/reference/semantics/gram-semantics.md` for current specification

## Core Concepts

Gram notation uses **patterns** as its fundamental building blocks. Patterns are containers that can hold elements (other patterns), labels, and properties.

### Pattern Types

1. **Anonymous Patterns**: Patterns without identifiers
   - Each instance is unique
   - Cannot be referenced
   - Must always be defined inline

2. **Identified Patterns**: Patterns with identifiers
   - Must have exactly one definition
   - Can be referenced multiple times
   - Immutable once defined

## Fundamental Rules

### The Definition Rule
**Brackets create definitions, bare identifiers create references**

- `[...]` always defines a pattern (anonymous or identified)
- A bare identifier always references an existing pattern
- Each identified pattern can only be defined once

### Examples

```
[a]                 // Defines pattern 'a' (empty)
[a {k:"v"}]        // Defines pattern 'a' with properties
[a:Label]          // Defines pattern 'a' with a label

[b | a]            // Defines 'b', references 'a'
[b | [a]]          // Defines both 'b' and 'a'
[b | [a], a]       // Defines 'b' and 'a', includes 'a' twice

[x | [], [], []]   // Defines 'x' with three unique anonymous elements
```

## Semantic Constraints

### Single Definition
An identified pattern can only be defined once within a file:

```
[a {k:"v"}]        // Defines 'a'
[b | a]            // OK: references 'a'
[a {k2:"v2"}]      // ERROR: DuplicateDefinition 'a'
[c | [a]]          // ERROR: DuplicateDefinition 'a'
```

### Immutability
Once defined, a pattern's structure, labels, and properties cannot be changed:

```
[a {k:"v"}]        // Initial definition
[a:Thing]          // ERROR: DuplicateDefinition 'a' (attempt to redefine)
[a | b]            // ERROR: DuplicateDefinition 'a'
```

### No Direct Self-Reference
A pattern cannot contain itself as a direct element:

```
[a | a]            // ERROR: SelfReference 'a'
[a | [b | a]]      // OK: 'a' referenced indirectly through 'b'
```

### Forward References
References to patterns defined later in the file are allowed:

```
[a | b]            // OK: 'b' defined below
[b {k:"v"}]        // Definition of 'b'
```

### Path Notation Consistency
When mixing path and pattern notation, definitions must be consistent.

- Path relationships imply a pattern with arity 2 (source, target).
- Pattern definitions must match this arity if referenced in a path as a relationship.

```
[r | a, b]         // Defines 'r' with arity 2
(a)-[r]->(b)       // OK: 'r' used as relationship (arity 2)

[k | a, b, c]      // Defines 'k' with arity 3
(a)-[k]->(b)       // ERROR: InconsistentDefinition 'k' (expected arity 2)
```

## Anonymous Patterns

Anonymous patterns are always unique definitions:

```
[x | []]           // One anonymous empty pattern
[y | [], []]       // Two different anonymous empty patterns
[z | [{k:"v"}]]    // Anonymous pattern with properties
```

Each `[]` creates a distinct pattern instance, even if structurally identical.

## Common Patterns

### Define and Reference
```
[person {name:"Alice"}]           // Define
[group | person, person]          // Reference twice
```

### Nested Definitions
```
[doc | [header {title:"Hello"}],  // Define 'header' inline
       [body | header]]            // Define 'body', reference 'header'
```

### Pure Structure
```
[tree | [leaf], [branch | tree]]  // Recursive structure via reference
```

## Error Cases

```
// Multiple definitions
[a]
[a {k:"v"}]        // ERROR: DuplicateDefinition 'a'

// Undefined reference
[b | c]            // ERROR: UndefinedReference 'c'

// Redefinition in elements
[x | [a], [a]]     // ERROR: DuplicateDefinition 'a'

// Inconsistent usage
[r | a, b, c]
(a)-[r]->(b)       // ERROR: InconsistentDefinition 'r'
```

## Best Practices

1. Define patterns before or at their first use for readability
2. Use anonymous patterns for one-off structures
3. Use identified patterns for reusable components
4. Keep definitions minimal - include only essential properties
5. Use references to express relationships and avoid duplication
