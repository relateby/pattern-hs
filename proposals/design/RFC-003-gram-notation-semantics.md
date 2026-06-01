# RFC-003: Gram Notation Semantics

**Status:** accepted
**Date:** 2025-11-01
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Supersedes:** [`proposals/design/SEMANTICS.md`](SEMANTICS.md), [`proposals/design/EXTENDED-SEMANTICS.md`](EXTENDED-SEMANTICS.md)
**Related modules:** `Gram.Core`, `Gram.Parser`, `Gram.Serializer`

## Summary

Gram notation is the textual serialization format for `Pattern Subject`. It supports two
complementary syntaxes — **pattern notation** using `[...]` brackets and **path notation**
using `(nodes)` and relationship arrows — that can be freely mixed in a single file. This
RFC specifies the semantic rules that govern how gram notation creates, references, and
constrains `Pattern Subject` values.

## Motivation

A serialization format for graphs must resolve several questions: when does a syntax
element define a new entity versus reference an existing one? What does it mean to use
the same identifier twice? How do path-style traversals and declarative bracket notation
interoperate? Gram notation answers these questions with a small, consistent rule set
that applies uniformly across both notations.

## Design

### Core Vocabulary

**Identifier** — a symbol that names a specific pattern instance. Identifiers must be
globally unique within a file: each identifier can be defined exactly once.

**Label** — a type marker for patterns (e.g. `:Person`, `:KNOWS`). Labels denote types
and can be used freely on multiple patterns.

**Anonymous element** — a pattern with no identifier. Each `[]` or `()` occurrence is
a distinct instance, even if structurally identical.

### Pattern Notation

Pattern notation uses brackets `[...]` for all pattern construction.

#### The Definition Rule

**Brackets create definitions; bare identifiers create references.**

```
[a]              -- defines pattern 'a' (empty, no elements)
[a {k:"v"}]      -- defines 'a' with property k="v"
[a:Label]        -- defines 'a' with label Label

[b | a]          -- defines 'b'; 'a' appears bare → references 'a'
[b | [a]]        -- defines both 'b' and 'a' (inline nested definition)
[b | [a], a]     -- defines 'b' and 'a' inline; then references 'a'
```

The separator `|` divides the subject (identity, labels, properties) from the elements
(the content sequence).

#### Single Definition Constraint

Each identified pattern can only be defined once within a file:

```
[a {k:"v"}]    -- defines 'a'
[b | a]        -- OK: references 'a'
[a {k2:"v2"}] -- ERROR: DuplicateDefinition 'a'
[c | [a]]      -- ERROR: DuplicateDefinition 'a'
```

#### Immutability

Once defined, a pattern's structure, labels, and properties cannot be changed:

```
[a {k:"v"}]    -- initial definition
[a:Thing]      -- ERROR: DuplicateDefinition 'a' (attempt to add label)
[a | b]        -- ERROR: DuplicateDefinition 'a' (attempt to add elements)
```

#### No Direct Self-Reference

A pattern cannot contain itself as a direct element:

```
[a | a]        -- ERROR: SelfReference 'a'
[a | [b | a]]  -- OK: 'a' referenced indirectly through 'b'
```

#### Forward References

References to patterns defined later in the file are allowed:

```
[a | b]        -- OK: 'b' defined below
[b {k:"v"}]    -- definition of 'b'
```

#### Anonymous Patterns

Each occurrence of `[]` (or `[{...}]`, `[:Label]`, etc.) is a distinct pattern instance:

```
[x | []]       -- one anonymous empty element
[y | [], []]   -- two different anonymous empty elements
[z | [{k:"v"}]] -- anonymous element with properties
```

Two anonymous patterns are always distinct even if structurally identical.

### Path Notation

Path notation uses Cypher-style `(node)` and `-[rel]->` syntax for declaring
graph-structured patterns as traversals.

#### Basic Syntax

```
(a)                    -- node 'a'
(a:Person)             -- node 'a' with label 'Person'
(a)-[r]->(b)           -- directed relationship 'r' from 'a' to 'b'
(a)<-[r]-(b)           -- directed relationship 'r' from 'b' to 'a'
(a)-[:knows]->(b)      -- anonymous relationship with label 'knows'
(a)-[k:knows]->(b)     -- identified relationship 'k' with label 'knows'
```

Relationships only exist between nodes; they cannot appear standalone.

#### First-Appearance Definition

In path notation, the first appearance of an identifier defines it:

```
(a)-[r1]->(b)          -- defines: a, r1, b
(b)-[r2]->(c)          -- defines: r2, c  (b already defined)
(a)-[r3]->(c)          -- defines: r3     (a and c already defined)
```

Subsequent appearances of the same identifier are references.

#### Direction Encoding

Path direction maps to element order in pattern notation. `element[0]` is the source,
`element[1]` is the target:

```
(a)-[r]->(b)           -- forward: translates to [r | a, b]
(a)<-[r]-(b)           -- reverse: translates to [r | b, a]

(a)-[r1]->(b)<-[r2]-(c)
-- translates to: [ | [r1 | a, b], [r2 | c, b]]
```

#### Cyclic Paths

Relationships can be referenced within cycles when the endpoints match:

```
(a)-[r1]->(b)-[r2]->(a)-[r1]->(b)
-- first r1: defines [r1 | a, b]
-- second r1: references same relationship (valid: same endpoints)
```

### Pattern-Path Integration

The two notations can be freely mixed. Paths translate to anonymous patterns containing
relationship patterns:

```
(a)-[r]->(b)                       -- translates to: [ | [r | a, b]]
(a)-[r1]->(b)-[r2]->(c)            -- translates to: [ | [r1 | a, b], [r2 | b, c]]
[p | (a)-[r1]->(b)-[r2]->(c)]      -- named path: [p | [r1 | a, b], [r2 | b, c]]
```

#### Patterns as Path Nodes

Any pattern (not just atoms) can serve as a node in path notation:

```
[team | alice, bob]
[project | frontend, backend]
(team)-[:works_on]->(project)      -- composite patterns relate to each other
```

This enables multi-level modeling where composites and atoms can freely interconnect.

#### Meta-Relationships

Identified relationships can be reused as nodes:

```
(alice)-[k:knows]->(bob)           -- defines relationship 'k'
(k)-[:type_of]->(social)           -- 'k' used as a node in another relationship
```

### Semantic Constraints

#### Cross-Notation Consistency

Definitions must be consistent regardless of which notation is used first:

```
[k | a, b]                         -- defines 'k' as pattern [k | a, b]
(a)-[k]->(c)                       -- ERROR: k already defined with [a,b], not [a,c]

(a)-[k]->(b)                       -- defines 'k' as [k | a, b]
[k | a, b]                         -- OK: same definition, consistent
[k | x, y]                         -- ERROR: different content
```

#### Path Notation Arity Constraint

When a pattern defined in pattern notation is used as a relationship in path notation,
its arity must be exactly 2:

```
[r | a, b]         -- defines 'r' with arity 2
(a)-[r]->(b)       -- OK: 'r' used as relationship (arity 2)

[k | a, b, c]      -- defines 'k' with arity 3
(a)-[k]->(b)       -- ERROR: InconsistentDefinition 'k' (expected arity 2)
```

#### Anonymous Relationship Uniqueness

Each anonymous relationship is a distinct instance:

```
(alice)-[:knows]->(bob)
(alice)-[:knows]->(bob)            -- two different relationships, not one
```

### Error Cases

| Error | Cause |
|-------|-------|
| `DuplicateDefinition 'a'` | Identifier `a` defined more than once |
| `UndefinedReference 'c'` | Bare identifier `c` never defined anywhere in file |
| `SelfReference 'a'` | `[a \| a]` — pattern directly contains itself |
| `InconsistentDefinition 'k'` | Same identifier defined with different content across notations |

### Examples

#### Building a Graph

```
(alice)-[:knows]->(bob)
(bob)-[:knows]->(charlie)
(alice)-[friendship:knows]->(charlie)
(friendship)-[:strength]->(strong)     -- meta-relationship
```

#### Hierarchical Structures

```
[eng_team | alice, bob]
[product_team | charlie, dana]
[company | eng_team, product_team]
(eng_team)-[:collaborates_with]->(product_team)
(alice)-[:leads]->(eng_team)
```

#### Named Paths

```
[review_cycle |
  (author)-[:writes]->(code)-[:reviewed_by]->(reviewer)
]
[process | review_cycle, deployment]
```

#### Pure Structure (no graph interpretation)

```
[tree | [leaf], [branch | [leaf], tree]]   -- recursive via reference
```

## Open Questions

None. Gram notation semantics are fully implemented in `Gram.Parser` and
`Gram.Serializer`. This RFC documents the implemented behavior.

## Alternatives

**Mutation semantics** — allowing identifiers to be redefined or properties to be
accumulated across multiple appearances was considered and rejected. Immutability
makes the format unambiguous: the meaning of any identifier is fixed at its definition
site, regardless of parse order. This also aligns with the `Subject`-level immutability
property in the data model.

**Requiring definitions before references** — enforcing lexical order was considered
but forward references were allowed because gram files are often generated and
rearranged, and requiring strict ordering would make generated output fragile.
