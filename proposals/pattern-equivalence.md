# Proposal: Pattern Equivalence for pattern-hs

> **Pre-RFC stub** — This document contains motivating examples but lacks a full design.
> It should be developed into a complete RFC before implementation. The core problem
> (resolving ambiguity between gram path notation and pattern notation for the same
> `Pattern Subject`) is distinct from RFC-010 (Pattern Reconciliation), which handles
> identity deduplication after parsing.

## Summary

The gram notation is a flexible representation of composable data structures that are
pattern-oriented rather than object-oriented. 

Building up from simple graph-like expressions, there are obvious and reliable equivalent
expressions in pure "pattern notation" using square brackets.

This proposal explores formal conventions to resolve any ambiguity of interpretation from
graph-like notation, pattern notation, and programmatic construction.

## Motivation

### The Problem

`Pattern Subject` accepts an arbitrary number of pattern elements, of arbitrary complexity.
Atomic elements and pairs have clear equivalence with simple graph nodes and relationships,
which have special syntactic sugar in gram notation. The generic pattern notation precisely
reflects what is possible programmatically composoing `Pattern Subject`, so it is sufficient
to map from graph-like notation to equivalent pattern notation.

In the following examples, both styles of gram notation are used in a "forward" and then "reverse"
direction to illustrate the mental mapping required in either direction (which imitates
what happens in round-trip parsing/serialization).

Consider that all graph-like notation describes a path:

1. single node path
  - ✅ forward: `(a:Thing {name:"example"})` =~ `[a:Thing {name:"example"}]`
  - notice the pattern notation omits the optional `|` followed by pattern elements
  - ✅ reverse: `[a:Thing {name:"example"}]` =~ `(a:Thing {name:"example"})`
2. single relationship path
  - ✅ forward: `(a)-[r:TO]->(b)` =~ `[r:TO | a, b]`
  - that looks unsurprising
  - ✅ reverse: `[r:TO | a, b]` =~ `(a)-[r:TO]->(b)`
3. 2 relationship path, alternative 1
  - 🤔 forward: `(a)-[r1]->(b)-[r2]->(c)` =~ `[r1 | a, [r2 | b, c]]`
  - the nesting would be the result of direct parsing, because the grammar builds up a path as a chain of nested relationships. 
  - that's ok for parsing, but may be confusing for users
  - 🤔 reverse: `[r1 | a, [r2 | b, c]]` =~ `(a)-[r1]->(b)-[r2]->(c)`
4. 🤔 2 relationship path, alternative 2: `(a)-[r1]->(b)-[r2]->(c)` =~ `[ | [r1 | a, b], [r2 | b, c]]`
  - extending a relationships into a path with 3 nodes and 2 relationships suddenly induces an anonymous path. ok?
  - 🤔 reverse: `[ | [r1 | a, b], [r2 | b, c]]` =~ `(a)-[r1]->(b)-[r2]->(c)`
5. annotated node (annotation is a pattern with a single element)
  - 🤔 forward: `@desc("special node") (a:Person)` =~ `[{desc: "special node"} | (a:Person)]`
  - 🤔 reverse: `[{desc: "special node"} | (a:Person)]` =~ `@desc("special node") (a:Person)`
6. annotated relationship
  - 🤔 forward: `@desc("special rel") (a)-[:TO]->(b)` =~ `[{desc: "special rel"} | [:TO | a, b]]`
  - 🤔 reverse: `[{desc: "special rel"} | [:TO | a, b]]` =~ `@desc("special rel") (a)-[:TO]->(b)`
7. annotated anonymous path with identified relationships
  - 🤔 forward: `@desc("special path") (a)-[r1]->(b)-[r2]->(c)` =~ `[{desc: "special path"} | [ | r1, r2]]`
  - 🤔 reverse: `[{desc: "special path"} | [ | r1, r2]]` =~ `@desc("special path") (a)-[r1]->(b)-[r2]->(c)`
8. identified path (reverse mapping from pattern notation to graph-like)
  - 🤔 reverse: `[r:KnownRoute {desc: "special path"} | r1, r2]]` =~ `@id(r) @labels([KnownRoute]) @desc("special path") (a)-[r1]->(b)-[r2]->(c)`
  - 🤔 forward: `@id(r) @labels([KnownRoute]) @desc("special path") (a)-[r1]->(b)-[r2]->(c)` =~ `[r:KnownRoute {desc: "special path"} | [ | r1, r2]]`
