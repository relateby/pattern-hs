# Proposal: GraphValue Instances for Common Types and Subject Conversion

**Status**: 📝 Design Only  
**Date**: 2026-02-20  
**Relates to**: Feature 34 (GraphClassifier), Feature 33 (PatternGraph), gram serialization  
**Peer projects**: pattern-lisp (s-expression storage, canonical conversion to/from Subject and Gram)

---

## Summary

Add a dedicated module that provides `GraphValue` instances for common, simple value types—starting with `String`—so that users can build and manipulate graphs without defining a custom value type. Provide a thin conversion layer from these simple patterns to `Pattern Subject`, enabling serialization to Gram and interoperability with the existing Subject-based pipeline. This gives pattern-hs a clear story for "simple graphs" and a canonical bridge for projects (e.g. pattern-lisp) that need conversion to/from Subject and Gram.

---

## Motivation

### Simple graphs without a custom value type

Today, building a `PatternGraph` or using `GraphClassifier` / `GraphLens` requires a value type `v` with a `GraphValue` instance. The only instance in the library is for `Subject`. Users who want minimal structure—e.g. string-labeled nodes and relationships—must either use `Subject` (and its `Symbol`, labels, properties) or define their own type and instance. For many use cases (prototypes, simple scripts, s-expression-style data), a string-based identity is enough. A `GraphValue String` instance with `Id String = String` and `identify = id` would allow:

- Building graphs with `Pattern String` and `fromPatterns canonicalClassifier` (or a string-capable classifier).
- Using `toGraphLens`, `neighbors`, `findPath`, etc., without pulling in Subject.
- Simplifying tests and examples that only need structural classification.

### Canonical conversion to Subject and serialization to Gram

Once a pattern is expressed over a simple type like `String`, converting it to `Pattern Subject` in a single, canonical way makes it possible to:

- Serialize to Gram (using the existing Gram.Serialize pipeline that expects `Pattern Subject`).
- Round-trip: String-based graph → Subject → Gram → parse → Subject → (optionally) back to String for identity-only views.

Conversion is trivial in one direction: each string identity maps to `Subject (Symbol s) Set.empty Map.empty`. The other direction (Subject → String) is lossy for labels and properties but well-defined for identity (symbol → string). Documenting this as the canonical conversion gives pattern-lisp and other consumers one place to depend on.

### pattern-lisp and general s-expression storage

The peer project **pattern-lisp** uses Pattern for general s-expression storage. It has a strong requirement for:

- Canonical conversion to and from `Subject`.
- Serialization to Gram.

Implementing that conversion and any shared `GraphValue` instances in pattern-hs (rather than only in pattern-lisp) keeps one canonical definition, avoids drift, and lets pattern-lisp depend on pattern-hs for the bridge. A subset of that functionality—trivial `GraphValue` instances plus conversion to/from Subject—fits naturally in this project; serialization to Gram already lives in the gram package and can be used once patterns are converted to Subject.

### Testing and examples

Specs and examples that only need to exercise classification or graph operations today use `Pattern Subject` and helpers (`node (Symbol "a")`, etc.). A `GraphValue String` instance would allow simpler test data (`point "a"`, `pattern "r" [n1, n2]`) where Subject is not semantically required, reducing boilerplate and keeping tests focused on the behavior under test.

---

## Design

### 1. Module for trivial GraphValue instances

Introduce a module (e.g. `Pattern.Graph.ValueInstances` or `Pattern.GraphValue.Instances`) that:

- Exports `GraphValue` instances for common types that have an obvious identity.
- Stays dependency-light: only the pattern library (and `base`); no Subject or Gram.

**First instance: String**

```haskell
instance GraphValue String where
  type Id String = String
  identify = id
```

`Ord String` is already in base, so the `Ord (Id v)` superclass is satisfied. This allows `Pattern String` to be used with `canonicalClassifier`, `fromPatterns`, `toGraphLens`, and all graph operations that only require `GraphValue v`.

**Optional future instances**

- `Int`, or other scalar types, if they prove useful for identity.
- A `newtype` over `Text` for those who prefer `Text` in APIs; identity can remain string-like.

The proposal does not commit to a fixed set; the module is the designated place for "trivial GraphValue for common data."

### 2. Conversion layer: simple patterns to/from Subject

Provide a small conversion API so that patterns over simple types (at least `String`) can be turned into `Pattern Subject` for serialization and vice versa.

**String → Subject (canonical)**

- Map each value (e.g. string) to a Subject with that identity and no labels/properties:  
  `Subject (Symbol s) Set.empty Map.empty`.
- Define a total function, e.g. `toSubjectPattern :: Pattern String -> Pattern Subject`, that traverses the pattern and applies this mapping at every node. This is the canonical way to "promote" a string-based graph to Subject for Gram or other Subject-based pipelines.

**Subject → String (identity-only)**

- Extract identity from each Subject (e.g. symbol to string).
- Define e.g. `fromSubjectPattern :: Pattern Subject -> Pattern String` that preserves structure but keeps only identity. Document that labels and properties are dropped. Useful for round-trips that care about structure and identity, or for feeding Subject-based data into code that expects `Pattern String`.

**Placement**

- Conversion can live in the same module as the instances, or in a sibling module (e.g. `Pattern.GraphValue.Subject` or `Pattern.Convert`) that depends on pattern and subject. If Gram serialization is mentioned in the API or docs, that layer can depend on gram and re-export or wrap the existing serialize step; the core conversion need not depend on gram.

### 3. Relationship to Gram serialization

- Serialization to Gram already operates on `Pattern Subject`. The new conversion layer produces `Pattern Subject` from `Pattern String`, so the pipeline becomes:  
  `Pattern String` → `toSubjectPattern` → `Pattern Subject` → existing Gram.Serialize.
- No change to the Gram package is required for this proposal; it only needs a canonical way to get from simple patterns to Subject.

### 4. Relationship to pattern-lisp

- pattern-lisp can depend on pattern-hs and use the new module(s) for:
  - `GraphValue String` (or other instances) if it stores s-expressions as patterns over strings.
  - Canonical conversion to/from Subject and, via existing gram package, serialization to Gram.
- This proposal does not define the full pattern-lisp feature set; it defines the subset that belongs in pattern-hs as shared infrastructure.

---

## What changes in pattern-hs

| Component | Action | Notes |
|---|---|---|
| `Pattern.Graph.ValueInstances` (or similar) | **New** | `GraphValue String` and any other trivial instances; minimal deps |
| Conversion API (`toSubjectPattern` / `fromSubjectPattern`) | **New** | String ↔ Subject; lives in same or sibling module; depends on subject package |
| Tests / examples | **Optional** | May switch to `Pattern String` where Subject is not required |
| Documentation | **Update** | Describe "simple graphs" path and conversion to Subject/Gram |

---

## Open questions

1. **Module naming**: `Pattern.Graph.ValueInstances` (next to GraphClassifier) vs. `Pattern.GraphValue.Instances` (sibling namespace). The former keeps graph-related code together; the latter gives a dedicated namespace for value-type concerns.

2. **Conversion module boundary**: Same module as instances vs. a separate module (e.g. `Pattern.GraphValue.Subject`) that imports instances and adds conversion. Separate keeps conversion (and Subject dependency) optional for code that only wants instances.

3. **Scope of "common" types**: Limit to String for the first cut, or include Int (or others) in the initial implementation based on early use cases.

---

## Summary of decisions

- **Dedicated module for trivial GraphValue instances**: One place for "common data" instances (String first), keeping core classifier/graph code unchanged.
- **String is the primary use case**: Enables simple graphs and s-expression-style usage; trivially convertible to Subject for serialization.
- **Canonical String ↔ Subject conversion**: One defined way to promote string patterns to Subject and to strip Subject patterns down to identity-only strings; supports Gram serialization and pattern-lisp interoperability.
- **pattern-lisp**: Subset of its requirements (instances + conversion) lives in pattern-hs as shared infrastructure; pattern-lisp consumes it rather than reimplementing.
- **Gram**: No change to Gram; conversion produces `Pattern Subject`, which existing serialization already handles.
