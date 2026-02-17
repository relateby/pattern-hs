# Research: Gram annotation-based identifiers and labels

**Feature**: 032-gram-annotation-syntax  
**Date**: 2026-02-17  
**Status**: Complete

## Research Questions

1. How does the existing Haskell parser handle annotations, and what must change to support `@@` and `@` disambiguation?
2. What CST representation should represent property vs identified annotations so downstream tools can consume them without re-parsing?
3. How should the annotation stack (at most one `@@`, then zero or more `@`) be enforced in Megaparsec?
4. How to align with tree-sitter-gram corpus and avoid drift?

## Findings

### 1. Current parser behavior and required changes

**Decision**: Keep `parseAnnotation` for single-`@` property form; add a separate parser for `@@` (identified) that consumes identifier and/or labels (at least one). Use `try (string "@@")` before `char '@'` so `@@` is recognized first. Parse annotation list as: optional one identified_annotation, then zero or more property_annotation (with at least one annotation total).

**Rationale**:
- Current code: `parseAnnotation` does `char '@'`, then key, `(`, value, `)`. It never sees `@@` as two chars; we need to look for `@@` first and branch.
- Megaparsec: `try (string "@@")` then parse identified header (identifier and/or labels) without parentheses; then `many parseAnnotation` for the rest. Order of choice matters: try `@@` before `@`.
- Empty `@@`: Parsing `@@` then requiring identifier or labels (with no optional that allows empty) yields parse failure; report as parse error (FR-005, corpus :error).

**Alternatives considered**:
- Single annotation parser with a single `@` vs `@@` choice at the start: Chosen in practice; the alternative of a single unified rule is equivalent if we try `@@` first.
- Keeping a single `Annotation` type with optional fields: Rejected; spec and tree-sitter contract require discriminable property vs identified (FR-006, SC-003).

### 2. CST representation (property vs identified)

**Decision**: Extend the annotation model to a sum type: `PropertyAnnotation` (key + value, current shape) and `IdentifiedAnnotation` (optional identifier, optional labels). Use a single `Annotation` type that is a sum (e.g. `data Annotation = PropAnn Symbol Value | IdentAnn (Maybe Identifier) (Set String)` or equivalent). `AnnotatedPattern` keeps `[Annotation]`; ordering constraint (at most one identified, and first) is enforced by the parser, not by type.

**Rationale**:
- tree-sitter-gram contract (003-extended-annotation) and spec require consumers to distinguish property-style from identified and to read key/value vs identifier/labels (FR-006, SC-003).
- Backward compatibility: Existing `Annotation` has key+value only. Call sites that only handle property annotations can pattern-match on the sum or use a helper; new code can handle both.
- Identified has two optional fields but at least one required; that invariant is enforced at parse time (reject empty `@@`).

**Alternatives considered**:
- Two separate lists (e.g. `apIdentified :: Maybe IdentifiedAnnotation`, `apProperties :: [PropertyAnnotation]`): Matches grammar structure but changes `AnnotatedPattern` more; single list with sum type keeps `apAnnotations :: [Annotation]` and preserves “stack” order for debugging and future extensions.

### 3. Annotation stack and Megaparsec

**Decision**: Parse annotations as: first, optional one identified (try `@@` then identifier and/or labels); then many property (`@` key `(` value `)`). If we parsed zero annotations (e.g. input starts with pattern), that’s valid for annotated_pattern with empty annotations; if we require at least one annotation when we see `@` or `@@`, then “at most one identified, it first” is natural. For the extended corpus, every example has at least one annotation when annotations are present. Enforce “at most one identified” in parser: after parsing one optional identified, parse only property_annotation in the rest.

**Rationale**:
- tree-sitter-gram: `annotations: choice(seq(identified_annotation, repeat(property_annotation)), repeat1(property_annotation))`. So: one optional identified first, then zero or more property; at least one annotation total when the annotations rule is used.
- Megaparsec: `optional parseIdentifiedAnnotation >> many parsePropertyAnnotation` and ensure we only call this when we’ve seen `@` or `@@`. If we see `@@` we must succeed on identified or fail (no empty header).

**Alternatives considered**:
- Parsing a flat list and validating in a second pass: Rejected; single-pass parse with clear ordering is simpler and gives better errors.

### 4. Alignment with tree-sitter-gram corpus

**Decision**: Treat `libs/gram/test-data/tree-sitter-gram/test/corpus/extended_annotations.txt` as the source of truth. Positive cases: parse and compare structure (logical equivalence to expected CST). Negative case: line with `:error`, expect parse failure. Reuse the same corpus runner approach as 016-gram-parsing-conformance (scan corpus, split on `---`, parse input and check success/failure). No separate Haskell-specific corpus; one corpus file for both tree-sitter and Haskell.

**Rationale**:
- Spec FR-007 and SC-001 require consistency with the reference corpus.
- Single corpus avoids drift between tree-sitter and Haskell; any grammar change is reflected in one file and both parsers must pass.

**Alternatives considered**:
- Duplicating corpus into a Haskell-only file: Rejected to avoid drift.

## Summary Table

| Topic                 | Decision                                                                 | Rationale / Note                          |
|-----------------------|---------------------------------------------------------------------------|--------------------------------------------|
| `@@` vs `@` in parser | Try `@@` first; then identified header (identifier and/or labels); then many `@` | Clear disambiguation; empty `@@` fails      |
| CST annotation type   | Sum type: PropertyAnnotation \| IdentifiedAnnotation                     | Discriminable for consumers (FR-006)       |
| Stack order           | Optional one identified, then zero or more property; enforced in parser   | Matches tree-sitter grammar                |
| Corpus alignment      | Use extended_annotations.txt as single source; same runner as 016         | FR-007, SC-001; no drift                    |
