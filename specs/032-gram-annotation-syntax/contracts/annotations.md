# Contract: Parser and CST for extended annotations

**Feature**: 032-gram-annotation-syntax

## Scope

This contract defines how the Haskell Gram parser and CST represent the extended annotation syntax (property-style `@` and identified/labeled `@@`) so that downstream tools can consume annotations without re-parsing. It aligns with the tree-sitter-gram contract at `libs/gram/test-data/tree-sitter-gram/specs/003-extended-annotation/contracts/annotations.md`.

## Introducer distinction

| Source   | CST / logical form       | Meaning |
|----------|---------------------------|---------|
| `@`      | PropertyAnnotation        | Key and value; body is the following pattern (same as today). |
| `@@`     | IdentifiedAnnotation      | Optional identifier and/or labels; body is the following pattern. |

The parser MUST produce a discriminable representation for `@` vs `@@` (e.g. sum type or distinct constructors) so that consumers can tell property-style from identified without heuristics.

## Property-style annotation (`@`)

- **Input**: `@` + key (symbol) + `(` + value + `)`.
- **CST**: PropertyAnnotation with key (Symbol) and value (Value). Value may be integer, symbol, string, or other grammar value types.
- **Body**: The pattern following the annotation stack is the single element of the parent AnnotatedPattern (same as today).

## Identified/labeled annotation (`@@`)

- **Input**: `@@` + header. Header is identifier and/or labels (at least one required). Same label syntax as subject position (`:L`, `::Label`). No parentheses around header; body is the pattern that follows the full annotation stack.
- **CST**: IdentifiedAnnotation with optional identifier (Identifier) and optional labels (e.g. Set String). At least one of identifier or labels must be present (enforced by parser; empty `@@` is invalid).
- **Body**: The pattern that follows all annotations is the single element of the parent AnnotatedPattern.

## Annotation stack

- **Order**: At most one IdentifiedAnnotation per annotated pattern; it MUST appear first if present. Zero or more PropertyAnnotations follow (or only PropertyAnnotations).
- **Parsing**: Parser MUST try `@@` before `@`. After parsing one optional identified annotation, only property annotations follow. Invalid: two or more `@@` before the body (e.g. `@@p @@q (a)`).

## Required behaviors

1. **Disambiguation**: Input starting with `@@` MUST be parsed as IdentifiedAnnotation; input starting with single `@` (and not `@@`) as PropertyAnnotation.
2. **Empty `@@`**: Input `@@` with no identifier and no labels before the pattern (e.g. `@@ (a)`) MUST be rejected with a parse error (FR-005).
3. **Structure**: Downstream consumers MUST be able to read annotation kind, and for each kind the relevant fields (key/value for property; identifier/labels for identified) from the CST without parsing the original text (FR-006, SC-003).
4. **Corpus**: All positive examples in `libs/gram/test-data/tree-sitter-gram/test/corpus/extended_annotations.txt` MUST parse successfully and produce the expected logical structure. The case marked `:error` (empty `@@ (a)`) MUST produce a parse failure.

## Examples (logical)

- `@x(1) ()` → one PropertyAnnotation (key `x`, value 1); body node `()`.
- `@@p (a)` → one IdentifiedAnnotation (identifier `p`, no labels); body node `(a)`.
- `@@:L (a)` → one IdentifiedAnnotation (no identifier, labels `L`); body node `(a)`.
- `@@p:L (a)` → one IdentifiedAnnotation (identifier `p`, labels including `L`); body node `(a)`.
- `@@ (a)` → parse error (invalid).
