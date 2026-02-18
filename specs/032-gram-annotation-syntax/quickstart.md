# Quickstart: Gram annotation-based identifiers and labels

**Feature**: 032-gram-annotation-syntax  
**Branch**: `032-gram-annotation-syntax`

## Overview

This feature extends the Haskell Gram parser to support the extended annotation syntax: property-style `@key(value)` and identified/labeled `@@` (identifier-only, labels-only, or both). The implementation must align with the tree-sitter-gram corpus `extended_annotations.txt`.

## Running the tests

1. Ensure the `tree-sitter-gram` test data is available (submodule or vendored):
   ```bash
   git submodule update --init --recursive
   ```

2. Run the Gram test suite:
   ```bash
   cabal test gram-test
   ```
   This runs unit tests and corpus-based tests. After implementation, corpus tests will include cases from `libs/gram/test-data/tree-sitter-gram/test/corpus/extended_annotations.txt`.

3. Run only corpus-related tests (filter by "Corpus" or "extended_annotations"):
   ```bash
   cabal test gram-test --test-option=--match="Corpus"
   ```
   (Exact filter name may depend on test naming; adjust to match the test suite.)

## Verifying annotation behavior

- **Property-style**: Parse `@x(1) ()`, `@desc(a) (a)`, `@desc("historic route") (a)-->(b)` and assert CST has PropertyAnnotation with correct key/value and body.
- **Identifier-only**: Parse `@@p (a)`, `@@r1 (a)-[r]->(b)` and assert IdentifiedAnnotation with identifier, body as node or relationship pattern.
- **Labels-only**: Parse `@@:L (a)`, `@@::Label (a)` and assert IdentifiedAnnotation with labels, body as node pattern.
- **Combined**: Parse `@@p:L (a)` and assert IdentifiedAnnotation with both identifier and labels.
- **Error**: Parse `@@ (a)` and assert parse failure (no valid pattern).

## Reference artifacts

- **Spec**: [spec.md](./spec.md)  
- **Plan**: [plan.md](./plan.md)  
- **Data model**: [data-model.md](./data-model.md)  
- **Contracts**: [contracts/](./contracts/)  
- **Corpus**: `libs/gram/test-data/tree-sitter-gram/test/corpus/extended_annotations.txt`  
- **Tree-sitter contract**: `libs/gram/test-data/tree-sitter-gram/specs/003-extended-annotation/contracts/annotations.md`
