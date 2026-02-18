# Contracts: 032-gram-annotation-syntax

This directory holds design contracts for the extended annotation syntax. There are no HTTP/API endpoints; the contracts define parser behavior and CST shape.

| Document | Purpose |
|----------|---------|
| [annotations.md](./annotations.md) | Parser and CST contract for annotation forms: property `@`, identified `@@`, stack order, and error cases. |

Alignment with the tree-sitter-gram grammar and corpus is specified in [../spec.md](../spec.md) (FR-007) and [../research.md](../research.md). The canonical grammar contract for the AST is in `libs/gram/test-data/tree-sitter-gram/specs/003-extended-annotation/contracts/annotations.md`; the Haskell implementation must produce a CST that is logically equivalent for downstream consumption.
