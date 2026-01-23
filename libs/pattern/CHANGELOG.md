# CHANGELOG

## [0.4.0.0] - 2026-01-23
### Added
- Feature 031: Pattern Subject Reconciliation
  - New `Pattern.Reconcile` module for normalizing duplicate identities
  - Four reconciliation policies: LastWriteWins, FirstWriteWins, Merge, Strict
  - Configurable merge strategies across three dimensions (labels, properties, elements)
  - Automatic reference completion for atomic patterns
  - Cycle detection to prevent infinite recursion
  - Detailed conflict reporting with Strict mode
  - `reconcileWithReport` for detailed statistics (duplicates, references, merges)
  - Comprehensive test coverage with 100+ tests
  - O(n) time, O(k) space performance for 10,000+ subjects
  - Complete Haddock documentation with usage examples

## [0.1.0] - 2025-11-29
### Added
- Feature 019: Integration and Polish - finalized API exports, completed Haddock documentation, and verified rigorous semantics via property tests.

## [0.0.1] - 2025-11-29
### Added
- Initial release
