# Quickstart: Scope Unification for Structure-Aware Operations

**Branch**: `038-scope-unification`

## Goal

Implement the shared scope model in `libs/pattern` while preserving the current behavior of `para` and `paraGraph`.

## 1. Implement the generic scope contract

Update `libs/pattern/src/Pattern/Core.hs` to add:
- `ScopeQuery`
- `TrivialScope`
- `ScopeDict`
- `toScopeDict`
- `paraWithScope`

Keep the existing `para` export and redefine it as the wrapper over `paraWithScope` plus `TrivialScope`.

## 2. Add graph-side scope adaptation

Update the graph-side implementation so the unified scope layer can answer generic scope questions across a full `GraphView` without changing the existing `GraphQuery(..)` record shape.

Likely touchpoints:
- `libs/pattern/src/Pattern/Graph/Transform.hs`
- `libs/pattern/src/Pattern/Graph/Types.hs`
- `libs/pattern/src/Pattern/Graph.hs` if public re-exports are needed

Keep these behaviors unchanged:
- `paraGraph` returns `Map (Id v) r`
- `paraGraph` still uses `topoShapeSort`
- cycle members still omit unavailable `subResults` rather than failing

## 3. Add and update tests

Update `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` with:
- `TrivialScope` behavior tests
- `ScopeDict` observational equivalence tests
- `paraWithScope` bottom-up behavior tests
- property or representative-equivalence test that `paraWithScope (trivialScope p)` matches `para`

Update `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs` with:
- wrapper-preservation coverage for `paraGraph`
- any graph adapter tests needed for generic scope answers
- confirmation that existing annotation / `GOther` / cycle cases remain unchanged

## 4. Refresh reference docs

Update:
- `docs/reference/features/paramorphism.md`
- `docs/reference/features/para-graph.md`
- `docs/reference/PORTING-GUIDE.md`

Document:
- one unified scope mental model
- tree scope vs graph scope boundaries
- typeclass plus dictionary/value-form correspondence for ports

## 5. Verification

Run the pattern test suite:

```bash
cabal test --enable-tests pattern:test:pattern-test
```

Run from repo root. This workspace currently requires the explicit `--enable-tests` package-qualified form so Cabal includes the `pattern-test` suite in the solver plan.

## Expected Outcome

- `para` callers change nothing
- `paraGraph` callers change nothing
- new scope abstractions are documented and tested
- the library has one explicit model of scope for structure-aware operations
