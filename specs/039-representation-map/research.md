# Research: RepresentationMap (039)

**Date**: 2026-03-17
**Branch**: `039-representation-map`

---

## Decision 1: `Text` vs `String` for names and conventions

**Decision**: Use `String` throughout (`kindName`, `name`, `conventions`).

**Rationale**: The project has no `text` package dependency in `pattern.cabal`. The `Subject` type uses `String` for labels and property keys. Introducing `Text` would add a new dependency for cosmetic benefit.

**Alternatives considered**: `Text` (Data.Text) — idiomatic Haskell for text data, but not used anywhere in the current codebase. Add only when the project adopts it broadly.

---

## Decision 2: Property-based test framework

**Decision**: Use QuickCheck (`Test.QuickCheck`) for property-based round-trip tests.

**Rationale**: The project already depends on QuickCheck (`^>=2.14`) and has `Arbitrary` instances for `Pattern String`, `Pattern Int`, etc. in `tests/Spec/Pattern/Properties.hs`. The proposal mentions Hedgehog, but that is not a current dependency. QuickCheck's `forAll` and `quickCheck`/`property` provide equivalent round-trip testing capability.

**Alternatives considered**: Hedgehog — shrinking behavior is superior, but requires adding a new dependency and learning a new API. Defer until the project decides to adopt it broadly.

---

## Decision 3: Module placement

**Decision**:
- `PatternKind` → `Pattern.Core` (alongside `ScopeQuery`, `para`, `paraWithScope`)
- `RepresentationMap` + `compose` → new `Pattern.RepresentationMap` module
- `diagnosticMap` → new `Pattern.RepresentationMap.Diagnostic` module (or inline in tests for the prototype)

**Rationale**: `PatternKind` is a pure `Pattern v` concept — a predicate with a name and example. It belongs beside `ScopeQuery` in `Pattern.Core`. `RepresentationMap` pairs two `PatternKind`s with transforms; it is a higher-level abstraction that warrants its own module. The diagnostic map is a concrete domain-specific instance, not part of the core abstraction.

**Alternatives considered**: All in `Pattern.Core` — would bloat the module significantly. All in a single `Pattern.RepresentationMap` — viable but mixes abstraction definition with concrete instances.

---

## Decision 4: `RankNTypes` for `forall q.` fields

**Decision**: Add `{-# LANGUAGE RankNTypes #-}` to `Pattern.Core` and `Pattern.RepresentationMap`.

**Rationale**: The `kindPred`, `forward`, `inverse`, and `roundTrip` fields in `PatternKind` and `RepresentationMap` are universally quantified over `q`: `forall q. ScopeQuery q v => q v -> ...`. This requires `RankNTypes`. The project already uses `TypeFamilies`, `MultiParamTypeClasses`, and `FlexibleInstances`; `RankNTypes` is a natural addition.

**Alternatives considered**: Reify to `ScopeDict` at the point of storage — would lose polymorphism and require callers to pre-reify. Not acceptable; the whole point is scope-agnostic transforms.

---

## Decision 5: Kind compatibility check in `compose`

**Decision**: Runtime check by comparing `kindName` strings. `compose m1 m2` fails if `kindName (codomain m1) /= kindName (domain m2)`.

**Rationale**: Per the proposal's open question, phantom-type-level kind checking is deferred until the abstraction is stable. The runtime check is safe and surfaces mismatches immediately.

**Alternatives considered**: Phantom type tags at the type level — would make mismatches compile-time errors but requires kinds to be named at the type level (complex GADT or type-tagged approach). Deferred.

---

## Decision 6: `diagnosticMap` placement

**Decision**: Define `diagnosticKinds` and `diagnosticMap` inline in a dedicated test/example file for the prototype: `tests/Spec/Pattern/RepresentationMapSpec.hs`. They are not exported from the library in this feature.

**Rationale**: The diagnostic map is a domain-specific instance of the abstraction, not part of the core library. Shipping it in the library would couple the library to a specific domain. For the prototype, test files are the right place: they validate the abstraction and serve as usage examples.

**Alternatives considered**: Export from `Pattern.RepresentationMap.Diagnostic` — appropriate once the abstraction is stable and the team decides to ship example maps. Deferred to a follow-on.

---

## Decision 7: `GraphScope` typeclass

**Decision**: Deferred. Not required for `PatternKind`, `RepresentationMap`, or `diagnosticMap`. The `diagnosticMap` forward and inverse transforms use only the generic `ScopeQuery` contract (`containers`, `allElements`, `byIdentity`). `GraphScope` (source/target/incidents) will be introduced when a concrete map requires graph-topology operations.

**Rationale**: Per spec Assumption. Adding `GraphScope` now would be speculative scope expansion with no concrete use case in this feature.

---

## Decision 8: `ScopeQuery` instance for `GraphQuery`

**Decision**: Deferred. The current `scopeDictFromGraphView` (in `Pattern.Graph.Transform`) already provides a `ScopeDict (Id v) v` from a `GraphView`, which is sufficient for any transform needing graph-wide scope. Making `GraphQuery` directly implement `ScopeQuery` is a design improvement that belongs in a follow-on focused on the `GraphScope` gap.

**Rationale**: Zero existing call sites need to change. The `diagnosticMap` uses `TrivialScope` or a graph-backed `ScopeDict`. No urgency.

---

## Existing infrastructure confirmed available

| Component | Location | Status |
|---|---|---|
| `ScopeQuery` typeclass | `Pattern/Core.hs:1175` | ✅ |
| `TrivialScope` + `trivialScope` | `Pattern/Core.hs:1208` | ✅ |
| `ScopeDict` + `toScopeDict` | `Pattern/Core.hs:1236` | ✅ |
| `paraWithScope` | `Pattern/Core.hs:1293` | ✅ |
| `para` | `Pattern/Core.hs:1350` | ✅ |
| `scopeDictFromGraphView` | `Pattern/Graph/Transform.hs:245` | ✅ |
| `GraphView` | `Pattern/Graph/Types.hs:40` | ✅ |
| `Arbitrary Pattern` instances | `tests/Spec/Pattern/Properties.hs` | ✅ |
| QuickCheck | cabal dependency | ✅ |
| `Subject` (with labels, properties) | `libs/subject/src/Subject/Core.hs` | ✅ |

---

## Language extensions required

New extensions needed:
- `RankNTypes` — for `forall q. ScopeQuery q v => ...` in record fields

Already present (no action needed):
- `TypeFamilies`, `MultiParamTypeClasses`, `FlexibleInstances`, `InstanceSigs`
