# Data Model: RepresentationMap (039)

**Date**: 2026-03-17
**Branch**: `039-representation-map`

---

## Core Types

### `PatternKind v`

A named, scope-aware description of a family of patterns. Not an individual pattern — the description of what structural constraints a pattern must satisfy to be considered an instance of that kind.

```
PatternKind v
  kindName    :: String               -- unique human-readable name, e.g. "DiagnosticPattern"
  kindPred    :: ∀q. ScopeQuery q v   -- true iff pattern is of this kind within a scope
                => q v -> Pattern v -> Bool
  kindExample :: Pattern v            -- canonical example; always satisfies kindPred
```

**Invariant**: `kindPred q (kindExample k) == True` for any valid scope `q`.

**Categorical interpretation**: A `PatternKind v` is a subobject classifier for `Pattern v` — it defines a subtype of `Pattern v` via a predicate. The `kindPred` is a characteristic morphism. The `kindExample` witnesses non-emptiness of the kind.

---

### `RepresentationMap v`

A named, invertible, composable isomorphism between two kinds of shape. The round-trip field is the machine-checkable witness to the isomorphism.

```
RepresentationMap v
  name        :: String               -- e.g. "DiagnosticMap"
  domain      :: PatternKind v        -- source kind
  codomain    :: PatternKind v        -- target kind
  forward     :: ∀q. ScopeQuery q v   -- domain → codomain transform
                => q v -> Pattern v -> Pattern v
  inverse     :: ∀q. ScopeQuery q v   -- codomain → domain transform
                => q v -> Pattern v -> Pattern v
  roundTrip   :: ∀q. ScopeQuery q v   -- isomorphism witness
                => q v -> Pattern v -> Bool
```

**Invariant**: For all `p` satisfying `kindPred (domain m) q p`:
1. `kindPred (codomain m) q (forward m q p) == True`
2. `roundTrip m q p == True`
3. `roundTrip m q p` implies `(inverse m q . forward m q) p == p` structurally

**Categorical interpretation**: A `RepresentationMap v` is a named isomorphism in the category of `PatternKind`s. `forward` and `inverse` are the two morphism components. `roundTrip` witnesses the identity law `inverse ∘ forward = id` on the domain subobject.

---

### `Convention`

A future design area, not part of the current runtime data model.

Encoding choices such as `_arity` or `_depth` may still matter to a concrete map,
but they are currently documented beside the implementation and exercised through
kind checks and round-trip tests rather than stored as inline prose on the
`RepresentationMap` value.

---

## Composition

```
compose :: RepresentationMap v -> RepresentationMap v -> Either String (RepresentationMap v)
```

**Precondition**: `kindName (codomain m1) == kindName (domain m2)`
**Failure**: `Left` with message identifying the mismatch if precondition not met
**Result**: A map from `domain m1` to `codomain m2` with:
- `name = name m1 <> " >>> " <> name m2`
- `forward q = forward m2 q . forward m1 q`
- `inverse q = inverse m1 q . inverse m2 q`
- `roundTrip q p = roundTrip m1 q p && roundTrip m2 q (forward m1 q p)`

**Categorical interpretation**: `compose` is morphism composition in the category of `PatternKind`s. The `Either` wraps a partial function — composition is only defined when codomain matches domain.

---

## Concrete Kinds for the `diagnosticMap` Test Example

### `DiagnosticPattern`

A nested structure: a `Location` pattern directly containing a `Diagnostic` pattern, which directly contains zero or more `Remediation` patterns.

**Predicate (informal)**: Pattern has label `"Location"` and `elements` contains exactly one sub-pattern with label `"Diagnostic"`, which in turn has zero or more sub-patterns with label `"Remediation"`.

**Structural form**:
```
Location
  Diagnostic
    Remediation?
    Remediation?
    ...
```

### `DiagnosticGraph`

A flat form: atomic patterns with labels `"Location"`, `"Diagnostic"`, `"Remediation"` connected by typed relationship patterns (`"AT"`, `"HAS_REMEDIATION"`), with `_arity` and `_depth` scalar properties on each.

**Predicate (informal)**: All patterns are atomic (empty elements). Labels are constrained to `{"Location", "Diagnostic", "Remediation", "AT", "HAS_REMEDIATION"}`. Each non-relationship pattern has `_arity` and `_depth` numeric properties.

---

## Module Relationships

```
Pattern.Core
  PatternKind v           -- new type
  (ScopeQuery, TrivialScope, ScopeDict, paraWithScope — existing)

Pattern.RepresentationMap
  RepresentationMap v     -- new type
  compose                 -- new function
  (imports Pattern.Core)

tests/Spec/Pattern/RepresentationMapSpec.hs
  diagnosticPatternKind   -- DiagnosticPattern PatternKind
  diagnosticGraphKind     -- DiagnosticGraph PatternKind
  diagnosticMap           -- RepresentationMap between above
  (QuickCheck round-trip properties)
```

---

## State Transitions

`RepresentationMap` has no mutable state. The types are pure values. Composition produces a new value; it does not modify the inputs.

The only "state" is the validation result of `roundTrip`, which is a pure predicate over a domain-kind pattern.
