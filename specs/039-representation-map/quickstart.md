# Quick Reference: RepresentationMap (039)

## New exports

### `Pattern.Core`

```haskell
data PatternKind v = PatternKind
  { kindName    :: String
  , kindPred    :: forall q. ScopeQuery q v => q v -> Pattern v -> Bool
  , kindExample :: Pattern v
  }

checkKind :: ScopeQuery q v => PatternKind v -> q v -> Pattern v -> Bool
```

### `Pattern.RepresentationMap` (new module)

```haskell
data RepresentationMap v = RepresentationMap
  { repMapName        :: String
  , repMapDomain      :: PatternKind v
  , repMapCodomain    :: PatternKind v
  , repMapForward     :: forall q. ScopeQuery q v => q v -> Pattern v -> Pattern v
  , repMapInverse     :: forall q. ScopeQuery q v => q v -> Pattern v -> Pattern v
  , repMapRoundTrip   :: forall q. ScopeQuery q v => q v -> Pattern v -> Bool
  }

compose :: RepresentationMap v -> RepresentationMap v -> Either String (RepresentationMap v)
```

---

## Define a shape kind

```haskell
{-# LANGUAGE RankNTypes #-}

import Pattern.Core (PatternKind(..), Pattern(..), ScopeQuery)

-- A pattern that has a specific label in its value
hasLabel :: String -> Pattern Subject -> Bool
hasLabel lbl p = lbl `Set.member` labels (value p)

locationKind :: PatternKind Subject
locationKind = PatternKind
  { kindName    = "Location"
  , kindPred    = \_ p -> hasLabel "Location" p
  , kindExample = Pattern (subject "loc1" ["Location"] []) []
  }
```

---

## Define a representation map

```haskell
import Pattern.RepresentationMap (RepresentationMap(..))

-- Nested → flat (forward) and flat → nested (inverse)
myMap :: RepresentationMap Subject
myMap = RepresentationMap
  { repMapName        = "LocationFlatMap"
  , repMapDomain      = nestedLocationKind
  , repMapCodomain    = flatLocationKind
  , repMapForward     = \q p -> flattenWith "_depth" q p
  , repMapInverse     = \q p -> nestFromDepth "_depth" q p
  , repMapRoundTrip   = \q p ->
      let result = (repMapInverse myMap q . repMapForward myMap q) p
      in  result == p
  }
```

---

## Check kind membership

```haskell
import Pattern.Core (trivialScope, checkKind)

let scope = trivialScope somePattern
let isMember = checkKind locationKind scope somePattern
-- True if somePattern has label "Location"
```

---

## Compose maps

```haskell
import Pattern.RepresentationMap (compose)

case compose mapAtoB mapBtoC of
  Left err     -> putStrLn $ "Incompatible: " <> err
  Right mapAtoC -> -- use the composed map
    let result = repMapForward mapAtoC (trivialScope p) p
```

---

## Round-trip property test (QuickCheck)

```haskell
import Test.QuickCheck

prop_roundTrip :: RepresentationMap Subject -> Gen (Pattern Subject) -> Property
prop_roundTrip m genDomainPattern =
  forAll genDomainPattern $ \p ->
    let scope = trivialScope p
    in  checkKind (repMapDomain m) scope p && repMapRoundTrip m scope p
```

---

## Implementation notes

- `RankNTypes` is required for `forall q.` in record fields
- `kindPred` is polymorphic: structural predicates ignore the scope argument; use `containers`/`allElements`/`byIdentity` for scope-relative checks
- `repMapRoundTrip` implementation typically just checks `(repMapInverse q . repMapForward q) p == p`; requires `Eq v`
- `compose` returns `Either String` — check the `Left` case when connecting maps from different domains
- Documentation about encoding choices belongs next to the map definition for now; attaching inline prose to the map value itself is deferred until the project has a more declarative, checkable design for map claims
