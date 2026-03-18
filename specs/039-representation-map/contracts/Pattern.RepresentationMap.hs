-- Contract: Pattern.RepresentationMap — new module for 039-representation-map
--
-- New module added to the Pattern library alongside Pattern.Core and Pattern.Graph.*.

{-# LANGUAGE RankNTypes #-}

module Pattern.RepresentationMap
  ( -- * Representation maps
    RepresentationMap(..)
  , compose
  ) where

import Pattern.Core (Pattern, ScopeQuery, PatternKind, kindName)

-- | A named, invertible, composable isomorphism between two kinds of Pattern shape.
--
-- A @RepresentationMap@ is a named isomorphism in the category of @PatternKind@s.
-- @repMapForward@ and @repMapInverse@ are the two morphism components.
-- @repMapRoundTrip@ witnesses the identity law:
-- @(repMapInverse q . repMapForward q) p == p@ for all domain-kind patterns.
--
-- Explanatory notes about how a concrete map works are intentionally kept outside the
-- runtime value for now. The current design stores executable behavior and the
-- round-trip witness only; declarative, machine-checkable map claims are deferred.
--
-- Usage:
--   myMap :: RepresentationMap Subject
--   myMap = RepresentationMap
--     { repMapName        = "MyMap"
--     , repMapDomain      = nestedKind
--     , repMapCodomain    = flatKind
--     , repMapForward     = \q p -> ...
--     , repMapInverse     = \q p -> ...
--     , repMapRoundTrip   = \q p -> (repMapInverse myMap q . repMapForward myMap q) p == p
--     }
data RepresentationMap v = RepresentationMap
  { repMapName        :: String
    -- ^ Unique human-readable name for this map, e.g. "DiagnosticMap".
  , repMapDomain      :: PatternKind v
    -- ^ The source kind. @repMapForward@ is only defined on patterns satisfying this kind.
  , repMapCodomain    :: PatternKind v
    -- ^ The target kind. @repMapForward@ MUST produce patterns satisfying this kind.
  , repMapForward     :: forall q. ScopeQuery q v => q v -> Pattern v -> Pattern v
    -- ^ The forward transform: domain kind → codomain kind.
    -- Polymorphic over scope: may use containers, allElements, byIdentity.
    -- MUST produce a result satisfying @kindPred (repMapCodomain m) q result@.
  , repMapInverse     :: forall q. ScopeQuery q v => q v -> Pattern v -> Pattern v
    -- ^ The inverse transform: codomain kind → domain kind.
    -- MUST satisfy: for all p of domain kind, @(repMapInverse q . repMapForward q) p == p@.
  , repMapRoundTrip   :: forall q. ScopeQuery q v => q v -> Pattern v -> Bool
    -- ^ Isomorphism witness. MUST hold for all p satisfying @kindPred (repMapDomain m) q p@.
    -- Canonical implementation: @\q p -> (repMapInverse q . repMapForward q) p == p@
    -- (requires Eq v; callers may substitute structural equality checks).
  }

-- | Compose two compatible @RepresentationMap@s into a single map.
--
-- @compose m1 m2@ requires
-- @kindName (repMapCodomain m1) == kindName (repMapDomain m2)@.
-- Returns @Left@ with a descriptive error if the kinds are incompatible.
--
-- The composed map:
-- - Name:        @repMapName m1 <> " >>> " <> repMapName m2@
-- - Domain:      @repMapDomain m1@
-- - Codomain:    @repMapCodomain m2@
-- - Forward:     @repMapForward m2 q . repMapForward m1 q@
-- - Inverse:     @repMapInverse m1 q . repMapInverse m2 q@
-- - RoundTrip:   @repMapRoundTrip m1 q p && repMapRoundTrip m2 q (repMapForward m1 q p)@
--
-- Categorical note: @compose@ is morphism composition in the category of PatternKinds.
-- Associativity holds: @compose (compose a b) c == compose a (compose b c)@ (up to repMapName).
compose :: RepresentationMap v -> RepresentationMap v -> Either String (RepresentationMap v)
compose m1 m2
  | kindName (repMapCodomain m1) /= kindName (repMapDomain m2) =
      Left $ "compose: codomain of '" <> repMapName m1
          <> "' (" <> kindName (repMapCodomain m1)
          <> ") does not match domain of '" <> repMapName m2
          <> "' (" <> kindName (repMapDomain m2) <> ")"
  | otherwise = Right RepresentationMap
      { repMapName        = repMapName m1 <> " >>> " <> repMapName m2
      , repMapDomain      = repMapDomain m1
      , repMapCodomain    = repMapCodomain m2
      , repMapForward     = \q p -> repMapForward m2 q (repMapForward m1 q p)
      , repMapInverse     = \q p -> repMapInverse m1 q (repMapInverse m2 q p)
      , repMapRoundTrip   = \q p -> repMapRoundTrip m1 q p && repMapRoundTrip m2 q (repMapForward m1 q p)
      }
