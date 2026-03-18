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
-- @forward@ and @inverse@ are the two morphism components. @roundTrip@ witnesses
-- the identity law: @(inverse q . forward q) p == p@ for all domain-kind patterns.
--
-- Explanatory notes about how a concrete map works are intentionally kept outside the
-- runtime value for now. The current design stores executable behavior and the
-- round-trip witness only; declarative, machine-checkable map claims are deferred.
--
-- Usage:
--   myMap :: RepresentationMap Subject
--   myMap = RepresentationMap
--     { name        = "MyMap"
--     , domain      = nestedKind
--     , codomain    = flatKind
--     , forward     = \q p -> ...
--     , inverse     = \q p -> ...
--     , roundTrip   = \q p -> (inverse myMap q . forward myMap q) p == p
--     }
data RepresentationMap v = RepresentationMap
  { name        :: String
    -- ^ Unique human-readable name for this map, e.g. "DiagnosticMap".
  , domain      :: PatternKind v
    -- ^ The source kind. @forward@ is only defined on patterns satisfying this kind.
  , codomain    :: PatternKind v
    -- ^ The target kind. @forward@ MUST produce patterns satisfying this kind.
  , forward     :: forall q. ScopeQuery q v => q v -> Pattern v -> Pattern v
    -- ^ The forward transform: domain kind → codomain kind.
    -- Polymorphic over scope: may use containers, allElements, byIdentity.
    -- MUST produce a result satisfying @kindPred (codomain m) q result@.
  , inverse     :: forall q. ScopeQuery q v => q v -> Pattern v -> Pattern v
    -- ^ The inverse transform: codomain kind → domain kind.
    -- MUST satisfy: for all p of domain kind, @(inverse q . forward q) p == p@.
  , roundTrip   :: forall q. ScopeQuery q v => q v -> Pattern v -> Bool
    -- ^ Isomorphism witness. MUST hold for all p satisfying @kindPred (domain m) q p@.
    -- Canonical implementation: @\q p -> (inverse q . forward q) p == p@
    -- (requires Eq v; callers may substitute structural equality checks).
  }

-- | Compose two compatible @RepresentationMap@s into a single map.
--
-- @compose m1 m2@ requires @kindName (codomain m1) == kindName (domain m2)@.
-- Returns @Left@ with a descriptive error if the kinds are incompatible.
--
-- The composed map:
-- - Name:        @name m1 <> " >>> " <> name m2@
-- - Domain:      @domain m1@
-- - Codomain:    @codomain m2@
-- - Forward:     @forward m2 q . forward m1 q@
-- - Inverse:     @inverse m1 q . inverse m2 q@
-- - RoundTrip:   @roundTrip m1 q p && roundTrip m2 q (forward m1 q p)@
--
-- Categorical note: @compose@ is morphism composition in the category of PatternKinds.
-- Associativity holds: @compose (compose a b) c == compose a (compose b c)@ (up to name).
compose :: RepresentationMap v -> RepresentationMap v -> Either String (RepresentationMap v)
compose m1 m2
  | kindName (codomain m1) /= kindName (domain m2) =
      Left $ "compose: codomain of '" <> name m1
          <> "' (" <> kindName (codomain m1)
          <> ") does not match domain of '" <> name m2
          <> "' (" <> kindName (domain m2) <> ")"
  | otherwise = Right RepresentationMap
      { name        = name m1 <> " >>> " <> name m2
      , domain      = domain m1
      , codomain    = codomain m2
      , forward     = \q p -> forward m2 q (forward m1 q p)
      , inverse     = \q p -> inverse m1 q (inverse m2 q p)
      , roundTrip   = \q p -> roundTrip m1 q p && roundTrip m2 q (forward m1 q p)
      }
