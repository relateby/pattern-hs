{-# LANGUAGE RankNTypes #-}
-- | Invertible mappings between named pattern kinds.
--
-- A 'RepresentationMap' names a pair of compatible structural transforms between
-- two 'PatternKind's. The map itself carries only executable behavior and its
-- round-trip witness. Explanatory metadata about how a map works is deferred for
-- a later design that can express declarative, checkable claims rather than
-- inline prose attached to the value.
module Pattern.RepresentationMap
  ( RepresentationMap(..)
  , compose
  )
where

import Pattern.Core (Pattern, PatternKind, ScopeQuery, kindName)

-- | A named, invertible mapping between two pattern kinds.
--
-- A 'RepresentationMap' is the executable form of an isomorphism between named
-- shapes. 'forward' and 'inverse' are the two morphism components, while
-- 'roundTrip' is the machine-checkable witness that the mapping preserves
-- information on the domain kind.
--
-- Invariants for domain-kind inputs:
--
-- * 'forward' should produce a pattern accepted by 'codomain'.
-- * 'roundTrip' should hold.
-- * When 'roundTrip' holds, @(inverse m q . forward m q) p == p@ structurally.
--
-- Notes:
--
-- * Scope remains polymorphic, so maps are not tied to a particular backing
--   representation.
-- * Declarative, machine-checkable claims about map-specific encoding choices
--   are deferred; for now those details live in documentation and tests next to
--   concrete maps.
data RepresentationMap v = RepresentationMap
  { name :: String
    -- ^ Unique human-readable name for the map.
  , domain :: PatternKind v
    -- ^ Source kind. 'forward' is intended for patterns of this kind.
  , codomain :: PatternKind v
    -- ^ Target kind. 'forward' should produce patterns of this kind.
  , forward :: forall q. ScopeQuery q v => q v -> Pattern v -> Pattern v
    -- ^ Domain-to-codomain transform, polymorphic over any valid scope.
  , inverse :: forall q. ScopeQuery q v => q v -> Pattern v -> Pattern v
    -- ^ Codomain-to-domain transform, polymorphic over any valid scope.
  , roundTrip :: forall q. ScopeQuery q v => q v -> Pattern v -> Bool
    -- ^ Isomorphism witness for domain-kind inputs at a given scope.
  }

-- | Compose two compatible representation maps.
--
-- Composition is defined only when the codomain kind of the first map has the
-- same 'kindName' as the domain kind of the second. The resulting map keeps
-- the first domain, the second codomain, composes 'forward' left-to-right,
-- composes 'inverse' right-to-left, and preserves round-trip validation in the
-- same order.
--
-- Categorical note: this is morphism composition for the category whose
-- objects are 'PatternKind's and whose isomorphisms are 'RepresentationMap's.
compose :: RepresentationMap v -> RepresentationMap v -> Either String (RepresentationMap v)
compose m1 m2
  | kindName (codomain m1) /= kindName (domain m2) =
      Left $
        "compose: codomain of '" <> name m1
          <> "' (" <> kindName (codomain m1)
          <> ") does not match domain of '" <> name m2
          <> "' (" <> kindName (domain m2) <> ")"
  | otherwise =
      Right
        RepresentationMap
          { name = name m1 <> " >>> " <> name m2
          , domain = domain m1
          , codomain = codomain m2
          , forward = \q p -> forward m2 q (forward m1 q p)
          , inverse = \q p -> inverse m1 q (inverse m2 q p)
          , roundTrip = \q p -> roundTrip m1 q p && roundTrip m2 q (forward m1 q p)
          }
