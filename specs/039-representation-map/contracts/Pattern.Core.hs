-- Contract: Pattern.Core additions for 039-representation-map
-- These declarations are added to the existing Pattern.Core module.
-- Existing exports (ScopeQuery, TrivialScope, ScopeDict, paraWithScope, para, etc.) are unchanged.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Pattern.Core
  ( -- ... existing exports unchanged ...

    -- * Shape kinds (039-representation-map)
    PatternKind(..)
  , checkKind

  ) where

-- | A named, scope-aware description of a family of Pattern shapes.
--
-- A @PatternKind@ is a subobject classifier for @Pattern v@: it defines a
-- subtype via a predicate. The @kindExample@ witnesses non-emptiness of the kind.
--
-- Categorical note: @kindPred@ is the characteristic morphism of the subobject.
-- @kindExample@ is an element of the represented subtype — it MUST satisfy
-- @kindPred@ for any valid scope.
--
-- Usage:
--   diagnosticKind :: PatternKind Subject
--   diagnosticKind = PatternKind
--     { kindName    = "DiagnosticPattern"
--     , kindPred    = \q p -> hasLabel "Location" p && ...
--     , kindExample = exampleDiagnosticPattern
--     }
data PatternKind v = PatternKind
  { kindName    :: String
    -- ^ Unique human-readable name for this kind. Used for kind-compat checks in compose.
  , kindPred    :: forall q. ScopeQuery q v => q v -> Pattern v -> Bool
    -- ^ Membership predicate. Polymorphic over scope: structural predicates ignore the
    -- scope; scope-relative predicates use containers/byIdentity/allElements.
  , kindExample :: Pattern v
    -- ^ Canonical example. MUST satisfy kindPred for any valid scope.
    -- Used for smoke tests and property-based test generation seeds.
  }

-- | Check whether a pattern is of a given kind within a scope.
--
-- Convenience wrapper: @checkKind k q p = kindPred k q p@.
checkKind :: ScopeQuery q v => PatternKind v -> q v -> Pattern v -> Bool
checkKind k q p = kindPred k q p
