module Pattern.Core
  ( Pattern(..)
  , ScopeQuery(..)
  , TrivialScope
  , trivialScope
  , ScopeDict(..)
  , toScopeDict
  , para
  , paraWithScope
  ) where

import Pattern.Core (Pattern(..))

-- Generic contract for "what is in scope" during a structure-aware operation.
class ScopeQuery q v where
  containers  :: q v -> Pattern v -> [Pattern v]
  siblings    :: q v -> Pattern v -> [Pattern v]
  byIdentity  :: q v -> Id v -> Maybe (Pattern v)
  allElements :: q v -> [Pattern v]

-- Subtree-only scope used to preserve para semantics.
newtype TrivialScope v = TrivialScope (Pattern v)

trivialScope :: Pattern v -> TrivialScope v

-- First-class value form for stored/dynamic scope behavior.
data ScopeDict v = ScopeDict
  { dictContainers  :: Pattern v -> [Pattern v]
  , dictSiblings    :: Pattern v -> [Pattern v]
  , dictByIdentity  :: Id v -> Maybe (Pattern v)
  , dictAllElements :: [Pattern v]
  }

instance ScopeQuery ScopeDict v where
  containers  = dictContainers
  siblings    = dictSiblings
  byIdentity  = dictByIdentity
  allElements = dictAllElements

toScopeDict :: ScopeQuery q v => q v -> ScopeDict v

-- Existing wrapper preserved.
para :: (Pattern v -> [r] -> r) -> Pattern v -> r

-- Canonical scope-aware tree fold.
paraWithScope
  :: ScopeQuery q v
  => q v
  -> (q v -> Pattern v -> [r] -> r)
  -> Pattern v
  -> r
