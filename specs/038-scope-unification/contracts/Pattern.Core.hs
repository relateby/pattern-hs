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
  type ScopeId q v

  containers  :: q v -> Pattern v -> [Pattern v]
  siblings    :: q v -> Pattern v -> [Pattern v]
  byIdentity  :: q v -> ScopeId q v -> Maybe (Pattern v)
  allElements :: q v -> [Pattern v]

-- Subtree-only scope used to preserve para semantics.
newtype TrivialScope v = TrivialScope (Pattern v)

trivialScope :: Pattern v -> TrivialScope v

-- First-class value form for stored/dynamic scope behavior.
data ScopeDict i v = ScopeDict
  { dictContainers  :: Pattern v -> [Pattern v]
  , dictSiblings    :: Pattern v -> [Pattern v]
  , dictByIdentity  :: i -> Maybe (Pattern v)
  , dictAllElements :: [Pattern v]
  }

instance ScopeQuery (ScopeDict i) v where
  type ScopeId (ScopeDict i) v = i

  containers  = dictContainers
  siblings    = dictSiblings
  byIdentity  = dictByIdentity
  allElements = dictAllElements

toScopeDict :: ScopeQuery q v => q v -> ScopeDict (ScopeId q v) v

-- Existing wrapper preserved.
para :: (Pattern v -> [r] -> r) -> Pattern v -> r

-- Canonical scope-aware tree fold.
paraWithScope
  :: ScopeQuery q v
  => q v
  -> (q v -> Pattern v -> [r] -> r)
  -> Pattern v
  -> r
