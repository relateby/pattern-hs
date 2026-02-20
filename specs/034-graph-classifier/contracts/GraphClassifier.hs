module Pattern.Graph.GraphClassifier
  ( GraphClass(..)
  , GraphClassifier(..)
  , canonicalClassifier
  , classifyByShape
  ) where

import Pattern.Core (Pattern)
import Subject.Core (Subject)

-- | Shared classification vocabulary
data GraphClass extra
  = GNode
  | GRelationship
  | GAnnotation
  | GWalk
  | GOther extra
  deriving (Eq, Show, Functor, Traversable, Foldable)

-- | Injectable classification logic
data GraphClassifier extra v = GraphClassifier
  { classify :: Pattern v -> GraphClass extra
  }

-- | The canonical arity-based classifier
canonicalClassifier :: GraphClassifier () Subject
canonicalClassifier = undefined

-- | Exposed pure function for arity classification
-- Note: Walks must be structurally validated to ensure their relationships form
-- a contiguous chain where consecutive relationships share at least one node
-- (ignoring relationship direction). Structurally invalid chains (e.g. "star patterns")
-- must be rejected (falling back to `GOther`).
classifyByShape :: Pattern Subject -> GraphClass ()
classifyByShape = undefined
