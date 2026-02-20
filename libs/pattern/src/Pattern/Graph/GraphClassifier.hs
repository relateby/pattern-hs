{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | GraphClassifier defines the shared vocabulary for categorizing pattern structures.
module Pattern.Graph.GraphClassifier
  ( GraphClass(..)
  , GraphClassifier(..)
  , classifyByShape
  , canonicalClassifier
  ) where

import Pattern.Core (Pattern(..))

-- | Represents the five structural categories of graph elements.
-- 'GOther' allows open extension.
data GraphClass extra
  = GNode
  | GRelationship
  | GAnnotation
  | GWalk
  | GOther extra
  deriving (Eq, Show, Functor, Traversable, Foldable)

-- | Pluggable record of functions defining the categorization logic.
data GraphClassifier extra v = GraphClassifier
  { classify :: Pattern v -> GraphClass extra
  }

-- | Default classification logic based on the shape/arity of the pattern.
-- Falls back to 'GOther ()' for patterns that don't match standard graph structures.
classifyByShape :: Eq v => Pattern v -> GraphClass ()
classifyByShape (Pattern _ els)
  | null els = GNode
  | length els == 1 = GAnnotation
  | length els == 2 && all isNodeLike els = GRelationship
  | length els >= 1 && all isRelationshipLike els && isValidWalk els = GWalk
  | otherwise = GOther ()
  where
    isNodeLike (Pattern _ inner) = null inner
    isRelationshipLike (Pattern _ inner) = length inner == 2 && all isNodeLike inner

-- | Determines if a sequence of relationships forms a valid walk.
-- Consecutive relationships must share at least one node (ignoring direction),
-- and must chain end-to-end.
isValidWalk :: Eq v => [Pattern v] -> Bool
isValidWalk [] = False
isValidWalk rels = not (null (foldl step [] rels))
  where
    step [] (Pattern _ [a, b]) = [a, b]
    step active (Pattern _ [a, b]) =
      let fromA = if a `elem` active then [b] else []
          fromB = if b `elem` active then [a] else []
      in fromA ++ fromB
    step _ _ = []

-- | The standard classifier used for canonical graph construction.
canonicalClassifier :: Eq v => GraphClassifier () v
canonicalClassifier = GraphClassifier
  { classify = classifyByShape
  }
