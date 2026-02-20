-- | Property tests for Pattern.PatternGraph (merge idempotence, consistency).
{-# LANGUAGE OverloadedStrings #-}

module Spec.Pattern.PatternGraphProperties where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Pattern.Core (Pattern(..))
import Pattern.PatternGraph (PatternGraph(..), empty, fromPatterns, merge)
import Pattern.Graph.GraphClassifier (canonicalClassifier)
import Subject.Core (Subject(..), Symbol(..))
import Test.Hspec
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary(..), property)

-- Local newtype to avoid duplicate orphan Arbitrary Symbol (ReconcileProperties already defines one).
newtype TestSymbol = TestSymbol Symbol deriving (Eq, Show)

instance Arbitrary TestSymbol where
  arbitrary = TestSymbol . Symbol <$> QC.elements ["a", "b", "c", "n1", "n2", "r1", "x", "y", "z"]

-- Helper: atomic node
node :: Symbol -> Pattern Subject
node s = Pattern (Subject s Set.empty Map.empty) []

-- Helper: relationship
rel :: Symbol -> Symbol -> Symbol -> Pattern Subject
rel r a b = Pattern (Subject r Set.empty Map.empty) [node a, node b]

-- | Merge idempotence: merging the same pattern twice is equivalent to merging once.
prop_merge_idempotence_node :: TestSymbol -> Bool
prop_merge_idempotence_node (TestSymbol s) =
  let g0 = empty
      g1 = merge canonicalClassifier (node s) g0
      g2 = merge canonicalClassifier (node s) g1
  in pgNodes g1 == pgNodes g2

prop_merge_idempotence_relationship :: TestSymbol -> TestSymbol -> TestSymbol -> TestSymbol -> Bool
prop_merge_idempotence_relationship (TestSymbol r) (TestSymbol a) (TestSymbol b) _
  | a == b = True  -- skip invalid rel
  | otherwise =
      let g0 = empty
          g1 = merge canonicalClassifier (node a) g0
          g2 = merge canonicalClassifier (node b) g1
          g3 = merge canonicalClassifier (rel r a b) g2
          g4 = merge canonicalClassifier (rel r a b) g3
      in Map.size (pgRelationships g3) == Map.size (pgRelationships g4)
      && Set.fromList (Map.keys (pgRelationships g3)) == Set.fromList (Map.keys (pgRelationships g4))

-- | fromPatterns order: for a list of distinct nodes, order of list does not change node set.
prop_fromPatterns_order_nodes :: [TestSymbol] -> Bool
prop_fromPatterns_order_nodes syms =
  let uniq = nub syms
      limited = take 20 uniq
      pats = map (\(TestSymbol s) -> node s) limited
      g = fromPatterns canonicalClassifier pats
      gRev = fromPatterns canonicalClassifier (reverse pats)
  in Set.fromList (Map.keys (pgNodes g)) == Set.fromList (Map.keys (pgNodes gRev))
  where
    nub = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

spec :: Spec
spec = do
  describe "Pattern.PatternGraph properties" $ do
    describe "merge idempotence (T019)" $ do
      it "merge same node twice ≈ merge once" $
        property prop_merge_idempotence_node
      it "merge same relationship twice ≈ merge once" $
        property prop_merge_idempotence_relationship
    describe "fromPatterns consistency (T019)" $ do
      it "fromPatterns order for distinct nodes yields same node set" $
        property prop_fromPatterns_order_nodes
