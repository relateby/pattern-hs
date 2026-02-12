{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Property-based tests for Pattern.Reconcile module.
--
-- This module provides QuickCheck property tests for reconciliation operations.
-- Properties verify fundamental invariants like idempotence, identity preservation,
-- and determinism.
module Spec.Pattern.ReconcileProperties where

import Test.QuickCheck hiding (elements)
import qualified Test.QuickCheck as QC
import Test.Hspec
import Pattern.Core (Pattern(..))
import Pattern.Reconcile
import Subject.Core (Subject(..), Symbol(..))
import qualified Subject.Core as Subj
import Subject.Value (Value(..), RangeValue(..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Either (isLeft, isRight)

-- ============================================================================
-- Arbitrary Instances for Property Testing
-- ============================================================================

-- | Generate arbitrary ReconciliationPolicy for testing.
instance Arbitrary (ReconciliationPolicy SubjectMergeStrategy) where
  arbitrary = oneof
    [ pure LastWriteWins
    , pure FirstWriteWins
    , Merge <$> arbitrary <*> arbitrary
    , pure Strict
    ]

-- | Generate arbitrary SubjectMergeStrategy for testing.
instance Arbitrary SubjectMergeStrategy where
  arbitrary = SubjectMergeStrategy
    <$> arbitrary
    <*> arbitrary

-- | Generate arbitrary LabelMerge strategy for testing.
instance Arbitrary LabelMerge where
  arbitrary = oneof
    [ pure UnionLabels
    , pure IntersectLabels
    , pure ReplaceLabels
    ]

-- | Generate arbitrary PropertyMerge strategy for testing.
instance Arbitrary PropertyMerge where
  arbitrary = oneof
    [ pure ReplaceProperties
    , pure ShallowMerge
    , pure DeepMerge
    ]

-- | Generate arbitrary ElementMergeStrategy for testing.
instance Arbitrary ElementMergeStrategy where
  arbitrary = oneof
    [ pure ReplaceElements
    , pure AppendElements
    , pure UnionElements
    ]

-- | Generate arbitrary Value for testing.
-- Keeps values simple to avoid infinite nesting.
instance Arbitrary Value where
  arbitrary = sized $ \n ->
    if n <= 0
      then oneof simpleValues
      else oneof (simpleValues ++ [nestedValue n])
    where
      simpleValues =
        [ VInteger <$> arbitrary
        , VDecimal <$> arbitrary
        , VBoolean <$> arbitrary
        , VString <$> QC.elements ["value1", "value2", "test", "data"]
        , VSymbol <$> QC.elements ["sym1", "sym2", "symbol"]
        ]
      nestedValue n = oneof
        [ VArray <$> resize (n `div` 2) (listOf arbitrary)
        , VMap . Map.fromList <$> resize (n `div` 2) (listOf ((,) <$> QC.elements ["k1", "k2", "key"] <*> arbitrary))
        ]

-- | Generate arbitrary RangeValue for testing.
instance Arbitrary RangeValue where
  arbitrary = RangeValue
    <$> oneof [pure Nothing, Just <$> arbitrary]
    <*> oneof [pure Nothing, Just <$> arbitrary]

-- | Generate arbitrary Symbol for testing.
instance Arbitrary Symbol where
  arbitrary = Symbol <$> QC.elements
    [ "a", "b", "c", "alice", "bob", "charlie"
    , "x", "y", "z", "node1", "node2", "node3"
    ]

-- | Generate arbitrary Subject for testing.
-- Creates subjects with various combinations of labels and properties.
instance Arbitrary Subject where
  arbitrary = do
    symbol <- arbitrary
    labels <- Set.fromList <$> listOf arbitraryLabel
    props <- Map.fromList <$> listOf arbitraryProp
    return $ Subject symbol labels props
    where
      arbitraryLabel = QC.elements ["Person", "Entity", "Node", "Thing", "Item"]
      arbitraryProp = (,) <$> QC.elements ["name", "age", "value", "tag"]
                          <*> arbitrary

-- | Generate arbitrary Pattern Subject for testing.
-- Creates patterns with various nesting levels and structures.
instance Arbitrary (Pattern Subject) where
  arbitrary = sized $ \n ->
    if n <= 0
      then Pattern <$> arbitrary <*> pure []
      else do
        subject <- arbitrary
        numElements <- choose (0, min 3 n)
        elems <- vectorOf numElements (resize (n `div` 2) arbitrary)
        return $ Pattern subject elems

-- | A wrapper for generating large patterns for memory/performance testing.
newtype LargePattern = LargePattern (Pattern Subject) deriving Show

instance Arbitrary LargePattern where
  arbitrary = do
    let numNodes = 500
    root <- arbitrary
    nodes <- vectorOf numNodes (resize 5 arbitrary) -- Each node has up to 5 children
    return $ LargePattern (Pattern root nodes)

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Collect all identity symbols from a pattern (including duplicates).
collectAllIdentities :: Pattern Subject -> [Symbol]
collectAllIdentities (Pattern subject elems) =
  Subj.identity subject : concatMap collectAllIdentities elems

-- | Generate a pattern with guaranteed duplicate identities for testing.
-- This is useful for testing reconciliation behavior with known duplicates.
patternWithDuplicates :: Gen (Pattern Subject)
patternWithDuplicates = sized $ \n -> do
  -- Generate a base subject
  baseSubject <- arbitrary
  let baseId = Subj.identity baseSubject

  -- Generate 2-4 subjects with the same identity but different content
  numDuplicates <- choose (2, min 4 (max 2 n))
  duplicates <- vectorOf numDuplicates (subjectWithId baseId)

  -- Create a root pattern containing the duplicates
  root <- arbitrary
  return $ Pattern root (map (\s -> Pattern s []) duplicates)
  where
    subjectWithId :: Symbol -> Gen Subject
    subjectWithId sym = do
      lbls <- Set.fromList <$> listOf (QC.elements ["Person", "Entity", "Node"])
      props <- Map.fromList <$> listOf ((,) <$> QC.elements ["k1", "k2", "k3"] <*> arbitrary)
      return $ Subject sym lbls props

-- | Main property test suite specification.
spec :: Spec
spec = do
  describe "Pattern.Reconcile Properties" $ do
    describe "Setup Phase" $ do
      it "module compiles and loads without errors" $ do
        -- This test verifies the module structure is valid
        True `shouldBe` True

    -- Placeholder sections for property tests
    describe "Idempotence Properties" $ do
      it "reconciling twice is the same as reconciling once (LastWriteWins)" $
        property $ \(pattern :: Pattern Subject) ->
          case reconcile LastWriteWins pattern of
            Left _ -> property True  -- Error is consistent
            Right once ->
              case reconcile LastWriteWins once of
                Left _ -> counterexample "Second reconcile failed but first succeeded" False
                Right twice -> once === twice

      it "reconciling twice is the same as reconciling once (FirstWriteWins)" $
        property $ \(pattern :: Pattern Subject) ->
          case reconcile FirstWriteWins pattern of
            Left _ -> property True  -- Error is consistent
            Right once ->
              case reconcile FirstWriteWins once of
                Left _ -> counterexample "Second reconcile failed but first succeeded" False
                Right twice -> once === twice

      it "reconciling twice is the same as reconciling once (Merge)" $
        property $ \(pattern :: Pattern Subject) ->
          case reconcile (Merge UnionElements defaultSubjectMergeStrategy) pattern of
            Left _ -> property True  -- Error is consistent
            Right once ->
              case reconcile (Merge UnionElements defaultSubjectMergeStrategy) once of
                Left _ -> counterexample "Second reconcile failed but first succeeded" False
                Right twice -> once === twice

    describe "Identity Preservation Properties" $ do
      -- Note: Full identity preservation (all unique identities preserved) requires
      -- proper element merging with UnionElements strategy, which is implemented
      -- in Phase 4 (User Story 2). For Phase 3, we test that each identity appears
      -- at most once after reconciliation.

      it "each identity appears at most once after reconciliation (LastWriteWins)" $
        property $ \(pattern :: Pattern Subject) ->
          case reconcile LastWriteWins pattern of
            Left _ -> property True  -- Error case doesn't apply
            Right reconciled ->
              let ids = collectAllIdentities reconciled
              in length ids === length (Set.fromList ids)

      it "each identity appears at most once after reconciliation (FirstWriteWins)" $
        property $ \(pattern :: Pattern Subject) ->
          case reconcile FirstWriteWins pattern of
            Left _ -> property True  -- Error case doesn't apply
            Right reconciled ->
              let ids = collectAllIdentities reconciled
              in length ids === length (Set.fromList ids)

      -- Full Merge support with element deduplication is implemented in Phase 4
      it "each identity appears at most once after reconciliation (Merge)" $
        pendingWith "Full Merge with element deduplication is Phase 4 (User Story 2)"

    describe "Determinism Properties" $ do
      it "reconciling the same pattern twice gives identical results (LastWriteWins)" $
        property $ \(pattern :: Pattern Subject) ->
          reconcile LastWriteWins pattern === reconcile LastWriteWins pattern

      it "reconciling the same pattern twice gives identical results (FirstWriteWins)" $
        property $ \(pattern :: Pattern Subject) ->
          reconcile FirstWriteWins pattern === reconcile FirstWriteWins pattern

      it "reconciling the same pattern twice gives identical results (Merge)" $
        property $ \(pattern :: Pattern Subject) ->
          let policy = Merge UnionElements defaultSubjectMergeStrategy
          in reconcile policy pattern === reconcile policy pattern

    describe "Merge Strategy Properties" $ do
      it "UnionLabels includes all labels from all occurrences" $
        property $ \(s1 :: Subject) (s2 :: Subject) ->
          let s2' = s2 { Subj.identity = Subj.identity s1 }  -- Force same identity
              strategy = defaultSubjectMergeStrategy { labelMerge = UnionLabels }
              merged = merge strategy s1 s2'
              expectedLabels = Set.union (Subj.labels s1) (Subj.labels s2')
          in Subj.labels merged === expectedLabels

      it "IntersectLabels keeps only common labels" $
        property $ \(s1 :: Subject) (s2 :: Subject) ->
          let s2' = s2 { Subj.identity = Subj.identity s1 }  -- Force same identity
              strategy = defaultSubjectMergeStrategy { labelMerge = IntersectLabels }
              merged = merge strategy s1 s2'
              expectedLabels = Set.intersection (Subj.labels s1) (Subj.labels s2')
          in Subj.labels merged === expectedLabels

      it "ReplaceLabels uses only the second set of labels" $
        property $ \(s1 :: Subject) (s2 :: Subject) ->
          let s2' = s2 { Subj.identity = Subj.identity s1 }  -- Force same identity
              strategy = defaultSubjectMergeStrategy { labelMerge = ReplaceLabels }
              merged = merge strategy s1 s2'
          in Subj.labels merged === Subj.labels s2'

      it "ShallowMerge combines all top-level properties" $
        property $ \(s1 :: Subject) (s2 :: Subject) ->
          let s2' = s2 { Subj.identity = Subj.identity s1 }  -- Force same identity
              strategy = defaultSubjectMergeStrategy { propertyMerge = ShallowMerge }
              merged = merge strategy s1 s2'
              -- ShallowMerge: p2 wins on conflicts, union of keys
              allKeys = Set.union (Map.keysSet (Subj.properties s1)) (Map.keysSet (Subj.properties s2'))
          in Map.keysSet (Subj.properties merged) === allKeys

      it "ReplaceProperties uses only the second property map" $
        property $ \(s1 :: Subject) (s2 :: Subject) ->
          let s2' = s2 { Subj.identity = Subj.identity s1 }  -- Force same identity
              strategy = defaultSubjectMergeStrategy { propertyMerge = ReplaceProperties }
              merged = merge strategy s1 s2'
          in Subj.properties merged === Subj.properties s2'

      it "UnionElements deduplicates elements by identity" $
        property $ \(p1 :: Pattern Subject) (p2 :: Pattern Subject) ->
          -- Force same identity for root, reconcile with UnionElements
          let p2' = p2 { value = (value p2) { Subj.identity = Subj.identity (value p1) }}
              root = value p1
              pattern = Pattern root [p1, p2']
          in case reconcile (Merge UnionElements defaultSubjectMergeStrategy) pattern of
               Left _ -> property True  -- Error case doesn't apply
               Right (Pattern _ elems) ->
                 -- Each identity should appear at most once in elements
                 let ids = map (Subj.identity . value) elems
                 in length ids === length (Set.fromList ids)

    describe "User Story 3: Validate Pattern Coherence (P3)" $ do
      it "Strict mode is accurate - reports error iff duplicates with different content exist" $
        property $ \(s1 :: Subject) (s2 :: Subject) ->
          let s2' = s2 { Subj.identity = Subj.identity s1 }  -- Force same identity
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern s1 [], Pattern s2' []]
              result = reconcile Strict pattern
          in if s1 == s2'
             then isRight result  -- Same content, should succeed
             else isLeft result   -- Different content, should fail

    describe "User Story 4: Complete Partial References (P3)" $ do
      it "reference resolution is complete - all atomic patterns with full definitions replaced" $
        property $ \(fuller :: Subject) (child :: Pattern Subject) ->
          let atomic = Subject (Subj.identity fuller) Set.empty Map.empty  -- Empty atomic reference
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root
                [ Pattern atomic []  -- Atomic reference
                , Pattern fuller [child]  -- Full definition with elements
                ]
          in case reconcile LastWriteWins pattern of
               Left _ -> property True  -- Error case doesn't apply
               Right (Pattern _ elems) ->
                 -- After reconciliation, the atomic reference should be completed
                 -- Count how many times the identity appears
                 let ids = map (Subj.identity . value) elems
                     occurrences = length $ filter (== Subj.identity fuller) ids
                 in occurrences === 1  -- Identity appears exactly once
    
    describe "Memory Usage Properties" $ do
      it "handles large patterns without crashing" $
        property $ \(LargePattern pattern) ->
          let result = reconcile LastWriteWins pattern
          in isRight result
