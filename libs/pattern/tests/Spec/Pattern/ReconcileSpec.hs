{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for Pattern.Reconcile module.
--
-- This module provides HSpec-based unit tests for reconciliation operations.
-- Tests are organized by user story and feature area.
module Spec.Pattern.ReconcileSpec (spec) where

import Test.Hspec
import Pattern.Core (Pattern(..), pattern, point)
import Pattern.Reconcile
import Subject.Core (Subject(..), Symbol(..))
import qualified Subject.Core as Subj
import Subject.Value (Value(..))
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | Main test suite specification.
spec :: Spec
spec = do
  describe "Pattern.Reconcile" $ do
    describe "Setup Phase" $ do
      it "module compiles and loads without errors" $ do
        -- This test verifies the module structure is valid
        True `shouldBe` True

    -- Placeholder sections for implementation phases
    describe "User Story 1: Normalize Parsed Patterns (P1)" $ do
      describe "LastWriteWins Policy" $ do
        it "keeps the last occurrence when identities have different properties" $ do
          let alice1 = Subject (Symbol "alice") (Set.singleton "Person")
                               (Map.singleton "age" (VInteger 30))
              alice2 = Subject (Symbol "alice") (Set.singleton "Person")
                               (Map.singleton "age" (VInteger 31))
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]

          case reconcile LastWriteWins pattern of
            Right (Pattern _ elems) -> do
              length elems `shouldBe` 1
              case elems of
                [Pattern subj _] -> do
                  Subj.identity subj `shouldBe` Symbol "alice"
                  Subj.properties subj `shouldBe` Map.singleton "age" (VInteger 31)
                _ -> expectationFailure "Expected single element"
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

      describe "FirstWriteWins Policy" $ do
        it "keeps the first occurrence when identities have different properties" $ do
          let alice1 = Subject (Symbol "alice") (Set.singleton "Person")
                               (Map.singleton "age" (VInteger 30))
              alice2 = Subject (Symbol "alice") (Set.singleton "Person")
                               (Map.singleton "age" (VInteger 31))
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]

          case reconcile FirstWriteWins pattern of
            Right (Pattern _ elems) -> do
              length elems `shouldBe` 1
              case elems of
                [Pattern subj _] -> do
                  Subj.identity subj `shouldBe` Symbol "alice"
                  Subj.properties subj `shouldBe` Map.singleton "age" (VInteger 30)
                _ -> expectationFailure "Expected single element"
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

      describe "Pattern without duplicates" $ do
        it "returns pattern unchanged when no duplicate identities exist" $ do
          let alice = Subject (Symbol "alice") (Set.singleton "Person") Map.empty
              bob = Subject (Symbol "bob") (Set.singleton "Person") Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice [], Pattern bob []]

          case reconcile LastWriteWins pattern of
            Right reconciled -> reconciled `shouldBe` pattern
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

      describe "Empty pattern" $ do
        it "returns empty pattern unchanged" $ do
          let root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root []

          case reconcile LastWriteWins pattern of
            Right reconciled -> reconciled `shouldBe` pattern
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

      describe "Nested duplicates" $ do
        it "handles deeply nested duplicate identities correctly" $ do
          -- Regression test: ensure that when rebuilding, visited IDs from
          -- nested descendants are properly tracked so siblings don't re-emit them
          let b1 = Subject (Symbol "b") (Set.singleton "Entity") (Map.singleton "age" (VInteger 30))
              b2 = Subject (Symbol "b") Set.empty Map.empty
              node3 = Subject (Symbol "node3") Set.empty Map.empty
              bob = Subject (Symbol "bob") Set.empty Map.empty
              alice = Subject (Symbol "alice") Set.empty Map.empty

              collectAllIds (Pattern subj elems) =
                Subj.identity subj : concatMap collectAllIds elems

              pattern = Pattern alice
                [ Pattern bob
                  [ Pattern b1 [Pattern node3 [Pattern b2 []]]
                  ]
                ]

          case reconcile LastWriteWins pattern of
            Right reconciled -> do
              let ids = collectAllIds reconciled
              -- Each ID should appear exactly once
              length ids `shouldBe` length (Set.fromList ids)
              Set.fromList ids `shouldBe` Set.fromList [Symbol "alice", Symbol "bob", Symbol "b", Symbol "node3"]
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

    describe "User Story 2: Merge Patterns from Multiple Sources (P2)" $ do
      describe "Label Merge Strategies" $ do
        it "UnionLabels combines all labels from all occurrences" $ do
          let alice1 = Subject (Symbol "alice") (Set.fromList ["Person", "User"]) Map.empty
              alice2 = Subject (Symbol "alice") (Set.fromList ["Employee", "User"]) Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]
              strategy = defaultMergeStrategy { labelMerge = UnionLabels }

          case reconcile (Merge strategy) pattern of
            Right (Pattern _ [Pattern subj _]) ->
              Subj.labels subj `shouldBe` Set.fromList ["Person", "User", "Employee"]
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

        it "IntersectLabels keeps only common labels" $ do
          let alice1 = Subject (Symbol "alice") (Set.fromList ["Person", "User"]) Map.empty
              alice2 = Subject (Symbol "alice") (Set.fromList ["Employee", "User"]) Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]
              strategy = defaultMergeStrategy { labelMerge = IntersectLabels }

          case reconcile (Merge strategy) pattern of
            Right (Pattern _ [Pattern subj _]) ->
              Subj.labels subj `shouldBe` Set.singleton "User"
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

        it "ReplaceLabels uses later labels" $ do
          let alice1 = Subject (Symbol "alice") (Set.fromList ["Person", "User"]) Map.empty
              alice2 = Subject (Symbol "alice") (Set.fromList ["Employee"]) Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]
              strategy = defaultMergeStrategy { labelMerge = ReplaceLabels }

          case reconcile (Merge strategy) pattern of
            Right (Pattern _ [Pattern subj _]) ->
              Subj.labels subj `shouldBe` Set.singleton "Employee"
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

      describe "Property Merge Strategies" $ do
        it "ShallowMerge merges top-level property keys" $ do
          let alice1 = Subject (Symbol "alice") Set.empty
                               (Map.fromList [("name", VString "Alice"), ("age", VInteger 30)])
              alice2 = Subject (Symbol "alice") Set.empty
                               (Map.fromList [("age", VInteger 31), ("role", VString "Engineer")])
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]
              strategy = defaultMergeStrategy { propertyMerge = ShallowMerge }

          case reconcile (Merge strategy) pattern of
            Right (Pattern _ [Pattern subj _]) -> do
              -- Should have all keys, with alice2's values winning on conflicts
              Map.keys (Subj.properties subj) `shouldMatchList` ["name", "age", "role"]
              Map.lookup "age" (Subj.properties subj) `shouldBe` Just (VInteger 31)  -- alice2 wins
              Map.lookup "name" (Subj.properties subj) `shouldBe` Just (VString "Alice")
              Map.lookup "role" (Subj.properties subj) `shouldBe` Just (VString "Engineer")
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

        it "ReplaceProperties completely replaces properties" $ do
          let alice1 = Subject (Symbol "alice") Set.empty
                               (Map.fromList [("name", VString "Alice"), ("age", VInteger 30)])
              alice2 = Subject (Symbol "alice") Set.empty
                               (Map.singleton "role" (VString "Engineer"))
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]
              strategy = defaultMergeStrategy { propertyMerge = ReplaceProperties }

          case reconcile (Merge strategy) pattern of
            Right (Pattern _ [Pattern subj _]) ->
              Subj.properties subj `shouldBe` Map.singleton "role" (VString "Engineer")
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

        it "DeepMerge recursively merges nested maps" $ do
          -- Note: Current implementation treats DeepMerge as ShallowMerge for non-map values
          -- Full recursive deep merge of Value maps would require Value-specific logic
          let alice1 = Subject (Symbol "alice") Set.empty
                               (Map.fromList [("name", VString "Alice"), ("age", VInteger 30)])
              alice2 = Subject (Symbol "alice") Set.empty
                               (Map.fromList [("age", VInteger 31), ("role", VString "Engineer")])
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]
              strategy = defaultMergeStrategy { propertyMerge = DeepMerge }

          case reconcile (Merge strategy) pattern of
            Right (Pattern _ [Pattern subj _]) -> do
              -- Should merge properties (current implementation: later wins on conflicts)
              Map.keys (Subj.properties subj) `shouldMatchList` ["name", "age", "role"]
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

      describe "Element Merge Strategies" $ do
        it "UnionElements deduplicates elements by identity" $
          pendingWith "Element merging not yet implemented"

        it "AppendElements concatenates all element lists" $
          pendingWith "Element merging not yet implemented"

        it "ReplaceElements uses later element list" $
          pendingWith "Element merging not yet implemented"

    describe "User Story 3: Validate Pattern Coherence (P3)" $ do
      it "placeholder - tests will be added during implementation" $
        pendingWith "US3 implementation pending"

    describe "User Story 4: Complete Partial References (P3)" $ do
      it "placeholder - tests will be added during implementation" $
        pendingWith "US4 implementation pending"

    describe "User Story 5: Track Reconciliation Actions (P4)" $ do
      it "placeholder - tests will be added during implementation" $
        pendingWith "US5 implementation pending"
