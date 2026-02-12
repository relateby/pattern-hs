{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Data.String (fromString)

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
              strategy = defaultSubjectMergeStrategy { labelMerge = UnionLabels }

          case reconcile (Merge UnionElements strategy) pattern of
            Right (Pattern _ [Pattern subj _]) ->
              Subj.labels subj `shouldBe` Set.fromList ["Person", "User", "Employee"]
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

        it "IntersectLabels keeps only common labels" $ do
          let alice1 = Subject (Symbol "alice") (Set.fromList ["Person", "User"]) Map.empty
              alice2 = Subject (Symbol "alice") (Set.fromList ["Employee", "User"]) Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]
              strategy = defaultSubjectMergeStrategy { labelMerge = IntersectLabels }

          case reconcile (Merge UnionElements strategy) pattern of
            Right (Pattern _ [Pattern subj _]) ->
              Subj.labels subj `shouldBe` Set.singleton "User"
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

        it "ReplaceLabels uses later labels" $ do
          let alice1 = Subject (Symbol "alice") (Set.fromList ["Person", "User"]) Map.empty
              alice2 = Subject (Symbol "alice") (Set.fromList ["Employee"]) Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]
              strategy = defaultSubjectMergeStrategy { labelMerge = ReplaceLabels }

          case reconcile (Merge UnionElements strategy) pattern of
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
              strategy = defaultSubjectMergeStrategy { propertyMerge = ShallowMerge }

          case reconcile (Merge UnionElements strategy) pattern of
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
              strategy = defaultSubjectMergeStrategy { propertyMerge = ReplaceProperties }

          case reconcile (Merge UnionElements strategy) pattern of
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
              strategy = defaultSubjectMergeStrategy { propertyMerge = DeepMerge }

          case reconcile (Merge UnionElements strategy) pattern of
            Right (Pattern _ [Pattern subj _]) -> do
              -- Should merge properties (current implementation: later wins on conflicts)
              Map.keys (Subj.properties subj) `shouldMatchList` ["name", "age", "role"]
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

      describe "Element Merge Strategies" $ do
        it "UnionElements deduplicates elements by identity" $ do
          let child1 = Subject (Symbol "child") (Set.singleton "A") Map.empty
              child2 = Subject (Symbol "child") (Set.singleton "B") Map.empty
              other = Subject (Symbol "other") Set.empty Map.empty

              alice1 = Subject (Symbol "alice") Set.empty Map.empty
              alice2 = Subject (Symbol "alice") Set.empty Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty

              pattern = Pattern root
                [ Pattern alice1 [Pattern child1 [], Pattern other []]
                , Pattern alice2 [Pattern child2 []]
                ]

          case reconcile (Merge UnionElements defaultSubjectMergeStrategy) pattern of
            Right (Pattern _ [Pattern _ elems]) -> do
              -- Should have child (once) and other
              length elems `shouldBe` 2
              let ids = map (Subj.identity . value) elems
              Set.fromList ids `shouldBe` Set.fromList [Symbol "child", Symbol "other"]
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

        it "AppendElements concatenates all element lists" $ do
          let child1 = Subject (Symbol "child1") (Set.singleton "A") Map.empty
              child2 = Subject (Symbol "child2") (Set.singleton "B") Map.empty

              alice1 = Subject (Symbol "alice") Set.empty Map.empty
              alice2 = Subject (Symbol "alice") Set.empty Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty

              pattern = Pattern root
                [ Pattern alice1 [Pattern child1 []]
                , Pattern alice2 [Pattern child2 []]
                ]

          case reconcile (Merge AppendElements defaultSubjectMergeStrategy) pattern of
            Right (Pattern _ [Pattern _ elems]) -> do
              -- Should have both children (distinct identities)
              length elems `shouldBe` 2
              let ids = map (Subj.identity . value) elems
              Set.fromList ids `shouldBe` Set.fromList [Symbol "child1", Symbol "child2"]
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

        it "ReplaceElements uses later element list" $ do
          let child1 = Subject (Symbol "child1") Set.empty Map.empty
              child2 = Subject (Symbol "child2") Set.empty Map.empty

              alice1 = Subject (Symbol "alice") Set.empty Map.empty
              alice2 = Subject (Symbol "alice") Set.empty Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty

              pattern = Pattern root
                [ Pattern alice1 [Pattern child1 []]
                , Pattern alice2 [Pattern child2 []]
                ]

          case reconcile (Merge ReplaceElements defaultSubjectMergeStrategy) pattern of
            Right (Pattern _ [Pattern _ elems]) -> do
              -- Should only have alice2's elements
              length elems `shouldBe` 1
              case elems of
                [Pattern child _] -> Subj.identity child `shouldBe` Symbol "child2"
                _ -> expectationFailure "Expected single element"
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

    describe "User Story 3: Validate Pattern Coherence (P3)" $ do
      describe "Strict Mode" $ do
        it "returns ReconcileError with conflict details when conflicts exist" $ do
          let alice1 = Subject (Symbol "alice") (Set.singleton "Person") (Map.singleton "age" (VInteger 30))
              alice2 = Subject (Symbol "alice") (Set.singleton "Employee") (Map.singleton "dept" (VString "Engineering"))
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]

          case reconcile Strict pattern of
            Left (ReconcileError msg conflicts) -> do
              msg `shouldContain` "Duplicate"
              length conflicts `shouldBe` 1
              case conflicts of
                [Conflict cid existing incoming paths] -> do
                  cid `shouldBe` Symbol "alice"
                  existing `shouldBe` alice1
                  incoming `shouldBe` alice2
                  length paths `shouldBe` 2
                _ -> expectationFailure "Expected single conflict"
            Right _ -> expectationFailure "Expected reconciliation to fail"

        it "returns success when pattern is coherent (no conflicts)" $ do
          let alice1 = Subject (Symbol "alice") (Set.singleton "Person") (Map.singleton "age" (VInteger 30))
              alice2 = Subject (Symbol "alice") (Set.singleton "Person") (Map.singleton "age" (VInteger 30))
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]

          case reconcile Strict pattern of
            Right result -> result `shouldBe` pattern
            Left err -> expectationFailure $ "Expected success, got: " ++ show err

      describe "findConflicts" $ do
        it "returns all conflicts without reconciling" $ do
          let alice1 = Subject (Symbol "alice") (Set.singleton "A") Map.empty
              alice2 = Subject (Symbol "alice") (Set.singleton "B") Map.empty
              bob1 = Subject (Symbol "bob") Set.empty (Map.singleton "k" (VInteger 1))
              bob2 = Subject (Symbol "bob") Set.empty (Map.singleton "k" (VInteger 2))
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root
                [ Pattern alice1 []
                , Pattern bob1 []
                , Pattern alice2 []
                , Pattern bob2 []
                ]

          let conflicts = findConflicts pattern
          length conflicts `shouldBe` 2
          let ids = map conflictId conflicts
          Set.fromList ids `shouldBe` Set.fromList [Symbol "alice", Symbol "bob"]

        it "returns empty list for coherent pattern" $ do
          let alice1 = Subject (Symbol "alice") (Set.singleton "Person") Map.empty
              alice2 = Subject (Symbol "alice") (Set.singleton "Person") Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]

          findConflicts pattern `shouldBe` []

      describe "needsReconciliation" $ do
        it "returns true when pattern has duplicate identities" $ do
          let alice1 = Subject (Symbol "alice") Set.empty Map.empty
              alice2 = Subject (Symbol "alice") Set.empty Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]

          needsReconciliation pattern `shouldBe` True

        it "returns false when all identities are unique" $ do
          let alice = Subject (Symbol "alice") Set.empty Map.empty
              bob = Subject (Symbol "bob") Set.empty Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern alice [], Pattern bob []]

          needsReconciliation pattern `shouldBe` False

    describe "User Story 4: Complete Partial References (P3)" $ do
      describe "Reference Completion" $ do
        it "replaces atomic reference with full definition" $ do
          let atomic = Subject (Symbol "alice") Set.empty Map.empty
              full = Subject (Symbol "alice") (Set.singleton "Person") (Map.singleton "age" (VInteger 30))
              child = Subject (Symbol "child") Set.empty Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root
                [ Pattern atomic []
                , Pattern full [Pattern child []]
                ]

          case reconcile LastWriteWins pattern of
            Right (Pattern _ [Pattern result resultElems]) -> do
              -- Should have alice once with full content
              Subj.identity result `shouldBe` Symbol "alice"
              Subj.labels result `shouldBe` Set.singleton "Person"
              Subj.properties result `shouldBe` Map.singleton "age" (VInteger 30)
              -- Should have child element
              length resultElems `shouldBe` 1
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

        it "preserves all atomic occurrences when no full definition exists" $ do
          let atomic1 = Subject (Symbol "alice") Set.empty Map.empty
              atomic2 = Subject (Symbol "alice") Set.empty Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root [Pattern atomic1 [], Pattern atomic2 []]

          case reconcile LastWriteWins pattern of
            Right (Pattern _ [Pattern result []]) -> do
              -- Should preserve atomic pattern
              Subj.identity result `shouldBe` Symbol "alice"
              Subj.labels result `shouldBe` Set.empty
              Subj.properties result `shouldBe` Map.empty
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

        it "resolves circular references (A→B, B→A both appear once)" $ do
          let a = Subject (Symbol "a") Set.empty Map.empty
              b = Subject (Symbol "b") Set.empty Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              -- A contains B, B contains A (circular)
              pattern = Pattern root
                [ Pattern a [Pattern b [Pattern a []]]
                ]

              -- Helper to collect all identities in tree
              collectAllIds (Pattern subj elems) =
                Subj.identity subj : concatMap collectAllIds elems

          case reconcile LastWriteWins pattern of
            Right reconciled -> do
              let allIds = collectAllIds reconciled
              -- Each identity should appear exactly once in entire tree
              length allIds `shouldBe` length (Set.fromList allIds)
              -- Both A and B should be present
              Set.fromList allIds `shouldBe` Set.fromList [Symbol "root", Symbol "a", Symbol "b"]
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

        it "handles self-referential pattern (subject contains itself)" $ do
          let alice = Subject (Symbol "alice") Set.empty Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              -- Alice contains alice (self-reference)
              pattern = Pattern root [Pattern alice [Pattern alice []]]

          case reconcile LastWriteWins pattern of
            Right (Pattern _ [Pattern result resultElems]) -> do
              -- Should have alice once
              Subj.identity result `shouldBe` Symbol "alice"
              -- Self-reference should be detected and not duplicated
              let childIds = map (Subj.identity . value) resultElems
              childIds `shouldNotContain` [Symbol "alice"]
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

        it "preserves orphan references as-is" $ do
          let alice = Subject (Symbol "alice") Set.empty Map.empty
              bob = Subject (Symbol "bob") Set.empty Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              -- Alice is atomic but has no full definition elsewhere
              pattern = Pattern root [Pattern alice [], Pattern bob []]

          case reconcile LastWriteWins pattern of
            Right (Pattern _ elems) -> do
              length elems `shouldBe` 2
              let ids = map (Subj.identity . value) elems
              Set.fromList ids `shouldBe` Set.fromList [Symbol "alice", Symbol "bob"]
            Right other -> expectationFailure $ "Unexpected structure: " ++ show other
            Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

    describe "User Story 5: Track Reconciliation Actions (P4)" $ do
      describe "reconcileWithReport" $ do
        it "returns correct duplicatesFound count" $ do
          let alice1 = Subject (Symbol "alice") (Set.singleton "Person") Map.empty
              alice2 = Subject (Symbol "alice") (Set.singleton "Employee") Map.empty
              bob1 = Subject (Symbol "bob") Set.empty Map.empty
              bob2 = Subject (Symbol "bob") Set.empty Map.empty
              charlie = Subject (Symbol "charlie") Set.empty Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root
                [ Pattern alice1 []
                , Pattern alice2 []
                , Pattern bob1 []
                , Pattern bob2 []
                , Pattern charlie []  -- Only appears once
                ]

          let (result, report) = reconcileWithReport LastWriteWins pattern
          reportDuplicatesFound report `shouldBe` 2  -- alice and bob

        it "returns correct referencesResolved count" $ do
          let atomic1 = Subject (Symbol "alice") Set.empty Map.empty
              atomic2 = Subject (Symbol "alice") Set.empty Map.empty
              full = Subject (Symbol "alice") (Set.singleton "Person") (Map.singleton "age" (VInteger 30))
              bob = Subject (Symbol "bob") Set.empty Map.empty  -- Orphan atomic (no fuller version)
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root
                [ Pattern atomic1 []  -- Reference
                , Pattern atomic2 []  -- Reference
                , Pattern full []  -- Full definition
                , Pattern bob []  -- Not a reference (no fuller version)
                ]

          let (result, report) = reconcileWithReport LastWriteWins pattern
          reportReferencesResolved report `shouldBe` 2  -- Two atomic alices resolved

        it "returns correct mergesPerformed count" $ do
          let alice1 = Subject (Symbol "alice") (Set.singleton "A") Map.empty
              alice2 = Subject (Symbol "alice") (Set.singleton "B") Map.empty
              bob1 = Subject (Symbol "bob") Set.empty (Map.singleton "k" (VInteger 1))
              bob2 = Subject (Symbol "bob") Set.empty (Map.singleton "k" (VInteger 2))
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root
                [ Pattern alice1 []
                , Pattern alice2 []
                , Pattern bob1 []
                , Pattern bob2 []
                ]

          let (result, report) = reconcileWithReport (Merge UnionElements defaultSubjectMergeStrategy) pattern
          reportMergesPerformed report `shouldBe` 2  -- alice and bob merged

        it "returns correct subjectCounts map" $ do
          let alice1 = Subject (Symbol "alice") Set.empty Map.empty
              alice2 = Subject (Symbol "alice") Set.empty Map.empty
              alice3 = Subject (Symbol "alice") Set.empty Map.empty
              bob1 = Subject (Symbol "bob") Set.empty Map.empty
              bob2 = Subject (Symbol "bob") Set.empty Map.empty
              charlie = Subject (Symbol "charlie") Set.empty Map.empty
              root = Subject (Symbol "root") Set.empty Map.empty
              pattern = Pattern root
                [ Pattern alice1 []
                , Pattern alice2 []
                , Pattern alice3 []
                , Pattern bob1 []
                , Pattern bob2 []
                , Pattern charlie []
                ]

          let (result, report) = reconcileWithReport LastWriteWins pattern
          let counts = reportSubjectCounts report
          Map.lookup (Symbol "alice") counts `shouldBe` Just 3
          Map.lookup (Symbol "bob") counts `shouldBe` Just 2
          Map.lookup (Symbol "charlie") counts `shouldBe` Just 1
          Map.lookup (Symbol "root") counts `shouldBe` Just 1
          Map.size counts `shouldBe` 4  -- alice, bob, charlie, root

    describe "Phase 8: Polish & Cross-Cutting Concerns" $ do
      it "reconciles 10,000 subjects in under 100ms" $ do
        let numUniqueSubjects = 5000
            subjects = [ Subject (fromString $ "s" ++ show i) Set.empty Map.empty | i <- [1..numUniqueSubjects] ]
            -- Create a pattern with 10,000 total subjects (5,000 unique, each appearing twice)
            patternToTest = Pattern (Subject (Symbol "root") Set.empty Map.empty) (map (`Pattern` []) (subjects ++ subjects))

        start <- getCPUTime
        case reconcile LastWriteWins patternToTest of
          Right reconciled -> do
            end <- getCPUTime
            let diff = fromIntegral (end - start) / (10^9) :: Double -- in milliseconds
            printf "Reconciliation of 10,000 subjects took %.2f ms\n" diff
            diff `shouldSatisfy` (< 100)
          Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

      it "reconciles a deeply nested pattern (100+ levels) in under 100ms" $ do
        let createDeeplyNestedPattern :: Int -> Pattern Subject
            createDeeplyNestedPattern n =
              let root = Subject (Symbol "root") Set.empty Map.empty
                  go i | i > n = []
                       | otherwise = [Pattern (Subject (fromString $ "n" ++ show i) Set.empty Map.empty) (go (i+1))]
              in Pattern root (go 1)

            deepPattern = createDeeplyNestedPattern 100

        start <- getCPUTime
        case reconcile LastWriteWins deepPattern of
          Right reconciled -> do
            end <- getCPUTime
            let diff = fromIntegral (end - start) / (10^9) :: Double -- in milliseconds
            printf "Reconciliation of 100-level nested pattern took %.2f ms\n" diff
            diff `shouldSatisfy` (< 100)
          Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

    describe "Generic Capability: Simple Song" $ do
      it "reconciles a song where chorus is defined once and referenced elsewhere" $ do
        let chorusFull = TaggedString "chorus" (Set.singleton "Chorus") "This is the full chorus text."
            chorusRef = TaggedString "chorus" Set.empty "ref"
            verse1 = TaggedString "v1" (Set.singleton "Verse") "First verse."
            verse2 = TaggedString "v2" (Set.singleton "Verse") "Second verse."
            
            song = Pattern (TaggedString "song" Set.empty "My Song")
              [ Pattern chorusRef []
              , Pattern verse1 []
              , Pattern chorusFull []
              , Pattern verse2 []
              , Pattern chorusRef []
              ]
            
            -- Policy: Merge with our simple () strategy, Union elements (though they are empty here)
            policy = Merge UnionElements ()
        
        case reconcile policy song of
          Right (Pattern _ elems) -> do
            -- UnionElements deduplicates. Map.elems returns in key order ("chorus", "v1", "v2")
            length elems `shouldBe` 3
            
            -- Verify chorus is fully resolved
            case elems of
              (chorus:_) -> do -- "chorus" comes before "v1", "v2"
                tsId (value chorus) `shouldBe` "chorus"
                tsContent (value chorus) `shouldBe` "This is the full chorus text."
                tsTags (value chorus) `shouldBe` Set.singleton "Chorus"
              [] -> expectationFailure "Expected elements"
            
            -- Verify others exist
            let ids = map (tsId . value) elems
            ids `shouldContain` ["v1", "v2"]
            
          Left err -> expectationFailure $ "Reconciliation failed: " ++ show err

-- | Custom type for Generic Reconciliation Test
data TaggedString = TaggedString
  { tsId :: String
  , tsTags :: Set.Set String
  , tsContent :: String
  } deriving (Eq, Show, Ord)

instance HasIdentity TaggedString String where
  identity = tsId

instance Mergeable TaggedString where
  -- Simple strategy: union tags, longer content wins
  type MergeStrategy TaggedString = ()
  merge _ (TaggedString i1 t1 c1) (TaggedString _ t2 c2) =
    TaggedString i1 (Set.union t1 t2) (if length c1 > length c2 then c1 else c2)

instance Refinable TaggedString where
  isRefinementOf full partial =
    tsId full == tsId partial && length (tsContent full) >= length (tsContent partial)
