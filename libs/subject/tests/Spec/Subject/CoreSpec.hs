-- | Unit tests for Subject.Core module.
module Spec.Subject.CoreSpec where

import Data.Hashable (hash, hashWithSalt)
import Data.Map (Map, empty, fromList, toList, (!?))
import Data.Monoid (mconcat, mempty)
import Data.Semigroup (sconcat, stimes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Hspec
import Subject.Core (Symbol (..), PropertyRecord, Subject (..))
import Subject.Construction (addProperty, hasProperty, removeProperty, subject, subjectWith, updateProperty)
import Subject.Value (Value (..), RangeValue (..))

spec :: Spec
spec = do
  describe "Subject.Core" $ do
    
    describe "Subject Construction" $ do
      
      describe "Creating subjects with different components" $ do
        
        it "creates a subject with identity, labels, and properties" $ do
          let s = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
          identity s `shouldBe` Symbol "n"
          labels s `shouldBe` Set.fromList ["Person"]
          properties s `shouldBe` fromList [("name", VString "Alice")]
        
        it "creates a subject with only identity" $ do
          let s = Subject (Symbol "n") Set.empty empty
          identity s `shouldBe` Symbol "n"
          labels s `shouldBe` Set.empty
          properties s `shouldBe` empty
        
        it "creates a subject with only labels" $ do
          let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
          identity s `shouldBe` Symbol "n"
          labels s `shouldBe` Set.fromList ["Person"]
          properties s `shouldBe` empty
        
        it "creates a subject with only properties" $ do
          let s = Subject (Symbol "n") Set.empty (fromList [("name", VString "Alice")])
          identity s `shouldBe` Symbol "n"
          labels s `shouldBe` Set.empty
          properties s `shouldBe` fromList [("name", VString "Alice")]
        
        it "creates an empty subject" $ do
          let s = Subject (Symbol "") Set.empty empty
          identity s `shouldBe` Symbol ""
          labels s `shouldBe` Set.empty
          properties s `shouldBe` empty
      
      describe "Field accessors" $ do
        
        it "returns the correct identity" $ do
          let s = Subject (Symbol "n") Set.empty empty
          identity s `shouldBe` Symbol "n"
        
        it "returns the correct labels" $ do
          let s = Subject (Symbol "n") (Set.fromList ["Person", "Employee"]) empty
          labels s `shouldBe` Set.fromList ["Person", "Employee"]
        
        it "returns the correct properties" $ do
          let s = Subject (Symbol "n") Set.empty (fromList [("name", VString "Alice"), ("age", VInteger 30)])
          properties s `shouldBe` fromList [("name", VString "Alice"), ("age", VInteger 30)]
    
    describe "Symbol" $ do
      
      describe "Symbol creation" $ do
        
        it "creates a Symbol" $ do
          let sym = Symbol "n"
          sym `shouldBe` Symbol "n"
        
        it "creates a Symbol from string" $ do
          let sym = Symbol "myId"
          sym `shouldBe` Symbol "myId"
        
        it "creates a Symbol from number string" $ do
          let sym = Symbol "42"
          sym `shouldBe` Symbol "42"
    
    describe "Typeclass Instances" $ do
      
      describe "Eq instance" $ do
        
        it "equal subjects are equal" $ do
          let s1 = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
          let s2 = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
          s1 `shouldBe` s2
        
        it "subjects with different identities are not equal" $ do
          let s1 = Subject (Symbol "n") (Set.fromList ["Person"]) empty
          let s2 = Subject (Symbol "m") (Set.fromList ["Person"]) empty
          s1 `shouldNotBe` s2
        
        it "subjects with different labels are not equal" $ do
          let s1 = Subject (Symbol "n") (Set.fromList ["Person"]) empty
          let s2 = Subject (Symbol "n") (Set.fromList ["Employee"]) empty
          s1 `shouldNotBe` s2
        
        it "subjects with different properties are not equal" $ do
          let s1 = Subject (Symbol "n") Set.empty (fromList [("name", VString "Alice")])
          let s2 = Subject (Symbol "n") Set.empty (fromList [("name", VString "Bob")])
          s1 `shouldNotBe` s2
      
      describe "Ord instance" $ do
        
        it "orders subjects lexicographically" $ do
          let s1 = Subject (Symbol "a") Set.empty empty
          let s2 = Subject (Symbol "b") Set.empty empty
          s1 `shouldBe` (min s1 s2)
          s2 `shouldBe` (max s1 s2)
        
        it "orders subjects by identity first, then labels, then properties" $ do
          let s1 = Subject (Symbol "a") (Set.fromList ["A"]) empty
          let s2 = Subject (Symbol "b") (Set.fromList ["A"]) empty
          s1 < s2 `shouldBe` True
      
      describe "Show instance" $ do
        
        it "shows subject with all components" $ do
          let s = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
          show s `shouldContain` "Subject"
          show s `shouldContain` "Symbol"
          show s `shouldContain` "Person"
          show s `shouldContain` "name"
      
      describe "Hashable instance" $ do
        
        it "hashes subjects consistently" $ do
          let s = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
          hash s `shouldBe` hash s
        
        it "hashes equal subjects to the same value" $ do
          let s1 = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
          let s2 = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
          hash s1 `shouldBe` hash s2
        
        it "hashes with custom salt" $ do
          let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
          hashWithSalt 42 s `shouldBe` hashWithSalt 42 s
      
      describe "Semigroup instance" $ do
        
        it "combines subjects by merging components" $ do
          let s1 = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
          let s2 = Subject (Symbol "m") (Set.fromList ["Employee"]) (fromList [("age", VInteger 30)])
          let combined = s1 <> s2
          identity combined `shouldBe` Symbol "n"
          labels combined `shouldBe` Set.fromList ["Person", "Employee"]
          properties combined `shouldBe` fromList [("name", VString "Alice"), ("age", VInteger 30)]
        
        it "is associative" $ do
          let s1 = Subject (Symbol "a") (Set.fromList ["A"]) empty
          let s2 = Subject (Symbol "b") (Set.fromList ["B"]) empty
          let s3 = Subject (Symbol "c") (Set.fromList ["C"]) empty
          (s1 <> s2) <> s3 `shouldBe` s1 <> (s2 <> s3)
        
        it "takes first identity when combining" $ do
          let s1 = Subject (Symbol "a") Set.empty empty
          let s2 = Subject (Symbol "b") Set.empty empty
          identity (s1 <> s2) `shouldBe` Symbol "a"
      
      describe "Monoid instance" $ do
        
        it "provides empty subject as identity" $ do
          let s = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
          mempty <> s `shouldBe` s
          s <> mempty `shouldBe` s
        
        it "empty subject has default identity" $ do
          let emptySubj = mempty :: Subject
          identity emptySubj `shouldBe` Symbol ""
          labels emptySubj `shouldBe` Set.empty
          properties emptySubj `shouldBe` empty
        
        it "mconcat combines multiple subjects" $ do
          let s1 = Subject (Symbol "a") (Set.fromList ["A"]) empty
          let s2 = Subject (Symbol "b") (Set.fromList ["B"]) empty
          let s3 = Subject (Symbol "c") (Set.fromList ["C"]) empty
          labels (mconcat [s1, s2, s3]) `shouldBe` Set.fromList ["A", "B", "C"]
    
    describe "Constructor Functions" $ do
      
      describe "subject function" $ do
        
        it "creates an empty subject" $ do
          let s = subject
          identity s `shouldBe` Symbol ""
          labels s `shouldBe` Set.empty
          properties s `shouldBe` empty
        
        it "is equivalent to mempty" $ do
          subject `shouldBe` (mempty :: Subject)
      
      describe "subjectWith function" $ do
        
        it "creates a subject with all components" $ do
          let s = subjectWith (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
          identity s `shouldBe` Symbol "n"
          labels s `shouldBe` Set.fromList ["Person"]
          properties s `shouldBe` fromList [("name", VString "Alice")]
        
        it "creates a subject with only some components" $ do
          let s = subjectWith (Symbol "n") (Set.fromList ["Person"]) empty
          identity s `shouldBe` Symbol "n"
          labels s `shouldBe` Set.fromList ["Person"]
          properties s `shouldBe` empty
    
    describe "Property Manipulation" $ do
      
      describe "addProperty" $ do
        
        it "adds a new property" $ do
          let s = subject
          let s' = addProperty "name" (VString "Alice") s
          hasProperty "name" s' `shouldBe` True
          properties s' !? "name" `shouldBe` Just (VString "Alice")
        
        it "replaces an existing property" $ do
          let s = subject { properties = fromList [("name", VString "Bob")] }
          let s' = addProperty "name" (VString "Alice") s
          properties s' !? "name" `shouldBe` Just (VString "Alice")
      
      describe "updateProperty" $ do
        
        it "updates an existing property" $ do
          let s = subject { properties = fromList [("name", VString "Bob")] }
          let s' = updateProperty "name" (VString "Alice") s
          properties s' !? "name" `shouldBe` Just (VString "Alice")
        
        it "does not add a non-existent property" $ do
          let s = subject
          let s' = updateProperty "name" (VString "Alice") s
          hasProperty "name" s' `shouldBe` False
      
      describe "removeProperty" $ do
        
        it "removes an existing property" $ do
          let s = subject { properties = fromList [("name", VString "Alice"), ("age", VInteger 30)] }
          let s' = removeProperty "age" s
          hasProperty "age" s' `shouldBe` False
          hasProperty "name" s' `shouldBe` True
        
        it "does nothing for non-existent property" $ do
          let s = subject { properties = fromList [("name", VString "Alice")] }
          let s' = removeProperty "age" s
          s' `shouldBe` s
      
      describe "hasProperty" $ do
        
        it "returns True for existing property" $ do
          let s = subject { properties = fromList [("name", VString "Alice")] }
          hasProperty "name" s `shouldBe` True
        
        it "returns False for non-existent property" $ do
          let s = subject
          hasProperty "name" s `shouldBe` False
    
    describe "Edge Cases" $ do
      
      describe "Empty subject" $ do
        
        it "empty subject has default identity" $ do
          let s = Subject (Symbol "") Set.empty empty
          identity s `shouldBe` Symbol ""
          labels s `shouldBe` Set.empty
          properties s `shouldBe` empty
        
        it "empty subject is equal to mempty" $ do
          let s = Subject (Symbol "") Set.empty empty
          s `shouldBe` (mempty :: Subject)
      
      describe "Subject with only identity" $ do
        
        it "subject with only identity works correctly" $ do
          let s = Subject (Symbol "n") Set.empty empty
          identity s `shouldBe` Symbol "n"
          labels s `shouldBe` Set.empty
          properties s `shouldBe` empty
      
      describe "Subject with multiple labels" $ do
        
        it "subject with multiple labels works correctly" $ do
          let s = Subject (Symbol "n") (Set.fromList ["Person", "Employee", "Manager"]) empty
          labels s `shouldBe` Set.fromList ["Person", "Employee", "Manager"]
      
      describe "Subject with multiple properties" $ do
        
        it "subject with multiple properties works correctly" $ do
          let s = Subject (Symbol "n") Set.empty (fromList [("name", VString "Alice"), ("age", VInteger 30), ("city", VString "NYC")])
          length (toList (properties s)) `shouldBe` 3
          hasProperty "name" s `shouldBe` True
          hasProperty "age" s `shouldBe` True
          hasProperty "city" s `shouldBe` True

