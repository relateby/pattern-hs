-- | Unit tests for Pattern.Core module.
module Spec.Pattern.CoreSpec where

import Data.Char (toUpper)
import Test.Hspec
import Pattern.Core (Pattern(..))

-- Custom type for testing
data Person = Person { name :: String, age :: Int }
  deriving (Eq, Show)

spec :: Spec
spec = do
  describe "Pattern.Core" $ do
    
    describe "Atomic Patterns (User Story 1)" $ do
      
      describe "Creating atomic patterns with different value types" $ do
        
        it "creates an atomic pattern with string value" $ do
          let atom = Pattern { value = "node1", elements = [] }
          value atom `shouldBe` "node1"
          elements atom `shouldBe` ([] :: [Pattern String])
        
        it "creates an atomic pattern with integer value" $ do
          let atom = Pattern { value = 42, elements = [] }
          value atom `shouldBe` (42 :: Int)
          elements atom `shouldBe` ([] :: [Pattern Int])
        
        it "creates an atomic pattern with custom type value" $ do
          let person = Person "Alice" 30
          let atom = Pattern { value = person, elements = [] }
          value atom `shouldBe` person
          elements atom `shouldBe` ([] :: [Pattern Person])
      
      describe "Value field accessor" $ do
        
        it "returns the correct value for an atomic pattern with string" $ do
          let atom = Pattern { value = "test", elements = [] }
          value atom `shouldBe` "test"
        
        it "returns the correct value for an atomic pattern with integer" $ do
          let atom = Pattern { value = 100, elements = [] }
          value atom `shouldBe` (100 :: Int)
        
        it "returns the correct value for an atomic pattern with custom type" $ do
          let person = Person "Bob" 25
          let atom = Pattern { value = person, elements = [] }
          value atom `shouldBe` person
      
      describe "Elements field accessor" $ do
        
        it "returns empty list for atomic pattern" $ do
          let atom = Pattern { value = "empty", elements = [] }
          elements atom `shouldBe` ([] :: [Pattern String])
        
        it "returns empty list for atomic pattern with different value types" $ do
          let atomInt = Pattern { value = 42, elements = [] }
          let atomString = Pattern { value = "test", elements = [] }
          elements atomInt `shouldBe` ([] :: [Pattern Int])
          elements atomString `shouldBe` ([] :: [Pattern String])
      
      describe "Edge cases" $ do
        
        it "atomic pattern with explicitly empty list of elements behaves correctly" $ do
          let atom = Pattern { value = "node", elements = [] }
          value atom `shouldBe` "node"
          elements atom `shouldBe` ([] :: [Pattern String])
          null (elements atom) `shouldBe` True
        
        it "multiple atomic patterns with same value type can be created independently" $ do
          let atom1 = Pattern { value = "node1", elements = [] }
          let atom2 = Pattern { value = "node2", elements = [] }
          value atom1 `shouldBe` "node1"
          value atom2 `shouldBe` "node2"
          elements atom1 `shouldBe` ([] :: [Pattern String])
          elements atom2 `shouldBe` ([] :: [Pattern String])
        
        it "atomic patterns with different value types are type-safe" $ do
          let atomString = Pattern { value = "text", elements = [] }
          let atomInt = Pattern { value = 123, elements = [] }
          value atomString `shouldBe` "text"
          value atomInt `shouldBe` (123 :: Int)
          -- Type system ensures they cannot be mixed
    
    describe "Patterns with Elements (User Story 2)" $ do
      
      describe "Creating patterns with elements" $ do
        
        it "creates a singular pattern" $ do
          let elem = Pattern { value = "elem", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem] }
          value pattern `shouldBe` "pattern"
          length (elements pattern) `shouldBe` 1
          head (elements pattern) `shouldBe` elem
        
        it "creates a pattern with multiple elements" $ do
          let elem1 = Pattern { value = "elem1", elements = [] }
          let elem2 = Pattern { value = "elem2", elements = [] }
          let elem3 = Pattern { value = "elem3", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          value pattern `shouldBe` "pattern"
          length (elements pattern) `shouldBe` 3
          elements pattern `shouldBe` [elem1, elem2, elem3]
      
      describe "Value field accessor for patterns with elements" $ do
        
        it "returns the correct value for singular pattern" $ do
          let elem = Pattern { value = "elem", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem] }
          value pattern `shouldBe` "pattern"
        
        it "returns the correct value for pattern with multiple elements" $ do
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem1, elem2] }
          value pattern `shouldBe` "pattern"
        
        it "returns the correct value for pattern with integer value and elements" $ do
          let elem = Pattern { value = 10, elements = [] }
          let pattern = Pattern { value = 100, elements = [elem] }
          value pattern `shouldBe` (100 :: Int)
      
      describe "Elements field accessor for patterns with elements" $ do
        
        it "returns correct element list for singular pattern" $ do
          let elem = Pattern { value = "elem", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem] }
          elements pattern `shouldBe` [elem]
        
        it "returns correct element list for pattern with multiple elements" $ do
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let elem3 = Pattern { value = "e3", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          elements pattern `shouldBe` [elem1, elem2, elem3]
        
        it "returns correct element list preserving order" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let elem3 = Pattern { value = "third", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          let elems = elements pattern
          value (head elems) `shouldBe` "first"
          value (elems !! 1) `shouldBe` "second"
          value (elems !! 2) `shouldBe` "third"
      
      describe "Elements accessibility and order" $ do
        
        it "elements are accessible in correct order" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let elem3 = Pattern { value = "c", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          let elems = elements pattern
          elems `shouldBe` [elem1, elem2, elem3]
          map value elems `shouldBe` ["a", "b", "c"]
        
        it "can access individual elements by index" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem1, elem2] }
          let elems = elements pattern
          head elems `shouldBe` elem1
          last elems `shouldBe` elem2
      
      describe "Edge cases" $ do
        
        it "pattern with zero elements behaves like atomic pattern" $ do
          let pattern = Pattern { value = "node", elements = [] }
          value pattern `shouldBe` "node"
          elements pattern `shouldBe` ([] :: [Pattern String])
          null (elements pattern) `shouldBe` True
        
        it "deeply nested patterns (multiple levels)" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let pattern = Pattern { value = "pattern", elements = [outer] }
          value pattern `shouldBe` "pattern"
          length (elements pattern) `shouldBe` 1
          value (head (elements pattern)) `shouldBe` "outer"
          let outerElems = elements (head (elements pattern))
          length outerElems `shouldBe` 1
          value (head outerElems) `shouldBe` "middle"
          let middleElems = elements (head outerElems)
          length middleElems `shouldBe` 1
          value (head middleElems) `shouldBe` "inner"
        
        it "pattern containing pattern containing pattern (arbitrary depth)" $ do
          let innermost = Pattern { value = "innermost", elements = [] }
          let middle = Pattern { value = "middle", elements = [innermost] }
          let outer = Pattern { value = "outer", elements = [middle] }
          value outer `shouldBe` "outer"
          value (head (elements outer)) `shouldBe` "middle"
          value (head (elements (head (elements outer)))) `shouldBe` "innermost"
        
        it "patterns with varying numbers of elements (zero, one, many)" $ do
          let zeroElements = Pattern { value = "zero", elements = [] }
          let singularPattern = Pattern { value = "one", elements = [Pattern { value = "elem", elements = [] }] }
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let elem3 = Pattern { value = "e3", elements = [] }
          let manyElements = Pattern { value = "many", elements = [elem1, elem2, elem3] }
          length (elements zeroElements) `shouldBe` 0
          length (elements singularPattern) `shouldBe` 1
          length (elements manyElements) `shouldBe` 3
    
    describe "Show Instance (Phase 2.1)" $ do
      
      describe "Show instance for atomic patterns" $ do
        
        it "shows atomic pattern with string value correctly" $ do
          let atom = Pattern { value = "test", elements = [] }
          show atom `shouldBe` "Pattern {value = \"test\", elements = []}"
        
        it "shows atomic pattern with integer value correctly" $ do
          let atom = Pattern { value = 42, elements = [] }
          show atom `shouldBe` "Pattern {value = 42, elements = []}"
        
        it "shows atomic pattern with custom type value correctly" $ do
          let person = Person "Alice" 30
          let atom = Pattern { value = person, elements = [] }
          show atom `shouldBe` "Pattern {value = Person {name = \"Alice\", age = 30}, elements = []}"
      
      describe "Show instance for patterns with elements" $ do
        
        it "shows singular pattern correctly" $ do
          let elem = Pattern { value = "elem", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem] }
          show pattern `shouldBe` "Pattern {value = \"pattern\", elements = [Pattern {value = \"elem\", elements = []}]}"
        
        it "shows pattern with multiple elements correctly" $ do
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem1, elem2] }
          show pattern `shouldBe` "Pattern {value = \"pattern\", elements = [Pattern {value = \"e1\", elements = []},Pattern {value = \"e2\", elements = []}]}"
        
        it "shows nested patterns correctly" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let pattern = Pattern { value = "pattern", elements = [outer] }
          show pattern `shouldContain` "Pattern {value = \"pattern\""
          show pattern `shouldContain` "Pattern {value = \"outer\""
          show pattern `shouldContain` "Pattern {value = \"middle\""
          show pattern `shouldContain` "Pattern {value = \"inner\""
    
    describe "Eq Instance (Phase 2.2)" $ do
      
      describe "Equality for atomic patterns" $ do
        
        it "two identical atomic patterns are equal" $ do
          let atom1 = Pattern { value = "node", elements = [] }
          let atom2 = Pattern { value = "node", elements = [] }
          atom1 `shouldBe` atom2
          (atom1 == atom2) `shouldBe` True
        
        it "two atomic patterns with different values are not equal" $ do
          let atom1 = Pattern { value = "node1", elements = [] }
          let atom2 = Pattern { value = "node2", elements = [] }
          atom1 `shouldNotBe` atom2
          (atom1 == atom2) `shouldBe` False
        
        it "two atomic patterns with same integer value are equal" $ do
          let atom1 = Pattern { value = 42, elements = [] }
          let atom2 = Pattern { value = 42, elements = [] }
          atom1 `shouldBe` atom2
        
        it "two atomic patterns with different integer values are not equal" $ do
          let atom1 = Pattern { value = 42, elements = [] }
          let atom2 = Pattern { value = 100, elements = [] }
          atom1 `shouldNotBe` atom2
      
      describe "Equality for patterns with elements" $ do
        
        it "two identical patterns with elements are equal" $ do
          let elem1 = Pattern { value = "elem1", elements = [] }
          let elem2 = Pattern { value = "elem2", elements = [] }
          let pattern1 = Pattern { value = "pattern", elements = [elem1, elem2] }
          let pattern2 = Pattern { value = "pattern", elements = [elem1, elem2] }
          pattern1 `shouldBe` pattern2
        
        it "patterns with same value but different elements are not equal" $ do
          let elem1 = Pattern { value = "elem1", elements = [] }
          let elem2 = Pattern { value = "elem2", elements = [] }
          let elem3 = Pattern { value = "elem3", elements = [] }
          let pattern1 = Pattern { value = "pattern", elements = [elem1, elem2] }
          let pattern2 = Pattern { value = "pattern", elements = [elem1, elem3] }
          pattern1 `shouldNotBe` pattern2
        
        it "patterns with different values but same elements are not equal" $ do
          let elem1 = Pattern { value = "elem1", elements = [] }
          let elem2 = Pattern { value = "elem2", elements = [] }
          let pattern1 = Pattern { value = "pattern1", elements = [elem1, elem2] }
          let pattern2 = Pattern { value = "pattern2", elements = [elem1, elem2] }
          pattern1 `shouldNotBe` pattern2
        
        it "patterns with different numbers of elements are not equal" $ do
          let elem1 = Pattern { value = "elem1", elements = [] }
          let elem2 = Pattern { value = "elem2", elements = [] }
          let pattern1 = Pattern { value = "pattern", elements = [elem1] }
          let pattern2 = Pattern { value = "pattern", elements = [elem1, elem2] }
          pattern1 `shouldNotBe` pattern2
      
      describe "Equality for nested patterns" $ do
        
        it "two identical deeply nested patterns are equal" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let pattern1 = Pattern { value = "pattern", elements = [outer] }
          let pattern2 = Pattern { value = "pattern", elements = [outer] }
          pattern1 `shouldBe` pattern2
        
        it "nested patterns with different structure are not equal" $ do
          let innerA = Pattern { value = "inner", elements = [] }
          let middleA = Pattern { value = "middle", elements = [innerA] }
          let outerA = Pattern { value = "outer", elements = [middleA] }
          let pattern1 = Pattern { value = "pattern", elements = [outerA] }
          
          let innerB = Pattern { value = "inner", elements = [] }
          let middleB = Pattern { value = "middle", elements = [] }
          let outerB = Pattern { value = "outer", elements = [middleB] }
          let pattern2 = Pattern { value = "pattern", elements = [outerB] }
          
          pattern1 `shouldNotBe` pattern2
      
      describe "Equality edge cases" $ do
        
        it "atomic patterns with same value are equal" $ do
          let atom1 = Pattern { value = "test", elements = [] }
          let atom2 = Pattern { value = "test", elements = [] }
          atom1 `shouldBe` atom2
        
        it "reflexivity: pattern equals itself" $ do
          let pattern = Pattern { value = "test", elements = [] }
          (pattern == pattern) `shouldBe` True
        
        it "symmetry: if a == b, then b == a" $ do
          let a = Pattern { value = "test", elements = [] }
          let b = Pattern { value = "test", elements = [] }
          (a == b) `shouldBe` (b == a)
    
    describe "Functor Instance (User Story 1)" $ do
      
      describe "Transforming atomic patterns" $ do
        
        it "transforms atomic pattern with string value" $ do
          let atom = Pattern { value = "test", elements = [] }
          let transformed = fmap (map toUpper) atom
          value transformed `shouldBe` "TEST"
          elements transformed `shouldBe` ([] :: [Pattern String])
      
      describe "Transforming patterns with multiple elements" $ do
        
        it "transforms pattern with multiple elements" $ do
          let elem1 = Pattern { value = "hello", elements = [] }
          let elem2 = Pattern { value = "world", elements = [] }
          let pattern = Pattern { value = "greeting", elements = [elem1, elem2] }
          let transformed = fmap (map toUpper) pattern
          value transformed `shouldBe` "GREETING"
          length (elements transformed) `shouldBe` 2
          value (head (elements transformed)) `shouldBe` "HELLO"
          value (last (elements transformed)) `shouldBe` "WORLD"
      
      describe "Transforming nested pattern structures" $ do
        
        it "transforms nested pattern structure" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let pattern = Pattern { value = "root", elements = [outer] }
          let transformed = fmap (map toUpper) pattern
          value transformed `shouldBe` "ROOT"
          length (elements transformed) `shouldBe` 1
          value (head (elements transformed)) `shouldBe` "OUTER"
          let outerElems = elements (head (elements transformed))
          length outerElems `shouldBe` 1
          value (head outerElems) `shouldBe` "MIDDLE"
          let middleElems = elements (head outerElems)
          length middleElems `shouldBe` 1
          value (head middleElems) `shouldBe` "INNER"
      
      describe "Transforming patterns with integer values" $ do
        
        it "transforms pattern with integer values" $ do
          let elem1 = Pattern { value = 5, elements = [] }
          let elem2 = Pattern { value = 10, elements = [] }
          let pattern = Pattern { value = 20, elements = [elem1, elem2] }
          let transformed = fmap (* 2) pattern
          value transformed `shouldBe` (40 :: Int)
          value (head (elements transformed)) `shouldBe` (10 :: Int)
          value (last (elements transformed)) `shouldBe` (20 :: Int)
      
      describe "Transforming patterns with custom type values" $ do
        
        it "transforms pattern with custom type values" $ do
          let person1 = Person "Alice" 30
          let person2 = Person "Bob" 25
          let elem1 = Pattern { value = person1, elements = [] }
          let elem2 = Pattern { value = person2, elements = [] }
          let pattern = Pattern { value = person1, elements = [elem1, elem2] }
          let transformed = fmap (\p -> Person (name p) (age p + 1)) pattern
          age (value transformed) `shouldBe` 31
          age (value (head (elements transformed))) `shouldBe` 31
          age (value (last (elements transformed))) `shouldBe` 26
      
      describe "Structure preservation" $ do
        
        it "preserves element count during transformation" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let elem3 = Pattern { value = "c", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          let transformed = fmap (map toUpper) pattern
          length (elements pattern) `shouldBe` length (elements transformed)
          length (elements transformed) `shouldBe` 3
        
        it "preserves element order during transformation" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let elem3 = Pattern { value = "third", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          let transformed = fmap (map toUpper) pattern
          let originalValues = map value (elements pattern)
          let transformedValues = map value (elements transformed)
          originalValues `shouldBe` ["first", "second", "third"]
          transformedValues `shouldBe` ["FIRST", "SECOND", "THIRD"]
          -- Verify order is preserved
          value (head (elements transformed)) `shouldBe` "FIRST"
          value (elements transformed !! 1) `shouldBe` "SECOND"
          value (last (elements transformed)) `shouldBe` "THIRD"
      
      describe "Type transformation" $ do
        
        it "transforms pattern from String to Int type" $ do
          let elem1 = Pattern { value = "5", elements = [] }
          let elem2 = Pattern { value = "10", elements = [] }
          let pattern = Pattern { value = "20", elements = [elem1, elem2] }
          let transformed = fmap (read :: String -> Int) pattern
          value transformed `shouldBe` (20 :: Int)
          value (head (elements transformed)) `shouldBe` (5 :: Int)
          value (last (elements transformed)) `shouldBe` (10 :: Int)
    
    describe "Functor Laws (User Story 2)" $ do
      
      describe "Identity Law" $ do
        
        it "fmap id = id for atomic pattern" $ do
          let atom = Pattern { value = "test", elements = [] }
          fmap id atom `shouldBe` atom
          fmap id atom `shouldBe` id atom
        
        it "fmap id = id for pattern with elements" $ do
          let elem1 = Pattern { value = "hello", elements = [] }
          let elem2 = Pattern { value = "world", elements = [] }
          let pattern = Pattern { value = "greeting", elements = [elem1, elem2] }
          fmap id pattern `shouldBe` pattern
          fmap id pattern `shouldBe` id pattern
      
      describe "Composition Law" $ do
        
        it "fmap (f . g) = fmap f . fmap g with two transformation functions" $ do
          let elem1 = Pattern { value = "hello", elements = [] }
          let elem2 = Pattern { value = "world", elements = [] }
          let pattern = Pattern { value = "greeting", elements = [elem1, elem2] }
          let f = map toUpper :: String -> String
          let g = reverse :: String -> String
          fmap (f . g) pattern `shouldBe` (fmap f . fmap g) pattern
        
        it "fmap (f . g) = fmap f . fmap g with nested patterns" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let pattern = Pattern { value = "root", elements = [outer] }
          let f = map toUpper :: String -> String
          let g = reverse :: String -> String
          fmap (f . g) pattern `shouldBe` (fmap f . fmap g) pattern
    
    describe "Nested Pattern Transformation (User Story 3)" $ do
      
      describe "Transforming patterns with 3+ levels of nesting" $ do
        
        it "transforms pattern with 3 levels of nesting" $ do
          let level3 = Pattern { value = "level3", elements = [] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let pattern = Pattern { value = "root", elements = [level1] }
          let transformed = fmap (map toUpper) pattern
          value transformed `shouldBe` "ROOT"
          value (head (elements transformed)) `shouldBe` "LEVEL1"
          value (head (elements (head (elements transformed)))) `shouldBe` "LEVEL2"
          value (head (elements (head (elements (head (elements transformed)))))) `shouldBe` "LEVEL3"
        
        it "transforms pattern with 4 levels of nesting" $ do
          let level4 = Pattern { value = "level4", elements = [] }
          let level3 = Pattern { value = "level3", elements = [level4] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let pattern = Pattern { value = "root", elements = [level1] }
          let transformed = fmap (map toUpper) pattern
          value transformed `shouldBe` "ROOT"
          value (head (elements transformed)) `shouldBe` "LEVEL1"
          value (head (elements (head (elements transformed)))) `shouldBe` "LEVEL2"
          value (head (elements (head (elements (head (elements transformed)))))) `shouldBe` "LEVEL3"
          value (head (elements (head (elements (head (elements (head (elements transformed)))))))) `shouldBe` "LEVEL4"
      
      describe "Transforming patterns with varying nesting depths in different branches" $ do
        
        it "transforms pattern with different nesting depths in different branches" $ do
          -- Branch 1: 2 levels deep
          let branch1Level2 = Pattern { value = "b1l2", elements = [] }
          let branch1Level1 = Pattern { value = "b1l1", elements = [branch1Level2] }
          -- Branch 2: 3 levels deep
          let branch2Level3 = Pattern { value = "b2l3", elements = [] }
          let branch2Level2 = Pattern { value = "b2l2", elements = [branch2Level3] }
          let branch2Level1 = Pattern { value = "b2l1", elements = [branch2Level2] }
          -- Branch 3: 1 level deep (atomic)
          let branch3Level1 = Pattern { value = "b3l1", elements = [] }
          let pattern = Pattern { value = "root", elements = [branch1Level1, branch2Level1, branch3Level1] }
          let transformed = fmap (map toUpper) pattern
          value transformed `shouldBe` "ROOT"
          length (elements transformed) `shouldBe` 3
          -- Verify branch 1 (2 levels)
          value (head (elements transformed)) `shouldBe` "B1L1"
          value (head (elements (head (elements transformed)))) `shouldBe` "B1L2"
          -- Verify branch 2 (3 levels)
          (elements transformed !! 1) `shouldBe` (Pattern { value = "B2L1", elements = [Pattern { value = "B2L2", elements = [Pattern { value = "B2L3", elements = [] }] }] })
          -- Verify branch 3 (1 level)
          value (last (elements transformed)) `shouldBe` "B3L1"
      
      describe "Transforming patterns with mixed structures at different levels" $ do
        
        it "transforms pattern with mixed structures (atomic and with elements) at different levels" $ do
          -- Level 1: pattern with multiple elements
          let atom1 = Pattern { value = "atom1", elements = [] }
          let atom2 = Pattern { value = "atom2", elements = [] }
          let level1 = Pattern { value = "level1", elements = [atom1, atom2] }
          -- Level 2: atomic pattern
          let level2 = Pattern { value = "level2", elements = [] }
          -- Level 3: pattern with single element
          let level3Atom = Pattern { value = "level3atom", elements = [] }
          let level3 = Pattern { value = "level3", elements = [level3Atom] }
          let pattern = Pattern { value = "root", elements = [level1, level2, level3] }
          let transformed = fmap (map toUpper) pattern
          value transformed `shouldBe` "ROOT"
          length (elements transformed) `shouldBe` 3
          -- Verify level 1 (multiple elements)
          value (head (elements transformed)) `shouldBe` "LEVEL1"
          length (elements (head (elements transformed))) `shouldBe` 2
          (head (elements (head (elements transformed)))) `shouldBe` (Pattern { value = "ATOM1", elements = [] })
          (last (elements (head (elements transformed)))) `shouldBe` (Pattern { value = "ATOM2", elements = [] })
          -- Verify level 2 (atomic)
          (elements transformed !! 1) `shouldBe` (Pattern { value = "LEVEL2", elements = [] })
          -- Verify level 3 (single element)
          value (last (elements transformed)) `shouldBe` "LEVEL3"
          length (elements (last (elements transformed))) `shouldBe` 1
          (head (elements (last (elements transformed)))) `shouldBe` (Pattern { value = "LEVEL3ATOM", elements = [] })
      
      describe "Verifying all nesting levels are transformed correctly" $ do
        
        it "verifies all nesting levels are transformed in a complex nested structure" $ do
          -- Create a complex structure: root -> [branch1, branch2]
          -- branch1 -> [leaf1, leaf2]
          -- branch2 -> [subbranch -> [leaf3]]
          let leaf1 = Pattern { value = "leaf1", elements = [] }
          let leaf2 = Pattern { value = "leaf2", elements = [] }
          let branch1 = Pattern { value = "branch1", elements = [leaf1, leaf2] }
          let leaf3 = Pattern { value = "leaf3", elements = [] }
          let subbranch = Pattern { value = "subbranch", elements = [leaf3] }
          let branch2 = Pattern { value = "branch2", elements = [subbranch] }
          let pattern = Pattern { value = "root", elements = [branch1, branch2] }
          let transformed = fmap (map toUpper) pattern
          -- Verify root level
          value transformed `shouldBe` "ROOT"
          -- Verify branch1 and its leaves
          value (head (elements transformed)) `shouldBe` "BRANCH1"
          (head (elements (head (elements transformed)))) `shouldBe` (Pattern { value = "LEAF1", elements = [] })
          (last (elements (head (elements transformed)))) `shouldBe` (Pattern { value = "LEAF2", elements = [] })
          -- Verify branch2, subbranch, and leaf3
          value (last (elements transformed)) `shouldBe` "BRANCH2"
          (head (elements (last (elements transformed)))) `shouldBe` (Pattern { value = "SUBBRANCH", elements = [Pattern { value = "LEAF3", elements = [] }] })
          (head (elements (head (elements (last (elements transformed)))))) `shouldBe` (Pattern { value = "LEAF3", elements = [] })
    
    describe "Edge Cases & Comprehensive Testing (Phase 4)" $ do
      
      describe "Transforming atomic patterns" $ do
        
        it "transforms atomic pattern (no elements)" $ do
          let atom = Pattern { value = "atom", elements = [] }
          let transformed = fmap (map toUpper) atom
          value transformed `shouldBe` "ATOM"
          elements transformed `shouldBe` ([] :: [Pattern String])
          length (elements transformed) `shouldBe` 0
      
      describe "Transforming patterns with empty elements list" $ do
        
        it "transforms pattern with empty elements list" $ do
          let pattern = Pattern { value = "empty", elements = [] }
          let transformed = fmap (map toUpper) pattern
          value transformed `shouldBe` "EMPTY"
          elements transformed `shouldBe` ([] :: [Pattern String])
          null (elements transformed) `shouldBe` True
      
      describe "Transforming singular patterns" $ do
        
        it "transforms singular pattern (one element)" $ do
          let elem = Pattern { value = "elem", elements = [] }
          let pattern = Pattern { value = "singular", elements = [elem] }
          let transformed = fmap (map toUpper) pattern
          value transformed `shouldBe` "SINGULAR"
          length (elements transformed) `shouldBe` 1
          value (head (elements transformed)) `shouldBe` "ELEM"
          elements (head (elements transformed)) `shouldBe` ([] :: [Pattern String])
      
      describe "Transforming pair patterns" $ do
        
        it "transforms pair pattern (two elements)" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let pattern = Pattern { value = "pair", elements = [elem1, elem2] }
          let transformed = fmap (map toUpper) pattern
          value transformed `shouldBe` "PAIR"
          length (elements transformed) `shouldBe` 2
          value (head (elements transformed)) `shouldBe` "FIRST"
          value (last (elements transformed)) `shouldBe` "SECOND"
      
      describe "Transforming extended patterns" $ do
        
        it "transforms extended pattern (many elements)" $ do
          let elems = map (\i -> Pattern { value = "elem" ++ show i, elements = [] }) [1..10]
          let pattern = Pattern { value = "extended", elements = elems }
          let transformed = fmap (map toUpper) pattern
          value transformed `shouldBe` "EXTENDED"
          length (elements transformed) `shouldBe` 10
          value (head (elements transformed)) `shouldBe` "ELEM1"
          value (last (elements transformed)) `shouldBe` "ELEM10"
          -- Verify all elements are transformed correctly
          map value (elements transformed) `shouldBe` map (\i -> "ELEM" ++ show i) [1..10]
      
      describe "Transforming patterns with different value types" $ do
        
        it "transforms patterns with string values" $ do
          let pattern = Pattern { value = "test", elements = [] }
          let transformed = fmap (map toUpper) pattern
          value transformed `shouldBe` "TEST"
        
        it "transforms patterns with integer values" $ do
          let pattern = Pattern { value = 42, elements = [] }
          let transformed = fmap (* 2) pattern
          value transformed `shouldBe` (84 :: Int)
        
        it "transforms patterns with custom type values" $ do
          let person = Person "Alice" 30
          let pattern = Pattern { value = person, elements = [] }
          let transformed = fmap (\p -> Person (name p) (age p + 5)) pattern
          age (value transformed) `shouldBe` 35
          name (value transformed) `shouldBe` "Alice"
        
        it "transforms patterns with nested custom types" $ do
          let person1 = Person "Alice" 30
          let person2 = Person "Bob" 25
          let elem1 = Pattern { value = person1, elements = [] }
          let elem2 = Pattern { value = person2, elements = [] }
          let pattern = Pattern { value = person1, elements = [elem1, elem2] }
          let transformed = fmap (\p -> Person (map toUpper (name p)) (age p)) pattern
          name (value transformed) `shouldBe` "ALICE"
          name (value (head (elements transformed))) `shouldBe` "ALICE"
          name (value (last (elements transformed))) `shouldBe` "BOB"
      
      describe "Type transformation edge cases" $ do
        
        it "transforms pattern from String to Int with empty string" $ do
          let pattern = Pattern { value = "0", elements = [] }
          let transformed = fmap (read :: String -> Int) pattern
          value transformed `shouldBe` (0 :: Int)
        
        it "transforms pattern from String to Int with negative numbers" $ do
          let elem1 = Pattern { value = "-5", elements = [] }
          let elem2 = Pattern { value = "-10", elements = [] }
          let pattern = Pattern { value = "-20", elements = [elem1, elem2] }
          let transformed = fmap (read :: String -> Int) pattern
          value transformed `shouldBe` (-20 :: Int)
          value (head (elements transformed)) `shouldBe` (-5 :: Int)
          value (last (elements transformed)) `shouldBe` (-10 :: Int)
        
        it "transforms pattern from Int to String" $ do
          let elem1 = Pattern { value = 5, elements = [] }
          let elem2 = Pattern { value = 10, elements = [] }
          let pattern = Pattern { value = 20, elements = [elem1, elem2] }
          let transformed = fmap show pattern
          value transformed `shouldBe` "20"
          value (head (elements transformed)) `shouldBe` "5"
          value (last (elements transformed)) `shouldBe` "10"
        
        it "transforms pattern with identity function preserves structure" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let pattern = Pattern { value = "root", elements = [elem1, elem2] }
          let transformed = fmap id pattern
          transformed `shouldBe` pattern
          value transformed `shouldBe` value pattern
          length (elements transformed) `shouldBe` length (elements pattern)