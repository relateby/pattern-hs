-- | Unit tests for Pattern.Core module.
module Spec.Pattern.CoreSpec where

import Test.Hspec
import Pattern.Core (Pattern(..))

-- Custom type for testing
data Person = Person { personName :: String, personAge :: Int }
  deriving (Eq, Show)

spec :: Spec
spec = do
  describe "Pattern.Core" $ do
    
    describe "Empty Patterns (User Story 1)" $ do
      
      describe "Creating empty patterns with different value types" $ do
        
        it "creates an empty pattern with string value" $ do
          let leaf = Pattern { value = "node1", elements = [] }
          value leaf `shouldBe` "node1"
          elements leaf `shouldBe` ([] :: [Pattern String])
        
        it "creates an empty pattern with integer value" $ do
          let leaf = Pattern { value = 42, elements = [] }
          value leaf `shouldBe` (42 :: Int)
          elements leaf `shouldBe` ([] :: [Pattern Int])
        
        it "creates an empty pattern with custom type value" $ do
          let person = Person "Alice" 30
          let leaf = Pattern { value = person, elements = [] }
          value leaf `shouldBe` person
          elements leaf `shouldBe` ([] :: [Pattern Person])
      
      describe "Value field accessor" $ do
        
        it "returns the correct value for an empty pattern with string" $ do
          let leaf = Pattern { value = "test", elements = [] }
          value leaf `shouldBe` "test"
        
        it "returns the correct value for an empty pattern with integer" $ do
          let leaf = Pattern { value = 100, elements = [] }
          value leaf `shouldBe` (100 :: Int)
        
        it "returns the correct value for an empty pattern with custom type" $ do
          let person = Person "Bob" 25
          let leaf = Pattern { value = person, elements = [] }
          value leaf `shouldBe` person
      
      describe "Elements field accessor" $ do
        
        it "returns empty list for empty pattern" $ do
          let leaf = Pattern { value = "leaf", elements = [] }
          elements leaf `shouldBe` ([] :: [Pattern String])
        
        it "returns empty list for empty pattern with different value types" $ do
          let leafInt = Pattern { value = 42, elements = [] }
          let leafString = Pattern { value = "test", elements = [] }
          elements leafInt `shouldBe` ([] :: [Pattern Int])
          elements leafString `shouldBe` ([] :: [Pattern String])
      
      describe "Edge cases" $ do
        
        it "empty pattern with explicitly empty list of elements behaves correctly" $ do
          let leaf = Pattern { value = "node", elements = [] }
          value leaf `shouldBe` "node"
          elements leaf `shouldBe` ([] :: [Pattern String])
          null (elements leaf) `shouldBe` True
        
        it "multiple empty patterns with same value type can be created independently" $ do
          let leaf1 = Pattern { value = "node1", elements = [] }
          let leaf2 = Pattern { value = "node2", elements = [] }
          value leaf1 `shouldBe` "node1"
          value leaf2 `shouldBe` "node2"
          elements leaf1 `shouldBe` ([] :: [Pattern String])
          elements leaf2 `shouldBe` ([] :: [Pattern String])
        
        it "empty patterns with different value types are type-safe" $ do
          let leafString = Pattern { value = "text", elements = [] }
          let leafInt = Pattern { value = 123, elements = [] }
          value leafString `shouldBe` "text"
          value leafInt `shouldBe` (123 :: Int)
          -- Type system ensures they cannot be mixed
    
    describe "Patterns with Children (User Story 2)" $ do
      
      describe "Creating patterns with children" $ do
        
        it "creates a pattern with single child" $ do
          let child = Pattern { value = "child", elements = [] }
          let parent = Pattern { value = "parent", elements = [child] }
          value parent `shouldBe` "parent"
          length (elements parent) `shouldBe` 1
          head (elements parent) `shouldBe` child
        
        it "creates a pattern with multiple children" $ do
          let child1 = Pattern { value = "child1", elements = [] }
          let child2 = Pattern { value = "child2", elements = [] }
          let child3 = Pattern { value = "child3", elements = [] }
          let parent = Pattern { value = "parent", elements = [child1, child2, child3] }
          value parent `shouldBe` "parent"
          length (elements parent) `shouldBe` 3
          elements parent `shouldBe` [child1, child2, child3]
      
      describe "Value field accessor for patterns with children" $ do
        
        it "returns the correct value for pattern with single child" $ do
          let child = Pattern { value = "child", elements = [] }
          let parent = Pattern { value = "parent", elements = [child] }
          value parent `shouldBe` "parent"
        
        it "returns the correct value for pattern with multiple children" $ do
          let child1 = Pattern { value = "c1", elements = [] }
          let child2 = Pattern { value = "c2", elements = [] }
          let parent = Pattern { value = "root", elements = [child1, child2] }
          value parent `shouldBe` "root"
        
        it "returns the correct value for pattern with integer value and children" $ do
          let child = Pattern { value = 10, elements = [] }
          let parent = Pattern { value = 100, elements = [child] }
          value parent `shouldBe` (100 :: Int)
      
      describe "Elements field accessor for patterns with children" $ do
        
        it "returns correct child list for pattern with single child" $ do
          let child = Pattern { value = "child", elements = [] }
          let parent = Pattern { value = "parent", elements = [child] }
          elements parent `shouldBe` [child]
        
        it "returns correct child list for pattern with multiple children" $ do
          let child1 = Pattern { value = "c1", elements = [] }
          let child2 = Pattern { value = "c2", elements = [] }
          let child3 = Pattern { value = "c3", elements = [] }
          let parent = Pattern { value = "parent", elements = [child1, child2, child3] }
          elements parent `shouldBe` [child1, child2, child3]
        
        it "returns correct child list preserving order" $ do
          let child1 = Pattern { value = "first", elements = [] }
          let child2 = Pattern { value = "second", elements = [] }
          let child3 = Pattern { value = "third", elements = [] }
          let parent = Pattern { value = "parent", elements = [child1, child2, child3] }
          let children = elements parent
          value (head children) `shouldBe` "first"
          value (children !! 1) `shouldBe` "second"
          value (children !! 2) `shouldBe` "third"
      
      describe "Elements accessibility and order" $ do
        
        it "elements are accessible in correct order" $ do
          let child1 = Pattern { value = "a", elements = [] }
          let child2 = Pattern { value = "b", elements = [] }
          let child3 = Pattern { value = "c", elements = [] }
          let parent = Pattern { value = "parent", elements = [child1, child2, child3] }
          let children = elements parent
          children `shouldBe` [child1, child2, child3]
          map value children `shouldBe` ["a", "b", "c"]
        
        it "can access individual children by index" $ do
          let child1 = Pattern { value = "first", elements = [] }
          let child2 = Pattern { value = "second", elements = [] }
          let parent = Pattern { value = "parent", elements = [child1, child2] }
          let children = elements parent
          head children `shouldBe` child1
          last children `shouldBe` child2
      
      describe "Edge cases" $ do
        
        it "pattern with zero elements behaves like empty pattern" $ do
          let pattern = Pattern { value = "node", elements = [] }
          value pattern `shouldBe` "node"
          elements pattern `shouldBe` ([] :: [Pattern String])
          null (elements pattern) `shouldBe` True
        
        it "deeply nested patterns (multiple levels)" $ do
          let level3 = Pattern { value = "level3", elements = [] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let root = Pattern { value = "root", elements = [level1] }
          value root `shouldBe` "root"
          length (elements root) `shouldBe` 1
          value (head (elements root)) `shouldBe` "level1"
          let l1Children = elements (head (elements root))
          length l1Children `shouldBe` 1
          value (head l1Children) `shouldBe` "level2"
          let l2Children = elements (head l1Children)
          length l2Children `shouldBe` 1
          value (head l2Children) `shouldBe` "level3"
        
        it "pattern containing pattern containing pattern (arbitrary depth)" $ do
          let innermost = Pattern { value = "innermost", elements = [] }
          let middle = Pattern { value = "middle", elements = [innermost] }
          let outer = Pattern { value = "outer", elements = [middle] }
          value outer `shouldBe` "outer"
          value (head (elements outer)) `shouldBe` "middle"
          value (head (elements (head (elements outer)))) `shouldBe` "innermost"
        
        it "patterns with varying numbers of children (zero, one, many)" $ do
          let zeroChildren = Pattern { value = "zero", elements = [] }
          let oneChild = Pattern { value = "one", elements = [Pattern { value = "child", elements = [] }] }
          let child1 = Pattern { value = "c1", elements = [] }
          let child2 = Pattern { value = "c2", elements = [] }
          let child3 = Pattern { value = "c3", elements = [] }
          let manyChildren = Pattern { value = "many", elements = [child1, child2, child3] }
          length (elements zeroChildren) `shouldBe` 0
          length (elements oneChild) `shouldBe` 1
          length (elements manyChildren) `shouldBe` 3
    
    describe "Show Instance (Phase 2.1)" $ do
      
      describe "Show instance for empty patterns" $ do
        
        it "shows empty pattern with string value correctly" $ do
          let leaf = Pattern { value = "test", elements = [] }
          show leaf `shouldBe` "Pattern {value = \"test\", elements = []}"
        
        it "shows empty pattern with integer value correctly" $ do
          let leaf = Pattern { value = 42, elements = [] }
          show leaf `shouldBe` "Pattern {value = 42, elements = []}"
        
        it "shows empty pattern with custom type value correctly" $ do
          let person = Person "Alice" 30
          let leaf = Pattern { value = person, elements = [] }
          show leaf `shouldBe` "Pattern {value = Person {personName = \"Alice\", personAge = 30}, elements = []}"
      
      describe "Show instance for patterns with children" $ do
        
        it "shows pattern with single child correctly" $ do
          let child = Pattern { value = "child", elements = [] }
          let parent = Pattern { value = "parent", elements = [child] }
          show parent `shouldBe` "Pattern {value = \"parent\", elements = [Pattern {value = \"child\", elements = []}]}"
        
        it "shows pattern with multiple children correctly" $ do
          let child1 = Pattern { value = "c1", elements = [] }
          let child2 = Pattern { value = "c2", elements = [] }
          let parent = Pattern { value = "parent", elements = [child1, child2] }
          show parent `shouldBe` "Pattern {value = \"parent\", elements = [Pattern {value = \"c1\", elements = []},Pattern {value = \"c2\", elements = []}]}"
        
        it "shows nested patterns correctly" $ do
          let level3 = Pattern { value = "level3", elements = [] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let root = Pattern { value = "root", elements = [level1] }
          show root `shouldContain` "Pattern {value = \"root\""
          show root `shouldContain` "Pattern {value = \"level1\""
          show root `shouldContain` "Pattern {value = \"level2\""
          show root `shouldContain` "Pattern {value = \"level3\""
    
    describe "Eq Instance (Phase 2.2)" $ do
      
      describe "Equality for empty patterns" $ do
        
        it "two identical empty patterns are equal" $ do
          let leaf1 = Pattern { value = "node", elements = [] }
          let leaf2 = Pattern { value = "node", elements = [] }
          leaf1 `shouldBe` leaf2
          (leaf1 == leaf2) `shouldBe` True
        
        it "two empty patterns with different values are not equal" $ do
          let leaf1 = Pattern { value = "node1", elements = [] }
          let leaf2 = Pattern { value = "node2", elements = [] }
          leaf1 `shouldNotBe` leaf2
          (leaf1 == leaf2) `shouldBe` False
        
        it "two empty patterns with same integer value are equal" $ do
          let leaf1 = Pattern { value = 42, elements = [] }
          let leaf2 = Pattern { value = 42, elements = [] }
          leaf1 `shouldBe` leaf2
        
        it "two empty patterns with different integer values are not equal" $ do
          let leaf1 = Pattern { value = 42, elements = [] }
          let leaf2 = Pattern { value = 100, elements = [] }
          leaf1 `shouldNotBe` leaf2
      
      describe "Equality for patterns with children" $ do
        
        it "two identical patterns with children are equal" $ do
          let child1 = Pattern { value = "child1", elements = [] }
          let child2 = Pattern { value = "child2", elements = [] }
          let parent1 = Pattern { value = "parent", elements = [child1, child2] }
          let parent2 = Pattern { value = "parent", elements = [child1, child2] }
          parent1 `shouldBe` parent2
        
        it "patterns with same value but different children are not equal" $ do
          let child1 = Pattern { value = "child1", elements = [] }
          let child2 = Pattern { value = "child2", elements = [] }
          let child3 = Pattern { value = "child3", elements = [] }
          let parent1 = Pattern { value = "parent", elements = [child1, child2] }
          let parent2 = Pattern { value = "parent", elements = [child1, child3] }
          parent1 `shouldNotBe` parent2
        
        it "patterns with different values but same children are not equal" $ do
          let child1 = Pattern { value = "child1", elements = [] }
          let child2 = Pattern { value = "child2", elements = [] }
          let parent1 = Pattern { value = "parent1", elements = [child1, child2] }
          let parent2 = Pattern { value = "parent2", elements = [child1, child2] }
          parent1 `shouldNotBe` parent2
        
        it "patterns with different numbers of children are not equal" $ do
          let child1 = Pattern { value = "child1", elements = [] }
          let child2 = Pattern { value = "child2", elements = [] }
          let parent1 = Pattern { value = "parent", elements = [child1] }
          let parent2 = Pattern { value = "parent", elements = [child1, child2] }
          parent1 `shouldNotBe` parent2
      
      describe "Equality for nested patterns" $ do
        
        it "two identical deeply nested patterns are equal" $ do
          let level3 = Pattern { value = "level3", elements = [] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let root1 = Pattern { value = "root", elements = [level1] }
          let root2 = Pattern { value = "root", elements = [level1] }
          root1 `shouldBe` root2
        
        it "nested patterns with different structure are not equal" $ do
          let level3a = Pattern { value = "level3", elements = [] }
          let level2a = Pattern { value = "level2", elements = [level3a] }
          let level1a = Pattern { value = "level1", elements = [level2a] }
          let root1 = Pattern { value = "root", elements = [level1a] }
          
          let level3b = Pattern { value = "level3", elements = [] }
          let level2b = Pattern { value = "level2", elements = [] }
          let level1b = Pattern { value = "level1", elements = [level2b] }
          let root2 = Pattern { value = "root", elements = [level1b] }
          
          root1 `shouldNotBe` root2
      
      describe "Equality edge cases" $ do
        
        it "empty patterns with same value are equal" $ do
          let p1 = Pattern { value = "test", elements = [] }
          let p2 = Pattern { value = "test", elements = [] }
          p1 `shouldBe` p2
        
        it "reflexivity: pattern equals itself" $ do
          let pattern = Pattern { value = "test", elements = [] }
          (pattern == pattern) `shouldBe` True
        
        it "symmetry: if a == b, then b == a" $ do
          let a = Pattern { value = "test", elements = [] }
          let b = Pattern { value = "test", elements = [] }
          (a == b) `shouldBe` (b == a)