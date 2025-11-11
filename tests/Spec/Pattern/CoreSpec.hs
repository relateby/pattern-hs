-- | Unit tests for Pattern.Core module.
module Spec.Pattern.CoreSpec where

import Test.Hspec
import Pattern.Core (Pattern(..), pattern, patternWith, fromList)

-- Custom type for testing
data Person = Person { name :: String, age :: Maybe Int }
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
          let person = Person "Alice" (Just 30)
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
          let person = Person "Bob" (Just 25)
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
          let person = Person "Alice" (Just 30)
          let atom = Pattern { value = person, elements = [] }
          show atom `shouldBe` "Pattern {value = Person {name = \"Alice\", age = Just 30}, elements = []}"
      
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
    
    describe "Constructor Functions (User Story 1)" $ do
      
      describe "pattern function - creating atomic patterns" $ do
        
        it "creates atomic pattern with string value using pattern function" $ do
          let atom = pattern "node1"
          value atom `shouldBe` "node1"
          elements atom `shouldBe` ([] :: [Pattern String])
        
        it "creates atomic pattern with integer value using pattern function" $ do
          let atom = pattern (42 :: Int)
          value atom `shouldBe` (42 :: Int)
          elements atom `shouldBe` ([] :: [Pattern Int])
        
        it "creates atomic pattern with custom type value using pattern function" $ do
          let person = Person "Alice" (Just 30)
          let atom = pattern person
          value atom `shouldBe` person
          elements atom `shouldBe` ([] :: [Pattern Person])
        
        it "pattern function produces patterns functionally identical to record syntax" $ do
          let atom1 = pattern "test"
          let atom2 = Pattern { value = "test", elements = [] }
          atom1 `shouldBe` atom2
          value atom1 `shouldBe` value atom2
          elements atom1 `shouldBe` elements atom2
          
          let atom3 = pattern (100 :: Int)
          let atom4 = Pattern { value = 100, elements = [] }
          atom3 `shouldBe` atom4
          
          let person = Person "Bob" (Just 25)
          let atom5 = pattern person
          let atom6 = Pattern { value = person, elements = [] }
          atom5 `shouldBe` atom6
    
    describe "Constructor Functions (User Story 2)" $ do
      
      describe "patternWith function - creating patterns with elements" $ do
        
        it "creates singular pattern (one element) using patternWith" $ do
          let elem = pattern "a team sport involving kicking a ball"
          let singular = patternWith "soccer" [elem]
          value singular `shouldBe` "soccer"
          length (elements singular) `shouldBe` 1
          head (elements singular) `shouldBe` elem
        
        it "creates role-based singular pattern with custom type" $ do
          -- "The goalie" is "Hans"
          let goalie = patternWith (Person "Goalie" Nothing) [pattern (Person "Hans" (Just 25))]
          value goalie `shouldBe` Person "Goalie" Nothing
          length (elements goalie) `shouldBe` 1
          value (head (elements goalie)) `shouldBe` Person "Hans" (Just 25)
        
        it "creates pair pattern (two elements) using patternWith" $ do
          let elem1 = pattern "Alice"
          let elem2 = pattern "Bob"
          let pair = patternWith "knows" [elem1, elem2]
          value pair `shouldBe` "knows"
          length (elements pair) `shouldBe` 2
          elements pair `shouldBe` [elem1, elem2]
        
        it "creates extended pattern (many elements) using patternWith" $ do
          let elem1 = pattern "elem1"
          let elem2 = pattern "elem2"
          let elem3 = pattern "elem3"
          let elem4 = pattern "elem4"
          let extended = patternWith "graph" [elem1, elem2, elem3, elem4]
          value extended `shouldBe` "graph"
          length (elements extended) `shouldBe` 4
          elements extended `shouldBe` [elem1, elem2, elem3, elem4]
        
        it "empty list in patternWith produces atomic pattern" $ do
          let atomic = patternWith "empty" []
          value atomic `shouldBe` "empty"
          elements atomic `shouldBe` ([] :: [Pattern String])
          atomic `shouldBe` pattern "empty"
        
        it "patternWith preserves element order" $ do
          let elem1 = pattern "first"
          let elem2 = pattern "second"
          let elem3 = pattern "third"
          let p = patternWith "sequence" [elem1, elem2, elem3]
          let elems = elements p
          value (head elems) `shouldBe` "first"
          value (elems !! 1) `shouldBe` "second"
          value (elems !! 2) `shouldBe` "third"
        
        it "patternWith produces patterns functionally identical to record syntax" $ do
          let elem1 = pattern "elem1"
          let elem2 = pattern "elem2"
          let p1 = patternWith "test" [elem1, elem2]
          let p2 = Pattern { value = "test", elements = [elem1, elem2] }
          p1 `shouldBe` p2
          value p1 `shouldBe` value p2
          elements p1 `shouldBe` elements p2
          
          let singular1 = patternWith "soccer" [pattern "a team sport involving kicking a ball"]
          let singular2 = Pattern { value = "soccer", elements = [Pattern { value = "a team sport involving kicking a ball", elements = [] }] }
          singular1 `shouldBe` singular2
    
    describe "Constructor Functions (User Story 3)" $ do
      
      describe "fromList function - creating patterns from lists of values" $ do
        
        it "creates pattern from list of strings using fromList" $ do
          let p = fromList "graph" ["Alice", "Bob", "Charlie"]
          value p `shouldBe` "graph"
          length (elements p) `shouldBe` 3
          map value (elements p) `shouldBe` ["Alice", "Bob", "Charlie"]
        
        it "creates pattern from list of integers using fromList" $ do
          let nums = fromList (0 :: Int) [1, 2, 3, 4, 5]
          value nums `shouldBe` (0 :: Int)
          length (elements nums) `shouldBe` 5
          map value (elements nums) `shouldBe` [1, 2, 3, 4, 5]
        
        it "creates pattern from list of custom types using fromList" $ do
          let person1 = Person "Alice" (Just 30)
          let person2 = Person "Bob" (Just 25)
          let person3 = Person "Charlie" (Just 35)
          let decoration = Person "Team" Nothing
          let p = fromList decoration [person1, person2, person3]
          value p `shouldBe` decoration
          length (elements p) `shouldBe` 3
          map value (elements p) `shouldBe` [person1, person2, person3]
        
        it "empty list in fromList produces atomic pattern" $ do
          let atomic = fromList "empty" []
          value atomic `shouldBe` "empty"
          elements atomic `shouldBe` ([] :: [Pattern String])
          atomic `shouldBe` pattern "empty"
        
        it "fromList preserves value order" $ do
          let p = fromList "sequence" ["first", "second", "third"]
          let elems = elements p
          value (head elems) `shouldBe` "first"
          value (elems !! 1) `shouldBe` "second"
          value (elems !! 2) `shouldBe` "third"
        
        it "fromList converts each value to atomic pattern" $ do
          let p = fromList "graph" ["a", "b", "c"]
          let elems = elements p
          all (\e -> elements e == []) elems `shouldBe` True
          map value elems `shouldBe` ["a", "b", "c"]
        
        it "fromList produces patterns functionally identical to patternWith decoration (map pattern values)" $ do
          let values = ["a", "b", "c"]
          let p1 = fromList "test" values
          let p2 = patternWith "test" (map pattern values)
          p1 `shouldBe` p2
          value p1 `shouldBe` value p2
          elements p1 `shouldBe` elements p2
          
          let intValues = [1, 2, 3]
          let p3 = fromList (0 :: Int) intValues
          let p4 = patternWith (0 :: Int) (map pattern intValues)
          p3 `shouldBe` p4
    
    describe "Constructor Functions (User Story 4)" $ do
      
      describe "Comprehensive edge case testing - patternWith" $ do
        
        it "patternWith with 0 elements produces atomic pattern" $ do
          let atomic = patternWith "test" []
          value atomic `shouldBe` "test"
          elements atomic `shouldBe` ([] :: [Pattern String])
          atomic `shouldBe` pattern "test"
        
        it "patternWith with 1 element produces singular pattern" $ do
          let elem = pattern "definition"
          let singular = patternWith "term" [elem]
          value singular `shouldBe` "term"
          length (elements singular) `shouldBe` 1
          head (elements singular) `shouldBe` elem
        
        it "patternWith with 2 elements produces pair pattern" $ do
          let elem1 = pattern "Alice"
          let elem2 = pattern "Bob"
          let pair = patternWith "relationship" [elem1, elem2]
          value pair `shouldBe` "relationship"
          length (elements pair) `shouldBe` 2
          elements pair `shouldBe` [elem1, elem2]
        
        it "patternWith with many elements produces extended pattern" $ do
          let elems = map pattern ["a", "b", "c", "d", "e"]
          let extended = patternWith "collection" elems
          value extended `shouldBe` "collection"
          length (elements extended) `shouldBe` 5
          elements extended `shouldBe` elems
      
      describe "Comprehensive edge case testing - fromList" $ do
        
        it "fromList with 0 values produces atomic pattern" $ do
          let atomic = fromList "empty" []
          value atomic `shouldBe` "empty"
          elements atomic `shouldBe` ([] :: [Pattern String])
          atomic `shouldBe` pattern "empty"
        
        it "fromList with 1 value produces singular pattern" $ do
          let singular = fromList "term" ["definition"]
          value singular `shouldBe` "term"
          length (elements singular) `shouldBe` 1
          value (head (elements singular)) `shouldBe` "definition"
        
        it "fromList with 2 values produces pair pattern" $ do
          let pair = fromList "relationship" ["Alice", "Bob"]
          value pair `shouldBe` "relationship"
          length (elements pair) `shouldBe` 2
          map value (elements pair) `shouldBe` ["Alice", "Bob"]
        
        it "fromList with many values produces extended pattern" $ do
          let values = ["a", "b", "c", "d", "e", "f"]
          let extended = fromList "collection" values
          value extended `shouldBe` "collection"
          length (elements extended) `shouldBe` 6
          map value (elements extended) `shouldBe` values
      
      describe "Nested patterns with all constructors" $ do
        
        it "nested patterns using pattern, patternWith, and fromList" $ do
          -- Atomic pattern at base
          let base = pattern "base"
          
          -- Pattern with elements using patternWith
          let mid = patternWith "middle" [base]
          
          -- Pattern from list using fromList
          let top = fromList "top" ["value1", "value2"]
          
          -- Combine all using patternWith
          let nested = patternWith "root" [mid, top]
          
          value nested `shouldBe` "root"
          length (elements nested) `shouldBe` 2
          value (head (elements nested)) `shouldBe` "middle"
          value (elements nested !! 1) `shouldBe` "top"
          
          -- Verify nested structure
          let midElem = head (elements nested)
          length (elements midElem) `shouldBe` 1
          value (head (elements midElem)) `shouldBe` "base"
          
          let topElem = elements nested !! 1
          length (elements topElem) `shouldBe` 2
          map value (elements topElem) `shouldBe` ["value1", "value2"]
        
        it "deeply nested patterns with all constructors" $ do
          -- Level 4: atomic
          let level4 = pattern "deep"
          
          -- Level 3: fromList
          let level3 = fromList "level3" ["a", "b"]
          
          -- Level 2: patternWith
          let level2 = patternWith "level2" [level4, level3]
          
          -- Level 1: patternWith with fromList
          let level1 = patternWith "level1" [level2, fromList "other" ["x", "y"]]
          
          -- Root: patternWith
          let root = patternWith "root" [level1]
          
          value root `shouldBe` "root"
          length (elements root) `shouldBe` 1
          value (head (elements root)) `shouldBe` "level1"
          
          let l1 = head (elements root)
          length (elements l1) `shouldBe` 2
          value (head (elements l1)) `shouldBe` "level2"
      
      describe "All value types with all constructors" $ do
        
        it "string values with pattern, patternWith, and fromList" $ do
          let p1 = pattern "string"
          let p2 = patternWith "soccer" [pattern "a team sport involving kicking a ball"]
          let p3 = fromList "list" ["a", "b", "c"]
          
          value p1 `shouldBe` "string"
          value p2 `shouldBe` "soccer"
          value p3 `shouldBe` "list"
          length (elements p1) `shouldBe` 0
          length (elements p2) `shouldBe` 1
          length (elements p3) `shouldBe` 3
        
        it "integer values with pattern, patternWith, and fromList" $ do
          let p1 = pattern (42 :: Int)
          let p2 = patternWith (100 :: Int) [pattern 10, pattern 20]
          let p3 = fromList (0 :: Int) [1, 2, 3, 4]
          
          value p1 `shouldBe` (42 :: Int)
          value p2 `shouldBe` (100 :: Int)
          value p3 `shouldBe` (0 :: Int)
          length (elements p1) `shouldBe` 0
          length (elements p2) `shouldBe` 2
          length (elements p3) `shouldBe` 4
        
        it "custom type values with pattern, patternWith, and fromList" $ do
          let person1 = Person "Alice" (Just 30)
          let person2 = Person "Bob" (Just 25)
          let person3 = Person "Charlie" (Just 35)
          let decoration = Person "Team" Nothing
          
          let p1 = pattern person1
          let p2 = patternWith decoration [pattern person1, pattern person2]
          let p3 = fromList decoration [person1, person2, person3]
          
          value p1 `shouldBe` person1
          value p2 `shouldBe` decoration
          value p3 `shouldBe` decoration
          length (elements p1) `shouldBe` 0
          length (elements p2) `shouldBe` 2
          length (elements p3) `shouldBe` 3
          
          map value (elements p2) `shouldBe` [person1, person2]
          map value (elements p3) `shouldBe` [person1, person2, person3]
    
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