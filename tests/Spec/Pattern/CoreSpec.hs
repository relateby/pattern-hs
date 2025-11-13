-- | Unit tests for Pattern.Core module.
module Spec.Pattern.CoreSpec where

import Control.Monad.State (State, get, put, runState)
import Data.Char (toUpper)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldMap, toList)
import Data.Functor.Identity (Identity(..))
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid (All(..), Sum(..))
import qualified Data.Set as Set
import Test.Hspec
import Pattern.Core (Pattern(..), pattern, patternWith, fromList, toTuple, size, depth, values)
import qualified Pattern.Core as PC

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
          let person1 = Person "Alice" (Just 30)
          let person2 = Person "Bob" (Just 25)
          let elem1 = Pattern { value = person1, elements = [] }
          let elem2 = Pattern { value = person2, elements = [] }
          let pattern = Pattern { value = person1, elements = [elem1, elem2] }
          let transformed = fmap (\p -> Person (name p) (fmap (+ 1) (age p))) pattern
          age (value transformed) `shouldBe` Just 31
          age (value (head (elements transformed))) `shouldBe` Just 31
          age (value (last (elements transformed))) `shouldBe` Just 26
      
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
          let person = Person "Alice" (Just 30)
          let pattern = Pattern { value = person, elements = [] }
          let transformed = fmap (\p -> Person (name p) (fmap (+ 5) (age p))) pattern
          age (value transformed) `shouldBe` Just 35
          name (value transformed) `shouldBe` "Alice"
        
        it "transforms patterns with nested custom types" $ do
          let person1 = Person "Alice" (Just 30)
          let person2 = Person "Bob" (Just 25)
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
    
    describe "Foldable Instance (User Story 1)" $ do
      
      describe "Folding atomic patterns with foldr" $ do
        
        it "folds atomic pattern with integer value using foldr" $ do
          let atom = Pattern { value = 5, elements = [] }
          foldr (+) 0 atom `shouldBe` (5 :: Int)
        
        it "folds atomic pattern with string value using foldr" $ do
          let atom = Pattern { value = "test", elements = [] }
          foldr (++) "" atom `shouldBe` "test"
      
      describe "Folding patterns with multiple values using foldr" $ do
        
        it "folds pattern with multiple integer values using foldr" $ do
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let elem3 = Pattern { value = 30, elements = [] }
          let pattern = Pattern { value = 100, elements = [elem1, elem2, elem3] }
          -- Should sum: 100 (pattern's value) + 10 + 20 + 30 = 160
          foldr (+) 0 pattern `shouldBe` (160 :: Int)
        
        it "folds pattern with string values using foldr" $ do
          let elem1 = Pattern { value = "hello", elements = [] }
          let elem2 = Pattern { value = "world", elements = [] }
          let pattern = Pattern { value = "greeting", elements = [elem1, elem2] }
          -- Should concatenate: "greeting" ++ "hello" ++ "world" = "greetinghelloworld"
          foldr (++) "" pattern `shouldBe` "greetinghelloworld"
      
      describe "Folding nested pattern structures using foldr" $ do
        
        it "folds nested pattern structure using foldr" $ do
          let inner = Pattern { value = 1, elements = [] }
          let middle = Pattern { value = 2, elements = [inner] }
          let outer = Pattern { value = 3, elements = [middle] }
          let pattern = Pattern { value = 4, elements = [outer] }
          -- Should sum: 4 + 3 + 2 + 1 = 10
          foldr (+) 0 pattern `shouldBe` (10 :: Int)
        
        it "folds deeply nested pattern structure using foldr" $ do
          let level4 = Pattern { value = 1, elements = [] }
          let level3 = Pattern { value = 2, elements = [level4] }
          let level2 = Pattern { value = 3, elements = [level3] }
          let level1 = Pattern { value = 4, elements = [level2] }
          let pattern = Pattern { value = 5, elements = [level1] }
          -- Should sum: 5 + 4 + 3 + 2 + 1 = 15
          foldr (+) 0 pattern `shouldBe` (15 :: Int)
      
      describe "Folding patterns with custom type values using foldr" $ do
        
        it "folds pattern with custom type values using foldr" $ do
          let person1 = Person "Alice" (Just 30)
          let person2 = Person "Bob" (Just 25)
          let elem1 = Pattern { value = person1, elements = [] }
          let elem2 = Pattern { value = person2, elements = [] }
          let pattern = Pattern { value = person1, elements = [elem1, elem2] }
          -- Count all Person values (pattern's value + 2 elements = 3)
          foldr (\_ acc -> acc + 1) 0 pattern `shouldBe` (3 :: Int)
      
      describe "Verifying foldr processes pattern's own value" $ do
        
        it "foldr processes pattern's own value" $ do
          let pattern = Pattern { value = 42, elements = [] }
          -- Should include the pattern's own value (42)
          foldr (+) 0 pattern `shouldBe` (42 :: Int)
        
        it "foldr processes pattern's own value even when elements exist" $ do
          let elem1 = Pattern { value = 10, elements = [] }
          let pattern = Pattern { value = 5, elements = [elem1] }
          -- Should sum: 5 (pattern's value) + 10 (element) = 15
          foldr (+) 0 pattern `shouldBe` (15 :: Int)
      
      describe "Verifying foldr processes all element values recursively" $ do
        
        it "foldr processes all element values recursively" $ do
          let elem1 = Pattern { value = 1, elements = [] }
          let elem2 = Pattern { value = 2, elements = [] }
          let elem3 = Pattern { value = 3, elements = [] }
          let pattern = Pattern { value = 0, elements = [elem1, elem2, elem3] }
          -- Should sum: 0 + 1 + 2 + 3 = 6
          foldr (+) 0 pattern `shouldBe` (6 :: Int)
        
        it "foldr processes nested element values recursively" $ do
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle = Pattern { value = 10, elements = [inner1, inner2] }
          let pattern = Pattern { value = 100, elements = [middle] }
          -- Should sum: 100 + 10 + 1 + 2 = 113
          foldr (+) 0 pattern `shouldBe` (113 :: Int)
        
        it "foldr processes all values from multiple nested elements" $ do
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle1 = Pattern { value = 10, elements = [inner1] }
          let middle2 = Pattern { value = 20, elements = [inner2] }
          let pattern = Pattern { value = 100, elements = [middle1, middle2] }
          -- Should sum: 100 + 10 + 1 + 20 + 2 = 133
          foldr (+) 0 pattern `shouldBe` (133 :: Int)
    
    describe "toList Operation (User Story 2)" $ do
      
      describe "toList on atomic patterns" $ do
        
        it "toList on atomic pattern returns single-element list" $ do
          let atom = Pattern { value = "test", elements = [] }
          toList atom `shouldBe` ["test"]
      
      describe "toList on patterns with multiple elements" $ do
        
        it "toList on pattern with multiple elements returns flat list with all values" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let elem3 = Pattern { value = "c", elements = [] }
          let pattern = Pattern { value = "root", elements = [elem1, elem2, elem3] }
          -- Should return flat list: ["root", "a", "b", "c"]
          toList pattern `shouldBe` ["root", "a", "b", "c"]
      
      describe "toList on nested patterns" $ do
        
        it "toList on nested pattern returns flat list with all values from all levels" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let pattern = Pattern { value = "root", elements = [outer] }
          -- Should return flat list: ["root", "outer", "middle", "inner"]
          toList pattern `shouldBe` ["root", "outer", "middle", "inner"]
      
      describe "toList on patterns with integer values" $ do
        
        it "toList on pattern with integer values returns flat list of integers" $ do
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let pattern = Pattern { value = 100, elements = [elem1, elem2] }
          -- Should return flat list: [100, 10, 20]
          toList pattern `shouldBe` [100, 10, 20]
      
      describe "Verifying toList includes pattern's own value" $ do
        
        it "toList includes pattern's own value" $ do
          let pattern = Pattern { value = "test", elements = [] }
          toList pattern `shouldBe` ["test"]
        
        it "toList includes pattern's own value even when elements exist" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let pattern = Pattern { value = "root", elements = [elem1] }
          -- Should include "root" as first element
          toList pattern `shouldBe` ["root", "a"]
      
      describe "Verifying toList preserves element order" $ do
        
        it "toList preserves element order" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let elem3 = Pattern { value = "third", elements = [] }
          let pattern = Pattern { value = "root", elements = [elem1, elem2, elem3] }
          -- Should preserve order: root, first, second, third
          toList pattern `shouldBe` ["root", "first", "second", "third"]
        
        it "toList preserves order in nested structures" $ do
          let inner1 = Pattern { value = "inner1", elements = [] }
          let inner2 = Pattern { value = "inner2", elements = [] }
          let middle1 = Pattern { value = "middle1", elements = [inner1] }
          let middle2 = Pattern { value = "middle2", elements = [inner2] }
          let pattern = Pattern { value = "root", elements = [middle1, middle2] }
          -- Should preserve order: root, middle1, inner1, middle2, inner2
          toList pattern `shouldBe` ["root", "middle1", "inner1", "middle2", "inner2"]
    
    describe "toTuple Operation (User Story 2b)" $ do
      
      describe "toTuple on atomic patterns" $ do
        
        it "toTuple on atomic pattern returns tuple with value and empty list" $ do
          let atom = Pattern { value = "test", elements = [] }
          toTuple atom `shouldBe` ("test", [] :: [Pattern String])
        
        it "toTuple on atomic pattern with integer value returns tuple with integer and empty list" $ do
          let atom = Pattern { value = 42, elements = [] }
          toTuple atom `shouldBe` (42 :: Int, [] :: [Pattern Int])
      
      describe "toTuple on patterns with multiple elements" $ do
        
        it "toTuple on pattern with multiple elements returns tuple with value and list of element patterns" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let elem3 = Pattern { value = "c", elements = [] }
          let pattern = Pattern { value = "root", elements = [elem1, elem2, elem3] }
          toTuple pattern `shouldBe` ("root", [elem1, elem2, elem3])
        
        it "toTuple on pattern with integer values returns tuple with integer value and list of Pattern Int" $ do
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let pattern = Pattern { value = 100, elements = [elem1, elem2] }
          toTuple pattern `shouldBe` (100 :: Int, [elem1, elem2])
      
      describe "toTuple on nested patterns" $ do
        
        it "toTuple on nested pattern returns tuple where elements list contains nested Pattern structures" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let pattern = Pattern { value = "root", elements = [outer] }
          let (val, els) = toTuple pattern
          val `shouldBe` "root"
          length els `shouldBe` 1
          head els `shouldBe` outer
          -- Verify nested structure is preserved
          let (outerVal, outerEls) = toTuple (head els)
          outerVal `shouldBe` "outer"
          length outerEls `shouldBe` 1
          head outerEls `shouldBe` middle
      
      describe "Verifying toTuple preserves pattern structure" $ do
        
        it "toTuple preserves pattern structure" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let pattern = Pattern { value = "root", elements = [elem1, elem2] }
          let (val, els) = toTuple pattern
          val `shouldBe` "root"
          length els `shouldBe` 2
          els `shouldBe` [elem1, elem2]
          -- Verify elements are still Pattern structures, not flattened
          value (head els) `shouldBe` "first"
          value (last els) `shouldBe` "second"
        
        it "toTuple preserves nested pattern structure" $ do
          let inner1 = Pattern { value = "inner1", elements = [] }
          let inner2 = Pattern { value = "inner2", elements = [] }
          let middle1 = Pattern { value = "middle1", elements = [inner1] }
          let middle2 = Pattern { value = "middle2", elements = [inner2] }
          let pattern = Pattern { value = "root", elements = [middle1, middle2] }
          let (val, els) = toTuple pattern
          val `shouldBe` "root"
          length els `shouldBe` 2
          -- Verify nested structures are preserved
          let (mid1Val, mid1Els) = toTuple (head els)
          mid1Val `shouldBe` "middle1"
          length mid1Els `shouldBe` 1
          head mid1Els `shouldBe` inner1
          let (mid2Val, mid2Els) = toTuple (last els)
          mid2Val `shouldBe` "middle2"
          length mid2Els `shouldBe` 1
          head mid2Els `shouldBe` inner2
    
    describe "Fold with Right-Associative Operations (User Story 3)" $ do
      
      describe "foldr processing values in correct order with addition" $ do
        
        it "foldr processes values in correct order with addition" $ do
          -- Pattern: value=100, elements=[10, 20]
          -- foldr should process: pattern's value (100) first, then elements in order (10, 20)
          -- Result: 100 + 10 + 20 = 130
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let pattern = Pattern { value = 100, elements = [elem1, elem2] }
          -- foldr (+) 0 should sum: 100 + 10 + 20 = 130
          foldr (+) 0 pattern `shouldBe` (130 :: Int)
        
        it "foldr processes values in correct order for pattern with three elements" $ do
          -- Pattern: value=1, elements=[2, 3, 4]
          -- foldr should process: 1 + 2 + 3 + 4 = 10
          let elem1 = Pattern { value = 2, elements = [] }
          let elem2 = Pattern { value = 3, elements = [] }
          let elem3 = Pattern { value = 4, elements = [] }
          let pattern = Pattern { value = 1, elements = [elem1, elem2, elem3] }
          foldr (+) 0 pattern `shouldBe` (10 :: Int)
        
        it "foldr processes atomic pattern value correctly" $ do
          -- Atomic pattern: value=42
          -- foldr should process: 42
          let atom = Pattern { value = 42, elements = [] }
          foldr (+) 0 atom `shouldBe` (42 :: Int)
      
      describe "foldr building list in correct order with string values" $ do
        
        it "foldr builds list in correct order with string values" $ do
          -- Pattern: value="root", elements=["a", "b"]
          -- foldr (:) [] should produce: ["root", "a", "b"]
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let pattern = Pattern { value = "root", elements = [elem1, elem2] }
          -- toList uses foldr internally, should preserve order: pattern's value first, then elements
          toList pattern `shouldBe` ["root", "a", "b"]
        
        it "foldr builds list in correct order for pattern with multiple string elements" $ do
          -- Pattern: value="first", elements=["second", "third", "fourth"]
          -- foldr should produce: ["first", "second", "third", "fourth"]
          let elem1 = Pattern { value = "second", elements = [] }
          let elem2 = Pattern { value = "third", elements = [] }
          let elem3 = Pattern { value = "fourth", elements = [] }
          let pattern = Pattern { value = "first", elements = [elem1, elem2, elem3] }
          toList pattern `shouldBe` ["first", "second", "third", "fourth"]
        
        it "foldr builds list correctly for atomic pattern with string value" $ do
          -- Atomic pattern: value="test"
          -- foldr should produce: ["test"]
          let atom = Pattern { value = "test", elements = [] }
          toList atom `shouldBe` ["test"]
      
      describe "foldr processing nested pattern values in correct order" $ do
        
        it "foldr processes nested pattern values in correct order" $ do
          -- Nested structure:
          -- Pattern { value = 4, elements = [
          --   Pattern { value = 3, elements = [
          --     Pattern { value = 2, elements = [
          --       Pattern { value = 1, elements = [] }
          --     ]}
          --   ]}
          -- ]}
          -- foldr should process: 4 + 3 + 2 + 1 = 10
          let inner = Pattern { value = 1, elements = [] }
          let middle = Pattern { value = 2, elements = [inner] }
          let outer = Pattern { value = 3, elements = [middle] }
          let pattern = Pattern { value = 4, elements = [outer] }
          foldr (+) 0 pattern `shouldBe` (10 :: Int)
        
        it "foldr processes nested pattern values in correct order for multiple nested elements" $ do
          -- Pattern { value = 100, elements = [
          --   Pattern { value = 10, elements = [Pattern { value = 1, elements = [] }] },
          --   Pattern { value = 20, elements = [Pattern { value = 2, elements = [] }] }
          -- ]}
          -- foldr should process: 100 + 10 + 1 + 20 + 2 = 133
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle1 = Pattern { value = 10, elements = [inner1] }
          let middle2 = Pattern { value = 20, elements = [inner2] }
          let pattern = Pattern { value = 100, elements = [middle1, middle2] }
          foldr (+) 0 pattern `shouldBe` (133 :: Int)
        
        it "foldr processes deeply nested pattern values in correct order" $ do
          -- 5 levels deep: 5 + 4 + 3 + 2 + 1 = 15
          let level4 = Pattern { value = 1, elements = [] }
          let level3 = Pattern { value = 2, elements = [level4] }
          let level2 = Pattern { value = 3, elements = [level3] }
          let level1 = Pattern { value = 4, elements = [level2] }
          let pattern = Pattern { value = 5, elements = [level1] }
          foldr (+) 0 pattern `shouldBe` (15 :: Int)
      
      describe "foldr right-associativity property" $ do
        
        it "foldr right-associativity property: foldr f z = foldr f z . toList" $ do
          -- For commutative operations, foldr on pattern should equal foldr on toList
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let pattern = Pattern { value = 100, elements = [elem1, elem2] }
          -- Addition is commutative, so both should produce same result
          let patternFold = foldr (+) 0 pattern
          let listFold = foldr (+) 0 (toList pattern)
          patternFold `shouldBe` listFold
          patternFold `shouldBe` (130 :: Int)
        
        it "foldr right-associativity property: order matters for non-commutative operations" $ do
          -- For non-commutative operations like list building, order is preserved
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let pattern = Pattern { value = "root", elements = [elem1, elem2] }
          -- toList preserves order: pattern's value first, then elements
          toList pattern `shouldBe` ["root", "a", "b"]
          -- foldr (:) [] should produce same order
          foldr (:) [] pattern `shouldBe` ["root", "a", "b"]
        
        it "foldr right-associativity property: pattern value processed before elements" $ do
          -- Verify that pattern's own value is processed first (combined with accumulated elements)
          -- Using a function that reveals order: building a list with markers
          let elem1 = Pattern { value = "elem1", elements = [] }
          let elem2 = Pattern { value = "elem2", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem1, elem2] }
          -- foldr builds: pattern's value first, then elements in order
          toList pattern `shouldBe` ["pattern", "elem1", "elem2"]
          -- Verify order by checking first element is pattern's value
          head (toList pattern) `shouldBe` "pattern"
    
    describe "Fold with Left-Associative Operations (User Story 4)" $ do
      
      describe "foldl processing values in left-to-right order with addition" $ do
        
        it "foldl processes values in left-to-right order with addition" $ do
          -- Pattern: value=100, elements=[10, 20]
          -- foldl should process: ((0 + 100) + 10) + 20 = 130
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let pattern = Pattern { value = 100, elements = [elem1, elem2] }
          -- foldl (+) 0 should sum: ((0 + 100) + 10) + 20 = 130
          foldl (+) 0 pattern `shouldBe` (130 :: Int)
        
        it "foldl processes values in left-to-right order for pattern with three elements" $ do
          -- Pattern: value=1, elements=[2, 3, 4]
          -- foldl should process: (((0 + 1) + 2) + 3) + 4 = 10
          let elem1 = Pattern { value = 2, elements = [] }
          let elem2 = Pattern { value = 3, elements = [] }
          let elem3 = Pattern { value = 4, elements = [] }
          let pattern = Pattern { value = 1, elements = [elem1, elem2, elem3] }
          foldl (+) 0 pattern `shouldBe` (10 :: Int)
        
        it "foldl processes atomic pattern value correctly" $ do
          -- Atomic pattern: value=42
          -- foldl should process: 0 + 42 = 42
          let atom = Pattern { value = 42, elements = [] }
          foldl (+) 0 atom `shouldBe` (42 :: Int)
      
      describe "foldl computing running total correctly with integer values" $ do
        
        it "foldl computes running total correctly with integer values" $ do
          -- Pattern: value=10, elements=[20, 30]
          -- foldl should compute: ((0 + 10) + 20) + 30 = 60
          let elem1 = Pattern { value = 20, elements = [] }
          let elem2 = Pattern { value = 30, elements = [] }
          let pattern = Pattern { value = 10, elements = [elem1, elem2] }
          foldl (+) 0 pattern `shouldBe` (60 :: Int)
        
        it "foldl computes running total correctly for nested pattern" $ do
          -- Pattern: value=1, elements=[Pattern { value=2, elements=[Pattern { value=3 }] }]
          -- foldl should compute: (((0 + 1) + 2) + 3) = 6
          let inner = Pattern { value = 3, elements = [] }
          let middle = Pattern { value = 2, elements = [inner] }
          let pattern = Pattern { value = 1, elements = [middle] }
          foldl (+) 0 pattern `shouldBe` (6 :: Int)
        
        it "foldl computes running total correctly with multiple nested elements" $ do
          -- Pattern: value=100, elements=[Pattern { value=10, elements=[Pattern { value=1 }] }, Pattern { value=20, elements=[Pattern { value=2 }] }]
          -- foldl should compute: (((((0 + 100) + 10) + 1) + 20) + 2) = 133
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle1 = Pattern { value = 10, elements = [inner1] }
          let middle2 = Pattern { value = 20, elements = [inner2] }
          let pattern = Pattern { value = 100, elements = [middle1, middle2] }
          foldl (+) 0 pattern `shouldBe` (133 :: Int)
      
      describe "foldl processing nested pattern values in left-to-right order" $ do
        
        it "foldl processes nested pattern values in left-to-right order" $ do
          -- Nested structure:
          -- Pattern { value = 4, elements = [
          --   Pattern { value = 3, elements = [
          --     Pattern { value = 2, elements = [
          --       Pattern { value = 1, elements = [] }
          --     ]}
          --   ]}
          -- ]}
          -- foldl should process: ((((0 + 4) + 3) + 2) + 1) = 10
          let inner = Pattern { value = 1, elements = [] }
          let middle = Pattern { value = 2, elements = [inner] }
          let outer = Pattern { value = 3, elements = [middle] }
          let pattern = Pattern { value = 4, elements = [outer] }
          foldl (+) 0 pattern `shouldBe` (10 :: Int)
        
        it "foldl processes nested pattern values in left-to-right order for multiple nested elements" $ do
          -- Pattern { value = 100, elements = [
          --   Pattern { value = 10, elements = [Pattern { value = 1, elements = [] }] },
          --   Pattern { value = 20, elements = [Pattern { value = 2, elements = [] }] }
          -- ]}
          -- foldl should process: (((((0 + 100) + 10) + 1) + 20) + 2) = 133
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle1 = Pattern { value = 10, elements = [inner1] }
          let middle2 = Pattern { value = 20, elements = [inner2] }
          let pattern = Pattern { value = 100, elements = [middle1, middle2] }
          foldl (+) 0 pattern `shouldBe` (133 :: Int)
        
        it "foldl processes deeply nested pattern values in left-to-right order" $ do
          -- 5 levels deep: (((((0 + 5) + 4) + 3) + 2) + 1) = 15
          let level4 = Pattern { value = 1, elements = [] }
          let level3 = Pattern { value = 2, elements = [level4] }
          let level2 = Pattern { value = 3, elements = [level3] }
          let level1 = Pattern { value = 4, elements = [level2] }
          let pattern = Pattern { value = 5, elements = [level1] }
          foldl (+) 0 pattern `shouldBe` (15 :: Int)
      
      describe "foldl left-associativity property" $ do
        
        it "foldl left-associativity property: foldl f z = foldl f z . toList for commutative operations" $ do
          -- For commutative operations, foldl on pattern should equal foldl on toList
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let pattern = Pattern { value = 100, elements = [elem1, elem2] }
          -- Addition is commutative, so both should produce same result
          let patternFold = foldl (+) 0 pattern
          let listFold = foldl (+) 0 (toList pattern)
          patternFold `shouldBe` listFold
          patternFold `shouldBe` (130 :: Int)
        
        it "foldl left-associativity property: order matters for non-commutative operations" $ do
          -- For non-commutative operations like subtraction, order matters
          let elem1 = Pattern { value = 5, elements = [] }
          let elem2 = Pattern { value = 3, elements = [] }
          let pattern = Pattern { value = 10, elements = [elem1, elem2] }
          -- foldl (-) 0 should compute: (((0 - 10) - 5) - 3) = -18
          foldl (-) 0 pattern `shouldBe` (-18 :: Int)
        
        it "foldl left-associativity property: pattern value processed first" $ do
          -- Verify that pattern's own value is processed first (left-to-right)
          -- Using subtraction to reveal order: ((0 - pattern) - elem1) - elem2
          let elem1 = Pattern { value = 5, elements = [] }
          let elem2 = Pattern { value = 3, elements = [] }
          let pattern = Pattern { value = 10, elements = [elem1, elem2] }
          -- foldl (-) 0 should compute: (((0 - 10) - 5) - 3) = -18
          foldl (-) 0 pattern `shouldBe` (-18 :: Int)
          -- Verify by checking intermediate steps
          foldl (-) 0 (Pattern { value = 10, elements = [] }) `shouldBe` (-10 :: Int)
    
    describe "Map Values to Monoids and Combine (User Story 5)" $ do
      
      describe "foldMap with Sum monoid on integer pattern" $ do
        
        it "foldMap with Sum monoid on atomic pattern with integer value" $ do
          let atom = Pattern { value = 5, elements = [] }
          getSum (foldMap Sum atom) `shouldBe` (5 :: Int)
        
        it "foldMap with Sum monoid on pattern with multiple integer values" $ do
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let elem3 = Pattern { value = 30, elements = [] }
          let pattern = Pattern { value = 100, elements = [elem1, elem2, elem3] }
          -- Should sum: 100 + 10 + 20 + 30 = 160
          getSum (foldMap Sum pattern) `shouldBe` (160 :: Int)
        
        it "foldMap with Sum monoid on pattern with negative integer values" $ do
          let elem1 = Pattern { value = -5, elements = [] }
          let elem2 = Pattern { value = -10, elements = [] }
          let pattern = Pattern { value = 100, elements = [elem1, elem2] }
          -- Should sum: 100 + (-5) + (-10) = 85
          getSum (foldMap Sum pattern) `shouldBe` (85 :: Int)
      
      describe "foldMap with list monoid on string pattern" $ do
        
        it "foldMap with list monoid on atomic pattern with string value" $ do
          let atom = Pattern { value = "test", elements = [] }
          foldMap (: []) atom `shouldBe` ["test"]
        
        it "foldMap with list monoid on pattern with multiple string values" $ do
          let elem1 = Pattern { value = "hello", elements = [] }
          let elem2 = Pattern { value = "world", elements = [] }
          let pattern = Pattern { value = "greeting", elements = [elem1, elem2] }
          -- Should concatenate: ["greeting", "hello", "world"]
          foldMap (: []) pattern `shouldBe` ["greeting", "hello", "world"]
        
        it "foldMap with list monoid concatenates string values correctly" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let elem3 = Pattern { value = "c", elements = [] }
          let pattern = Pattern { value = "root", elements = [elem1, elem2, elem3] }
          -- Should produce: ["root", "a", "b", "c"]
          foldMap (: []) pattern `shouldBe` ["root", "a", "b", "c"]
      
      describe "foldMap with All monoid on boolean pattern" $ do
        
        it "foldMap with All monoid on atomic pattern with boolean value" $ do
          let atom = Pattern { value = True, elements = [] }
          getAll (foldMap All atom) `shouldBe` True
        
        it "foldMap with All monoid on pattern with multiple boolean values (all True)" $ do
          let elem1 = Pattern { value = True, elements = [] }
          let elem2 = Pattern { value = True, elements = [] }
          let pattern = Pattern { value = True, elements = [elem1, elem2] }
          -- Should produce: True && True && True = True
          getAll (foldMap All pattern) `shouldBe` True
        
        it "foldMap with All monoid on pattern with multiple boolean values (one False)" $ do
          let elem1 = Pattern { value = True, elements = [] }
          let elem2 = Pattern { value = False, elements = [] }
          let pattern = Pattern { value = True, elements = [elem1, elem2] }
          -- Should produce: True && True && False = False
          getAll (foldMap All pattern) `shouldBe` False
        
        it "foldMap with All monoid on pattern with multiple boolean values (all False)" $ do
          let elem1 = Pattern { value = False, elements = [] }
          let elem2 = Pattern { value = False, elements = [] }
          let pattern = Pattern { value = False, elements = [elem1, elem2] }
          -- Should produce: False && False && False = False
          getAll (foldMap All pattern) `shouldBe` False
      
      describe "foldMap processing nested pattern values correctly" $ do
        
        it "foldMap with Sum monoid processes nested pattern values correctly" $ do
          let inner = Pattern { value = 1, elements = [] }
          let middle = Pattern { value = 2, elements = [inner] }
          let outer = Pattern { value = 3, elements = [middle] }
          let pattern = Pattern { value = 4, elements = [outer] }
          -- Should sum: 4 + 3 + 2 + 1 = 10
          getSum (foldMap Sum pattern) `shouldBe` (10 :: Int)
        
        it "foldMap with list monoid processes nested pattern values correctly" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let pattern = Pattern { value = "root", elements = [outer] }
          -- Should produce: ["root", "outer", "middle", "inner"]
          foldMap (: []) pattern `shouldBe` ["root", "outer", "middle", "inner"]
        
        it "foldMap with All monoid processes nested pattern values correctly" $ do
          let inner = Pattern { value = True, elements = [] }
          let middle = Pattern { value = True, elements = [inner] }
          let outer = Pattern { value = True, elements = [middle] }
          let pattern = Pattern { value = True, elements = [outer] }
          -- Should produce: True && True && True && True = True
          getAll (foldMap All pattern) `shouldBe` True
        
        it "foldMap processes deeply nested pattern values correctly" $ do
          let level4 = Pattern { value = 1, elements = [] }
          let level3 = Pattern { value = 2, elements = [level4] }
          let level2 = Pattern { value = 3, elements = [level3] }
          let level1 = Pattern { value = 4, elements = [level2] }
          let pattern = Pattern { value = 5, elements = [level1] }
          -- Should sum: 5 + 4 + 3 + 2 + 1 = 15
          getSum (foldMap Sum pattern) `shouldBe` (15 :: Int)
        
        it "foldMap processes multiple nested elements correctly" $ do
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle1 = Pattern { value = 10, elements = [inner1] }
          let middle2 = Pattern { value = 20, elements = [inner2] }
          let pattern = Pattern { value = 100, elements = [middle1, middle2] }
          -- Should sum: 100 + 10 + 1 + 20 + 2 = 133
          getSum (foldMap Sum pattern) `shouldBe` (133 :: Int)
    
    describe "Edge Cases & Comprehensive Testing (Phase 7)" $ do
      
      describe "Folding atomic patterns (no elements)" $ do
        
        it "folds atomic pattern (no elements) with foldr" $ do
          let atom = Pattern { value = 42, elements = [] }
          foldr (+) 0 atom `shouldBe` (42 :: Int)
          toList atom `shouldBe` [42]
          foldl (+) 0 atom `shouldBe` (42 :: Int)
          getSum (foldMap Sum atom) `shouldBe` (42 :: Int)
        
        it "folds atomic pattern (no elements) with string value" $ do
          let atom = Pattern { value = "test", elements = [] }
          foldr (++) "" atom `shouldBe` "test"
          toList atom `shouldBe` ["test"]
          foldl (++) "" atom `shouldBe` "test"
          foldMap (: []) atom `shouldBe` ["test"]
        
        it "folds atomic pattern (no elements) with custom type" $ do
          let person = Person "Alice" (Just 30)
          let atom = Pattern { value = person, elements = [] }
          foldr (\_ acc -> acc + 1) 0 atom `shouldBe` (1 :: Int)
          toList atom `shouldBe` [person]
          length (toList atom) `shouldBe` 1
      
      describe "Folding patterns with empty elements list" $ do
        
        it "folds pattern with empty elements list using foldr" $ do
          let pattern = Pattern { value = 10, elements = [] }
          foldr (+) 0 pattern `shouldBe` (10 :: Int)
          toList pattern `shouldBe` [10]
          length (toList pattern) `shouldBe` 1
        
        it "folds pattern with empty elements list using foldl" $ do
          let pattern = Pattern { value = 20, elements = [] }
          foldl (+) 0 pattern `shouldBe` (20 :: Int)
          toList pattern `shouldBe` [20]
        
        it "folds pattern with empty elements list using foldMap" $ do
          let pattern = Pattern { value = 30, elements = [] }
          getSum (foldMap Sum pattern) `shouldBe` (30 :: Int)
          toList pattern `shouldBe` [30]
        
        it "folds pattern with empty elements list preserves structure" $ do
          let pattern = Pattern { value = "empty", elements = [] }
          toTuple pattern `shouldBe` ("empty", [] :: [Pattern String])
          toList pattern `shouldBe` ["empty"]
      
      describe "Folding singular patterns (one element)" $ do
        
        it "folds singular pattern (one element) using foldr" $ do
          let elem = Pattern { value = 5, elements = [] }
          let pattern = Pattern { value = 10, elements = [elem] }
          -- Should sum: 10 + 5 = 15
          foldr (+) 0 pattern `shouldBe` (15 :: Int)
          toList pattern `shouldBe` [10, 5]
        
        it "folds singular pattern (one element) using foldl" $ do
          let elem = Pattern { value = 3, elements = [] }
          let pattern = Pattern { value = 7, elements = [elem] }
          -- Should sum: ((0 + 7) + 3) = 10
          foldl (+) 0 pattern `shouldBe` (10 :: Int)
          toList pattern `shouldBe` [7, 3]
        
        it "folds singular pattern (one element) using foldMap" $ do
          let elem = Pattern { value = 2, elements = [] }
          let pattern = Pattern { value = 8, elements = [elem] }
          -- Should sum: 8 + 2 = 10
          getSum (foldMap Sum pattern) `shouldBe` (10 :: Int)
          toList pattern `shouldBe` [8, 2]
        
        it "folds singular pattern (one element) with string values" $ do
          let elem = Pattern { value = "world", elements = [] }
          let pattern = Pattern { value = "hello", elements = [elem] }
          toList pattern `shouldBe` ["hello", "world"]
          foldr (++) "" pattern `shouldBe` "helloworld"
      
      describe "Folding patterns with many elements" $ do
        
        it "folds pattern with many elements using foldr" $ do
          let elems = map (\i -> Pattern { value = i, elements = [] }) [1..10]
          let pattern = Pattern { value = 100, elements = elems }
          -- Should sum: 100 + 1 + 2 + ... + 10 = 100 + 55 = 155
          foldr (+) 0 pattern `shouldBe` (155 :: Int)
          length (toList pattern) `shouldBe` 11
          head (toList pattern) `shouldBe` 100
        
        it "folds pattern with many elements using foldl" $ do
          let elems = map (\i -> Pattern { value = i, elements = [] }) [1..5]
          let pattern = Pattern { value = 50, elements = elems }
          -- Should sum: (((((0 + 50) + 1) + 2) + 3) + 4) + 5 = 65
          foldl (+) 0 pattern `shouldBe` (65 :: Int)
          length (toList pattern) `shouldBe` 6
        
        it "folds pattern with many elements using foldMap" $ do
          let elems = map (\i -> Pattern { value = i * 2, elements = [] }) [1..5]
          let pattern = Pattern { value = 100, elements = elems }
          -- Should sum: 100 + 2 + 4 + 6 + 8 + 10 = 130
          getSum (foldMap Sum pattern) `shouldBe` (130 :: Int)
          length (toList pattern) `shouldBe` 6
        
        it "folds pattern with many string elements" $ do
          let elems = map (\i -> Pattern { value = "elem" ++ show i, elements = [] }) [1..10]
          let pattern = Pattern { value = "root", elements = elems }
          length (toList pattern) `shouldBe` 11
          head (toList pattern) `shouldBe` "root"
          last (toList pattern) `shouldBe` "elem10"
      
      describe "Folding nested patterns with varying depths" $ do
        
        it "folds nested patterns with depth 2" $ do
          let inner = Pattern { value = 1, elements = [] }
          let pattern = Pattern { value = 2, elements = [inner] }
          foldr (+) 0 pattern `shouldBe` (3 :: Int)
          toList pattern `shouldBe` [2, 1]
        
        it "folds nested patterns with depth 3" $ do
          let inner = Pattern { value = 1, elements = [] }
          let middle = Pattern { value = 2, elements = [inner] }
          let pattern = Pattern { value = 3, elements = [middle] }
          foldr (+) 0 pattern `shouldBe` (6 :: Int)
          toList pattern `shouldBe` [3, 2, 1]
        
        it "folds nested patterns with depth 4" $ do
          let level3 = Pattern { value = 1, elements = [] }
          let level2 = Pattern { value = 2, elements = [level3] }
          let level1 = Pattern { value = 3, elements = [level2] }
          let pattern = Pattern { value = 4, elements = [level1] }
          foldr (+) 0 pattern `shouldBe` (10 :: Int)
          toList pattern `shouldBe` [4, 3, 2, 1]
        
        it "folds nested patterns with varying depths in different branches" $ do
          -- Branch 1: depth 2
          let branch1Inner = Pattern { value = 1, elements = [] }
          let branch1 = Pattern { value = 10, elements = [branch1Inner] }
          -- Branch 2: depth 3
          let branch2Level2 = Pattern { value = 2, elements = [] }
          let branch2Level1 = Pattern { value = 20, elements = [branch2Level2] }
          let branch2 = Pattern { value = 200, elements = [branch2Level1] }
          -- Branch 3: depth 1 (atomic)
          let branch3 = Pattern { value = 300, elements = [] }
          let pattern = Pattern { value = 1000, elements = [branch1, branch2, branch3] }
          -- Should sum: 1000 + 10 + 1 + 200 + 20 + 2 + 300 = 1533
          foldr (+) 0 pattern `shouldBe` (1533 :: Int)
          length (toList pattern) `shouldBe` 7
        
        it "folds nested patterns with multiple elements at each level" $ do
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle = Pattern { value = 10, elements = [inner1, inner2] }
          let pattern = Pattern { value = 100, elements = [middle] }
          -- Should sum: 100 + 10 + 1 + 2 = 113
          foldr (+) 0 pattern `shouldBe` (113 :: Int)
          toList pattern `shouldBe` [100, 10, 1, 2]
      
      describe "Folding patterns with different value types" $ do
        
        it "folds patterns with string values" $ do
          let elem1 = Pattern { value = "hello", elements = [] }
          let elem2 = Pattern { value = "world", elements = [] }
          let pattern = Pattern { value = "greeting", elements = [elem1, elem2] }
          foldr (++) "" pattern `shouldBe` "greetinghelloworld"
          toList pattern `shouldBe` ["greeting", "hello", "world"]
          foldMap (: []) pattern `shouldBe` ["greeting", "hello", "world"]
        
        it "folds patterns with integer values" $ do
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let pattern = Pattern { value = 100, elements = [elem1, elem2] }
          foldr (+) 0 pattern `shouldBe` (130 :: Int)
          toList pattern `shouldBe` [100, 10, 20]
          getSum (foldMap Sum pattern) `shouldBe` (130 :: Int)
        
        it "folds patterns with custom type values" $ do
          let person1 = Person "Alice" (Just 30)
          let person2 = Person "Bob" (Just 25)
          let elem1 = Pattern { value = person1, elements = [] }
          let elem2 = Pattern { value = person2, elements = [] }
          let pattern = Pattern { value = person1, elements = [elem1, elem2] }
          -- Count all Person values
          foldr (\_ acc -> acc + 1) 0 pattern `shouldBe` (3 :: Int)
          length (toList pattern) `shouldBe` 3
          head (toList pattern) `shouldBe` person1
        
        it "folds patterns with mixed value types in nested structure" $ do
          -- All values must be same type, so test with strings
          let inner1 = Pattern { value = "inner1", elements = [] }
          let inner2 = Pattern { value = "inner2", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner1, inner2] }
          let pattern = Pattern { value = "root", elements = [middle] }
          toList pattern `shouldBe` ["root", "middle", "inner1", "inner2"]
          foldr (++) "" pattern `shouldBe` "rootmiddleinner1inner2"
      
      describe "Order preservation in folding operations" $ do
        
        it "order preservation in toList" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let elem3 = Pattern { value = "third", elements = [] }
          let pattern = Pattern { value = "root", elements = [elem1, elem2, elem3] }
          -- toList should preserve order: root, first, second, third
          toList pattern `shouldBe` ["root", "first", "second", "third"]
          head (toList pattern) `shouldBe` "root"
          last (toList pattern) `shouldBe` "third"
        
        it "order preservation in foldr" $ do
          let elem1 = Pattern { value = 1, elements = [] }
          let elem2 = Pattern { value = 2, elements = [] }
          let elem3 = Pattern { value = 3, elements = [] }
          let pattern = Pattern { value = 0, elements = [elem1, elem2, elem3] }
          -- foldr (:) [] should preserve order
          foldr (:) [] pattern `shouldBe` [0, 1, 2, 3]
          toList pattern `shouldBe` [0, 1, 2, 3]
        
        it "order preservation in foldl" $ do
          let elem1 = Pattern { value = "1", elements = [] }
          let elem2 = Pattern { value = "2", elements = [] }
          let pattern = Pattern { value = "0", elements = [elem1, elem2] }
          -- foldl should process in left-to-right order
          foldl (++) "" pattern `shouldBe` "012"
          toList pattern `shouldBe` ["0", "1", "2"]
        
        it "order preservation in nested structures" $ do
          let inner1 = Pattern { value = "inner1", elements = [] }
          let inner2 = Pattern { value = "inner2", elements = [] }
          let middle1 = Pattern { value = "middle1", elements = [inner1] }
          let middle2 = Pattern { value = "middle2", elements = [inner2] }
          let pattern = Pattern { value = "root", elements = [middle1, middle2] }
          -- Order should be: root, middle1, inner1, middle2, inner2
          toList pattern `shouldBe` ["root", "middle1", "inner1", "middle2", "inner2"]
        
        it "order preservation with foldMap" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let pattern = Pattern { value = "root", elements = [elem1, elem2] }
          -- foldMap (: []) should preserve order
          foldMap (: []) pattern `shouldBe` ["root", "a", "b"]
          toList pattern `shouldBe` ["root", "a", "b"]
      
      describe "Deep nesting (3+ levels)" $ do
        
        it "folds pattern with 3 levels of nesting" $ do
          let level3 = Pattern { value = 1, elements = [] }
          let level2 = Pattern { value = 2, elements = [level3] }
          let level1 = Pattern { value = 3, elements = [level2] }
          let pattern = Pattern { value = 4, elements = [level1] }
          -- Should sum: 4 + 3 + 2 + 1 = 10
          foldr (+) 0 pattern `shouldBe` (10 :: Int)
          toList pattern `shouldBe` [4, 3, 2, 1]
          foldl (+) 0 pattern `shouldBe` (10 :: Int)
          getSum (foldMap Sum pattern) `shouldBe` (10 :: Int)
        
        it "folds pattern with 4 levels of nesting" $ do
          let level4 = Pattern { value = 1, elements = [] }
          let level3 = Pattern { value = 2, elements = [level4] }
          let level2 = Pattern { value = 3, elements = [level3] }
          let level1 = Pattern { value = 4, elements = [level2] }
          let pattern = Pattern { value = 5, elements = [level1] }
          -- Should sum: 5 + 4 + 3 + 2 + 1 = 15
          foldr (+) 0 pattern `shouldBe` (15 :: Int)
          toList pattern `shouldBe` [5, 4, 3, 2, 1]
          length (toList pattern) `shouldBe` 5
        
        it "folds pattern with 5 levels of nesting" $ do
          let level5 = Pattern { value = 1, elements = [] }
          let level4 = Pattern { value = 2, elements = [level5] }
          let level3 = Pattern { value = 3, elements = [level4] }
          let level2 = Pattern { value = 4, elements = [level3] }
          let level1 = Pattern { value = 5, elements = [level2] }
          let pattern = Pattern { value = 6, elements = [level1] }
          -- Should sum: 6 + 5 + 4 + 3 + 2 + 1 = 21
          foldr (+) 0 pattern `shouldBe` (21 :: Int)
          toList pattern `shouldBe` [6, 5, 4, 3, 2, 1]
          length (toList pattern) `shouldBe` 6
        
        it "folds pattern with deep nesting and multiple elements at each level" $ do
          let level3a = Pattern { value = 1, elements = [] }
          let level3b = Pattern { value = 2, elements = [] }
          let level2 = Pattern { value = 10, elements = [level3a, level3b] }
          let level1 = Pattern { value = 20, elements = [level2] }
          let pattern = Pattern { value = 100, elements = [level1] }
          -- Should sum: 100 + 20 + 10 + 1 + 2 = 133
          foldr (+) 0 pattern `shouldBe` (133 :: Int)
          toList pattern `shouldBe` [100, 20, 10, 1, 2]
          length (toList pattern) `shouldBe` 5
        
        it "folds pattern with deep nesting using all foldable operations" $ do
          let level3 = Pattern { value = "level3", elements = [] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let pattern = Pattern { value = "root", elements = [level1] }
          -- Test all operations
          toList pattern `shouldBe` ["root", "level1", "level2", "level3"]
          foldr (++) "" pattern `shouldBe` "rootlevel1level2level3"
          foldl (++) "" pattern `shouldBe` "rootlevel1level2level3"
          foldMap (: []) pattern `shouldBe` ["root", "level1", "level2", "level3"]
    
    describe "Traversable Instance (User Story 1)" $ do
      
      describe "Traversing atomic patterns with Identity" $ do
        
        it "traverses atomic pattern with Identity" $ do
          -- T009: Unit test for traversing atomic pattern with Identity
          let atom = pattern "test"
              result = traverse Identity atom
          runIdentity result `shouldBe` atom
      
      describe "Traversing atomic patterns with Maybe" $ do
        
        it "traverses atomic pattern with Maybe (Just value)" $ do
          -- T010: Unit test for traversing atomic pattern with Maybe (Just value)
          let validate x = if x > 0 then Just x else Nothing
              atom = pattern 5
              result = traverse validate atom
          result `shouldBe` Just atom
        
        it "traverses atomic pattern with Maybe (Nothing on failure)" $ do
          -- T011: Unit test for traversing atomic pattern with Maybe (Nothing on failure)
          let validate x = if x > 0 then Just x else Nothing
              atom = pattern (-3)
              result = traverse validate atom
          result `shouldBe` Nothing
      
      describe "Traversing atomic patterns with Either" $ do
        
        it "traverses atomic pattern with Either (Right value)" $ do
          -- T012: Unit test for traversing atomic pattern with Either (Right value)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              atom = pattern 5
              result = traverse validate atom
          result `shouldBe` Right atom
        
        it "traverses atomic pattern with Either (Left error)" $ do
          -- T013: Unit test for traversing atomic pattern with Either (Left error)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              atom = pattern (-3)
              result = traverse validate atom
          result `shouldBe` Left "Invalid: -3"
      
      describe "Traversing patterns with multiple elements using Identity" $ do
        
        it "traverses pattern with multiple elements using Identity" $ do
          -- T014: Unit test for traversing pattern with multiple elements using Identity
          let elem1 = pattern "elem1"
              elem2 = pattern "elem2"
              p = patternWith "root" [elem1, elem2]
              result = traverse Identity p
          runIdentity result `shouldBe` p
      
      describe "Traversing patterns with multiple elements using Maybe" $ do
        
        it "traverses pattern with multiple elements using Maybe (all succeed)" $ do
          -- T015: Unit test for traversing pattern with multiple elements using Maybe (all succeed)
          let validate x = if x > 0 then Just x else Nothing
              elem1 = pattern 5
              elem2 = pattern 10
              p = patternWith 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Just p
        
        it "traverses pattern with multiple elements using Maybe (one fails)" $ do
          -- T016: Unit test for traversing pattern with multiple elements using Maybe (one fails)
          let validate x = if x > 0 then Just x else Nothing
              elem1 = pattern 5
              elem2 = pattern (-3)
              p = patternWith 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Nothing
      
      describe "Traversing patterns with multiple elements using Either" $ do
        
        it "traverses pattern with multiple elements using Either (all succeed)" $ do
          -- T017: Unit test for traversing pattern with multiple elements using Either (all succeed)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              elem1 = pattern 5
              elem2 = pattern 10
              p = patternWith 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Right p
        
        it "traverses pattern with multiple elements using Either (one fails)" $ do
          -- T018: Unit test for traversing pattern with multiple elements using Either (one fails)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              elem1 = pattern 5
              elem2 = pattern (-3)
              p = patternWith 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Left "Invalid: -3"
      
      describe "Traversing nested pattern structures with Identity" $ do
        
        it "traverses nested pattern structure with Identity" $ do
          -- T019: Unit test for traversing nested pattern structure with Identity
          let inner = pattern "inner"
              middle = patternWith "middle" [inner]
              outer = patternWith "outer" [middle]
              p = patternWith "root" [outer]
              result = traverse Identity p
          runIdentity result `shouldBe` p
      
      describe "Traversing nested pattern structures with Maybe" $ do
        
        it "traverses nested pattern structure with Maybe (all succeed)" $ do
          -- T020: Unit test for traversing nested pattern structure with Maybe (all succeed)
          let validate x = if x > 0 then Just x else Nothing
              inner = pattern 1
              middle = patternWith 2 [inner]
              outer = patternWith 3 [middle]
              p = patternWith 4 [outer]
              result = traverse validate p
          result `shouldBe` Just p
        
        it "traverses nested pattern structure with Maybe (one fails)" $ do
          -- T021: Unit test for traversing nested pattern structure with Maybe (one fails)
          let validate x = if x > 0 then Just x else Nothing
              inner = pattern (-1)
              middle = patternWith 2 [inner]
              outer = patternWith 3 [middle]
              p = patternWith 4 [outer]
              result = traverse validate p
          result `shouldBe` Nothing
      
      describe "Traversing nested pattern structures with Either" $ do
        
        it "traverses nested pattern structure with Either (all succeed)" $ do
          -- T022: Unit test for traversing nested pattern structure with Either (all succeed)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              inner = pattern 1
              middle = patternWith 2 [inner]
              outer = patternWith 3 [middle]
              p = patternWith 4 [outer]
              result = traverse validate p
          result `shouldBe` Right p
        
        it "traverses nested pattern structure with Either (one fails)" $ do
          -- T023: Unit test for traversing nested pattern structure with Either (one fails)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              inner = pattern (-1)
              middle = patternWith 2 [inner]
              outer = patternWith 3 [middle]
              p = patternWith 4 [outer]
              result = traverse validate p
          result `shouldBe` Left "Invalid: -1"
      
      describe "Structure preservation in traverse" $ do
        
        it "traverse preserves pattern structure (element count, nesting depth, element order)" $ do
          -- T024: Unit test verifying traverse preserves pattern structure
          let elem1 = pattern "a"
              elem2 = pattern "b"
              elem3 = pattern "c"
              p = patternWith "root" [elem1, elem2, elem3]
              result = traverse Identity p
              p' = runIdentity result
          length (elements p') `shouldBe` 3
          value (elements p' !! 0) `shouldBe` "a"
          value (elements p' !! 1) `shouldBe` "b"
          value (elements p' !! 2) `shouldBe` "c"
        
        it "traverse processes pattern's own value" $ do
          -- T025: Unit test verifying traverse processes pattern's own value
          let validate x = if x > 0 then Just (x * 2) else Nothing
              atom = pattern 5
              result = traverse validate atom
          result `shouldBe` Just (pattern 10)
        
        it "traverse processes all element values recursively" $ do
          -- T026: Unit test verifying traverse processes all element values recursively
          let validate x = if x > 0 then Just (x * 2) else Nothing
              elem1 = pattern 5
              elem2 = pattern 10
              p = patternWith 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Just (patternWith 40 [pattern 10, pattern 20])
      
      describe "Traversing patterns with different value types" $ do
        
        it "traverses pattern with string values using Identity" $ do
          -- T027: Unit test for traversing pattern with string values using Identity
          let elem1 = pattern "hello"
              elem2 = pattern "world"
              p = patternWith "greeting" [elem1, elem2]
              result = traverse Identity p
          runIdentity result `shouldBe` p
        
        it "traverses pattern with integer values using Maybe" $ do
          -- T028: Unit test for traversing pattern with integer values using Maybe
          let validate x = if x > 0 then Just x else Nothing
              elem1 = pattern 5
              elem2 = pattern 10
              p = patternWith 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Just p
        
        it "traverses pattern with custom type values using Either" $ do
          -- T029: Unit test for traversing pattern with custom type values using Either
          let validate (Person name age) = 
                if name /= "" && age /= Nothing 
                then Right (Person name age)
                else Left "Invalid person"
              person1 = Person "Alice" (Just 30)
              person2 = Person "Bob" (Just 25)
              elem1 = pattern person1
              elem2 = pattern person2
              rootPerson = Person "Root" (Just 40)
              p = patternWith rootPerson [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Right p
    
    describe "Sequence Applicative Effects (User Story 2)" $ do
      
      describe "sequenceA on patterns containing Identity values" $ do
        
        it "sequences pattern containing Identity values" $ do
          -- T037: Unit test for sequenceA on pattern containing Identity values
          let atom = pattern (Identity "test")
              result = sequenceA atom
          runIdentity result `shouldBe` pattern "test"
      
      describe "sequenceA on patterns containing Maybe values" $ do
        
        it "sequences pattern containing Maybe values (all Just)" $ do
          -- T038: Unit test for sequenceA on pattern containing Maybe values (all Just)
          let elem1 = pattern (Just 5)
              elem2 = pattern (Just 10)
              p = patternWith (Just 20) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` Just (patternWith 20 [pattern 5, pattern 10])
        
        it "sequences pattern containing Maybe values (one Nothing)" $ do
          -- T039: Unit test for sequenceA on pattern containing Maybe values (one Nothing)
          let elem1 = pattern (Just 5)
              elem2 = pattern Nothing
              p = patternWith (Just 20) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` Nothing
      
      describe "sequenceA on patterns containing Either values" $ do
        
        it "sequences pattern containing Either values (all Right)" $ do
          -- T040: Unit test for sequenceA on pattern containing Either values (all Right)
          let elem1 = pattern (Right 5 :: Either String Int)
              elem2 = pattern (Right 10 :: Either String Int)
              p = patternWith (Right 20 :: Either String Int) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` (Right (patternWith 20 [pattern 5, pattern 10]) :: Either String (Pattern Int))
        
        it "sequences pattern containing Either values (one Left)" $ do
          -- T041: Unit test for sequenceA on pattern containing Either values (one Left)
          let elem1 = pattern (Right 5 :: Either String Int)
              elem2 = pattern (Left "error" :: Either String Int)
              p = patternWith (Right 20 :: Either String Int) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` (Left "error" :: Either String (Pattern Int))
      
      describe "sequenceA on nested pattern structures with Maybe values" $ do
        
        it "sequences nested pattern structure with Maybe values (all Just)" $ do
          -- T042: Unit test for sequenceA on nested pattern structure with Maybe values (all Just)
          let inner = pattern (Just 1)
              middle = patternWith (Just 2) [inner]
              outer = patternWith (Just 3) [middle]
              p = patternWith (Just 4) [outer]
              result = sequenceA p
          result `shouldBe` Just (patternWith 4 [patternWith 3 [patternWith 2 [pattern 1]]])
        
        it "sequences nested pattern structure with Maybe values (one Nothing)" $ do
          -- T043: Unit test for sequenceA on nested pattern structure with Maybe values (one Nothing)
          let inner = pattern Nothing
              middle = patternWith (Just 2) [inner]
              outer = patternWith (Just 3) [middle]
              p = patternWith (Just 4) [outer]
              result = sequenceA p
          result `shouldBe` Nothing
      
      describe "sequenceA on nested pattern structures with Either values" $ do
        
        it "sequences nested pattern structure with Either values (all Right)" $ do
          -- T044: Unit test for sequenceA on nested pattern structure with Either values (all Right)
          let inner = pattern (Right 1 :: Either String Int)
              middle = patternWith (Right 2 :: Either String Int) [inner]
              outer = patternWith (Right 3 :: Either String Int) [middle]
              p = patternWith (Right 4 :: Either String Int) [outer]
              result = sequenceA p
          result `shouldBe` (Right (patternWith 4 [patternWith 3 [patternWith 2 [pattern 1]]]) :: Either String (Pattern Int))
        
        it "sequences nested pattern structure with Either values (one Left)" $ do
          -- T045: Unit test for sequenceA on nested pattern structure with Either values (one Left)
          let inner = pattern (Left "error" :: Either String Int)
              middle = patternWith (Right 2 :: Either String Int) [inner]
              outer = patternWith (Right 3 :: Either String Int) [middle]
              p = patternWith (Right 4 :: Either String Int) [outer]
              result = sequenceA p
          result `shouldBe` (Left "error" :: Either String (Pattern Int))
      
      describe "Structure preservation in sequenceA" $ do
        
        it "sequenceA preserves pattern structure" $ do
          -- T046: Unit test verifying sequenceA preserves pattern structure
          let elem1 = pattern (Just "a")
              elem2 = pattern (Just "b")
              elem3 = pattern (Just "c")
              p = patternWith (Just "root") [elem1, elem2, elem3]
              result = sequenceA p
              p' = fromJust result
          length (elements p') `shouldBe` 3
          value (elements p' !! 0) `shouldBe` "a"
          value (elements p' !! 1) `shouldBe` "b"
          value (elements p' !! 2) `shouldBe` "c"
        
        it "sequenceA collects effects from all values" $ do
          -- T047: Unit test verifying sequenceA collects effects from all values
          let elem1 = pattern (Just 5)
              elem2 = pattern (Just 10)
              p = patternWith (Just 20) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` Just (patternWith 20 [pattern 5, pattern 10])
      
      describe "Short-circuiting behavior in sequenceA" $ do
        
        it "sequenceA short-circuits for Maybe (returns Nothing on first Nothing)" $ do
          -- T048: Unit test verifying sequenceA short-circuits for Maybe (returns Nothing on first Nothing)
          let elem1 = pattern Nothing
              elem2 = pattern (Just 10)
              p = patternWith (Just 20) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` Nothing
        
        it "sequenceA short-circuits for Either (returns Left on first Left)" $ do
          -- T049: Unit test verifying sequenceA short-circuits for Either (returns Left on first Left)
          let elem1 = pattern (Left "first error" :: Either String Int)
              elem2 = pattern (Right 10 :: Either String Int)
              p = patternWith (Right 20 :: Either String Int) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` (Left "first error" :: Either String (Pattern Int))
    
    describe "Validate Pattern Values with Error Handling (User Story 3)" $ do
      
      describe "Validation with Maybe" $ do
        
        it "validates pattern with Maybe (all values valid)" $ do
          -- T055: Unit test for validation with Maybe (all values valid)
          let validate x = if x > 0 then Just x else Nothing
              elem1 = pattern 5
              elem2 = pattern 10
              p = patternWith 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Just p
        
        it "validates pattern with Maybe (some values invalid)" $ do
          -- T056: Unit test for validation with Maybe (some values invalid)
          let validate x = if x > 0 then Just x else Nothing
              elem1 = pattern 5
              elem2 = pattern (-3)
              p = patternWith 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Nothing
      
      describe "Validation with Either" $ do
        
        it "validates pattern with Either (all values valid)" $ do
          -- T057: Unit test for validation with Either (all values valid)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              elem1 = pattern 5
              elem2 = pattern 10
              p = patternWith 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Right p
        
        it "validates pattern with Either (some values invalid, first error returned)" $ do
          -- T058: Unit test for validation with Either (some values invalid, first error returned)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              elem1 = pattern 5
              elem2 = pattern (-3)
              p = patternWith 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Left "Invalid: -3"
      
      describe "Validation on nested pattern structures with Maybe" $ do
        
        it "validates nested pattern structure with Maybe (all valid)" $ do
          -- T059: Unit test for validation on nested pattern structure with Maybe (all valid)
          let validate x = if x > 0 then Just x else Nothing
              inner = pattern 1
              middle = patternWith 2 [inner]
              outer = patternWith 3 [middle]
              p = patternWith 4 [outer]
              result = traverse validate p
          result `shouldBe` Just p
        
        it "validates nested pattern structure with Maybe (one invalid at any level)" $ do
          -- T060: Unit test for validation on nested pattern structure with Maybe (one invalid at any level)
          let validate x = if x > 0 then Just x else Nothing
              inner = pattern (-1)
              middle = patternWith 2 [inner]
              outer = patternWith 3 [middle]
              p = patternWith 4 [outer]
              result = traverse validate p
          result `shouldBe` Nothing
      
      describe "Validation on nested pattern structures with Either" $ do
        
        it "validates nested pattern structure with Either (all valid)" $ do
          -- T061: Unit test for validation on nested pattern structure with Either (all valid)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              inner = pattern 1
              middle = patternWith 2 [inner]
              outer = patternWith 3 [middle]
              p = patternWith 4 [outer]
              result = traverse validate p
          result `shouldBe` Right p
        
        it "validates nested pattern structure with Either (one invalid at any level)" $ do
          -- T062: Unit test for validation on nested pattern structure with Either (one invalid at any level)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              inner = pattern (-1)
              middle = patternWith 2 [inner]
              outer = patternWith 3 [middle]
              p = patternWith 4 [outer]
              result = traverse validate p
          result `shouldBe` Left "Invalid: -1"
      
      describe "Validation failure behavior" $ do
        
        it "validation fails if any value at any nesting level is invalid" $ do
          -- T063: Unit test verifying validation fails if any value at any nesting level is invalid
          let validate x = if x > 0 then Just x else Nothing
              -- Test with invalid value at root level
              p1 = patternWith (-1) [pattern 5, pattern 10]
              result1 = traverse validate p1
              -- Test with invalid value at element level
              p2 = patternWith 20 [pattern (-3), pattern 10]
              result2 = traverse validate p2
              -- Test with invalid value at nested level
              inner = pattern (-1)
              middle = patternWith 2 [inner]
              p3 = patternWith 4 [middle]
              result3 = traverse validate p3
          result1 `shouldBe` Nothing
          result2 `shouldBe` Nothing
          result3 `shouldBe` Nothing
    
    describe "IO and State Support (Phase 6)" $ do
      
      describe "IO applicative functor support" $ do
        
        it "traverse works with IO applicative functor" $ do
          -- T079: Verify traversable instance works with IO applicative functor
          let readValue :: String -> IO Int
              readValue = readIO
              p = patternWith "10" [pattern "5", pattern "3"]
              ioResult = traverse readValue p
          -- Execute IO and verify result
          result <- ioResult
          value result `shouldBe` (10 :: Int)
          length (elements result) `shouldBe` 2
          value (elements result !! 0) `shouldBe` (5 :: Int)
          value (elements result !! 1) `shouldBe` (3 :: Int)
      
      describe "State applicative functor support" $ do
        
        it "traverse works with State applicative functor" $ do
          -- T080: Verify traversable instance works with State applicative functor
          let addState :: Int -> State Int Int
              addState x = do
                s <- get
                put (s + x)
                return (s + x)
              p = patternWith 10 [pattern 5, pattern 3]
              stateResult = traverse addState p
              (result, finalState) = runState stateResult 0
          -- Verify result pattern structure
          -- State processes: pattern value (10) first, then elements (5, 3)
          -- Initial state: 0
          -- Pattern value 10: state 0 -> 10, return 10
          -- Element 5: state 10 -> 15, return 15
          -- Element 3: state 15 -> 18, return 18
          value result `shouldBe` (10 :: Int)  -- Pattern value processed with state 0
          length (elements result) `shouldBe` 2
          value (elements result !! 0) `shouldBe` (15 :: Int)  -- First element processed with state 10
          value (elements result !! 1) `shouldBe` (18 :: Int)  -- Second element processed with state 15
          -- Verify final state (0 + 10 + 5 + 3 = 18)
          finalState `shouldBe` (18 :: Int)
    
    describe "Query Functions (User Story 1 - Length)" $ do
      
      describe "length function - unit tests" $ do
        
        it "T001: returns 0 for atomic pattern" $ do
          let atom = Pattern { value = "atom", elements = [] }
          PC.length atom `shouldBe` 0
        
        it "T002: returns 1 for single element pattern" $ do
          let elem = Pattern { value = "elem", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem] }
          PC.length pattern `shouldBe` 1
        
        it "T003: returns correct count for multiple elements pattern" $ do
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let elem3 = Pattern { value = "e3", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          PC.length pattern `shouldBe` 3
        
        it "T004: returns only direct children count for nested pattern" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let pattern = Pattern { value = "root", elements = [outer] }
          -- Should return 1 (only direct child), not count nested descendants
          PC.length pattern `shouldBe` 1
          -- Verify nested structure has its own length
          PC.length outer `shouldBe` 1
          PC.length middle `shouldBe` 1
          PC.length inner `shouldBe` 0
    
    describe "Query Functions (User Story 2 - Size)" $ do
      
      describe "size function - unit tests" $ do
        
        it "T011: returns 1 for atomic pattern" $ do
          let atom = Pattern { value = "atom", elements = [] }
          size atom `shouldBe` 1
        
        it "T012: returns 1 + element count for pattern with direct elements" $ do
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let elem3 = Pattern { value = "e3", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          -- Should return 1 (root) + 3 (elements) = 4
          size pattern `shouldBe` 4
        
        it "T013: counts all nodes in deeply nested pattern" $ do
          let level4 = Pattern { value = "level4", elements = [] }
          let level3 = Pattern { value = "level3", elements = [level4] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let pattern = Pattern { value = "root", elements = [level1] }
          -- Should count all nodes: root(1) + level1(1) + level2(1) + level3(1) + level4(1) = 5
          size pattern `shouldBe` 5
        
        it "T014: counts all nodes across branches with varying depths" $ do
          let branch1Leaf = Pattern { value = "b1leaf", elements = [] }
          let branch1 = Pattern { value = "b1", elements = [branch1Leaf] }
          let branch2Mid = Pattern { value = "b2mid", elements = [] }
          let branch2Leaf = Pattern { value = "b2leaf", elements = [] }
          let branch2 = Pattern { value = "b2", elements = [branch2Mid, branch2Leaf] }
          let branch3 = Pattern { value = "b3", elements = [] }
          let pattern = Pattern { value = "root", elements = [branch1, branch2, branch3] }
          -- Should count: root(1) + b1(1) + b1leaf(1) + b2(1) + b2mid(1) + b2leaf(1) + b3(1) = 7
          size pattern `shouldBe` 7
    
    describe "Query Functions (User Story 3 - Depth)" $ do
      
      describe "depth function - unit tests" $ do
        
        it "T022: returns 0 for atomic pattern" $ do
          let atom = Pattern { value = "atom", elements = [] }
          -- Atomic pattern has no nesting, depth is 0 (root only)
          depth atom `shouldBe` 0
        
        it "T023: returns 1 for one level of nesting" $ do
          let elem = Pattern { value = "elem", elements = [] }
          let pattern = Pattern { value = "pattern", elements = [elem] }
          -- One level of nesting: root -> elem, depth is 1
          depth pattern `shouldBe` 1
        
        it "T024: returns maximum depth across branches with different depths" $ do
          let branch1Leaf = Pattern { value = "b1leaf", elements = [] }
          let branch1 = Pattern { value = "b1", elements = [branch1Leaf] }
          -- branch1 depth: b1 -> b1leaf = 1
          let branch2Mid = Pattern { value = "b2mid", elements = [] }
          let branch2Leaf = Pattern { value = "b2leaf", elements = [] }
          let branch2Inner = Pattern { value = "b2inner", elements = [branch2Leaf] }
          let branch2 = Pattern { value = "b2", elements = [branch2Mid, branch2Inner] }
          -- branch2 depth: b2 -> b2inner -> b2leaf = 2 (maximum among branches)
          let branch3 = Pattern { value = "b3", elements = [] }
          -- branch3 depth: b3 = 0
          let pattern = Pattern { value = "root", elements = [branch1, branch2, branch3] }
          -- Should return: root -> branch2 -> branch2Inner -> branch2Leaf = 3
          -- (1 for root -> branch2, plus branch2's depth of 2)
          depth pattern `shouldBe` 3
        
        it "T025: returns maximum depth for deeply nested pattern" $ do
          let level4 = Pattern { value = "level4", elements = [] }
          let level3 = Pattern { value = "level3", elements = [level4] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let pattern = Pattern { value = "root", elements = [level1] }
          -- Depth: root -> level1 -> level2 -> level3 -> level4 = 4
          depth pattern `shouldBe` 4
    
    describe "Query Functions (User Story 4 - Values)" $ do
      
      describe "values function - unit tests" $ do
        
        it "T032: returns single value for atomic pattern" $ do
          let atom = Pattern { value = "atom", elements = [] }
          values atom `shouldBe` ["atom"]
        
        it "T033: returns all values for pattern with multiple elements" $ do
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let elem3 = Pattern { value = "e3", elements = [] }
          let pattern = Pattern { value = "root", elements = [elem1, elem2, elem3] }
          -- Should return: root value first, then element values in order
          values pattern `shouldBe` ["root", "e1", "e2", "e3"]
        
        it "T034: returns all values from all levels for nested pattern" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let pattern = Pattern { value = "root", elements = [outer] }
          -- Should return all values: root, outer, middle, inner
          values pattern `shouldBe` ["root", "outer", "middle", "inner"]
        
        it "T035: returns values in consistent order for varying nesting depths" $ do
          let branch1Leaf = Pattern { value = "b1leaf", elements = [] }
          let branch1 = Pattern { value = "b1", elements = [branch1Leaf] }
          let branch2Mid = Pattern { value = "b2mid", elements = [] }
          let branch2Leaf = Pattern { value = "b2leaf", elements = [] }
          let branch2 = Pattern { value = "b2", elements = [branch2Mid, branch2Leaf] }
          let branch3 = Pattern { value = "b3", elements = [] }
          let pattern = Pattern { value = "root", elements = [branch1, branch2, branch3] }
          -- Should return: root, then branch values in order, then nested values
          -- Order: root -> b1 -> b1leaf -> b2 -> b2mid -> b2leaf -> b3
          values pattern `shouldBe` ["root", "b1", "b1leaf", "b2", "b2mid", "b2leaf", "b3"]
    
    describe "Query Functions (User Story 5 - Value Accessor)" $ do
      
      describe "value field accessor - unit tests" $ do
        
        it "T044: returns correct value for string pattern" $ do
          let pattern = Pattern { value = "test", elements = [] }
          value pattern `shouldBe` "test"
        
        it "T045: returns correct value for integer pattern" $ do
          let pattern = Pattern { value = 42, elements = [] }
          value pattern `shouldBe` (42 :: Int)
        
        it "T046: returns correct value for custom type pattern" $ do
          let person = Person "Alice" (Just 30)
          let pattern = Pattern { value = person, elements = [] }
          value pattern `shouldBe` person
        
        it "T047: returns correct value for each level in nested patterns" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let pattern = Pattern { value = "root", elements = [outer] }
          -- Verify each level returns its correct value
          value pattern `shouldBe` "root"
          value outer `shouldBe` "outer"
          value middle `shouldBe` "middle"
          value inner `shouldBe` "inner"
    
    describe "Query Functions - Integration Tests (Phase 6)" $ do
      
      describe "Integration with pattern constructors" $ do
        
        it "T051: query functions work with patterns created using pattern function" $ do
          let atom = pattern "atom"
          let elem1 = pattern "e1"
          let elem2 = pattern "e2"
          let pattern1 = patternWith "root" [elem1, elem2]
          -- Verify all query functions work
          PC.length atom `shouldBe` 0
          size atom `shouldBe` 1
          depth atom `shouldBe` 0
          values atom `shouldBe` ["atom"]
          value atom `shouldBe` "atom"
          PC.length pattern1 `shouldBe` 2
          size pattern1 `shouldBe` 3
          depth pattern1 `shouldBe` 1
          values pattern1 `shouldBe` ["root", "e1", "e2"]
          value pattern1 `shouldBe` "root"
        
        it "T052: query functions work with patterns created using patternWith function" $ do
          let elem1 = pattern "e1"
          let elem2 = pattern "e2"
          let elem3 = pattern "e3"
          let pattern1 = patternWith "root" [elem1, elem2, elem3]
          -- Verify all query functions work
          PC.length pattern1 `shouldBe` 3
          size pattern1 `shouldBe` 4
          depth pattern1 `shouldBe` 1
          values pattern1 `shouldBe` ["root", "e1", "e2", "e3"]
          value pattern1 `shouldBe` "root"
        
        it "T053: query functions work with patterns created using fromList function" $ do
          let pattern1 = fromList "root" ["a", "b", "c"]
          -- Verify all query functions work
          PC.length pattern1 `shouldBe` 3
          size pattern1 `shouldBe` 4
          depth pattern1 `shouldBe` 1
          values pattern1 `shouldBe` ["root", "a", "b", "c"]
          value pattern1 `shouldBe` "root"
      
      describe "Integration with type class instances" $ do
        
        it "T054: query functions work with patterns transformed using fmap (Functor)" $ do
          let pattern1 = fromList "root" ["a", "b", "c"]
          let pattern2 = fmap (map toUpper) pattern1
          -- Verify all query functions work on transformed pattern
          PC.length pattern2 `shouldBe` 3
          size pattern2 `shouldBe` 4
          depth pattern2 `shouldBe` 1
          values pattern2 `shouldBe` ["ROOT", "A", "B", "C"]
          value pattern2 `shouldBe` "ROOT"
          -- Verify structure is preserved
          PC.length pattern1 `shouldBe` PC.length pattern2
          size pattern1 `shouldBe` size pattern2
          depth pattern1 `shouldBe` depth pattern2
        
        it "T055: query functions work with patterns used in Foldable operations" $ do
          let pattern1 = fromList (10 :: Int) [1, 2, 3, 4, 5]
          -- Verify query functions work
          PC.length pattern1 `shouldBe` 5
          size pattern1 `shouldBe` 6
          depth pattern1 `shouldBe` 1
          values pattern1 `shouldBe` [10, 1, 2, 3, 4, 5]
          value pattern1 `shouldBe` (10 :: Int)
          -- Verify integration with Foldable
          sum pattern1 `shouldBe` 25
          length (toList pattern1) `shouldBe` 6
          -- Verify values matches toList
          values pattern1 `shouldBe` toList pattern1
        
        it "T056: query functions work with patterns used in Traversable operations" $ do
          let pattern1 = fromList 10 [1, 2, 3]
          -- Verify query functions work
          PC.length pattern1 `shouldBe` 3
          size pattern1 `shouldBe` 4
          depth pattern1 `shouldBe` 1
          values pattern1 `shouldBe` [10, 1, 2, 3]
          value pattern1 `shouldBe` (10 :: Int)
          -- Verify integration with Traversable
          let validate x = if x > 0 then Just x else Nothing
          let result = traverse validate pattern1
          case result of
            Just pattern2 -> do
              -- Verify query functions work on traversed pattern
              PC.length pattern2 `shouldBe` 3
              size pattern2 `shouldBe` 4
              depth pattern2 `shouldBe` 1
              values pattern2 `shouldBe` [10, 1, 2, 3]
              value pattern2 `shouldBe` (10 :: Int)
            Nothing -> fail "Traverse should succeed for positive values"
      
      describe "Edge case tests" $ do
        
        it "T057: query functions work with very deeply nested patterns (100+ levels)" $ do
          -- Create a deeply nested pattern (100 levels)
          let createDeep n = if n <= 0
                             then pattern "leaf"
                             else patternWith ("level" ++ show n) [createDeep (n - 1)]
          let deepPattern = createDeep 100
          -- Verify all query functions work (should not stack overflow)
          PC.length deepPattern `shouldBe` 1
          size deepPattern `shouldBe` 101
          depth deepPattern `shouldBe` 100
          length (values deepPattern) `shouldBe` 101
          value deepPattern `shouldBe` "level100"
        
        it "T058: query functions work with patterns having many direct elements (100+)" $ do
          -- Create a pattern with 100 direct elements
          let elems = map (\i -> pattern ("elem" ++ show i)) [1..100]
          let pattern1 = patternWith "root" elems
          -- Verify all query functions work
          PC.length pattern1 `shouldBe` 100
          size pattern1 `shouldBe` 101
          depth pattern1 `shouldBe` 1
          length (values pattern1) `shouldBe` 101
          value pattern1 `shouldBe` "root"
          -- Verify first and last elements
          head (values pattern1) `shouldBe` "root"
          last (values pattern1) `shouldBe` "elem100"
        
        it "T059: query functions work with patterns containing duplicate values" $ do
          -- Create pattern with duplicate values
          let pattern1 = fromList "root" ["a", "a", "b", "b", "c", "c"]
          -- Verify all query functions work
          PC.length pattern1 `shouldBe` 6
          size pattern1 `shouldBe` 7
          depth pattern1 `shouldBe` 1
          values pattern1 `shouldBe` ["root", "a", "a", "b", "b", "c", "c"]
          value pattern1 `shouldBe` "root"
          -- Verify duplicates are preserved
          filter (== "a") (values pattern1) `shouldBe` ["a", "a"]
          filter (== "b") (values pattern1) `shouldBe` ["b", "b"]
    
    describe "Ord Instance (User Story 1)" $ do
      
      describe "compare function with atomic patterns" $ do
        
        it "T001: compare atomic patterns with different values" $ do
          let p1 = pattern "a"
              p2 = pattern "b"
          compare p1 p2 `shouldBe` LT
          compare p2 p1 `shouldBe` GT
          compare p1 p1 `shouldBe` EQ
        
        it "T002: compare atomic patterns with same value" $ do
          let p1 = pattern "test"
              p2 = pattern "test"
          compare p1 p2 `shouldBe` EQ
          compare p2 p1 `shouldBe` EQ
      
      describe "compare function with patterns having elements" $ do
        
        it "T003: compare patterns with same value but different elements" $ do
          let p1 = patternWith "root" [pattern "a"]
              p2 = patternWith "root" [pattern "b"]
          compare p1 p2 `shouldBe` LT
          compare p2 p1 `shouldBe` GT
        
        it "T004: compare patterns with same value and same number of elements" $ do
          let p1 = patternWith "root" [pattern "a", pattern "b"]
              p2 = patternWith "root" [pattern "a", pattern "b"]
          compare p1 p2 `shouldBe` EQ
          compare p2 p1 `shouldBe` EQ
      
      describe "compare function with nested patterns" $ do
        
        it "T005: compare nested patterns (recursive comparison)" $ do
          let inner1 = pattern "inner1"
              inner2 = pattern "inner2"
              middle1 = patternWith "middle" [inner1]
              middle2 = patternWith "middle" [inner2]
              outer1 = patternWith "outer" [middle1]
              outer2 = patternWith "outer" [middle2]
          compare outer1 outer2 `shouldBe` LT
    
    describe "Integration & Validation (Phase 4)" $ do
      
      describe "Integration with existing functions" $ do
        
        it "T042: all query functions work with Ord instance" $ do
          let p1 = pattern "a"
              p2 = pattern "b"
              p3 = patternWith "root" [pattern "a", pattern "b"]
          -- Verify query functions work with patterns that can be compared
          value p1 `shouldBe` "a"
          PC.length p3 `shouldBe` 2
          size p3 `shouldBe` 3
          depth p3 `shouldBe` 1
          values p3 `shouldBe` ["root", "a", "b"]
          -- Verify we can compare patterns and use query functions together
          let patterns = [p3, p1, p2]
              sorted = sort patterns
          -- Query functions should work on sorted patterns
          map value sorted `shouldBe` ["a", "b", "root"]
          map PC.length sorted `shouldBe` [0, 0, 2]
        
        it "T043: Ord instance works with pattern constructors" $ do
          -- Test with pattern constructor
          let p1 = pattern "a"
              p2 = pattern "b"
          compare p1 p2 `shouldBe` LT
          -- Test with patternWith constructor
          let p3 = patternWith "root" [pattern "a"]
              p4 = patternWith "root" [pattern "b"]
          compare p3 p4 `shouldBe` LT
          -- Test with fromList constructor
          let p5 = fromList "root" ["a", "b"]
              p6 = fromList "root" ["a", "c"]
          compare p5 p6 `shouldBe` LT
          -- Verify constructors produce comparable patterns
          let patterns = [p2, p1, p4, p3]
              sorted = sort patterns
          length sorted `shouldBe` 4
          -- Verify sorted order
          and (zipWith (<=) sorted (drop 1 sorted)) `shouldBe` True
        
        it "T044: Ord instance works with type class instances" $ do
          -- Test with Functor
          let p1 = pattern "a"
              p2 = pattern "b"
          fmap (map toUpper) p1 `shouldBe` pattern "A"
          -- Functor preserves ordering
          compare (fmap (map toUpper) p1) (fmap (map toUpper) p2) `shouldBe` LT
          -- Test with Foldable
          let p3 = patternWith "root" [pattern "a", pattern "b"]
              p4 = patternWith "root" [pattern "a", pattern "c"]
          -- Foldable operations work, and we can still compare
          toList p3 `shouldBe` ["root", "a", "b"]
          compare p3 p4 `shouldBe` LT
          -- Test with Traversable
          let p5 = pattern (Just "a")
              p6 = pattern (Just "b")
          -- Traversable operations work, and we can still compare
          traverse Just p5 `shouldBe` Just (pattern (Just "a"))
          compare p5 p6 `shouldBe` LT
          -- Verify type class operations preserve comparability
          let patterns = [p4, p3]
              sorted = sort patterns
          length sorted `shouldBe` 2
          sorted `shouldBe` [p3, p4]
        
        it "T006: comparison operators (<, <=, >, >=) with patterns" $ do
          let p1 = pattern "a"
              p2 = pattern "b"
              p3 = pattern "a"
          (p1 < p2) `shouldBe` True
          (p1 <= p2) `shouldBe` True
          (p2 > p1) `shouldBe` True
          (p2 >= p1) `shouldBe` True
          (p1 < p3) `shouldBe` False
          (p1 <= p3) `shouldBe` True
          (p1 > p3) `shouldBe` False
          (p1 >= p3) `shouldBe` True
      
      describe "min and max functions" $ do
        
        it "T007: min and max functions with patterns" $ do
          let p1 = pattern "a"
              p2 = pattern "b"
              p3 = pattern "c"
          min p1 p2 `shouldBe` p1
          max p1 p2 `shouldBe` p2
          min p1 p3 `shouldBe` p1
          max p1 p3 `shouldBe` p3
          min p2 p3 `shouldBe` p2
          max p2 p3 `shouldBe` p3
    
    describe "Ord Instance Integration (User Story 2)" $ do
      
      describe "Data.Set integration" $ do
        
        it "T016: Data.Set with patterns (insertion and ordering)" $ do
          let p1 = pattern "a"
              p2 = pattern "b"
              p3 = pattern "c"
              s = Set.insert p3 $ Set.insert p1 $ Set.insert p2 Set.empty
          -- Set should maintain sorted order
          Set.toList s `shouldBe` [p1, p2, p3]
          Set.member p2 s `shouldBe` True
          Set.member (pattern "d") s `shouldBe` False
        
        it "T017: Data.Set membership lookup with patterns" $ do
          let p1 = pattern "a"
              p2 = patternWith "root" [pattern "b"]
              p3 = patternWith "root" [pattern "c"]
              s = Set.fromList [p1, p2, p3]
          Set.member p1 s `shouldBe` True
          Set.member p2 s `shouldBe` True
          Set.member p3 s `shouldBe` True
          Set.member (pattern "d") s `shouldBe` False
          Set.member (patternWith "root" [pattern "d"]) s `shouldBe` False
        
        it "T018: Data.Set with patterns having duplicate values but different structures" $ do
          let p1 = patternWith "root" [pattern "a"]
              p2 = patternWith "root" [pattern "b"]
              p3 = patternWith "root" [pattern "a", pattern "b"]
              s = Set.fromList [p1, p2, p3]
          -- All three should be distinct (different structures)
          Set.size s `shouldBe` 3
          Set.member p1 s `shouldBe` True
          Set.member p2 s `shouldBe` True
          Set.member p3 s `shouldBe` True
      
      describe "Data.Map integration" $ do
        
        it "T019: Data.Map with patterns as keys (insertion and lookup)" $ do
          let p1 = pattern "a"
              p2 = pattern "b"
              p3 = pattern "c"
              m = Map.insert p3 "value3" $ Map.insert p1 "value1" $ Map.insert p2 "value2" Map.empty
          Map.lookup p1 m `shouldBe` Just "value1"
          Map.lookup p2 m `shouldBe` Just "value2"
          Map.lookup p3 m `shouldBe` Just "value3"
          Map.lookup (pattern "d") m `shouldBe` Nothing
        
        it "T020: Data.Map key matching with patterns" $ do
          let p1 = patternWith "root" [pattern "a"]
              p2 = patternWith "root" [pattern "b"]
              p3 = patternWith "root" [pattern "a", pattern "b"]
              m = Map.fromList [(p1, "val1"), (p2, "val2"), (p3, "val3")]
          Map.member p1 m `shouldBe` True
          Map.member p2 m `shouldBe` True
          Map.member p3 m `shouldBe` True
          Map.lookup p1 m `shouldBe` Just "val1"
          Map.lookup p2 m `shouldBe` Just "val2"
          Map.lookup p3 m `shouldBe` Just "val3"
          Map.member (patternWith "root" [pattern "d"]) m `shouldBe` False
      
      describe "Sorting and min/max functions" $ do
        
        it "T021: sorting patterns using sort function" $ do
          let p1 = pattern "c"
              p2 = pattern "a"
              p3 = pattern "b"
              sorted = sort [p1, p2, p3]
          sorted `shouldBe` [p2, p3, p1]
          -- Verify sorted order
          and (zipWith (<=) sorted (tail sorted)) `shouldBe` True
        
        it "T022: minimum and maximum functions with pattern lists" $ do
          let p1 = pattern "a"
              p2 = pattern "b"
              p3 = pattern "c"
              patterns = [p3, p1, p2]
          minimum patterns `shouldBe` p1
          maximum patterns `shouldBe` p3
          -- Test with nested patterns
          let nested1 = patternWith "root" [pattern "a"]
              nested2 = patternWith "root" [pattern "b"]
              nested3 = patternWith "root" [pattern "c"]
              nestedPatterns = [nested3, nested1, nested2]
          minimum nestedPatterns `shouldBe` nested1
          maximum nestedPatterns `shouldBe` nested3
    
    describe "Ord Instance Consistency with Eq (User Story 3)" $ do
      
      describe "consistency between Ord and Eq" $ do
        
        it "T028: patterns equal by Eq compare as EQ" $ do
          let p1 = pattern "test"
              p2 = pattern "test"
          p1 == p2 `shouldBe` True
          compare p1 p2 `shouldBe` EQ
          -- Test with patterns having elements
          let p3 = patternWith "root" [pattern "a", pattern "b"]
              p4 = patternWith "root" [pattern "a", pattern "b"]
          p3 == p4 `shouldBe` True
          compare p3 p4 `shouldBe` EQ
          -- Test with nested patterns
          let inner1 = pattern "inner"
              inner2 = pattern "inner"
              outer1 = patternWith "outer" [inner1]
              outer2 = patternWith "outer" [inner2]
          outer1 == outer2 `shouldBe` True
          compare outer1 outer2 `shouldBe` EQ
        
        it "T029: patterns not equal by Eq don't compare as EQ" $ do
          let p1 = pattern "a"
              p2 = pattern "b"
          p1 == p2 `shouldBe` False
          compare p1 p2 `shouldNotBe` EQ
          compare p2 p1 `shouldNotBe` EQ
          -- Test with patterns having same value but different elements
          let p3 = patternWith "root" [pattern "a"]
              p4 = patternWith "root" [pattern "b"]
          p3 == p4 `shouldBe` False
          compare p3 p4 `shouldNotBe` EQ
          -- Test with patterns having different values
          let p5 = patternWith "root1" [pattern "a"]
              p6 = patternWith "root2" [pattern "a"]
          p5 == p6 `shouldBe` False
          compare p5 p6 `shouldNotBe` EQ
        
        it "T030: patterns with same structure but different values compare by value" $ do
          let p1 = patternWith "a" [pattern "x"]
              p2 = patternWith "b" [pattern "x"]
          -- Same structure (one element with same value), different root values
          p1 == p2 `shouldBe` False
          compare p1 p2 `shouldBe` LT  -- "a" < "b"
          compare p2 p1 `shouldBe` GT
          -- Verify ordering respects value comparison
          (p1 < p2) `shouldBe` True
          (p2 > p1) `shouldBe` True
        
        it "T031: patterns with same value but different element structures compare by elements" $ do
          let p1 = patternWith "root" [pattern "a"]
              p2 = patternWith "root" [pattern "b"]
          -- Same root value, different element values
          p1 == p2 `shouldBe` False
          compare p1 p2 `shouldBe` LT  -- "a" < "b" in elements
          compare p2 p1 `shouldBe` GT
          -- Test with multiple elements
          let p3 = patternWith "root" [pattern "a", pattern "b"]
              p4 = patternWith "root" [pattern "a", pattern "c"]
          p3 == p4 `shouldBe` False
          compare p3 p4 `shouldBe` LT  -- "b" < "c" in second element
          -- Test with different number of elements
          let p5 = patternWith "root" [pattern "a"]
              p6 = patternWith "root" [pattern "a", pattern "b"]
          p5 == p6 `shouldBe` False
          compare p5 p6 `shouldBe` LT  -- shorter list comes first
          compare p6 p5 `shouldBe` GT
    
    describe "Edge Cases (Phase 4)" $ do
      
      describe "Edge case tests for Ord instance" $ do
        
        it "T037: comparing atomic patterns (no elements)" $ do
          let p1 = pattern "a"
              p2 = pattern "b"
              p3 = pattern "a"
          -- Atomic patterns compare by value only
          compare p1 p2 `shouldBe` LT
          compare p2 p1 `shouldBe` GT
          compare p1 p3 `shouldBe` EQ
          -- Verify with different value types
          let p4 = pattern (1 :: Int)
              p5 = pattern (2 :: Int)
          compare p4 p5 `shouldBe` LT
          compare p5 p4 `shouldBe` GT
        
        it "T038: comparing patterns with different numbers of elements" $ do
          let p1 = patternWith "root" []  -- 0 elements
              p2 = patternWith "root" [pattern "a"]  -- 1 element
              p3 = patternWith "root" [pattern "a", pattern "b"]  -- 2 elements
          -- Patterns with fewer elements come first when values are equal
          compare p1 p2 `shouldBe` LT
          compare p2 p3 `shouldBe` LT
          compare p1 p3 `shouldBe` LT
          compare p3 p2 `shouldBe` GT
          compare p2 p1 `shouldBe` GT
          -- Test with different values
          let p4 = patternWith "a" [pattern "x"]
              p5 = patternWith "b" []  -- Different value, fewer elements
          -- Value comparison takes precedence
          compare p4 p5 `shouldBe` LT  -- "a" < "b"
        
        it "T039: comparing deeply nested patterns (100+ levels)" $ do
          -- Create deeply nested patterns
          let createDeep n = if n == 0
                             then pattern "leaf"
                             else patternWith ("level" ++ show n) [createDeep (n - 1)]
          let deep1 = createDeep 100
          let deep2 = createDeep 100
          let deep3 = createDeep 101
          -- Same depth patterns should compare as equal if structure is identical
          compare deep1 deep2 `shouldBe` EQ
          -- Different depth patterns should compare correctly
          compare deep1 deep3 `shouldBe` LT  -- 100 < 101 in level names
          compare deep3 deep1 `shouldBe` GT
          -- Verify no stack overflow or performance issues
          (deep1 <= deep2) `shouldBe` True
          (deep1 < deep3) `shouldBe` True
        
        it "T040: comparing patterns with same flattened values but different structures" $ do
          -- Patterns with same flattened values but different structures should be distinct
          let p1 = patternWith "root" [pattern "a", pattern "b"]
              p2 = patternWith "root" [patternWith "a" [pattern "b"]]
          -- These have different structures even though flattened values might be similar
          p1 == p2 `shouldBe` False
          compare p1 p2 `shouldNotBe` EQ
          -- Test with more complex structures
          let p3 = patternWith "x" [pattern "y", pattern "z"]
              p4 = patternWith "x" [patternWith "y" [pattern "z"]]
          p3 == p4 `shouldBe` False
          compare p3 p4 `shouldNotBe` EQ
          -- Verify structure is preserved in comparison
          let p5 = patternWith "root" [pattern "a"]
              p6 = patternWith "root" [pattern "a", pattern "b"]
          -- Even if first element is same, different structures
          p5 == p6 `shouldBe` False
          compare p5 p6 `shouldBe` LT  -- shorter list first
        
        it "T041: type constraint: Ord v requirement" $ do
          -- This test verifies that Ord instance requires Ord v constraint
          -- We test with types that have Ord instances
          let p1 = pattern ("a" :: String)
              p2 = pattern ("b" :: String)
          compare p1 p2 `shouldBe` LT
          -- Test with Int (has Ord instance)
          let p3 = pattern (1 :: Int)
              p4 = pattern (2 :: Int)
          compare p3 p4 `shouldBe` LT
          -- Test with patterns containing Int values
          let p5 = patternWith (10 :: Int) [pattern (1 :: Int), pattern (2 :: Int)]
              p6 = patternWith (10 :: Int) [pattern (1 :: Int), pattern (3 :: Int)]
          compare p5 p6 `shouldBe` LT
          -- Verify that comparison works with nested patterns of orderable types
          let inner1 = pattern (5 :: Int)
              inner2 = pattern (6 :: Int)
              outer1 = patternWith (100 :: Int) [inner1]
              outer2 = patternWith (100 :: Int) [inner2]
          compare outer1 outer2 `shouldBe` LT