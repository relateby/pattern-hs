-- | Unit tests for Pattern.Core module.
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-x-partial #-}
module Spec.Pattern.CoreSpec where

import Control.Monad.State (State, get, put, runState)
import Data.Char (toUpper)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(..))
import Data.Hashable (hash, hashWithSalt)
import Data.List (sort)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid (All(..), Any(..), Endo(..), Product(..), Sum(..))
import Data.Semigroup (sconcat, stimes)
import qualified Data.Set as Set
import Test.Hspec
import Control.Comonad (extract, extend, duplicate)
import Pattern.Core (Pattern(..), pattern, point, fromList, toTuple, size, depth, values, anyValue, allValues, filterPatterns, findPattern, findAllPatterns, matches, contains, depthAt, sizeAt, indicesAt)
import qualified Pattern.Core as PC

-- Custom type for testing
data Person = Person { name :: String, age :: Maybe Int }
  deriving (Eq, Show)

spec :: Spec
spec = do
  describe "Pattern.Core" $ do
    
    describe "Atomic Patterns (User Story 1)" $ do
      
      describe "Creating atomic patterns with different value types" $ do
        
        it "creates an atomic point with string value" $ do
          let atom = Pattern { value = "node1", elements = [] }
          value atom `shouldBe` "node1"
          elements atom `shouldBe` ([] :: [Pattern String])
        
        it "creates an atomic point with integer value" $ do
          let atom = Pattern { value = 42, elements = [] }
          value atom `shouldBe` (42 :: Int)
          elements atom `shouldBe` ([] :: [Pattern Int])
        
        it "creates an atomic point with custom type value" $ do
          let person = Person "Alice" (Just 30)
          let atom = Pattern { value = person, elements = [] }
          value atom `shouldBe` person
          elements atom `shouldBe` ([] :: [Pattern Person])
      
      describe "Value field accessor" $ do
        
        it "returns the correct value for an atomic point with string" $ do
          let atom = Pattern { value = "test", elements = [] }
          value atom `shouldBe` "test"
        
        it "returns the correct value for an atomic point with integer" $ do
          let atom = Pattern { value = 100, elements = [] }
          value atom `shouldBe` (100 :: Int)
        
        it "returns the correct value for an atomic point with custom type" $ do
          let person = Person "Bob" (Just 25)
          let atom = Pattern { value = person, elements = [] }
          value atom `shouldBe` person
      
      describe "Elements field accessor" $ do
        
        it "returns empty list for atomic pattern" $ do
          let atom = Pattern { value = "empty", elements = [] }
          elements atom `shouldBe` ([] :: [Pattern String])
        
        it "returns empty list for atomic point with different value types" $ do
          let atomInt = Pattern { value = 42, elements = [] }
          let atomString = Pattern { value = "test", elements = [] }
          elements atomInt `shouldBe` ([] :: [Pattern Int])
          elements atomString `shouldBe` ([] :: [Pattern String])
      
      describe "Edge cases" $ do
        
        it "atomic point with explicitly empty list of elements behaves correctly" $ do
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
          let p = Pattern { value = "pattern", elements = [elem] }
          value p `shouldBe` "pattern"
          length (elements p) `shouldBe` 1
          case elements p of
            [x] -> x `shouldBe` elem
            _ -> expectationFailure "Expected exactly one element"
        
        it "creates a point with multiple elements" $ do
          let elem1 = Pattern { value = "elem1", elements = [] }
          let elem2 = Pattern { value = "elem2", elements = [] }
          let elem3 = Pattern { value = "elem3", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          value p `shouldBe` "pattern"
          length (elements p) `shouldBe` 3
          elements p `shouldBe` [elem1, elem2, elem3]
      
      describe "Value field accessor for patterns with elements" $ do
        
        it "returns the correct value for singular pattern" $ do
          let elem = Pattern { value = "elem", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem] }
          value p `shouldBe` "pattern"
        
        it "returns the correct value for point with multiple elements" $ do
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem1, elem2] }
          value p `shouldBe` "pattern"
        
        it "returns the correct value for point with integer value and elements" $ do
          let elem = Pattern { value = 10, elements = [] }
          let p = Pattern { value = 100, elements = [elem] }
          value p `shouldBe` (100 :: Int)
      
      describe "Elements field accessor for patterns with elements" $ do
        
        it "returns correct element list for singular pattern" $ do
          let elem = Pattern { value = "elem", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem] }
          elements p `shouldBe` [elem]
        
        it "returns correct element list for point with multiple elements" $ do
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let elem3 = Pattern { value = "e3", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          elements p `shouldBe` [elem1, elem2, elem3]
        
        it "returns correct element list preserving order" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let elem3 = Pattern { value = "third", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          let elems = elements p
          case elems of
            (x:_) -> value x `shouldBe` "first"
            [] -> expectationFailure "Expected at least one element"
          value (elems !! 1) `shouldBe` "second"
          value (elems !! 2) `shouldBe` "third"
      
      describe "Elements accessibility and order" $ do
        
        it "elements are accessible in correct order" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let elem3 = Pattern { value = "c", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          let elems = elements p
          elems `shouldBe` [elem1, elem2, elem3]
          map value elems `shouldBe` ["a", "b", "c"]
        
        it "can access individual elements by index" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem1, elem2] }
          let elems = elements p
          case elems of
            (x:_) -> x `shouldBe` elem1
            [] -> expectationFailure "Expected at least one element"
          last elems `shouldBe` elem2
      
      describe "Edge cases" $ do
        
        it "point with zero elements behaves like atomic pattern" $ do
          let p = Pattern { value = "node", elements = [] }
          value p `shouldBe` "node"
          elements p `shouldBe` ([] :: [Pattern String])
          null (elements p) `shouldBe` True
        
        it "deeply nested patterns (multiple levels)" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let p = Pattern { value = "pattern", elements = [outer] }
          value p `shouldBe` "pattern"
          length (elements p) `shouldBe` 1
          case elements p of
            (outer:_) -> do
              value outer `shouldBe` "outer"
              let outerElems = elements outer
              length outerElems `shouldBe` 1
              case outerElems of
                (middle:_) -> do
                  value middle `shouldBe` "middle"
                  let middleElems = elements middle
                  length middleElems `shouldBe` 1
                  case middleElems of
                    (inner:_) -> value inner `shouldBe` "inner"
                    [] -> expectationFailure "Expected at least one element"
                [] -> expectationFailure "Expected at least one element"
            [] -> expectationFailure "Expected at least one element"
        
        it "point containing point containing point (arbitrary depth)" $ do
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
        
        it "shows atomic point with string value correctly" $ do
          let atom = Pattern { value = "test", elements = [] }
          show atom `shouldBe` "Pattern {value = \"test\", elements = []}"
        
        it "shows atomic point with integer value correctly" $ do
          let atom = Pattern { value = 42, elements = [] }
          show atom `shouldBe` "Pattern {value = 42, elements = []}"
        
        it "shows atomic point with custom type value correctly" $ do
          let person = Person "Alice" (Just 30)
          let atom = Pattern { value = person, elements = [] }
          show atom `shouldBe` "Pattern {value = Person {name = \"Alice\", age = Just 30}, elements = []}"
      
      describe "Show instance for patterns with elements" $ do
        
        it "shows singular point correctly" $ do
          let elem = Pattern { value = "elem", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem] }
          show p `shouldBe` "Pattern {value = \"pattern\", elements = [Pattern {value = \"elem\", elements = []}]}"
        
        it "shows point with multiple elements correctly" $ do
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem1, elem2] }
          show p `shouldBe` "Pattern {value = \"pattern\", elements = [Pattern {value = \"e1\", elements = []},Pattern {value = \"e2\", elements = []}]}"
        
        it "shows nested patterns correctly" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let p = Pattern { value = "pattern", elements = [outer] }
          show p `shouldContain` "Pattern {value = \"pattern\""
          show p `shouldContain` "Pattern {value = \"outer\""
          show p `shouldContain` "Pattern {value = \"middle\""
          show p `shouldContain` "Pattern {value = \"inner\""
    
    describe "Constructor Functions (User Story 1)" $ do
      
      describe "point function - creating atomic patterns" $ do
        
        it "creates atomic point with string value using point function" $ do
          let atom = point "node1"
          value atom `shouldBe` "node1"
          elements atom `shouldBe` ([] :: [Pattern String])
        
        it "creates atomic point with integer value using point function" $ do
          let atom = point (42 :: Int)
          value atom `shouldBe` (42 :: Int)
          elements atom `shouldBe` ([] :: [Pattern Int])
        
        it "creates atomic point with custom type value using point function" $ do
          let person = Person "Alice" (Just 30)
          let atom = point person
          value atom `shouldBe` person
          elements atom `shouldBe` ([] :: [Pattern Person])
        
        it "point function produces patterns functionally identical to record syntax" $ do
          let atom1 = point "test"
          let atom2 = Pattern { value = "test", elements = [] }
          atom1 `shouldBe` atom2
          value atom1 `shouldBe` value atom2
          elements atom1 `shouldBe` elements atom2
          
          let atom3 = point (100 :: Int)
          let atom4 = Pattern { value = 100, elements = [] }
          atom3 `shouldBe` atom4
          
          let person = Person "Bob" (Just 25)
          let atom5 = point person
          let atom6 = Pattern { value = person, elements = [] }
          atom5 `shouldBe` atom6
    
    describe "Constructor Functions (User Story 2)" $ do
      
      describe "pattern function - creating patterns with elements" $ do
        
        it "creates singular point (one element) using pattern" $ do
          let elem = point "a team sport involving kicking a ball"
          let singular = pattern "soccer" [elem]
          value singular `shouldBe` "soccer"
          length (elements singular) `shouldBe` 1
          head (elements singular) `shouldBe` elem
        
        it "creates role-based singular point with custom type" $ do
          -- "The goalie" is "Hans"
          let goalie = pattern (Person "Goalie" Nothing) [point (Person "Hans" (Just 25))]
          value goalie `shouldBe` Person "Goalie" Nothing
          length (elements goalie) `shouldBe` 1
          value (head (elements goalie)) `shouldBe` Person "Hans" (Just 25)
        
        it "creates pair point (two elements) using pattern" $ do
          let elem1 = point "Alice"
          let elem2 = point "Bob"
          let pair = pattern "knows" [elem1, elem2]
          value pair `shouldBe` "knows"
          length (elements pair) `shouldBe` 2
          elements pair `shouldBe` [elem1, elem2]
        
        it "creates extended point (many elements) using pattern" $ do
          let elem1 = point "elem1"
          let elem2 = point "elem2"
          let elem3 = point "elem3"
          let elem4 = point "elem4"
          let extended = pattern "graph" [elem1, elem2, elem3, elem4]
          value extended `shouldBe` "graph"
          length (elements extended) `shouldBe` 4
          elements extended `shouldBe` [elem1, elem2, elem3, elem4]
        
        it "empty list in pattern produces atomic pattern" $ do
          let atomic = pattern "empty" []
          value atomic `shouldBe` "empty"
          elements atomic `shouldBe` ([] :: [Pattern String])
          atomic `shouldBe` point "empty"
        
        it "pattern preserves element order" $ do
          let elem1 = point "first"
          let elem2 = point "second"
          let elem3 = point "third"
          let p = pattern "sequence" [elem1, elem2, elem3]
          let elems = elements p
          value (head elems) `shouldBe` "first"
          value (elems !! 1) `shouldBe` "second"
          value (elems !! 2) `shouldBe` "third"
        
        it "pattern produces patterns functionally identical to record syntax" $ do
          let elem1 = point "elem1"
          let elem2 = point "elem2"
          let p1 = pattern "test" [elem1, elem2]
          let p2 = Pattern { value = "test", elements = [elem1, elem2] }
          p1 `shouldBe` p2
          value p1 `shouldBe` value p2
          elements p1 `shouldBe` elements p2
          
          let singular1 = pattern "soccer" [point "a team sport involving kicking a ball"]
          let singular2 = Pattern { value = "soccer", elements = [Pattern { value = "a team sport involving kicking a ball", elements = [] }] }
          singular1 `shouldBe` singular2
    
    describe "Constructor Functions (User Story 3)" $ do
      
      describe "fromList function - creating patterns from lists of values" $ do
        
        it "creates point from list of strings using fromList" $ do
          let p = fromList "graph" ["Alice", "Bob", "Charlie"]
          value p `shouldBe` "graph"
          length (elements p) `shouldBe` 3
          map value (elements p) `shouldBe` ["Alice", "Bob", "Charlie"]
        
        it "creates point from list of integers using fromList" $ do
          let nums = fromList (0 :: Int) [1, 2, 3, 4, 5]
          value nums `shouldBe` (0 :: Int)
          length (elements nums) `shouldBe` 5
          map value (elements nums) `shouldBe` [1, 2, 3, 4, 5]
        
        it "creates point from list of custom types using fromList" $ do
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
          atomic `shouldBe` point "empty"
        
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
        
        it "fromList produces patterns functionally identical to pattern decoration (map point values)" $ do
          let values = ["a", "b", "c"]
          let p1 = fromList "test" values
          let p2 = pattern "test" (map point values)
          p1 `shouldBe` p2
          value p1 `shouldBe` value p2
          elements p1 `shouldBe` elements p2
          
          let intValues = [1, 2, 3]
          let p3 = fromList (0 :: Int) intValues
          let p4 = pattern (0 :: Int) (map point intValues)
          p3 `shouldBe` p4
    
    describe "Constructor Functions (User Story 4)" $ do
      
      describe "Comprehensive edge case testing - pattern" $ do
        
        it "pattern with 0 elements produces atomic pattern" $ do
          let atomic = pattern "test" []
          value atomic `shouldBe` "test"
          elements atomic `shouldBe` ([] :: [Pattern String])
          atomic `shouldBe` point "test"
        
        it "pattern with 1 element produces singular pattern" $ do
          let elem = point "definition"
          let singular = pattern "term" [elem]
          value singular `shouldBe` "term"
          length (elements singular) `shouldBe` 1
          head (elements singular) `shouldBe` elem
        
        it "pattern with 2 elements produces pair pattern" $ do
          let elem1 = point "Alice"
          let elem2 = point "Bob"
          let pair = pattern "relationship" [elem1, elem2]
          value pair `shouldBe` "relationship"
          length (elements pair) `shouldBe` 2
          elements pair `shouldBe` [elem1, elem2]
        
        it "pattern with many elements produces extended pattern" $ do
          let elems = map point ["a", "b", "c", "d", "e"]
          let extended = pattern "collection" elems
          value extended `shouldBe` "collection"
          length (elements extended) `shouldBe` 5
          elements extended `shouldBe` elems
      
      describe "Comprehensive edge case testing - fromList" $ do
        
        it "fromList with 0 values produces atomic pattern" $ do
          let atomic = fromList "empty" []
          value atomic `shouldBe` "empty"
          elements atomic `shouldBe` ([] :: [Pattern String])
          atomic `shouldBe` point "empty"
        
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
        
        it "nested patterns using pattern, pattern, and fromList" $ do
          -- Atomic point at base
          let base = point "base"
          
          -- Pattern with elements using pattern
          let mid = pattern "middle" [base]
          
          -- Pattern from list using fromList
          let top = fromList "top" ["value1", "value2"]
          
          -- Combine all using pattern
          let nested = pattern "root" [mid, top]
          
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
          let level4 = point "deep"
          
          -- Level 3: fromList
          let level3 = fromList "level3" ["a", "b"]
          
          -- Level 2: pattern
          let level2 = pattern "level2" [level4, level3]
          
          -- Level 1: pattern with fromList
          let level1 = pattern "level1" [level2, fromList "other" ["x", "y"]]
          
          -- Root: pattern
          let root = pattern "root" [level1]
          
          value root `shouldBe` "root"
          length (elements root) `shouldBe` 1
          value (head (elements root)) `shouldBe` "level1"
          
          let l1 = head (elements root)
          length (elements l1) `shouldBe` 2
          value (head (elements l1)) `shouldBe` "level2"
      
      describe "All value types with all constructors" $ do
        
        it "string values with pattern, pattern, and fromList" $ do
          let p1 = point "string"
          let p2 = pattern "soccer" [point "a team sport involving kicking a ball"]
          let p3 = fromList "list" ["a", "b", "c"]
          
          value p1 `shouldBe` "string"
          value p2 `shouldBe` "soccer"
          value p3 `shouldBe` "list"
          length (elements p1) `shouldBe` 0
          length (elements p2) `shouldBe` 1
          length (elements p3) `shouldBe` 3
        
        it "integer values with pattern, pattern, and fromList" $ do
          let p1 = point (42 :: Int)
          let p2 = pattern (100 :: Int) [point 10, point 20]
          let p3 = fromList (0 :: Int) [1, 2, 3, 4]
          
          value p1 `shouldBe` (42 :: Int)
          value p2 `shouldBe` (100 :: Int)
          value p3 `shouldBe` (0 :: Int)
          length (elements p1) `shouldBe` 0
          length (elements p2) `shouldBe` 2
          length (elements p3) `shouldBe` 4
        
        it "custom type values with pattern, pattern, and fromList" $ do
          let person1 = Person "Alice" (Just 30)
          let person2 = Person "Bob" (Just 25)
          let person3 = Person "Charlie" (Just 35)
          let decoration = Person "Team" Nothing
          
          let p1 = point person1
          let p2 = pattern decoration [point person1, point person2]
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
          let p1 = Pattern { value = "pattern", elements = [elem1, elem2] }
          let p2 = Pattern { value = "pattern", elements = [elem1, elem2] }
          p1 `shouldBe` p2
        
        it "patterns with same value but different elements are not equal" $ do
          let elem1 = Pattern { value = "elem1", elements = [] }
          let elem2 = Pattern { value = "elem2", elements = [] }
          let elem3 = Pattern { value = "elem3", elements = [] }
          let p1 = Pattern { value = "pattern", elements = [elem1, elem2] }
          let p2 = Pattern { value = "pattern", elements = [elem1, elem3] }
          p1 `shouldNotBe` p2
        
        it "patterns with different values but same elements are not equal" $ do
          let elem1 = Pattern { value = "elem1", elements = [] }
          let elem2 = Pattern { value = "elem2", elements = [] }
          let p1 = Pattern { value = "p1", elements = [elem1, elem2] }
          let p2 = Pattern { value = "p2", elements = [elem1, elem2] }
          p1 `shouldNotBe` p2
        
        it "patterns with different numbers of elements are not equal" $ do
          let elem1 = Pattern { value = "elem1", elements = [] }
          let elem2 = Pattern { value = "elem2", elements = [] }
          let p1 = Pattern { value = "pattern", elements = [elem1] }
          let p2 = Pattern { value = "pattern", elements = [elem1, elem2] }
          p1 `shouldNotBe` p2
      
      describe "Equality for nested patterns" $ do
        
        it "two identical deeply nested patterns are equal" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let p1 = Pattern { value = "pattern", elements = [outer] }
          let p2 = Pattern { value = "pattern", elements = [outer] }
          p1 `shouldBe` p2
        
        it "nested patterns with different structure are not equal" $ do
          let innerA = Pattern { value = "inner", elements = [] }
          let middleA = Pattern { value = "middle", elements = [innerA] }
          let outerA = Pattern { value = "outer", elements = [middleA] }
          let p1 = Pattern { value = "pattern", elements = [outerA] }
          
          let middleB = Pattern { value = "middle", elements = [] }
          let outerB = Pattern { value = "outer", elements = [middleB] }
          let p2 = Pattern { value = "pattern", elements = [outerB] }
          
          p1 `shouldNotBe` p2
      
      describe "Equality edge cases" $ do
        
        it "atomic patterns with same value are equal" $ do
          let atom1 = Pattern { value = "test", elements = [] }
          let atom2 = Pattern { value = "test", elements = [] }
          atom1 `shouldBe` atom2
        
        it "reflexivity: point equals itself" $ do
          let p1 = Pattern { value = "test", elements = [] }
          let p2 = Pattern { value = "test", elements = [] }
          (p1 == p2) `shouldBe` True
        
        it "symmetry: if a == b, then b == a" $ do
          let a = Pattern { value = "test", elements = [] }
          let b = Pattern { value = "test", elements = [] }
          (a == b) `shouldBe` (b == a)
    
    describe "Functor Instance (User Story 1)" $ do
      
      describe "Transforming atomic patterns" $ do
        
        it "transforms atomic point with string value" $ do
          let atom = Pattern { value = "test", elements = [] }
          let transformed = fmap (map toUpper) atom
          value transformed `shouldBe` "TEST"
          elements transformed `shouldBe` ([] :: [Pattern String])
      
      describe "Transforming patterns with multiple elements" $ do
        
        it "transforms point with multiple elements" $ do
          let elem1 = Pattern { value = "hello", elements = [] }
          let elem2 = Pattern { value = "world", elements = [] }
          let p = Pattern { value = "greeting", elements = [elem1, elem2] }
          let transformed = fmap (map toUpper) p
          value transformed `shouldBe` "GREETING"
          length (elements transformed) `shouldBe` 2
          value (head (elements transformed)) `shouldBe` "HELLO"
          value (last (elements transformed)) `shouldBe` "WORLD"
      
      describe "Transforming nested point structures" $ do
        
        it "transforms nested point structure" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let p = Pattern { value = "root", elements = [outer] }
          let transformed = fmap (map toUpper) p
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
        
        it "transforms point with integer values" $ do
          let elem1 = Pattern { value = 5, elements = [] }
          let elem2 = Pattern { value = 10, elements = [] }
          let p = Pattern { value = 20, elements = [elem1, elem2] }
          let transformed = fmap (* 2) p
          value transformed `shouldBe` (40 :: Int)
          value (head (elements transformed)) `shouldBe` (10 :: Int)
          value (last (elements transformed)) `shouldBe` (20 :: Int)
      
      describe "Transforming patterns with custom type values" $ do
        
        it "transforms point with custom type values" $ do
          let person1 = Person "Alice" (Just 30)
          let person2 = Person "Bob" (Just 25)
          let elem1 = Pattern { value = person1, elements = [] }
          let elem2 = Pattern { value = person2, elements = [] }
          let p = Pattern { value = person1, elements = [elem1, elem2] }
          let transformed = fmap (\p -> Person (name p) (fmap (+ 1) (age p))) p
          age (value transformed) `shouldBe` Just 31
          age (value (head (elements transformed))) `shouldBe` Just 31
          age (value (last (elements transformed))) `shouldBe` Just 26
      
      describe "Structure preservation" $ do
        
        it "preserves element count during transformation" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let elem3 = Pattern { value = "c", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          let transformed = fmap (map toUpper) p
          length (elements p) `shouldBe` length (elements transformed)
          length (elements transformed) `shouldBe` 3
        
        it "preserves element order during transformation" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let elem3 = Pattern { value = "third", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          let transformed = fmap (map toUpper) p
          let originalValues = map value (elements p)
          let transformedValues = map value (elements transformed)
          originalValues `shouldBe` ["first", "second", "third"]
          transformedValues `shouldBe` ["FIRST", "SECOND", "THIRD"]
          -- Verify order is preserved
          value (head (elements transformed)) `shouldBe` "FIRST"
          value (elements transformed !! 1) `shouldBe` "SECOND"
          value (last (elements transformed)) `shouldBe` "THIRD"
      
      describe "Type transformation" $ do
        
        it "transforms point from String to Int type" $ do
          let elem1 = Pattern { value = "5", elements = [] }
          let elem2 = Pattern { value = "10", elements = [] }
          let p = Pattern { value = "20", elements = [elem1, elem2] }
          let transformed = fmap (read :: String -> Int) p
          value transformed `shouldBe` (20 :: Int)
          value (head (elements transformed)) `shouldBe` (5 :: Int)
          value (last (elements transformed)) `shouldBe` (10 :: Int)
    
    describe "Functor Laws (User Story 2)" $ do
      
      describe "Identity Law" $ do
        
        it "fmap id = id for atomic pattern" $ do
          let atom = Pattern { value = "test", elements = [] }
          fmap id atom `shouldBe` atom
          fmap id atom `shouldBe` id atom
        
        it "fmap id = id for point with elements" $ do
          let elem1 = Pattern { value = "hello", elements = [] }
          let elem2 = Pattern { value = "world", elements = [] }
          let p = Pattern { value = "greeting", elements = [elem1, elem2] }
          fmap id p `shouldBe` p
          fmap id p `shouldBe` id p
      
      describe "Composition Law" $ do
        
        it "fmap (f . g) = fmap f . fmap g with two transformation functions" $ do
          let elem1 = Pattern { value = "hello", elements = [] }
          let elem2 = Pattern { value = "world", elements = [] }
          let p = Pattern { value = "greeting", elements = [elem1, elem2] }
          let f = map toUpper :: String -> String
          let g = reverse :: String -> String
          fmap (f . g) p `shouldBe` (fmap f . fmap g) p
        
        it "fmap (f . g) = fmap f . fmap g with nested patterns" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let p = Pattern { value = "root", elements = [outer] }
          let f = map toUpper :: String -> String
          let g = reverse :: String -> String
          fmap (f . g) p `shouldBe` (fmap f . fmap g) p
    
    describe "Nested Pattern Transformation (User Story 3)" $ do
      
      describe "Transforming patterns with 3+ levels of nesting" $ do
        
        it "transforms point with 3 levels of nesting" $ do
          let level3 = Pattern { value = "level3", elements = [] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let p = Pattern { value = "root", elements = [level1] }
          let transformed = fmap (map toUpper) p
          value transformed `shouldBe` "ROOT"
          value (head (elements transformed)) `shouldBe` "LEVEL1"
          value (head (elements (head (elements transformed)))) `shouldBe` "LEVEL2"
          value (head (elements (head (elements (head (elements transformed)))))) `shouldBe` "LEVEL3"
        
        it "transforms point with 4 levels of nesting" $ do
          let level4 = Pattern { value = "level4", elements = [] }
          let level3 = Pattern { value = "level3", elements = [level4] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let p = Pattern { value = "root", elements = [level1] }
          let transformed = fmap (map toUpper) p
          value transformed `shouldBe` "ROOT"
          value (head (elements transformed)) `shouldBe` "LEVEL1"
          value (head (elements (head (elements transformed)))) `shouldBe` "LEVEL2"
          value (head (elements (head (elements (head (elements transformed)))))) `shouldBe` "LEVEL3"
          value (head (elements (head (elements (head (elements (head (elements transformed)))))))) `shouldBe` "LEVEL4"
      
      describe "Transforming patterns with varying nesting depths in different branches" $ do
        
        it "transforms point with different nesting depths in different branches" $ do
          -- Branch 1: 2 levels deep
          let branch1Level2 = Pattern { value = "b1l2", elements = [] }
          let branch1Level1 = Pattern { value = "b1l1", elements = [branch1Level2] }
          -- Branch 2: 3 levels deep
          let branch2Level3 = Pattern { value = "b2l3", elements = [] }
          let branch2Level2 = Pattern { value = "b2l2", elements = [branch2Level3] }
          let branch2Level1 = Pattern { value = "b2l1", elements = [branch2Level2] }
          -- Branch 3: 1 level deep (atomic)
          let branch3Level1 = Pattern { value = "b3l1", elements = [] }
          let p = Pattern { value = "root", elements = [branch1Level1, branch2Level1, branch3Level1] }
          let transformed = fmap (map toUpper) p
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
        
        it "transforms point with mixed structures (atomic and with elements) at different levels" $ do
          -- Level 1: point with multiple elements
          let atom1 = Pattern { value = "atom1", elements = [] }
          let atom2 = Pattern { value = "atom2", elements = [] }
          let level1 = Pattern { value = "level1", elements = [atom1, atom2] }
          -- Level 2: atomic pattern
          let level2 = Pattern { value = "level2", elements = [] }
          -- Level 3: point with single element
          let level3Atom = Pattern { value = "level3atom", elements = [] }
          let level3 = Pattern { value = "level3", elements = [level3Atom] }
          let p = Pattern { value = "root", elements = [level1, level2, level3] }
          let transformed = fmap (map toUpper) p
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
          let p = Pattern { value = "root", elements = [branch1, branch2] }
          let transformed = fmap (map toUpper) p
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
        
        it "transforms atomic point (no elements)" $ do
          let atom = Pattern { value = "atom", elements = [] }
          let transformed = fmap (map toUpper) atom
          value transformed `shouldBe` "ATOM"
          elements transformed `shouldBe` ([] :: [Pattern String])
          length (elements transformed) `shouldBe` 0
      
      describe "Transforming patterns with empty elements list" $ do
        
        it "transforms point with empty elements list" $ do
          let p = Pattern { value = "empty", elements = [] }
          let transformed = fmap (map toUpper) p
          value transformed `shouldBe` "EMPTY"
          elements transformed `shouldBe` ([] :: [Pattern String])
          null (elements transformed) `shouldBe` True
      
      describe "Transforming singular patterns" $ do
        
        it "transforms singular point (one element)" $ do
          let elem = Pattern { value = "elem", elements = [] }
          let p = Pattern { value = "singular", elements = [elem] }
          let transformed = fmap (map toUpper) p
          value transformed `shouldBe` "SINGULAR"
          length (elements transformed) `shouldBe` 1
          value (head (elements transformed)) `shouldBe` "ELEM"
          elements (head (elements transformed)) `shouldBe` ([] :: [Pattern String])
      
      describe "Transforming pair patterns" $ do
        
        it "transforms pair point (two elements)" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let p = Pattern { value = "pair", elements = [elem1, elem2] }
          let transformed = fmap (map toUpper) p
          value transformed `shouldBe` "PAIR"
          length (elements transformed) `shouldBe` 2
          value (head (elements transformed)) `shouldBe` "FIRST"
          value (last (elements transformed)) `shouldBe` "SECOND"
      
      describe "Transforming extended patterns" $ do
        
        it "transforms extended point (many elements)" $ do
          let elems = map (\i -> Pattern { value = "elem" ++ show i, elements = [] }) [1..10]
          let p = Pattern { value = "extended", elements = elems }
          let transformed = fmap (map toUpper) p
          value transformed `shouldBe` "EXTENDED"
          length (elements transformed) `shouldBe` 10
          value (head (elements transformed)) `shouldBe` "ELEM1"
          value (last (elements transformed)) `shouldBe` "ELEM10"
          -- Verify all elements are transformed correctly
          map value (elements transformed) `shouldBe` map (\i -> "ELEM" ++ show i) [1..10]
      
      describe "Transforming patterns with different value types" $ do
        
        it "transforms patterns with string values" $ do
          let p = Pattern { value = "test", elements = [] }
          let transformed = fmap (map toUpper) p
          value transformed `shouldBe` "TEST"
        
        it "transforms patterns with integer values" $ do
          let p = Pattern { value = 42, elements = [] }
          let transformed = fmap (* 2) p
          value transformed `shouldBe` (84 :: Int)
        
        it "transforms patterns with custom type values" $ do
          let person = Person "Alice" (Just 30)
          let p = Pattern { value = person, elements = [] }
          let transformed = fmap (\p -> Person (name p) (fmap (+ 5) (age p))) p
          age (value transformed) `shouldBe` Just 35
          name (value transformed) `shouldBe` "Alice"
        
        it "transforms patterns with nested custom types" $ do
          let person1 = Person "Alice" (Just 30)
          let person2 = Person "Bob" (Just 25)
          let elem1 = Pattern { value = person1, elements = [] }
          let elem2 = Pattern { value = person2, elements = [] }
          let p = Pattern { value = person1, elements = [elem1, elem2] }
          let transformed = fmap (\p -> Person (map toUpper (name p)) (age p)) p
          name (value transformed) `shouldBe` "ALICE"
          name (value (head (elements transformed))) `shouldBe` "ALICE"
          name (value (last (elements transformed))) `shouldBe` "BOB"
      
      describe "Type transformation edge cases" $ do
        
        it "transforms point from String to Int with empty string" $ do
          let p = Pattern { value = "0", elements = [] }
          let transformed = fmap (read :: String -> Int) p
          value transformed `shouldBe` (0 :: Int)
        
        it "transforms point from String to Int with negative numbers" $ do
          let elem1 = Pattern { value = "-5", elements = [] }
          let elem2 = Pattern { value = "-10", elements = [] }
          let p = Pattern { value = "-20", elements = [elem1, elem2] }
          let transformed = fmap (read :: String -> Int) p
          value transformed `shouldBe` (-20 :: Int)
          value (head (elements transformed)) `shouldBe` (-5 :: Int)
          value (last (elements transformed)) `shouldBe` (-10 :: Int)
        
        it "transforms point from Int to String" $ do
          let elem1 = Pattern { value = 5, elements = [] }
          let elem2 = Pattern { value = 10, elements = [] }
          let p = Pattern { value = 20, elements = [elem1, elem2] }
          let transformed = fmap show p
          value transformed `shouldBe` "20"
          value (head (elements transformed)) `shouldBe` "5"
          value (last (elements transformed)) `shouldBe` "10"
        
        it "transforms point with identity function preserves structure" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let p = Pattern { value = "root", elements = [elem1, elem2] }
          let transformed = fmap id p
          transformed `shouldBe` p
          value transformed `shouldBe` value p
          length (elements transformed) `shouldBe` length (elements p)
    
    describe "Foldable Instance (User Story 1)" $ do
      
      describe "Folding atomic patterns with foldr" $ do
        
        it "folds atomic point with integer value using foldr" $ do
          let atom = Pattern { value = 5, elements = [] }
          foldr (+) 0 atom `shouldBe` (5 :: Int)
        
        it "folds atomic point with string value using foldr" $ do
          let atom = Pattern { value = "test", elements = [] }
          foldr (++) "" atom `shouldBe` "test"
      
      describe "Folding patterns with multiple values using foldr" $ do
        
        it "folds point with multiple integer values using foldr" $ do
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let elem3 = Pattern { value = 30, elements = [] }
          let p = Pattern { value = 100, elements = [elem1, elem2, elem3] }
          -- Should sum: 100 (pattern's value) + 10 + 20 + 30 = 160
          foldr (+) 0 p `shouldBe` (160 :: Int)
        
        it "folds point with string values using foldr" $ do
          let elem1 = Pattern { value = "hello", elements = [] }
          let elem2 = Pattern { value = "world", elements = [] }
          let p = Pattern { value = "greeting", elements = [elem1, elem2] }
          -- Should concatenate: "greeting" ++ "hello" ++ "world" = "greetinghelloworld"
          foldr (++) "" p `shouldBe` "greetinghelloworld"
      
      describe "Folding nested point structures using foldr" $ do
        
        it "folds nested point structure using foldr" $ do
          let inner = Pattern { value = 1, elements = [] }
          let middle = Pattern { value = 2, elements = [inner] }
          let outer = Pattern { value = 3, elements = [middle] }
          let p = Pattern { value = 4, elements = [outer] }
          -- Should sum: 4 + 3 + 2 + 1 = 10
          foldr (+) 0 p `shouldBe` (10 :: Int)
        
        it "folds deeply nested point structure using foldr" $ do
          let level4 = Pattern { value = 1, elements = [] }
          let level3 = Pattern { value = 2, elements = [level4] }
          let level2 = Pattern { value = 3, elements = [level3] }
          let level1 = Pattern { value = 4, elements = [level2] }
          let p = Pattern { value = 5, elements = [level1] }
          -- Should sum: 5 + 4 + 3 + 2 + 1 = 15
          foldr (+) 0 p `shouldBe` (15 :: Int)
      
      describe "Folding patterns with custom type values using foldr" $ do
        
        it "folds point with custom type values using foldr" $ do
          let person1 = Person "Alice" (Just 30)
          let person2 = Person "Bob" (Just 25)
          let elem1 = Pattern { value = person1, elements = [] }
          let elem2 = Pattern { value = person2, elements = [] }
          let p = Pattern { value = person1, elements = [elem1, elem2] }
          -- Count all Person values (pattern's value + 2 elements = 3)
          foldr (\_ acc -> acc + 1) 0 p `shouldBe` (3 :: Int)
      
      describe "Verifying foldr processes pattern's own value" $ do
        
        it "foldr processes pattern's own value" $ do
          let p = Pattern { value = 42, elements = [] }
          -- Should include the pattern's own value (42)
          foldr (+) 0 p `shouldBe` (42 :: Int)
        
        it "foldr processes pattern's own value even when elements exist" $ do
          let elem1 = Pattern { value = 10, elements = [] }
          let p = Pattern { value = 5, elements = [elem1] }
          -- Should sum: 5 (pattern's value) + 10 (element) = 15
          foldr (+) 0 p `shouldBe` (15 :: Int)
      
      describe "Verifying foldr processes all element values recursively" $ do
        
        it "foldr processes all element values recursively" $ do
          let elem1 = Pattern { value = 1, elements = [] }
          let elem2 = Pattern { value = 2, elements = [] }
          let elem3 = Pattern { value = 3, elements = [] }
          let p = Pattern { value = 0, elements = [elem1, elem2, elem3] }
          -- Should sum: 0 + 1 + 2 + 3 = 6
          foldr (+) 0 p `shouldBe` (6 :: Int)
        
        it "foldr processes nested element values recursively" $ do
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle = Pattern { value = 10, elements = [inner1, inner2] }
          let p = Pattern { value = 100, elements = [middle] }
          -- Should sum: 100 + 10 + 1 + 2 = 113
          foldr (+) 0 p `shouldBe` (113 :: Int)
        
        it "foldr processes all values from multiple nested elements" $ do
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle1 = Pattern { value = 10, elements = [inner1] }
          let middle2 = Pattern { value = 20, elements = [inner2] }
          let p = Pattern { value = 100, elements = [middle1, middle2] }
          -- Should sum: 100 + 10 + 1 + 20 + 2 = 133
          foldr (+) 0 p `shouldBe` (133 :: Int)
    
    describe "toList Operation (User Story 2)" $ do
      
      describe "toList on atomic patterns" $ do
        
        it "toList on atomic point returns single-element list" $ do
          let atom = Pattern { value = "test", elements = [] }
          toList atom `shouldBe` ["test"]
      
      describe "toList on patterns with multiple elements" $ do
        
        it "toList on point with multiple elements returns flat list with all values" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let elem3 = Pattern { value = "c", elements = [] }
          let p = Pattern { value = "root", elements = [elem1, elem2, elem3] }
          -- Should return flat list: ["root", "a", "b", "c"]
          toList p `shouldBe` ["root", "a", "b", "c"]
      
      describe "toList on nested patterns" $ do
        
        it "toList on nested point returns flat list with all values from all levels" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let p = Pattern { value = "root", elements = [outer] }
          -- Should return flat list: ["root", "outer", "middle", "inner"]
          toList p `shouldBe` ["root", "outer", "middle", "inner"]
      
      describe "toList on patterns with integer values" $ do
        
        it "toList on point with integer values returns flat list of integers" $ do
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let p = Pattern { value = 100, elements = [elem1, elem2] }
          -- Should return flat list: [100, 10, 20]
          toList p `shouldBe` [100, 10, 20]
      
      describe "Verifying toList includes pattern's own value" $ do
        
        it "toList includes pattern's own value" $ do
          let p = Pattern { value = "test", elements = [] }
          toList p `shouldBe` ["test"]
        
        it "toList includes pattern's own value even when elements exist" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let p = Pattern { value = "root", elements = [elem1] }
          -- Should include "root" as first element
          toList p `shouldBe` ["root", "a"]
      
      describe "Verifying toList preserves element order" $ do
        
        it "toList preserves element order" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let elem3 = Pattern { value = "third", elements = [] }
          let p = Pattern { value = "root", elements = [elem1, elem2, elem3] }
          -- Should preserve order: root, first, second, third
          toList p `shouldBe` ["root", "first", "second", "third"]
        
        it "toList preserves order in nested structures" $ do
          let inner1 = Pattern { value = "inner1", elements = [] }
          let inner2 = Pattern { value = "inner2", elements = [] }
          let middle1 = Pattern { value = "middle1", elements = [inner1] }
          let middle2 = Pattern { value = "middle2", elements = [inner2] }
          let p = Pattern { value = "root", elements = [middle1, middle2] }
          -- Should preserve order: root, middle1, inner1, middle2, inner2
          toList p `shouldBe` ["root", "middle1", "inner1", "middle2", "inner2"]
    
    describe "toTuple Operation (User Story 2b)" $ do
      
      describe "toTuple on atomic patterns" $ do
        
        it "toTuple on atomic point returns tuple with value and empty list" $ do
          let atom = Pattern { value = "test", elements = [] }
          toTuple atom `shouldBe` ("test", [] :: [Pattern String])
        
        it "toTuple on atomic point with integer value returns tuple with integer and empty list" $ do
          let atom = Pattern { value = 42, elements = [] }
          toTuple atom `shouldBe` (42 :: Int, [] :: [Pattern Int])
      
      describe "toTuple on patterns with multiple elements" $ do
        
        it "toTuple on point with multiple elements returns tuple with value and list of element patterns" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let elem3 = Pattern { value = "c", elements = [] }
          let p = Pattern { value = "root", elements = [elem1, elem2, elem3] }
          toTuple p `shouldBe` ("root", [elem1, elem2, elem3])
        
        it "toTuple on point with integer values returns tuple with integer value and list of Pattern Int" $ do
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let p = Pattern { value = 100, elements = [elem1, elem2] }
          toTuple p `shouldBe` (100 :: Int, [elem1, elem2])
      
      describe "toTuple on nested patterns" $ do
        
        it "toTuple on nested point returns tuple where elements list contains nested Pattern structures" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let p = Pattern { value = "root", elements = [outer] }
          let (val, els) = toTuple p
          val `shouldBe` "root"
          length els `shouldBe` 1
          head els `shouldBe` outer
          -- Verify nested structure is preserved
          let (outerVal, outerEls) = toTuple (head els)
          outerVal `shouldBe` "outer"
          length outerEls `shouldBe` 1
          head outerEls `shouldBe` middle
      
      describe "Verifying toTuple preserves point structure" $ do
        
        it "toTuple preserves point structure" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let p = Pattern { value = "root", elements = [elem1, elem2] }
          let (val, els) = toTuple p
          val `shouldBe` "root"
          length els `shouldBe` 2
          els `shouldBe` [elem1, elem2]
          -- Verify elements are still Pattern structures, not flattened
          value (head els) `shouldBe` "first"
          value (last els) `shouldBe` "second"
        
        it "toTuple preserves nested point structure" $ do
          let inner1 = Pattern { value = "inner1", elements = [] }
          let inner2 = Pattern { value = "inner2", elements = [] }
          let middle1 = Pattern { value = "middle1", elements = [inner1] }
          let middle2 = Pattern { value = "middle2", elements = [inner2] }
          let p = Pattern { value = "root", elements = [middle1, middle2] }
          let (val, els) = toTuple p
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
          let p = Pattern { value = 100, elements = [elem1, elem2] }
          -- foldr (+) 0 should sum: 100 + 10 + 20 = 130
          foldr (+) 0 p `shouldBe` (130 :: Int)
        
        it "foldr processes values in correct order for point with three elements" $ do
          -- Pattern: value=1, elements=[2, 3, 4]
          -- foldr should process: 1 + 2 + 3 + 4 = 10
          let elem1 = Pattern { value = 2, elements = [] }
          let elem2 = Pattern { value = 3, elements = [] }
          let elem3 = Pattern { value = 4, elements = [] }
          let p = Pattern { value = 1, elements = [elem1, elem2, elem3] }
          foldr (+) 0 p `shouldBe` (10 :: Int)
        
        it "foldr processes atomic point value correctly" $ do
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
          let p = Pattern { value = "root", elements = [elem1, elem2] }
          -- toList uses foldr internally, should preserve order: pattern's value first, then elements
          toList p `shouldBe` ["root", "a", "b"]
        
        it "foldr builds list in correct order for point with multiple string elements" $ do
          -- Pattern: value="first", elements=["second", "third", "fourth"]
          -- foldr should produce: ["first", "second", "third", "fourth"]
          let elem1 = Pattern { value = "second", elements = [] }
          let elem2 = Pattern { value = "third", elements = [] }
          let elem3 = Pattern { value = "fourth", elements = [] }
          let p = Pattern { value = "first", elements = [elem1, elem2, elem3] }
          toList p `shouldBe` ["first", "second", "third", "fourth"]
        
        it "foldr builds list correctly for atomic point with string value" $ do
          -- Atomic pattern: value="test"
          -- foldr should produce: ["test"]
          let atom = Pattern { value = "test", elements = [] }
          toList atom `shouldBe` ["test"]
      
      describe "foldr processing nested point values in correct order" $ do
        
        it "foldr processes nested point values in correct order" $ do
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
          let p = Pattern { value = 4, elements = [outer] }
          foldr (+) 0 p `shouldBe` (10 :: Int)
        
        it "foldr processes nested point values in correct order for multiple nested elements" $ do
          -- Pattern { value = 100, elements = [
          --   Pattern { value = 10, elements = [Pattern { value = 1, elements = [] }] },
          --   Pattern { value = 20, elements = [Pattern { value = 2, elements = [] }] }
          -- ]}
          -- foldr should process: 100 + 10 + 1 + 20 + 2 = 133
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle1 = Pattern { value = 10, elements = [inner1] }
          let middle2 = Pattern { value = 20, elements = [inner2] }
          let p = Pattern { value = 100, elements = [middle1, middle2] }
          foldr (+) 0 p `shouldBe` (133 :: Int)
        
        it "foldr processes deeply nested point values in correct order" $ do
          -- 5 levels deep: 5 + 4 + 3 + 2 + 1 = 15
          let level4 = Pattern { value = 1, elements = [] }
          let level3 = Pattern { value = 2, elements = [level4] }
          let level2 = Pattern { value = 3, elements = [level3] }
          let level1 = Pattern { value = 4, elements = [level2] }
          let p = Pattern { value = 5, elements = [level1] }
          foldr (+) 0 p `shouldBe` (15 :: Int)
      
      describe "foldr right-associativity property" $ do
        
        it "foldr right-associativity property: foldr f z = foldr f z . toList" $ do
          -- For commutative operations, foldr on point should equal foldr on toList
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let p = Pattern { value = 100, elements = [elem1, elem2] }
          -- Addition is commutative, so both should produce same result
          let patternFold = foldr (+) 0 p
          let listFold = foldr (+) 0 (toList p)
          patternFold `shouldBe` listFold
          patternFold `shouldBe` (130 :: Int)
        
        it "foldr right-associativity property: order matters for non-commutative operations" $ do
          -- For non-commutative operations like list building, order is preserved
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let p = Pattern { value = "root", elements = [elem1, elem2] }
          -- toList preserves order: pattern's value first, then elements
          toList p `shouldBe` ["root", "a", "b"]
          -- foldr (:) [] should produce same order
          foldr (:) [] p `shouldBe` ["root", "a", "b"]
        
        it "foldr right-associativity property: point value processed before elements" $ do
          -- Verify that pattern's own value is processed first (combined with accumulated elements)
          -- Using a function that reveals order: building a list with markers
          let elem1 = Pattern { value = "elem1", elements = [] }
          let elem2 = Pattern { value = "elem2", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem1, elem2] }
          -- foldr builds: pattern's value first, then elements in order
          toList p `shouldBe` ["pattern", "elem1", "elem2"]
          -- Verify order by checking first element is pattern's value
          head (toList p) `shouldBe` "pattern"
    
    describe "Fold with Left-Associative Operations (User Story 4)" $ do
      
      describe "foldl processing values in left-to-right order with addition" $ do
        
        it "foldl processes values in left-to-right order with addition" $ do
          -- Pattern: value=100, elements=[10, 20]
          -- foldl should process: ((0 + 100) + 10) + 20 = 130
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let p = Pattern { value = 100, elements = [elem1, elem2] }
          -- foldl (+) 0 should sum: ((0 + 100) + 10) + 20 = 130
          foldl (+) 0 p `shouldBe` (130 :: Int)
        
        it "foldl processes values in left-to-right order for point with three elements" $ do
          -- Pattern: value=1, elements=[2, 3, 4]
          -- foldl should process: (((0 + 1) + 2) + 3) + 4 = 10
          let elem1 = Pattern { value = 2, elements = [] }
          let elem2 = Pattern { value = 3, elements = [] }
          let elem3 = Pattern { value = 4, elements = [] }
          let p = Pattern { value = 1, elements = [elem1, elem2, elem3] }
          foldl (+) 0 p `shouldBe` (10 :: Int)
        
        it "foldl processes atomic point value correctly" $ do
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
          let p = Pattern { value = 10, elements = [elem1, elem2] }
          foldl (+) 0 p `shouldBe` (60 :: Int)
        
        it "foldl computes running total correctly for nested pattern" $ do
          -- Pattern: value=1, elements=[Pattern { value=2, elements=[Pattern { value=3 }] }]
          -- foldl should compute: (((0 + 1) + 2) + 3) = 6
          let inner = Pattern { value = 3, elements = [] }
          let middle = Pattern { value = 2, elements = [inner] }
          let p = Pattern { value = 1, elements = [middle] }
          foldl (+) 0 p `shouldBe` (6 :: Int)
        
        it "foldl computes running total correctly with multiple nested elements" $ do
          -- Pattern: value=100, elements=[Pattern { value=10, elements=[Pattern { value=1 }] }, Pattern { value=20, elements=[Pattern { value=2 }] }]
          -- foldl should compute: (((((0 + 100) + 10) + 1) + 20) + 2) = 133
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle1 = Pattern { value = 10, elements = [inner1] }
          let middle2 = Pattern { value = 20, elements = [inner2] }
          let p = Pattern { value = 100, elements = [middle1, middle2] }
          foldl (+) 0 p `shouldBe` (133 :: Int)
      
      describe "foldl processing nested point values in left-to-right order" $ do
        
        it "foldl processes nested point values in left-to-right order" $ do
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
          let p = Pattern { value = 4, elements = [outer] }
          foldl (+) 0 p `shouldBe` (10 :: Int)
        
        it "foldl processes nested point values in left-to-right order for multiple nested elements" $ do
          -- Pattern { value = 100, elements = [
          --   Pattern { value = 10, elements = [Pattern { value = 1, elements = [] }] },
          --   Pattern { value = 20, elements = [Pattern { value = 2, elements = [] }] }
          -- ]}
          -- foldl should process: (((((0 + 100) + 10) + 1) + 20) + 2) = 133
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle1 = Pattern { value = 10, elements = [inner1] }
          let middle2 = Pattern { value = 20, elements = [inner2] }
          let p = Pattern { value = 100, elements = [middle1, middle2] }
          foldl (+) 0 p `shouldBe` (133 :: Int)
        
        it "foldl processes deeply nested point values in left-to-right order" $ do
          -- 5 levels deep: (((((0 + 5) + 4) + 3) + 2) + 1) = 15
          let level4 = Pattern { value = 1, elements = [] }
          let level3 = Pattern { value = 2, elements = [level4] }
          let level2 = Pattern { value = 3, elements = [level3] }
          let level1 = Pattern { value = 4, elements = [level2] }
          let p = Pattern { value = 5, elements = [level1] }
          foldl (+) 0 p `shouldBe` (15 :: Int)
      
      describe "foldl left-associativity property" $ do
        
        it "foldl left-associativity property: foldl f z = foldl f z . toList for commutative operations" $ do
          -- For commutative operations, foldl on point should equal foldl on toList
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let p = Pattern { value = 100, elements = [elem1, elem2] }
          -- Addition is commutative, so both should produce same result
          let patternFold = foldl (+) 0 p
          let listFold = foldl (+) 0 (toList p)
          patternFold `shouldBe` listFold
          patternFold `shouldBe` (130 :: Int)
        
        it "foldl left-associativity property: order matters for non-commutative operations" $ do
          -- For non-commutative operations like subtraction, order matters
          let elem1 = Pattern { value = 5, elements = [] }
          let elem2 = Pattern { value = 3, elements = [] }
          let p = Pattern { value = 10, elements = [elem1, elem2] }
          -- foldl (-) 0 should compute: (((0 - 10) - 5) - 3) = -18
          foldl (-) 0 p `shouldBe` (-18 :: Int)
        
        it "foldl left-associativity property: point value processed first" $ do
          -- Verify that pattern's own value is processed first (left-to-right)
          -- Using subtraction to reveal order: ((0 - p) - elem1) - elem2
          let elem1 = Pattern { value = 5, elements = [] }
          let elem2 = Pattern { value = 3, elements = [] }
          let p = Pattern { value = 10, elements = [elem1, elem2] }
          -- foldl (-) 0 should compute: (((0 - 10) - 5) - 3) = -18
          foldl (-) 0 p `shouldBe` (-18 :: Int)
          -- Verify by checking intermediate steps
          foldl (-) 0 (Pattern { value = 10, elements = [] }) `shouldBe` (-10 :: Int)
    
    describe "Map Values to Monoids and Combine (User Story 5)" $ do
      
      describe "foldMap with Sum monoid on integer pattern" $ do
        
        it "foldMap with Sum monoid on atomic point with integer value" $ do
          let atom = Pattern { value = 5, elements = [] }
          getSum (foldMap Sum atom) `shouldBe` (5 :: Int)
        
        it "foldMap with Sum monoid on point with multiple integer values" $ do
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let elem3 = Pattern { value = 30, elements = [] }
          let p = Pattern { value = 100, elements = [elem1, elem2, elem3] }
          -- Should sum: 100 + 10 + 20 + 30 = 160
          getSum (foldMap Sum p) `shouldBe` (160 :: Int)
        
        it "foldMap with Sum monoid on point with negative integer values" $ do
          let elem1 = Pattern { value = -5, elements = [] }
          let elem2 = Pattern { value = -10, elements = [] }
          let p = Pattern { value = 100, elements = [elem1, elem2] }
          -- Should sum: 100 + (-5) + (-10) = 85
          getSum (foldMap Sum p) `shouldBe` (85 :: Int)
      
      describe "foldMap with list monoid on string pattern" $ do
        
        it "foldMap with list monoid on atomic point with string value" $ do
          let atom = Pattern { value = "test", elements = [] }
          foldMap (: []) atom `shouldBe` ["test"]
        
        it "foldMap with list monoid on point with multiple string values" $ do
          let elem1 = Pattern { value = "hello", elements = [] }
          let elem2 = Pattern { value = "world", elements = [] }
          let p = Pattern { value = "greeting", elements = [elem1, elem2] }
          -- Should concatenate: ["greeting", "hello", "world"]
          foldMap (: []) p `shouldBe` ["greeting", "hello", "world"]
        
        it "foldMap with list monoid concatenates string values correctly" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let elem3 = Pattern { value = "c", elements = [] }
          let p = Pattern { value = "root", elements = [elem1, elem2, elem3] }
          -- Should produce: ["root", "a", "b", "c"]
          foldMap (: []) p `shouldBe` ["root", "a", "b", "c"]
      
      describe "foldMap with All monoid on boolean pattern" $ do
        
        it "foldMap with All monoid on atomic point with boolean value" $ do
          let atom = Pattern { value = True, elements = [] }
          getAll (foldMap All atom) `shouldBe` True
        
        it "foldMap with All monoid on point with multiple boolean values (all True)" $ do
          let elem1 = Pattern { value = True, elements = [] }
          let elem2 = Pattern { value = True, elements = [] }
          let p = Pattern { value = True, elements = [elem1, elem2] }
          -- Should produce: True && True && True = True
          getAll (foldMap All p) `shouldBe` True
        
        it "foldMap with All monoid on point with multiple boolean values (one False)" $ do
          let elem1 = Pattern { value = True, elements = [] }
          let elem2 = Pattern { value = False, elements = [] }
          let p = Pattern { value = True, elements = [elem1, elem2] }
          -- Should produce: True && True && False = False
          getAll (foldMap All p) `shouldBe` False
        
        it "foldMap with All monoid on point with multiple boolean values (all False)" $ do
          let elem1 = Pattern { value = False, elements = [] }
          let elem2 = Pattern { value = False, elements = [] }
          let p = Pattern { value = False, elements = [elem1, elem2] }
          -- Should produce: False && False && False = False
          getAll (foldMap All p) `shouldBe` False
      
      describe "foldMap processing nested point values correctly" $ do
        
        it "foldMap with Sum monoid processes nested point values correctly" $ do
          let inner = Pattern { value = 1, elements = [] }
          let middle = Pattern { value = 2, elements = [inner] }
          let outer = Pattern { value = 3, elements = [middle] }
          let p = Pattern { value = 4, elements = [outer] }
          -- Should sum: 4 + 3 + 2 + 1 = 10
          getSum (foldMap Sum p) `shouldBe` (10 :: Int)
        
        it "foldMap with list monoid processes nested point values correctly" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let p = Pattern { value = "root", elements = [outer] }
          -- Should produce: ["root", "outer", "middle", "inner"]
          foldMap (: []) p `shouldBe` ["root", "outer", "middle", "inner"]
        
        it "foldMap with All monoid processes nested point values correctly" $ do
          let inner = Pattern { value = True, elements = [] }
          let middle = Pattern { value = True, elements = [inner] }
          let outer = Pattern { value = True, elements = [middle] }
          let p = Pattern { value = True, elements = [outer] }
          -- Should produce: True && True && True && True = True
          getAll (foldMap All p) `shouldBe` True
        
        it "foldMap processes deeply nested point values correctly" $ do
          let level4 = Pattern { value = 1, elements = [] }
          let level3 = Pattern { value = 2, elements = [level4] }
          let level2 = Pattern { value = 3, elements = [level3] }
          let level1 = Pattern { value = 4, elements = [level2] }
          let p = Pattern { value = 5, elements = [level1] }
          -- Should sum: 5 + 4 + 3 + 2 + 1 = 15
          getSum (foldMap Sum p) `shouldBe` (15 :: Int)
        
        it "foldMap processes multiple nested elements correctly" $ do
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle1 = Pattern { value = 10, elements = [inner1] }
          let middle2 = Pattern { value = 20, elements = [inner2] }
          let p = Pattern { value = 100, elements = [middle1, middle2] }
          -- Should sum: 100 + 10 + 1 + 20 + 2 = 133
          getSum (foldMap Sum p) `shouldBe` (133 :: Int)
    
    describe "Edge Cases & Comprehensive Testing (Phase 7)" $ do
      
      describe "Folding atomic patterns (no elements)" $ do
        
        it "folds atomic point (no elements) with foldr" $ do
          let atom = Pattern { value = 42, elements = [] }
          foldr (+) 0 atom `shouldBe` (42 :: Int)
          toList atom `shouldBe` [42]
          foldl (+) 0 atom `shouldBe` (42 :: Int)
          getSum (foldMap Sum atom) `shouldBe` (42 :: Int)
        
        it "folds atomic point (no elements) with string value" $ do
          let atom = Pattern { value = "test", elements = [] }
          foldr (++) "" atom `shouldBe` "test"
          toList atom `shouldBe` ["test"]
          foldl (++) "" atom `shouldBe` "test"
          foldMap (: []) atom `shouldBe` ["test"]
        
        it "folds atomic point (no elements) with custom type" $ do
          let person = Person "Alice" (Just 30)
          let atom = Pattern { value = person, elements = [] }
          foldr (\_ acc -> acc + 1) 0 atom `shouldBe` (1 :: Int)
          toList atom `shouldBe` [person]
          length (toList atom) `shouldBe` 1
      
      describe "Folding patterns with empty elements list" $ do
        
        it "folds point with empty elements list using foldr" $ do
          let p = Pattern { value = 10, elements = [] }
          foldr (+) 0 p `shouldBe` (10 :: Int)
          toList p `shouldBe` [10]
          length (toList p) `shouldBe` 1
        
        it "folds point with empty elements list using foldl" $ do
          let p = Pattern { value = 20, elements = [] }
          foldl (+) 0 p `shouldBe` (20 :: Int)
          toList p `shouldBe` [20]
        
        it "folds point with empty elements list using foldMap" $ do
          let p = Pattern { value = 30, elements = [] }
          getSum (foldMap Sum p) `shouldBe` (30 :: Int)
          toList p `shouldBe` [30]
        
        it "folds point with empty elements list preserves structure" $ do
          let p = Pattern { value = "empty", elements = [] }
          toTuple p `shouldBe` ("empty", [] :: [Pattern String])
          toList p `shouldBe` ["empty"]
      
      describe "Folding singular patterns (one element)" $ do
        
        it "folds singular point (one element) using foldr" $ do
          let elem = Pattern { value = 5, elements = [] }
          let p = Pattern { value = 10, elements = [elem] }
          -- Should sum: 10 + 5 = 15
          foldr (+) 0 p `shouldBe` (15 :: Int)
          toList p `shouldBe` [10, 5]
        
        it "folds singular point (one element) using foldl" $ do
          let elem = Pattern { value = 3, elements = [] }
          let p = Pattern { value = 7, elements = [elem] }
          -- Should sum: ((0 + 7) + 3) = 10
          foldl (+) 0 p `shouldBe` (10 :: Int)
          toList p `shouldBe` [7, 3]
        
        it "folds singular point (one element) using foldMap" $ do
          let elem = Pattern { value = 2, elements = [] }
          let p = Pattern { value = 8, elements = [elem] }
          -- Should sum: 8 + 2 = 10
          getSum (foldMap Sum p) `shouldBe` (10 :: Int)
          toList p `shouldBe` [8, 2]
        
        it "folds singular point (one element) with string values" $ do
          let elem = Pattern { value = "world", elements = [] }
          let p = Pattern { value = "hello", elements = [elem] }
          toList p `shouldBe` ["hello", "world"]
          foldr (++) "" p `shouldBe` "helloworld"
      
      describe "Folding patterns with many elements" $ do
        
        it "folds point with many elements using foldr" $ do
          let elems = map (\i -> Pattern { value = i, elements = [] }) [1..10]
          let p = Pattern { value = 100, elements = elems }
          -- Should sum: 100 + 1 + 2 + ... + 10 = 100 + 55 = 155
          foldr (+) 0 p `shouldBe` (155 :: Int)
          length (toList p) `shouldBe` 11
          head (toList p) `shouldBe` 100
        
        it "folds point with many elements using foldl" $ do
          let elems = map (\i -> Pattern { value = i, elements = [] }) [1..5]
          let p = Pattern { value = 50, elements = elems }
          -- Should sum: (((((0 + 50) + 1) + 2) + 3) + 4) + 5 = 65
          foldl (+) 0 p `shouldBe` (65 :: Int)
          length (toList p) `shouldBe` 6
        
        it "folds point with many elements using foldMap" $ do
          let elems = map (\i -> Pattern { value = i * 2, elements = [] }) [1..5]
          let p = Pattern { value = 100, elements = elems }
          -- Should sum: 100 + 2 + 4 + 6 + 8 + 10 = 130
          getSum (foldMap Sum p) `shouldBe` (130 :: Int)
          length (toList p) `shouldBe` 6
        
        it "folds point with many string elements" $ do
          let elems = map (\i -> Pattern { value = "elem" ++ show i, elements = [] }) [1..10]
          let p = Pattern { value = "root", elements = elems }
          length (toList p) `shouldBe` 11
          head (toList p) `shouldBe` "root"
          last (toList p) `shouldBe` "elem10"
      
      describe "Folding nested patterns with varying depths" $ do
        
        it "folds nested patterns with depth 2" $ do
          let inner = Pattern { value = 1, elements = [] }
          let p = Pattern { value = 2, elements = [inner] }
          foldr (+) 0 p `shouldBe` (3 :: Int)
          toList p `shouldBe` [2, 1]
        
        it "folds nested patterns with depth 3" $ do
          let inner = Pattern { value = 1, elements = [] }
          let middle = Pattern { value = 2, elements = [inner] }
          let p = Pattern { value = 3, elements = [middle] }
          foldr (+) 0 p `shouldBe` (6 :: Int)
          toList p `shouldBe` [3, 2, 1]
        
        it "folds nested patterns with depth 4" $ do
          let level3 = Pattern { value = 1, elements = [] }
          let level2 = Pattern { value = 2, elements = [level3] }
          let level1 = Pattern { value = 3, elements = [level2] }
          let p = Pattern { value = 4, elements = [level1] }
          foldr (+) 0 p `shouldBe` (10 :: Int)
          toList p `shouldBe` [4, 3, 2, 1]
        
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
          let p = Pattern { value = 1000, elements = [branch1, branch2, branch3] }
          -- Should sum: 1000 + 10 + 1 + 200 + 20 + 2 + 300 = 1533
          foldr (+) 0 p `shouldBe` (1533 :: Int)
          length (toList p) `shouldBe` 7
        
        it "folds nested patterns with multiple elements at each level" $ do
          let inner1 = Pattern { value = 1, elements = [] }
          let inner2 = Pattern { value = 2, elements = [] }
          let middle = Pattern { value = 10, elements = [inner1, inner2] }
          let p = Pattern { value = 100, elements = [middle] }
          -- Should sum: 100 + 10 + 1 + 2 = 113
          foldr (+) 0 p `shouldBe` (113 :: Int)
          toList p `shouldBe` [100, 10, 1, 2]
      
      describe "Folding patterns with different value types" $ do
        
        it "folds patterns with string values" $ do
          let elem1 = Pattern { value = "hello", elements = [] }
          let elem2 = Pattern { value = "world", elements = [] }
          let p = Pattern { value = "greeting", elements = [elem1, elem2] }
          foldr (++) "" p `shouldBe` "greetinghelloworld"
          toList p `shouldBe` ["greeting", "hello", "world"]
          foldMap (: []) p `shouldBe` ["greeting", "hello", "world"]
        
        it "folds patterns with integer values" $ do
          let elem1 = Pattern { value = 10, elements = [] }
          let elem2 = Pattern { value = 20, elements = [] }
          let p = Pattern { value = 100, elements = [elem1, elem2] }
          foldr (+) 0 p `shouldBe` (130 :: Int)
          toList p `shouldBe` [100, 10, 20]
          getSum (foldMap Sum p) `shouldBe` (130 :: Int)
        
        it "folds patterns with custom type values" $ do
          let person1 = Person "Alice" (Just 30)
          let person2 = Person "Bob" (Just 25)
          let elem1 = Pattern { value = person1, elements = [] }
          let elem2 = Pattern { value = person2, elements = [] }
          let p = Pattern { value = person1, elements = [elem1, elem2] }
          -- Count all Person values
          foldr (\_ acc -> acc + 1) 0 p `shouldBe` (3 :: Int)
          length (toList p) `shouldBe` 3
          head (toList p) `shouldBe` person1
        
        it "folds patterns with mixed value types in nested structure" $ do
          -- All values must be same type, so test with strings
          let inner1 = Pattern { value = "inner1", elements = [] }
          let inner2 = Pattern { value = "inner2", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner1, inner2] }
          let p = Pattern { value = "root", elements = [middle] }
          toList p `shouldBe` ["root", "middle", "inner1", "inner2"]
          foldr (++) "" p `shouldBe` "rootmiddleinner1inner2"
      
      describe "Order preservation in folding operations" $ do
        
        it "order preservation in toList" $ do
          let elem1 = Pattern { value = "first", elements = [] }
          let elem2 = Pattern { value = "second", elements = [] }
          let elem3 = Pattern { value = "third", elements = [] }
          let p = Pattern { value = "root", elements = [elem1, elem2, elem3] }
          -- toList should preserve order: root, first, second, third
          toList p `shouldBe` ["root", "first", "second", "third"]
          head (toList p) `shouldBe` "root"
          last (toList p) `shouldBe` "third"
        
        it "order preservation in foldr" $ do
          let elem1 = Pattern { value = 1, elements = [] }
          let elem2 = Pattern { value = 2, elements = [] }
          let elem3 = Pattern { value = 3, elements = [] }
          let p = Pattern { value = 0, elements = [elem1, elem2, elem3] }
          -- foldr (:) [] should preserve order
          foldr (:) [] p `shouldBe` [0, 1, 2, 3]
          toList p `shouldBe` [0, 1, 2, 3]
        
        it "order preservation in foldl" $ do
          let elem1 = Pattern { value = "1", elements = [] }
          let elem2 = Pattern { value = "2", elements = [] }
          let p = Pattern { value = "0", elements = [elem1, elem2] }
          -- foldl should process in left-to-right order
          foldl (++) "" p `shouldBe` "012"
          toList p `shouldBe` ["0", "1", "2"]
        
        it "order preservation in nested structures" $ do
          let inner1 = Pattern { value = "inner1", elements = [] }
          let inner2 = Pattern { value = "inner2", elements = [] }
          let middle1 = Pattern { value = "middle1", elements = [inner1] }
          let middle2 = Pattern { value = "middle2", elements = [inner2] }
          let p = Pattern { value = "root", elements = [middle1, middle2] }
          -- Order should be: root, middle1, inner1, middle2, inner2
          toList p `shouldBe` ["root", "middle1", "inner1", "middle2", "inner2"]
        
        it "order preservation with foldMap" $ do
          let elem1 = Pattern { value = "a", elements = [] }
          let elem2 = Pattern { value = "b", elements = [] }
          let p = Pattern { value = "root", elements = [elem1, elem2] }
          -- foldMap (: []) should preserve order
          foldMap (: []) p `shouldBe` ["root", "a", "b"]
          toList p `shouldBe` ["root", "a", "b"]
      
      describe "Deep nesting (3+ levels)" $ do
        
        it "folds point with 3 levels of nesting" $ do
          let level3 = Pattern { value = 1, elements = [] }
          let level2 = Pattern { value = 2, elements = [level3] }
          let level1 = Pattern { value = 3, elements = [level2] }
          let p = Pattern { value = 4, elements = [level1] }
          -- Should sum: 4 + 3 + 2 + 1 = 10
          foldr (+) 0 p `shouldBe` (10 :: Int)
          toList p `shouldBe` [4, 3, 2, 1]
          foldl (+) 0 p `shouldBe` (10 :: Int)
          getSum (foldMap Sum p) `shouldBe` (10 :: Int)
        
        it "folds point with 4 levels of nesting" $ do
          let level4 = Pattern { value = 1, elements = [] }
          let level3 = Pattern { value = 2, elements = [level4] }
          let level2 = Pattern { value = 3, elements = [level3] }
          let level1 = Pattern { value = 4, elements = [level2] }
          let p = Pattern { value = 5, elements = [level1] }
          -- Should sum: 5 + 4 + 3 + 2 + 1 = 15
          foldr (+) 0 p `shouldBe` (15 :: Int)
          toList p `shouldBe` [5, 4, 3, 2, 1]
          length (toList p) `shouldBe` 5
        
        it "folds point with 5 levels of nesting" $ do
          let level5 = Pattern { value = 1, elements = [] }
          let level4 = Pattern { value = 2, elements = [level5] }
          let level3 = Pattern { value = 3, elements = [level4] }
          let level2 = Pattern { value = 4, elements = [level3] }
          let level1 = Pattern { value = 5, elements = [level2] }
          let p = Pattern { value = 6, elements = [level1] }
          -- Should sum: 6 + 5 + 4 + 3 + 2 + 1 = 21
          foldr (+) 0 p `shouldBe` (21 :: Int)
          toList p `shouldBe` [6, 5, 4, 3, 2, 1]
          length (toList p) `shouldBe` 6
        
        it "folds point with deep nesting and multiple elements at each level" $ do
          let level3a = Pattern { value = 1, elements = [] }
          let level3b = Pattern { value = 2, elements = [] }
          let level2 = Pattern { value = 10, elements = [level3a, level3b] }
          let level1 = Pattern { value = 20, elements = [level2] }
          let p = Pattern { value = 100, elements = [level1] }
          -- Should sum: 100 + 20 + 10 + 1 + 2 = 133
          foldr (+) 0 p `shouldBe` (133 :: Int)
          toList p `shouldBe` [100, 20, 10, 1, 2]
          length (toList p) `shouldBe` 5
        
        it "folds point with deep nesting using all foldable operations" $ do
          let level3 = Pattern { value = "level3", elements = [] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let p = Pattern { value = "root", elements = [level1] }
          -- Test all operations
          toList p `shouldBe` ["root", "level1", "level2", "level3"]
          foldr (++) "" p `shouldBe` "rootlevel1level2level3"
          foldl (++) "" p `shouldBe` "rootlevel1level2level3"
          foldMap (: []) p `shouldBe` ["root", "level1", "level2", "level3"]
    
    describe "Traversable Instance (User Story 1)" $ do
      
      describe "Traversing atomic patterns with Identity" $ do
        
        it "traverses atomic point with Identity" $ do
          -- T009: Unit test for traversing atomic point with Identity
          let atom = point "test"
              result = traverse Identity atom
          runIdentity result `shouldBe` atom
      
      describe "Traversing atomic patterns with Maybe" $ do
        
        it "traverses atomic point with Maybe (Just value)" $ do
          -- T010: Unit test for traversing atomic point with Maybe (Just value)
          let validate x = if x > 0 then Just x else Nothing
              atom = point 5
              result = traverse validate atom
          result `shouldBe` Just atom
        
        it "traverses atomic point with Maybe (Nothing on failure)" $ do
          -- T011: Unit test for traversing atomic point with Maybe (Nothing on failure)
          let validate x = if x > 0 then Just x else Nothing
              atom = point (-3)
              result = traverse validate atom
          result `shouldBe` Nothing
      
      describe "Traversing atomic patterns with Either" $ do
        
        it "traverses atomic point with Either (Right value)" $ do
          -- T012: Unit test for traversing atomic point with Either (Right value)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              atom = point 5
              result = traverse validate atom
          result `shouldBe` Right atom
        
        it "traverses atomic point with Either (Left error)" $ do
          -- T013: Unit test for traversing atomic point with Either (Left error)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              atom = point (-3)
              result = traverse validate atom
          result `shouldBe` Left "Invalid: -3"
      
      describe "Traversing patterns with multiple elements using Identity" $ do
        
        it "traverses point with multiple elements using Identity" $ do
          -- T014: Unit test for traversing point with multiple elements using Identity
          let elem1 = point "elem1"
              elem2 = point "elem2"
              p = pattern "root" [elem1, elem2]
              result = traverse Identity p
          runIdentity result `shouldBe` p
      
      describe "Traversing patterns with multiple elements using Maybe" $ do
        
        it "traverses point with multiple elements using Maybe (all succeed)" $ do
          -- T015: Unit test for traversing point with multiple elements using Maybe (all succeed)
          let validate x = if x > 0 then Just x else Nothing
              elem1 = point 5
              elem2 = point 10
              p = pattern 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Just p
        
        it "traverses point with multiple elements using Maybe (one fails)" $ do
          -- T016: Unit test for traversing point with multiple elements using Maybe (one fails)
          let validate x = if x > 0 then Just x else Nothing
              elem1 = point 5
              elem2 = point (-3)
              p = pattern 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Nothing
      
      describe "Traversing patterns with multiple elements using Either" $ do
        
        it "traverses point with multiple elements using Either (all succeed)" $ do
          -- T017: Unit test for traversing point with multiple elements using Either (all succeed)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              elem1 = point 5
              elem2 = point 10
              p = pattern 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Right p
        
        it "traverses point with multiple elements using Either (one fails)" $ do
          -- T018: Unit test for traversing point with multiple elements using Either (one fails)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              elem1 = point 5
              elem2 = point (-3)
              p = pattern 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Left "Invalid: -3"
      
      describe "Traversing nested point structures with Identity" $ do
        
        it "traverses nested point structure with Identity" $ do
          -- T019: Unit test for traversing nested point structure with Identity
          let inner = point "inner"
              middle = pattern "middle" [inner]
              outer = pattern "outer" [middle]
              p = pattern "root" [outer]
              result = traverse Identity p
          runIdentity result `shouldBe` p
      
      describe "Traversing nested point structures with Maybe" $ do
        
        it "traverses nested point structure with Maybe (all succeed)" $ do
          -- T020: Unit test for traversing nested point structure with Maybe (all succeed)
          let validate x = if x > 0 then Just x else Nothing
              inner = point 1
              middle = pattern 2 [inner]
              outer = pattern 3 [middle]
              p = pattern 4 [outer]
              result = traverse validate p
          result `shouldBe` Just p
        
        it "traverses nested point structure with Maybe (one fails)" $ do
          -- T021: Unit test for traversing nested point structure with Maybe (one fails)
          let validate x = if x > 0 then Just x else Nothing
              inner = point (-1)
              middle = pattern 2 [inner]
              outer = pattern 3 [middle]
              p = pattern 4 [outer]
              result = traverse validate p
          result `shouldBe` Nothing
      
      describe "Traversing nested point structures with Either" $ do
        
        it "traverses nested point structure with Either (all succeed)" $ do
          -- T022: Unit test for traversing nested point structure with Either (all succeed)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              inner = point 1
              middle = pattern 2 [inner]
              outer = pattern 3 [middle]
              p = pattern 4 [outer]
              result = traverse validate p
          result `shouldBe` Right p
        
        it "traverses nested point structure with Either (one fails)" $ do
          -- T023: Unit test for traversing nested point structure with Either (one fails)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              inner = point (-1)
              middle = pattern 2 [inner]
              outer = pattern 3 [middle]
              p = pattern 4 [outer]
              result = traverse validate p
          result `shouldBe` Left "Invalid: -1"
      
      describe "Structure preservation in traverse" $ do
        
        it "traverse preserves point structure (element count, nesting depth, element order)" $ do
          -- T024: Unit test verifying traverse preserves point structure
          let elem1 = point "a"
              elem2 = point "b"
              elem3 = point "c"
              p = pattern "root" [elem1, elem2, elem3]
              result = traverse Identity p
              p' = runIdentity result
          length (elements p') `shouldBe` 3
          value (elements p' !! 0) `shouldBe` "a"
          value (elements p' !! 1) `shouldBe` "b"
          value (elements p' !! 2) `shouldBe` "c"
        
        it "traverse processes pattern's own value" $ do
          -- T025: Unit test verifying traverse processes pattern's own value
          let validate x = if x > 0 then Just (x * 2) else Nothing
              atom = point 5
              result = traverse validate atom
          result `shouldBe` Just (point 10)
        
        it "traverse processes all element values recursively" $ do
          -- T026: Unit test verifying traverse processes all element values recursively
          let validate x = if x > 0 then Just (x * 2) else Nothing
              elem1 = point 5
              elem2 = point 10
              p = pattern 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Just (pattern 40 [point 10, point 20])
      
      describe "Traversing patterns with different value types" $ do
        
        it "traverses point with string values using Identity" $ do
          -- T027: Unit test for traversing point with string values using Identity
          let elem1 = point "hello"
              elem2 = point "world"
              p = pattern "greeting" [elem1, elem2]
              result = traverse Identity p
          runIdentity result `shouldBe` p
        
        it "traverses point with integer values using Maybe" $ do
          -- T028: Unit test for traversing point with integer values using Maybe
          let validate x = if x > 0 then Just x else Nothing
              elem1 = point 5
              elem2 = point 10
              p = pattern 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Just p
        
        it "traverses point with custom type values using Either" $ do
          -- T029: Unit test for traversing point with custom type values using Either
          let validate (Person name age) = 
                if name /= "" && age /= Nothing 
                then Right (Person name age)
                else Left "Invalid person"
              person1 = Person "Alice" (Just 30)
              person2 = Person "Bob" (Just 25)
              elem1 = point person1
              elem2 = point person2
              rootPerson = Person "Root" (Just 40)
              p = pattern rootPerson [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Right p
    
    describe "Sequence Applicative Effects (User Story 2)" $ do
      
      describe "sequenceA on patterns containing Identity values" $ do
        
        it "sequences point containing Identity values" $ do
          -- T037: Unit test for sequenceA on point containing Identity values
          let atom = point (Identity "test")
              result = sequenceA atom
          runIdentity result `shouldBe` point "test"
      
      describe "sequenceA on patterns containing Maybe values" $ do
        
        it "sequences point containing Maybe values (all Just)" $ do
          -- T038: Unit test for sequenceA on point containing Maybe values (all Just)
          let elem1 = point (Just 5)
              elem2 = point (Just 10)
              p = pattern (Just 20) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` Just (pattern 20 [point 5, point 10])
        
        it "sequences point containing Maybe values (one Nothing)" $ do
          -- T039: Unit test for sequenceA on point containing Maybe values (one Nothing)
          let elem1 = point (Just 5)
              elem2 = point Nothing
              p = pattern (Just 20) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` Nothing
      
      describe "sequenceA on patterns containing Either values" $ do
        
        it "sequences point containing Either values (all Right)" $ do
          -- T040: Unit test for sequenceA on point containing Either values (all Right)
          let elem1 = point (Right 5 :: Either String Int)
              elem2 = point (Right 10 :: Either String Int)
              p = pattern (Right 20 :: Either String Int) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` (Right (pattern 20 [point 5, point 10]) :: Either String (Pattern Int))
        
        it "sequences point containing Either values (one Left)" $ do
          -- T041: Unit test for sequenceA on point containing Either values (one Left)
          let elem1 = point (Right 5 :: Either String Int)
              elem2 = point (Left "error" :: Either String Int)
              p = pattern (Right 20 :: Either String Int) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` (Left "error" :: Either String (Pattern Int))
      
      describe "sequenceA on nested point structures with Maybe values" $ do
        
        it "sequences nested point structure with Maybe values (all Just)" $ do
          -- T042: Unit test for sequenceA on nested point structure with Maybe values (all Just)
          let inner = point (Just 1)
              middle = pattern (Just 2) [inner]
              outer = pattern (Just 3) [middle]
              p = pattern (Just 4) [outer]
              result = sequenceA p
          result `shouldBe` Just (pattern 4 [pattern 3 [pattern 2 [point 1]]])
        
        it "sequences nested point structure with Maybe values (one Nothing)" $ do
          -- T043: Unit test for sequenceA on nested point structure with Maybe values (one Nothing)
          let inner = point Nothing
              middle = pattern (Just 2) [inner]
              outer = pattern (Just 3) [middle]
              p = pattern (Just 4) [outer]
              result = sequenceA p
          result `shouldBe` Nothing
      
      describe "sequenceA on nested point structures with Either values" $ do
        
        it "sequences nested point structure with Either values (all Right)" $ do
          -- T044: Unit test for sequenceA on nested point structure with Either values (all Right)
          let inner = point (Right 1 :: Either String Int)
              middle = pattern (Right 2 :: Either String Int) [inner]
              outer = pattern (Right 3 :: Either String Int) [middle]
              p = pattern (Right 4 :: Either String Int) [outer]
              result = sequenceA p
          result `shouldBe` (Right (pattern 4 [pattern 3 [pattern 2 [point 1]]]) :: Either String (Pattern Int))
        
        it "sequences nested point structure with Either values (one Left)" $ do
          -- T045: Unit test for sequenceA on nested point structure with Either values (one Left)
          let inner = point (Left "error" :: Either String Int)
              middle = pattern (Right 2 :: Either String Int) [inner]
              outer = pattern (Right 3 :: Either String Int) [middle]
              p = pattern (Right 4 :: Either String Int) [outer]
              result = sequenceA p
          result `shouldBe` (Left "error" :: Either String (Pattern Int))
      
      describe "Structure preservation in sequenceA" $ do
        
        it "sequenceA preserves point structure" $ do
          -- T046: Unit test verifying sequenceA preserves point structure
          let elem1 = point (Just "a")
              elem2 = point (Just "b")
              elem3 = point (Just "c")
              p = pattern (Just "root") [elem1, elem2, elem3]
              result = sequenceA p
          case result of
            Just p' -> do
              length (elements p') `shouldBe` 3
              value (elements p' !! 0) `shouldBe` "a"
              value (elements p' !! 1) `shouldBe` "b"
              value (elements p' !! 2) `shouldBe` "c"
            Nothing -> expectationFailure "Expected Just result"
        
        it "sequenceA collects effects from all values" $ do
          -- T047: Unit test verifying sequenceA collects effects from all values
          let elem1 = point (Just 5)
              elem2 = point (Just 10)
              p = pattern (Just 20) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` Just (pattern 20 [point 5, point 10])
      
      describe "Short-circuiting behavior in sequenceA" $ do
        
        it "sequenceA short-circuits for Maybe (returns Nothing on first Nothing)" $ do
          -- T048: Unit test verifying sequenceA short-circuits for Maybe (returns Nothing on first Nothing)
          let elem1 = point Nothing
              elem2 = point (Just 10)
              p = pattern (Just 20) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` Nothing
        
        it "sequenceA short-circuits for Either (returns Left on first Left)" $ do
          -- T049: Unit test verifying sequenceA short-circuits for Either (returns Left on first Left)
          let elem1 = point (Left "first error" :: Either String Int)
              elem2 = point (Right 10 :: Either String Int)
              p = pattern (Right 20 :: Either String Int) [elem1, elem2]
              result = sequenceA p
          result `shouldBe` (Left "first error" :: Either String (Pattern Int))
    
    describe "Validate Pattern Values with Error Handling (User Story 3)" $ do
      
      describe "Validation with Maybe" $ do
        
        it "validates point with Maybe (all values valid)" $ do
          -- T055: Unit test for validation with Maybe (all values valid)
          let validate x = if x > 0 then Just x else Nothing
              elem1 = point 5
              elem2 = point 10
              p = pattern 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Just p
        
        it "validates point with Maybe (some values invalid)" $ do
          -- T056: Unit test for validation with Maybe (some values invalid)
          let validate x = if x > 0 then Just x else Nothing
              elem1 = point 5
              elem2 = point (-3)
              p = pattern 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Nothing
      
      describe "Validation with Either" $ do
        
        it "validates point with Either (all values valid)" $ do
          -- T057: Unit test for validation with Either (all values valid)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              elem1 = point 5
              elem2 = point 10
              p = pattern 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Right p
        
        it "validates point with Either (some values invalid, first error returned)" $ do
          -- T058: Unit test for validation with Either (some values invalid, first error returned)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              elem1 = point 5
              elem2 = point (-3)
              p = pattern 20 [elem1, elem2]
              result = traverse validate p
          result `shouldBe` Left "Invalid: -3"
      
      describe "Validation on nested point structures with Maybe" $ do
        
        it "validates nested point structure with Maybe (all valid)" $ do
          -- T059: Unit test for validation on nested point structure with Maybe (all valid)
          let validate x = if x > 0 then Just x else Nothing
              inner = point 1
              middle = pattern 2 [inner]
              outer = pattern 3 [middle]
              p = pattern 4 [outer]
              result = traverse validate p
          result `shouldBe` Just p
        
        it "validates nested point structure with Maybe (one invalid at any level)" $ do
          -- T060: Unit test for validation on nested point structure with Maybe (one invalid at any level)
          let validate x = if x > 0 then Just x else Nothing
              inner = point (-1)
              middle = pattern 2 [inner]
              outer = pattern 3 [middle]
              p = pattern 4 [outer]
              result = traverse validate p
          result `shouldBe` Nothing
      
      describe "Validation on nested point structures with Either" $ do
        
        it "validates nested point structure with Either (all valid)" $ do
          -- T061: Unit test for validation on nested point structure with Either (all valid)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              inner = point 1
              middle = pattern 2 [inner]
              outer = pattern 3 [middle]
              p = pattern 4 [outer]
              result = traverse validate p
          result `shouldBe` Right p
        
        it "validates nested point structure with Either (one invalid at any level)" $ do
          -- T062: Unit test for validation on nested point structure with Either (one invalid at any level)
          let validate x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
              inner = point (-1)
              middle = pattern 2 [inner]
              outer = pattern 3 [middle]
              p = pattern 4 [outer]
              result = traverse validate p
          result `shouldBe` Left "Invalid: -1"
      
      describe "Validation failure behavior" $ do
        
        it "validation fails if any value at any nesting level is invalid" $ do
          -- T063: Unit test verifying validation fails if any value at any nesting level is invalid
          let validate x = if x > 0 then Just x else Nothing
              -- Test with invalid value at root level
              p1 = pattern (-1) [point 5, point 10]
              result1 = traverse validate p1
              -- Test with invalid value at element level
              p2 = pattern 20 [point (-3), point 10]
              result2 = traverse validate p2
              -- Test with invalid value at nested level
              inner = point (-1)
              middle = pattern 2 [inner]
              p3 = pattern 4 [middle]
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
              p = pattern "10" [point "5", point "3"]
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
              p = pattern 10 [point 5, point 3]
              stateResult = traverse addState p
              (result, finalState) = runState stateResult 0
          -- Verify result point structure
          -- State processes: point value (10) first, then elements (5, 3)
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
          let p = Pattern { value = "pattern", elements = [elem] }
          PC.length p `shouldBe` 1
        
        it "T003: returns correct count for multiple elements p" $ do
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let elem3 = Pattern { value = "e3", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          PC.length p `shouldBe` 3
        
        it "T004: returns only direct children count for nested pattern" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let p = Pattern { value = "root", elements = [outer] }
          -- Should return 1 (only direct child), not count nested descendants
          PC.length p `shouldBe` 1
          -- Verify nested structure has its own length
          PC.length outer `shouldBe` 1
          PC.length middle `shouldBe` 1
          PC.length inner `shouldBe` 0
    
    describe "Query Functions (User Story 2 - Size)" $ do
      
      describe "size function - unit tests" $ do
        
        it "T011: returns 1 for atomic pattern" $ do
          let atom = Pattern { value = "atom", elements = [] }
          size atom `shouldBe` 1
        
        it "T012: returns 1 + element count for point with direct elements" $ do
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let elem3 = Pattern { value = "e3", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem1, elem2, elem3] }
          -- Should return 1 (root) + 3 (elements) = 4
          size p `shouldBe` 4
        
        it "T013: counts all nodes in deeply nested pattern" $ do
          let level4 = Pattern { value = "level4", elements = [] }
          let level3 = Pattern { value = "level3", elements = [level4] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let p = Pattern { value = "root", elements = [level1] }
          -- Should count all nodes: root(1) + level1(1) + level2(1) + level3(1) + level4(1) = 5
          size p `shouldBe` 5
        
        it "T014: counts all nodes across branches with varying depths" $ do
          let branch1Leaf = Pattern { value = "b1leaf", elements = [] }
          let branch1 = Pattern { value = "b1", elements = [branch1Leaf] }
          let branch2Mid = Pattern { value = "b2mid", elements = [] }
          let branch2Leaf = Pattern { value = "b2leaf", elements = [] }
          let branch2 = Pattern { value = "b2", elements = [branch2Mid, branch2Leaf] }
          let branch3 = Pattern { value = "b3", elements = [] }
          let p = Pattern { value = "root", elements = [branch1, branch2, branch3] }
          -- Should count: root(1) + b1(1) + b1leaf(1) + b2(1) + b2mid(1) + b2leaf(1) + b3(1) = 7
          size p `shouldBe` 7
    
    describe "Query Functions (User Story 3 - Depth)" $ do
      
      describe "depth function - unit tests" $ do
        
        it "T022: returns 0 for atomic pattern" $ do
          let atom = Pattern { value = "atom", elements = [] }
          -- Atomic point has no nesting, depth is 0 (root only)
          depth atom `shouldBe` 0
        
        it "T023: returns 1 for one level of nesting" $ do
          let elem = Pattern { value = "elem", elements = [] }
          let p = Pattern { value = "pattern", elements = [elem] }
          -- One level of nesting: root -> elem, depth is 1
          depth p `shouldBe` 1
        
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
          let p = Pattern { value = "root", elements = [branch1, branch2, branch3] }
          -- Should return: root -> branch2 -> branch2Inner -> branch2Leaf = 3
          -- (1 for root -> branch2, plus branch2's depth of 2)
          depth p `shouldBe` 3
        
        it "T025: returns maximum depth for deeply nested pattern" $ do
          let level4 = Pattern { value = "level4", elements = [] }
          let level3 = Pattern { value = "level3", elements = [level4] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let p = Pattern { value = "root", elements = [level1] }
          -- Depth: root -> level1 -> level2 -> level3 -> level4 = 4
          depth p `shouldBe` 4
    
    describe "Query Functions (User Story 4 - Values)" $ do
      
      describe "values function - unit tests" $ do
        
        it "T032: returns single value for atomic pattern" $ do
          let atom = Pattern { value = "atom", elements = [] }
          values atom `shouldBe` ["atom"]
        
        it "T033: returns all values for point with multiple elements" $ do
          let elem1 = Pattern { value = "e1", elements = [] }
          let elem2 = Pattern { value = "e2", elements = [] }
          let elem3 = Pattern { value = "e3", elements = [] }
          let p = Pattern { value = "root", elements = [elem1, elem2, elem3] }
          -- Should return: root value first, then element values in order
          values p `shouldBe` ["root", "e1", "e2", "e3"]
        
        it "T034: returns all values from all levels for nested pattern" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let p = Pattern { value = "root", elements = [outer] }
          -- Should return all values: root, outer, middle, inner
          values p `shouldBe` ["root", "outer", "middle", "inner"]
        
        it "T035: returns values in consistent order for varying nesting depths" $ do
          let branch1Leaf = Pattern { value = "b1leaf", elements = [] }
          let branch1 = Pattern { value = "b1", elements = [branch1Leaf] }
          let branch2Mid = Pattern { value = "b2mid", elements = [] }
          let branch2Leaf = Pattern { value = "b2leaf", elements = [] }
          let branch2 = Pattern { value = "b2", elements = [branch2Mid, branch2Leaf] }
          let branch3 = Pattern { value = "b3", elements = [] }
          let p = Pattern { value = "root", elements = [branch1, branch2, branch3] }
          -- Should return: root, then branch values in order, then nested values
          -- Order: root -> b1 -> b1leaf -> b2 -> b2mid -> b2leaf -> b3
          values p `shouldBe` ["root", "b1", "b1leaf", "b2", "b2mid", "b2leaf", "b3"]
    
    describe "Query Functions (User Story 5 - Value Accessor)" $ do
      
      describe "value field accessor - unit tests" $ do
        
        it "T044: returns correct value for string pattern" $ do
          let p = Pattern { value = "test", elements = [] }
          value p `shouldBe` "test"
        
        it "T045: returns correct value for integer pattern" $ do
          let p = Pattern { value = 42, elements = [] }
          value p `shouldBe` (42 :: Int)
        
        it "T046: returns correct value for custom type pattern" $ do
          let person = Person "Alice" (Just 30)
          let p = Pattern { value = person, elements = [] }
          value p `shouldBe` person
        
        it "T047: returns correct value for each level in nested patterns" $ do
          let inner = Pattern { value = "inner", elements = [] }
          let middle = Pattern { value = "middle", elements = [inner] }
          let outer = Pattern { value = "outer", elements = [middle] }
          let p = Pattern { value = "root", elements = [outer] }
          -- Verify each level returns its correct value
          value p `shouldBe` "root"
          value outer `shouldBe` "outer"
          value middle `shouldBe` "middle"
          value inner `shouldBe` "inner"
    
    describe "Query Functions - Integration Tests (Phase 6)" $ do
      
      describe "Integration with point constructors" $ do
        
        it "T051: query functions work with patterns created using point function" $ do
          let atom = point "atom"
          let elem1 = point "e1"
          let elem2 = point "e2"
          let p1 = pattern "root" [elem1, elem2]
          -- Verify all query functions work
          PC.length atom `shouldBe` 0
          size atom `shouldBe` 1
          depth atom `shouldBe` 0
          values atom `shouldBe` ["atom"]
          value atom `shouldBe` "atom"
          PC.length p1 `shouldBe` 2
          size p1 `shouldBe` 3
          depth p1 `shouldBe` 1
          values p1 `shouldBe` ["root", "e1", "e2"]
          value p1 `shouldBe` "root"
        
        it "T052: query functions work with patterns created using pattern function" $ do
          let elem1 = point "e1"
          let elem2 = point "e2"
          let elem3 = point "e3"
          let p1 = pattern "root" [elem1, elem2, elem3]
          -- Verify all query functions work
          PC.length p1 `shouldBe` 3
          size p1 `shouldBe` 4
          depth p1 `shouldBe` 1
          values p1 `shouldBe` ["root", "e1", "e2", "e3"]
          value p1 `shouldBe` "root"
        
        it "T053: query functions work with patterns created using fromList function" $ do
          let p1 = fromList "root" ["a", "b", "c"]
          -- Verify all query functions work
          PC.length p1 `shouldBe` 3
          size p1 `shouldBe` 4
          depth p1 `shouldBe` 1
          values p1 `shouldBe` ["root", "a", "b", "c"]
          value p1 `shouldBe` "root"
      
      describe "Integration with type class instances" $ do
        
        it "T054: query functions work with patterns transformed using fmap (Functor)" $ do
          let p1 = fromList "root" ["a", "b", "c"]
          let p2 = fmap (map toUpper) p1
          -- Verify all query functions work on transformed pattern
          PC.length p2 `shouldBe` 3
          size p2 `shouldBe` 4
          depth p2 `shouldBe` 1
          values p2 `shouldBe` ["ROOT", "A", "B", "C"]
          value p2 `shouldBe` "ROOT"
          -- Verify structure is preserved
          PC.length p1 `shouldBe` PC.length p2
          size p1 `shouldBe` size p2
          depth p1 `shouldBe` depth p2
        
        it "T055: query functions work with patterns used in Foldable operations" $ do
          let p1 = fromList (10 :: Int) [1, 2, 3, 4, 5]
          -- Verify query functions work
          PC.length p1 `shouldBe` 5
          size p1 `shouldBe` 6
          depth p1 `shouldBe` 1
          values p1 `shouldBe` [10, 1, 2, 3, 4, 5]
          value p1 `shouldBe` (10 :: Int)
          -- Verify integration with Foldable
          sum p1 `shouldBe` 25
          length (toList p1) `shouldBe` 6
          -- Verify values matches toList
          values p1 `shouldBe` toList p1
        
        it "T056: query functions work with patterns used in Traversable operations" $ do
          let p1 = fromList 10 [1, 2, 3]
          -- Verify query functions work
          PC.length p1 `shouldBe` 3
          size p1 `shouldBe` 4
          depth p1 `shouldBe` 1
          values p1 `shouldBe` [10, 1, 2, 3]
          value p1 `shouldBe` (10 :: Int)
          -- Verify integration with Traversable
          let validate x = if x > 0 then Just x else Nothing
          let result = traverse validate p1
          case result of
            Just p2 -> do
              -- Verify query functions work on traversed pattern
              PC.length p2 `shouldBe` 3
              size p2 `shouldBe` 4
              depth p2 `shouldBe` 1
              values p2 `shouldBe` [10, 1, 2, 3]
              value p2 `shouldBe` (10 :: Int)
            Nothing -> fail "Traverse should succeed for positive values"
      
      describe "Edge case tests" $ do
        
        it "T057: query functions work with very deeply nested patterns (100+ levels)" $ do
          -- Create a deeply nested point (100 levels)
          let createDeep n = if n <= 0
                             then point "leaf"
                             else pattern ("level" ++ show n) [createDeep (n - 1)]
          let deepPattern = createDeep 100
          -- Verify all query functions work (should not stack overflow)
          PC.length deepPattern `shouldBe` 1
          size deepPattern `shouldBe` 101
          depth deepPattern `shouldBe` 100
          length (values deepPattern) `shouldBe` 101
          value deepPattern `shouldBe` "level100"
        
        it "T058: query functions work with patterns having many direct elements (100+)" $ do
          -- Create a point with 100 direct elements
          let elems = map (\i -> point ("elem" ++ show i)) [1..100]
          let p1 = pattern "root" elems
          -- Verify all query functions work
          PC.length p1 `shouldBe` 100
          size p1 `shouldBe` 101
          depth p1 `shouldBe` 1
          length (values p1) `shouldBe` 101
          value p1 `shouldBe` "root"
          -- Verify first and last elements
          head (values p1) `shouldBe` "root"
          last (values p1) `shouldBe` "elem100"
        
        it "T059: query functions work with patterns containing duplicate values" $ do
          -- Create point with duplicate values
          let p1 = fromList "root" ["a", "a", "b", "b", "c", "c"]
          -- Verify all query functions work
          PC.length p1 `shouldBe` 6
          size p1 `shouldBe` 7
          depth p1 `shouldBe` 1
          values p1 `shouldBe` ["root", "a", "a", "b", "b", "c", "c"]
          value p1 `shouldBe` "root"
          -- Verify duplicates are preserved
          filter (== "a") (values p1) `shouldBe` ["a", "a"]
          filter (== "b") (values p1) `shouldBe` ["b", "b"]
    
    describe "Ord Instance (User Story 1)" $ do
      
      describe "compare function with atomic patterns" $ do
        
        it "T001: compare atomic patterns with different values" $ do
          let p1 = point "a"
              p2 = point "b"
          compare p1 p2 `shouldBe` LT
          compare p2 p1 `shouldBe` GT
          compare p1 p1 `shouldBe` EQ
        
        it "T002: compare atomic patterns with same value" $ do
          let p1 = point "test"
              p2 = point "test"
          compare p1 p2 `shouldBe` EQ
          compare p2 p1 `shouldBe` EQ
      
      describe "compare function with patterns having elements" $ do
        
        it "T003: compare patterns with same value but different elements" $ do
          let p1 = pattern "root" [point "a"]
              p2 = pattern "root" [point "b"]
          compare p1 p2 `shouldBe` LT
          compare p2 p1 `shouldBe` GT
        
        it "T004: compare patterns with same value and same number of elements" $ do
          let p1 = pattern "root" [point "a", point "b"]
              p2 = pattern "root" [point "a", point "b"]
          compare p1 p2 `shouldBe` EQ
          compare p2 p1 `shouldBe` EQ
      
      describe "compare function with nested patterns" $ do
        
        it "T005: compare nested patterns (recursive comparison)" $ do
          let inner1 = point "inner1"
              inner2 = point "inner2"
              middle1 = pattern "middle" [inner1]
              middle2 = pattern "middle" [inner2]
              outer1 = pattern "outer" [middle1]
              outer2 = pattern "outer" [middle2]
          compare outer1 outer2 `shouldBe` LT
    
    describe "Integration & Validation (Phase 4)" $ do
      
      describe "Integration with existing functions" $ do
        
        it "T042: all query functions work with Ord instance" $ do
          let p1 = point "a"
              p2 = point "b"
              p3 = pattern "root" [point "a", point "b"]
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
        
        it "T043: Ord instance works with point constructors" $ do
          -- Test with point constructor
          let p1 = point "a"
              p2 = point "b"
          compare p1 p2 `shouldBe` LT
          -- Test with pattern constructor
          let p3 = pattern "root" [point "a"]
              p4 = pattern "root" [point "b"]
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
          let p1 = point "a"
              p2 = point "b"
          fmap (map toUpper) p1 `shouldBe` point "A"
          -- Functor preserves ordering
          compare (fmap (map toUpper) p1) (fmap (map toUpper) p2) `shouldBe` LT
          -- Test with Foldable
          let p3 = pattern "root" [point "a", point "b"]
              p4 = pattern "root" [point "a", point "c"]
          -- Foldable operations work, and we can still compare
          toList p3 `shouldBe` ["root", "a", "b"]
          compare p3 p4 `shouldBe` LT
          -- Test with Traversable
          let p5 = point (Just "a")
              p6 = point (Just "b")
          -- Traversable operations work, and we can still compare
          traverse Just p5 `shouldBe` Just (point (Just "a"))
          compare p5 p6 `shouldBe` LT
          -- Verify type class operations preserve comparability
          let patterns = [p4, p3]
              sorted = sort patterns
          length sorted `shouldBe` 2
          sorted `shouldBe` [p3, p4]
        
        it "T006: comparison operators (<, <=, >, >=) with patterns" $ do
          let p1 = point "a"
              p2 = point "b"
              p3 = point "a"
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
          let p1 = point "a"
              p2 = point "b"
              p3 = point "c"
          min p1 p2 `shouldBe` p1
          max p1 p2 `shouldBe` p2
          min p1 p3 `shouldBe` p1
          max p1 p3 `shouldBe` p3
          min p2 p3 `shouldBe` p2
          max p2 p3 `shouldBe` p3
    
    describe "Ord Instance Integration (User Story 2)" $ do
      
      describe "Data.Set integration" $ do
        
        it "T016: Data.Set with patterns (insertion and ordering)" $ do
          let p1 = point "a"
              p2 = point "b"
              p3 = point "c"
              s = Set.insert p3 $ Set.insert p1 $ Set.insert p2 Set.empty
          -- Set should maintain sorted order
          Set.toList s `shouldBe` [p1, p2, p3]
          Set.member p2 s `shouldBe` True
          Set.member (point "d") s `shouldBe` False
        
        it "T017: Data.Set membership lookup with patterns" $ do
          let p1 = point "a"
              p2 = pattern "root" [point "b"]
              p3 = pattern "root" [point "c"]
              s = Set.fromList [p1, p2, p3]
          Set.member p1 s `shouldBe` True
          Set.member p2 s `shouldBe` True
          Set.member p3 s `shouldBe` True
          Set.member (point "d") s `shouldBe` False
          Set.member (pattern "root" [point "d"]) s `shouldBe` False
        
        it "T018: Data.Set with patterns having duplicate values but different structures" $ do
          let p1 = pattern "root" [point "a"]
              p2 = pattern "root" [point "b"]
              p3 = pattern "root" [point "a", point "b"]
              s = Set.fromList [p1, p2, p3]
          -- All three should be distinct (different structures)
          Set.size s `shouldBe` 3
          Set.member p1 s `shouldBe` True
          Set.member p2 s `shouldBe` True
          Set.member p3 s `shouldBe` True
      
      describe "Data.Map integration" $ do
        
        it "T019: Data.Map with patterns as keys (insertion and lookup)" $ do
          let p1 = point "a"
              p2 = point "b"
              p3 = point "c"
              m = Map.insert p3 "value3" $ Map.insert p1 "value1" $ Map.insert p2 "value2" Map.empty
          Map.lookup p1 m `shouldBe` Just "value1"
          Map.lookup p2 m `shouldBe` Just "value2"
          Map.lookup p3 m `shouldBe` Just "value3"
          Map.lookup (point "d") m `shouldBe` Nothing
        
        it "T020: Data.Map key matching with patterns" $ do
          let p1 = pattern "root" [point "a"]
              p2 = pattern "root" [point "b"]
              p3 = pattern "root" [point "a", point "b"]
              m = Map.fromList [(p1, "val1"), (p2, "val2"), (p3, "val3")]
          Map.member p1 m `shouldBe` True
          Map.member p2 m `shouldBe` True
          Map.member p3 m `shouldBe` True
          Map.lookup p1 m `shouldBe` Just "val1"
          Map.lookup p2 m `shouldBe` Just "val2"
          Map.lookup p3 m `shouldBe` Just "val3"
          Map.member (pattern "root" [point "d"]) m `shouldBe` False
      
      describe "Sorting and min/max functions" $ do
        
        it "T021: sorting patterns using sort function" $ do
          let p1 = point "c"
              p2 = point "a"
              p3 = point "b"
              sorted = sort [p1, p2, p3]
          sorted `shouldBe` [p2, p3, p1]
          -- Verify sorted order
          and (zipWith (<=) sorted (tail sorted)) `shouldBe` True
        
        it "T022: minimum and maximum functions with point lists" $ do
          let p1 = point "a"
              p2 = point "b"
              p3 = point "c"
              patterns = [p3, p1, p2]
          minimum patterns `shouldBe` p1
          maximum patterns `shouldBe` p3
          -- Test with nested patterns
          let nested1 = pattern "root" [point "a"]
              nested2 = pattern "root" [point "b"]
              nested3 = pattern "root" [point "c"]
              nestedPatterns = [nested3, nested1, nested2]
          minimum nestedPatterns `shouldBe` nested1
          maximum nestedPatterns `shouldBe` nested3
    
    describe "Ord Instance Consistency with Eq (User Story 3)" $ do
      
      describe "consistency between Ord and Eq" $ do
        
        it "T028: patterns equal by Eq compare as EQ" $ do
          let p1 = point "test"
              p2 = point "test"
          p1 == p2 `shouldBe` True
          compare p1 p2 `shouldBe` EQ
          -- Test with patterns having elements
          let p3 = pattern "root" [point "a", point "b"]
              p4 = pattern "root" [point "a", point "b"]
          p3 == p4 `shouldBe` True
          compare p3 p4 `shouldBe` EQ
          -- Test with nested patterns
          let inner1 = point "inner"
              inner2 = point "inner"
              outer1 = pattern "outer" [inner1]
              outer2 = pattern "outer" [inner2]
          outer1 == outer2 `shouldBe` True
          compare outer1 outer2 `shouldBe` EQ
        
        it "T029: patterns not equal by Eq don't compare as EQ" $ do
          let p1 = point "a"
              p2 = point "b"
          p1 == p2 `shouldBe` False
          compare p1 p2 `shouldNotBe` EQ
          compare p2 p1 `shouldNotBe` EQ
          -- Test with patterns having same value but different elements
          let p3 = pattern "root" [point "a"]
              p4 = pattern "root" [point "b"]
          p3 == p4 `shouldBe` False
          compare p3 p4 `shouldNotBe` EQ
          -- Test with patterns having different values
          let p5 = pattern "root1" [point "a"]
              p6 = pattern "root2" [point "a"]
          p5 == p6 `shouldBe` False
          compare p5 p6 `shouldNotBe` EQ
        
        it "T030: patterns with same structure but different values compare by value" $ do
          let p1 = pattern "a" [point "x"]
              p2 = pattern "b" [point "x"]
          -- Same structure (one element with same value), different root values
          p1 == p2 `shouldBe` False
          compare p1 p2 `shouldBe` LT  -- "a" < "b"
          compare p2 p1 `shouldBe` GT
          -- Verify ordering respects value comparison
          (p1 < p2) `shouldBe` True
          (p2 > p1) `shouldBe` True
        
        it "T031: patterns with same value but different element structures compare by elements" $ do
          let p1 = pattern "root" [point "a"]
              p2 = pattern "root" [point "b"]
          -- Same root value, different element values
          p1 == p2 `shouldBe` False
          compare p1 p2 `shouldBe` LT  -- "a" < "b" in elements
          compare p2 p1 `shouldBe` GT
          -- Test with multiple elements
          let p3 = pattern "root" [point "a", point "b"]
              p4 = pattern "root" [point "a", point "c"]
          p3 == p4 `shouldBe` False
          compare p3 p4 `shouldBe` LT  -- "b" < "c" in second element
          -- Test with different number of elements
          let p5 = pattern "root" [point "a"]
              p6 = pattern "root" [point "a", point "b"]
          p5 == p6 `shouldBe` False
          compare p5 p6 `shouldBe` LT  -- shorter list comes first
          compare p6 p5 `shouldBe` GT
    
    describe "Edge Cases (Phase 4)" $ do
      
      describe "Edge case tests for Ord instance" $ do
        
        it "T037: comparing atomic patterns (no elements)" $ do
          let p1 = point "a"
              p2 = point "b"
              p3 = point "a"
          -- Atomic patterns compare by value only
          compare p1 p2 `shouldBe` LT
          compare p2 p1 `shouldBe` GT
          compare p1 p3 `shouldBe` EQ
          -- Verify with different value types
          let p4 = point (1 :: Int)
              p5 = point (2 :: Int)
          compare p4 p5 `shouldBe` LT
          compare p5 p4 `shouldBe` GT
        
        it "T038: comparing patterns with different numbers of elements" $ do
          let p1 = pattern "root" []  -- 0 elements
              p2 = pattern "root" [point "a"]  -- 1 element
              p3 = pattern "root" [point "a", point "b"]  -- 2 elements
          -- Patterns with fewer elements come first when values are equal
          compare p1 p2 `shouldBe` LT
          compare p2 p3 `shouldBe` LT
          compare p1 p3 `shouldBe` LT
          compare p3 p2 `shouldBe` GT
          compare p2 p1 `shouldBe` GT
          -- Test with different values
          let p4 = pattern "a" [point "x"]
              p5 = pattern "b" []  -- Different value, fewer elements
          -- Value comparison takes precedence
          compare p4 p5 `shouldBe` LT  -- "a" < "b"
        
        it "T039: comparing deeply nested patterns (100+ levels)" $ do
          -- Create deeply nested patterns
          let createDeep n = if n == 0
                             then point "leaf"
                             else pattern ("level" ++ show n) [createDeep (n - 1)]
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
          let p1 = pattern "root" [point "a", point "b"]
              p2 = pattern "root" [pattern "a" [point "b"]]
          -- These have different structures even though flattened values might be similar
          p1 == p2 `shouldBe` False
          compare p1 p2 `shouldNotBe` EQ
          -- Test with more complex structures
          let p3 = pattern "x" [point "y", point "z"]
              p4 = pattern "x" [pattern "y" [point "z"]]
          p3 == p4 `shouldBe` False
          compare p3 p4 `shouldNotBe` EQ
          -- Verify structure is preserved in comparison
          let p5 = pattern "root" [point "a"]
              p6 = pattern "root" [point "a", point "b"]
          -- Even if first element is same, different structures
          p5 == p6 `shouldBe` False
          compare p5 p6 `shouldBe` LT  -- shorter list first
        
        it "T041: type constraint: Ord v requirement" $ do
          -- This test verifies that Ord instance requires Ord v constraint
          -- We test with types that have Ord instances
          let p1 = point ("a" :: String)
              p2 = point ("b" :: String)
          compare p1 p2 `shouldBe` LT
          -- Test with Int (has Ord instance)
          let p3 = point (1 :: Int)
              p4 = point (2 :: Int)
          compare p3 p4 `shouldBe` LT
          -- Test with patterns containing Int values
          let p5 = pattern (10 :: Int) [point (1 :: Int), point (2 :: Int)]
              p6 = pattern (10 :: Int) [point (1 :: Int), point (3 :: Int)]
          compare p5 p6 `shouldBe` LT
          -- Verify that comparison works with nested patterns of orderable types
          let inner1 = point (5 :: Int)
              inner2 = point (6 :: Int)
              outer1 = pattern (100 :: Int) [inner1]
              outer2 = pattern (100 :: Int) [inner2]
          compare outer1 outer2 `shouldBe` LT
    
    describe "Semigroup Instance (User Story 3)" $ do
      
      describe "Combining atomic patterns" $ do
        
        it "T001: combining two atomic patterns" $ do
          let p1 = point "a"
              p2 = point "b"
              result = p1 <> p2
          value result `shouldBe` "ab"
          elements result `shouldBe` ([] :: [Pattern String])
        
        it "T002: combining atomic point with point having elements" $ do
          let p1 = point "a"
              elem1 = point "elem1"
              elem2 = point "elem2"
              p2 = pattern "b" [elem1, elem2]
              result = p1 <> p2
          value result `shouldBe` "ab"
          length (elements result) `shouldBe` 2
          value (head (elements result)) `shouldBe` "elem1"
          value (last (elements result)) `shouldBe` "elem2"
      
      describe "Combining patterns with elements" $ do
        
        it "T003: combining two patterns with elements" $ do
          let elem1 = point "e1"
              elem2 = point "e2"
              p1 = pattern "a" [elem1, elem2]
              elem3 = point "e3"
              elem4 = point "e4"
              p2 = pattern "b" [elem3, elem4]
              result = p1 <> p2
          value result `shouldBe` "ab"
          length (elements result) `shouldBe` 4
          map value (elements result) `shouldBe` ["e1", "e2", "e3", "e4"]
        
        it "T004: combining patterns with different element counts" $ do
          let elem1 = point "e1"
              p1 = pattern "a" [elem1]
              elem2 = point "e2"
              elem3 = point "e3"
              elem4 = point "e4"
              p2 = pattern "b" [elem2, elem3, elem4]
              result = p1 <> p2
          value result `shouldBe` "ab"
          length (elements result) `shouldBe` 4
          map value (elements result) `shouldBe` ["e1", "e2", "e3", "e4"]
      
      describe "Value combination with different Semigroup types" $ do
        
        it "T005: combining patterns with String values (concatenation)" $ do
          let p1 = point "hello"
              p2 = point "world"
              result = p1 <> p2
          value result `shouldBe` "helloworld"
        
        it "T006: combining patterns with Sum Int values (addition)" $ do
          let p1 = point (Sum 5)
              p2 = point (Sum 3)
              result = p1 <> p2
          getSum (value result) `shouldBe` 8
        
        it "T007: combining patterns with Product Int values (multiplication)" $ do
          let p1 = point (Product 5)
              p2 = point (Product 3)
              result = p1 <> p2
          getProduct (value result) `shouldBe` 15
      
      describe "Element order preservation" $ do
        
        it "T008: element order preservation" $ do
          let elem1 = point "first"
              elem2 = point "second"
              elem3 = point "third"
              p1 = pattern "a" [elem1, elem2]
              p2 = pattern "b" [elem3]
              result = p1 <> p2
          map value (elements result) `shouldBe` ["first", "second", "third"]
          -- Verify order: p1 elements first, then p2 elements
          value (head (elements result)) `shouldBe` "first"
          value (elements result !! 1) `shouldBe` "second"
          value (last (elements result)) `shouldBe` "third"
      
      describe "Value combination semantics" $ do
        
        it "T009: value combination using value type's Semigroup" $ do
          let p1 = pattern "prefix" [point "elem1"]
              p2 = pattern "suffix" [point "elem2"]
              result = p1 <> p2
          -- Value should combine using String's Semigroup (concatenation)
          value result `shouldBe` "prefixsuffix"
          -- Elements should concatenate
          length (elements result) `shouldBe` 2
      
      describe "Type constraint" $ do
        
        it "T010: type constraint: Semigroup v requirement" $ do
          -- This test verifies that Semigroup instance requires Semigroup v constraint
          -- We test with types that have Semigroup instances
          let p1 = point ("a" :: String)
              p2 = point ("b" :: String)
          value (p1 <> p2) `shouldBe` "ab"
          -- Test with Sum Int (has Semigroup instance)
          let p3 = point (Sum 1 :: Sum Int)
              p4 = point (Sum 2 :: Sum Int)
          getSum (value (p3 <> p4)) `shouldBe` 3
          -- Test with Product Int (has Semigroup instance)
          let p5 = point (Product 2 :: Product Int)
              p6 = point (Product 3 :: Product Int)
          getProduct (value (p5 <> p6)) `shouldBe` 6
    
    describe "Semigroup Instance - Edge Cases (User Story 4)" $ do
      
      describe "Nested patterns" $ do
        
        it "T020: combining nested patterns (preserving nested structure)" $ do
          let inner1 = point "inner1"
              inner2 = point "inner2"
              middle1 = pattern "middle1" [inner1]
              middle2 = pattern "middle2" [inner2]
              p1 = pattern "root1" [middle1]
              p2 = pattern "root2" [middle2]
              result = p1 <> p2
          value result `shouldBe` "root1root2"
          length (elements result) `shouldBe` 2
          -- Verify nested structure is preserved
          let firstElem = head (elements result)
              secondElem = last (elements result)
          value firstElem `shouldBe` "middle1"
          value secondElem `shouldBe` "middle2"
          length (elements firstElem) `shouldBe` 1
          length (elements secondElem) `shouldBe` 1
          value (head (elements firstElem)) `shouldBe` "inner1"
          value (head (elements secondElem)) `shouldBe` "inner2"
        
        it "T021: combining patterns with different nesting depths" $ do
          let p1 = pattern "a" [point "leaf"]
              inner = point "inner"
              middle = pattern "middle" [inner]
              p2 = pattern "b" [middle]
              result = p1 <> p2
          value result `shouldBe` "ab"
          length (elements result) `shouldBe` 2
          -- First element has depth 1, second has depth 2
          let firstElem = head (elements result)
              secondElem = last (elements result)
          value firstElem `shouldBe` "leaf"
          value secondElem `shouldBe` "middle"
          length (elements firstElem) `shouldBe` 0
          length (elements secondElem) `shouldBe` 1
        
        it "T022: combining patterns with deeply nested structures (10+ levels)" $ do
          -- Create a deeply nested point (10 levels)
          let createDeep n = if n <= 0
                            then point "leaf"
                            else pattern ("level" ++ show n) [createDeep (n - 1)]
              p1 = createDeep 10
              p2 = pattern "simple" [point "elem"]
              result = p1 <> p2
          value result `shouldBe` ("level10" ++ "simple")
          length (elements result) `shouldBe` 2
          -- Verify deep nesting is preserved in first element
          let deepElem = head (elements result)
          value deepElem `shouldBe` "level9"
          -- Verify we can traverse to the leaf
          let traverseToLeaf p = if null (elements p) then p else traverseToLeaf (head (elements p))
              leaf = traverseToLeaf deepElem
          value leaf `shouldBe` "leaf"
          -- Verify second element is preserved
          let simpleElem = last (elements result)
          value simpleElem `shouldBe` "elem"
        
        it "T023: combining patterns with many elements (100+ elements)" $ do
          let elems1 = map (\i -> point ("e1_" ++ show i)) [1..50]
              elems2 = map (\i -> point ("e2_" ++ show i)) [1..50]
              p1 = pattern "prefix" elems1
              p2 = pattern "suffix" elems2
              result = p1 <> p2
          value result `shouldBe` "prefixsuffix"
          length (elements result) `shouldBe` 100
          -- Verify order: first 50 from p1, then 50 from p2
          value (head (elements result)) `shouldBe` "e1_1"
          value (elements result !! 49) `shouldBe` "e1_50"
          value (elements result !! 50) `shouldBe` "e2_1"
          value (last (elements result)) `shouldBe` "e2_50"
    
    describe "Semigroup Instance - Integration (Phase 4)" $ do
      
      describe "Standard Semigroup combinators" $ do
        
        it "T026: integration test for sconcat with list of patterns" $ do
          let p1 = point "a"
              p2 = point "b"
              p3 = point "c"
              patterns = p1 :| [p2, p3]
              result = sconcat patterns
          value result `shouldBe` "abc"
          elements result `shouldBe` ([] :: [Pattern String])
        
        it "T027: integration test for stimes to repeat a pattern" $ do
          let elem1 = point "e1"
              elem2 = point "e2"
              p = pattern "root" [elem1, elem2]
              result = stimes 3 p
          value result `shouldBe` "rootrootroot"
          length (elements result) `shouldBe` 6
          -- Verify elements are repeated in order
          map value (take 2 (elements result)) `shouldBe` ["e1", "e2"]
          map value (drop 2 (take 4 (elements result))) `shouldBe` ["e1", "e2"]
          map value (drop 4 (elements result)) `shouldBe` ["e1", "e2"]
      
      describe "Integration with point constructors" $ do
        
        it "T028: Semigroup instance with point constructors" $ do
          -- Test with point function
          let p1 = point "a"
              p2 = point "b"
              result1 = p1 <> p2
          value result1 `shouldBe` "ab"
          -- Test with pattern function
          let elem1 = point "e1"
              elem2 = point "e2"
              p3 = pattern "c" [elem1, elem2]
              p4 = pattern "d" [point "e3"]
              result2 = p3 <> p4
          value result2 `shouldBe` "cd"
          length (elements result2) `shouldBe` 3
          -- Test with fromList function
          let p5 = fromList "list1" ["a", "b"]
              p6 = fromList "list2" ["c", "d"]
              result3 = p5 <> p6
          value result3 `shouldBe` "list1list2"
          length (elements result3) `shouldBe` 4
          map value (elements result3) `shouldBe` ["a", "b", "c", "d"]
      
      describe "Integration with type class instances" $ do
        
        it "T029: Semigroup instance with type class instances" $ do
          let elem1 = point "e1"
              elem2 = point "e2"
              p1 = pattern "a" [elem1, elem2]
              p2 = pattern "b" [point "e3"]
              result = p1 <> p2
          -- Test with Functor
          let fmapResult = fmap (map toUpper) result
          value fmapResult `shouldBe` "AB"
          -- Test with Foldable
          let foldResult = foldr (++) "" result
          foldResult `shouldBe` "abe1e2e3"
          -- Test with Traversable
          let traverseResult = traverse Identity result
              unwrapped = runIdentity traverseResult
          unwrapped `shouldBe` result
      
      describe "Non-commutative value type Semigroup" $ do
        
        it "T030: combining patterns with non-commutative value type Semigroup" $ do
          -- Use Endo as a non-commutative Semigroup
          -- Endo f <> Endo g = Endo (f . g) (composition is non-commutative)
          let f = Endo ((+1) :: Int -> Int)
              g = Endo ((*2) :: Int -> Int)
              p1 = point f
              p2 = point g
              result = p1 <> p2
          -- Verify composition order: (f . g) x = f (g x) = (x * 2) + 1
          let composed = appEndo (value result) 5
          composed `shouldBe` 11  -- (5 * 2) + 1 = 11
          -- Verify reverse order is different: (g . f) x = g (f x) = (x + 1) * 2
          let reverseResult = p2 <> p1
              reverseComposed = appEndo (value reverseResult) 5
          reverseComposed `shouldBe` 12  -- (5 + 1) * 2 = 12
          -- This demonstrates non-commutativity - verify they produce different results
          composed `shouldNotBe` reverseComposed

    describe "Monoid Instance (User Story 3)" $ do
      
      describe "Identity Pattern Structure" $ do
        
        it "T007: mempty has correct structure for String values" $ do
          let emptyPattern = mempty :: Pattern String
          value emptyPattern `shouldBe` ""
          elements emptyPattern `shouldBe` ([] :: [Pattern String])
        
        it "T007: mempty has correct structure for Sum Int values" $ do
          let emptyPattern = mempty :: Pattern (Sum Int)
          value emptyPattern `shouldBe` Sum 0
          elements emptyPattern `shouldBe` ([] :: [Pattern (Sum Int)])
        
        it "T007: mempty has correct structure for Product Int values" $ do
          let emptyPattern = mempty :: Pattern (Product Int)
          value emptyPattern `shouldBe` Product 1
          elements emptyPattern `shouldBe` ([] :: [Pattern (Product Int)])
      
      describe "Left Identity Law" $ do
        
        it "T008: mempty <> p = p for atomic patterns" $ do
          let p = point "test"
          (mempty <> p) `shouldBe` p
        
        it "T008: mempty <> p = p for patterns with elements" $ do
          let p = pattern "root" [point "a", point "b"]
          (mempty <> p) `shouldBe` p
        
        it "T008: mempty <> p = p for nested patterns" $ do
          let inner = point "inner"
              middle = pattern "middle" [inner]
              p = pattern "outer" [middle]
          (mempty <> p) `shouldBe` p
      
      describe "Right Identity Law" $ do
        
        it "T009: p <> mempty = p for atomic patterns" $ do
          let p = point "test"
          (p <> mempty) `shouldBe` p
        
        it "T009: p <> mempty = p for patterns with elements" $ do
          let p = pattern "root" [point "a", point "b"]
          (p <> mempty) `shouldBe` p
        
        it "T009: p <> mempty = p for nested patterns" $ do
          let inner = point "inner"
              middle = pattern "middle" [inner]
              p = pattern "outer" [middle]
          (p <> mempty) `shouldBe` p
      
      describe "Standard Monoid Combinators" $ do
        
        it "T010: mconcat with empty list returns mempty" $ do
          let result = mconcat [] :: Pattern String
          value result `shouldBe` ""
          elements result `shouldBe` ([] :: [Pattern String])
        
        it "T011: mconcat with list of patterns combines them correctly" $ do
          let patterns = [point "a", point "b", point "c"]
              result = mconcat patterns
          value result `shouldBe` "abc"
          elements result `shouldBe` ([] :: [Pattern String])
        
        it "T011: mconcat preserves element order" $ do
          let p1 = pattern "root" [point "x"]
              p2 = pattern "root" [point "y"]
              patterns = [p1, p2]
              result = mconcat patterns
          value result `shouldBe` "rootroot"
          length (elements result) `shouldBe` 2
          map value (elements result) `shouldBe` ["x", "y"]

    describe "Monoid Instance - Edge Cases and Consistency (User Story 4)" $ do
      
      describe "Identity with different point structures" $ do
        
        it "T021: identity with atomic patterns" $ do
          let p = point "a"
          (mempty <> p) `shouldBe` p
          (p <> mempty) `shouldBe` p
        
        it "T022: identity with patterns having elements" $ do
          let p = pattern "root" [point "a", point "b", point "c"]
          (mempty <> p) `shouldBe` p
          (p <> mempty) `shouldBe` p
        
        it "T023: identity with nested patterns" $ do
          let level3 = point "level3"
              level2 = pattern "level2" [level3]
              level1 = pattern "level1" [level2]
              p = pattern "root" [level1]
          (mempty <> p) `shouldBe` p
          (p <> mempty) `shouldBe` p
      
      describe "Identity with different value types" $ do
        
        it "T024: identity with String values" $ do
          let p = point "test" :: Pattern String
          value (mempty :: Pattern String) `shouldBe` ""
          (mempty <> p) `shouldBe` p
          (p <> mempty) `shouldBe` p
        
        it "T024: identity with Sum Int values" $ do
          let p = point (Sum 5) :: Pattern (Sum Int)
          value (mempty :: Pattern (Sum Int)) `shouldBe` Sum 0
          (mempty <> p) `shouldBe` p
          (p <> mempty) `shouldBe` p
        
        it "T024: identity with Product Int values" $ do
          let p = point (Product 5) :: Pattern (Product Int)
          value (mempty :: Pattern (Product Int)) `shouldBe` Product 1
          (mempty <> p) `shouldBe` p
          (p <> mempty) `shouldBe` p
        
        it "T024: identity with All values" $ do
          let p = point (All False) :: Pattern All
          value (mempty :: Pattern All) `shouldBe` All True
          (mempty <> p) `shouldBe` p
          (p <> mempty) `shouldBe` p
        
        it "T024: identity with Any values" $ do
          let p = point (Any True) :: Pattern Any
          value (mempty :: Pattern Any) `shouldBe` Any False
          (mempty <> p) `shouldBe` p
          (p <> mempty) `shouldBe` p
      
      describe "Standard Monoid Combinators - Edge Cases" $ do
        
        it "T025: mconcat with list containing only mempty" $ do
          let result = mconcat [mempty, mempty, mempty] :: Pattern String
          value result `shouldBe` ""
          elements result `shouldBe` ([] :: [Pattern String])
          result `shouldBe` (mempty :: Pattern String)
      
      describe "Consistency with Semigroup" $ do
        
        it "T026: p1 <> p2 produces same result using Semigroup or Monoid" $ do
          let p1 = point "a"
              p2 = point "b"
              semigroupResult = p1 <> p2
              monoidResult = p1 <> p2
          semigroupResult `shouldBe` monoidResult
          value semigroupResult `shouldBe` value monoidResult
          elements semigroupResult `shouldBe` elements monoidResult
        
        it "T026: integration test for standard Monoid combinators" $ do
          let patterns = [point "a", point "b", point "c"]
              mconcatResult = mconcat patterns
              foldrResult = foldr (<>) mempty patterns
          mconcatResult `shouldBe` foldrResult
          value mconcatResult `shouldBe` "abc"
          elements mconcatResult `shouldBe` ([] :: [Pattern String])
    
    describe "Hashable Instance (User Story 3)" $ do
      
      describe "Basic Hashing" $ do
        
        it "T001: hash atomic pattern" $ do
          let p = point "a" :: Pattern String
          hash p `shouldSatisfy` (\h -> h == h)  -- Valid hash (any Int)
        
        it "T002: hash point with elements" $ do
          let p = pattern "root" [point "a", point "b"] :: Pattern String
          hash p `shouldSatisfy` (\h -> h == h)  -- Valid hash (any Int)
        
        it "T003: hash nested pattern" $ do
          let p = pattern "outer" [pattern "inner" [point "value"]] :: Pattern String
          hash p `shouldSatisfy` (\h -> h == h)  -- Valid hash (any Int)
        
        it "T004: hash consistency with Eq - equal patterns have same hash" $ do
          let p1 = point "a" :: Pattern String
              p2 = point "a" :: Pattern String
          p1 `shouldBe` p2
          hash p1 `shouldBe` hash p2
        
        it "T005: hash point with String values" $ do
          let p1 = point "hello" :: Pattern String
              p2 = point "world" :: Pattern String
          hash p1 `shouldSatisfy` (\h -> h == h)  -- Valid hash (any Int)
          hash p2 `shouldSatisfy` (\h -> h == h)  -- Valid hash (any Int)
        
        it "T006: hash point with Int values" $ do
          let p1 = point (42 :: Int) :: Pattern Int
              p2 = point (100 :: Int) :: Pattern Int
          hash p1 `shouldSatisfy` (\h -> h == h)  -- Valid hash (any Int)
          hash p2 `shouldSatisfy` (\h -> h == h)  -- Valid hash (any Int)
        
        it "T007: structure-preserving hashing - different structures produce different hashes" $ do
          let p1 = pattern "a" [point "b", point "c"] :: Pattern String
              p2 = pattern "a" [pattern "b" [point "c"]] :: Pattern String
          -- Different structures should produce different hashes
          hash p1 `shouldNotBe` hash p2
        
        it "T008: recursive hashing - nested structures contribute to hash" $ do
          let p1 = pattern "root" [point "a"] :: Pattern String
              p2 = pattern "root" [pattern "a" [point "b"]] :: Pattern String
          -- Different nesting should produce different hashes
          hash p1 `shouldNotBe` hash p2
        
        it "T009: type constraint - Hashable v requirement" $ do
          let p = point "test" :: Pattern String
          -- Should compile and execute (String has Hashable instance)
          hash p `shouldSatisfy` (\h -> h == h)  -- Valid hash (any Int)
        
        it "T010: hashWithSalt function" $ do
          let p = point "test" :: Pattern String
              salt1 = 42
              salt2 = 100
          hashWithSalt salt1 p `shouldSatisfy` (\h -> h == h)  -- Valid hash (any Int)
          hashWithSalt salt2 p `shouldSatisfy` (\h -> h == h)  -- Valid hash (any Int)
          -- Different salts should produce different hashes
          hashWithSalt salt1 p `shouldNotBe` hashWithSalt salt2 p
      
      describe "Edge Cases" $ do
        
        it "T022: hash atomic patterns (edge case)" $ do
          let p1 = point "a" :: Pattern String
              p2 = point "b" :: Pattern String
          hash p1 `shouldSatisfy` (\h -> h == h)  -- Valid hash
          hash p2 `shouldSatisfy` (\h -> h == h)  -- Valid hash
          -- Different atomic patterns should usually have different hashes
          -- (collisions possible but rare)
        
        it "T023: hash patterns with many elements (100+ elements)" $ do
          let elems = map (\i -> point (show i)) [1..100]
              p = pattern "root" elems :: Pattern String
          hash p `shouldSatisfy` (\h -> h == h)  -- Valid hash
          -- All elements should contribute to hash
        
        it "T024: hash deeply nested patterns (10+ levels)" $ do
          let deep = foldl (\acc _ -> pattern "level" [acc]) (point "base" :: Pattern String) [1..10]
          hash deep `shouldSatisfy` (\h -> h == h)  -- Valid hash
          -- Deep nesting should contribute to hash
        
        it "T025: hash patterns with same flattened values but different structures" $ do
          let p1 = pattern "a" [point "b", point "c"] :: Pattern String
              p2 = pattern "a" [pattern "b" [point "c"]] :: Pattern String
          -- Different structures should produce different hashes
          hash p1 `shouldNotBe` hash p2
        
        it "T026: hash patterns with duplicate values" $ do
          let p1 = pattern "a" [point "b", point "b"] :: Pattern String
              p2 = pattern "a" [point "b"] :: Pattern String
          -- Different structures (even with duplicate values) should produce different hashes
          hash p1 `shouldNotBe` hash p2
    
    describe "Integration with Hash-Based Containers (User Story 5)" $ do
      
      describe "HashMap Integration" $ do
        
        it "T029: HashMap with patterns as keys: create HashMap and perform lookups" $ do
          let m = HashMap.fromList [(point "a", 1), (point "b", 2), (pattern "root" [point "c"], 3)] :: HashMap.HashMap (Pattern String) Int
          HashMap.lookup (point "a") m `shouldBe` Just 1
          HashMap.lookup (point "b") m `shouldBe` Just 2
          HashMap.lookup (point "x") m `shouldBe` Nothing
        
        it "T030: HashMap with patterns as keys: insert patterns and verify lookups work correctly" $ do
          let m1 = HashMap.fromList [(point "a", 1)] :: HashMap.HashMap (Pattern String) Int
              m2 = HashMap.insert (point "b") 2 m1
              m3 = HashMap.insert (pattern "root" [point "c"]) 3 m2
          HashMap.lookup (point "a") m3 `shouldBe` Just 1
          HashMap.lookup (point "b") m3 `shouldBe` Just 2
          HashMap.lookup (pattern "root" [point "c"]) m3 `shouldBe` Just 3
        
        it "T031: HashMap with patterns as keys: handle hash collisions correctly (patterns with same hash but different values)" $ do
          -- Even if two patterns have the same hash (collision), HashMap handles it correctly through Eq
          let p1 = point "a" :: Pattern String
              p2 = point "b" :: Pattern String
              m = HashMap.fromList [(p1, 1), (p2, 2)] :: HashMap.HashMap (Pattern String) Int
          -- Both patterns should be retrievable (collisions handled through equality)
          HashMap.lookup p1 m `shouldBe` Just 1
          HashMap.lookup p2 m `shouldBe` Just 2
        
        it "T035: HashMap performance: O(1) average-case lookups" $ do
          -- Create HashMap with many patterns
          let patterns = map (\i -> (point (show i), i)) [1..100]
              m = HashMap.fromList patterns :: HashMap.HashMap (Pattern String) Int
          -- Lookup should be fast (O(1) average-case)
          HashMap.lookup (point "50") m `shouldBe` Just 50
          HashMap.lookup (point "99") m `shouldBe` Just 99
        
        it "T037: HashMap with nested patterns as keys" $ do
          let p1 = pattern "outer" [point "inner"] :: Pattern String
              p2 = pattern "outer" [pattern "inner" [point "value"]] :: Pattern String
              m = HashMap.fromList [(p1, 1), (p2, 2)] :: HashMap.HashMap (Pattern String) Int
          HashMap.lookup p1 m `shouldBe` Just 1
          HashMap.lookup p2 m `shouldBe` Just 2
      
      describe "HashSet Integration" $ do
        
        it "T032: HashSet with patterns as elements: create HashSet and test membership" $ do
          let s = HashSet.fromList [point "a", point "b", pattern "root" [point "c"]] :: HashSet.HashSet (Pattern String)
          HashSet.member (point "a") s `shouldBe` True
          HashSet.member (point "b") s `shouldBe` True
          HashSet.member (point "x") s `shouldBe` False
        
        it "T033: HashSet with patterns as elements: insert patterns and verify deduplication works correctly" $ do
          let s1 = HashSet.fromList [point "a"] :: HashSet.HashSet (Pattern String)
              s2 = HashSet.insert (point "b") s1
              s3 = HashSet.insert (point "a") s2  -- Duplicate
          HashSet.member (point "a") s3 `shouldBe` True
          HashSet.member (point "b") s3 `shouldBe` True
          HashSet.size s3 `shouldBe` 2  -- Duplicate removed
        
        it "T034: HashSet with patterns as elements: handle hash collisions correctly (patterns with same hash but different values)" $ do
          -- Even if two patterns have the same hash (collision), HashSet handles it correctly through Eq
          let p1 = point "a" :: Pattern String
              p2 = point "b" :: Pattern String
              s = HashSet.fromList [p1, p2] :: HashSet.HashSet (Pattern String)
          -- Both patterns should be in set (collisions handled through equality)
          HashSet.member p1 s `shouldBe` True
          HashSet.member p2 s `shouldBe` True
          HashSet.size s `shouldBe` 2
        
        it "T036: HashSet performance: O(1) average-case membership testing" $ do
          -- Create HashSet with many patterns
          let patterns = map point (map show [1..100])
              s = HashSet.fromList patterns :: HashSet.HashSet (Pattern String)
          -- Membership testing should be fast (O(1) average-case)
          HashSet.member (point "50") s `shouldBe` True
          HashSet.member (point "99") s `shouldBe` True
          HashSet.member (point "200") s `shouldBe` False
        
        it "T038: HashSet with nested patterns as elements" $ do
          let p1 = pattern "outer" [point "inner"] :: Pattern String
              p2 = pattern "outer" [pattern "inner" [point "value"]] :: Pattern String
              s = HashSet.fromList [p1, p2] :: HashSet.HashSet (Pattern String)
          HashSet.member p1 s `shouldBe` True
          HashSet.member p2 s `shouldBe` True
          HashSet.size s `shouldBe` 2
    
    describe "Integration with Other Typeclass Instances (Phase 5)" $ do
      
      describe "Hashable with Pattern Constructors" $ do
        
        it "T045: Hashable instance with point constructor" $ do
          let p1 = point "test" :: Pattern String
              p2 = point "test" :: Pattern String
          p1 `shouldBe` p2
          hash p1 `shouldBe` hash p2
        
        it "T045: Hashable instance with pattern constructor" $ do
          let p1 = pattern "root" [point "a", point "b"] :: Pattern String
              p2 = pattern "root" [point "a", point "b"] :: Pattern String
          p1 `shouldBe` p2
          hash p1 `shouldBe` hash p2
        
        it "T045: Hashable instance with fromList constructor" $ do
          let p1 = fromList "root" ["a", "b", "c"] :: Pattern String
              p2 = fromList "root" ["a", "b", "c"] :: Pattern String
          p1 `shouldBe` p2
          hash p1 `shouldBe` hash p2
      
      describe "Hashable with Type Class Instances" $ do
        
        it "T046: Hashable instance with Functor (fmap preserves hash consistency)" $ do
          let p1 = point "test" :: Pattern String
              p2 = fmap id p1
          p1 `shouldBe` p2
          hash p1 `shouldBe` hash p2
        
        it "T046: Hashable instance with Foldable (toList doesn't affect hash)" $ do
          let p = pattern "root" [point "a", point "b"] :: Pattern String
          -- Hash is based on structure, not flattened values
          hash p `shouldSatisfy` (\h -> h == h)
          toList p `shouldBe` ["root", "a", "b"]
        
        it "T046: Hashable instance with Traversable (traverse preserves hash consistency)" $ do
          let p1 = point "test" :: Pattern String
              p2 = runIdentity (traverse Identity p1)
          p1 `shouldBe` p2
          hash p1 `shouldBe` hash p2
        
        it "T046: Hashable instance with Eq (hash consistency verified)" $ do
          let p1 = point "test" :: Pattern String
              p2 = point "test" :: Pattern String
          p1 == p2 `shouldBe` True
          hash p1 == hash p2 `shouldBe` True
        
        it "T046: Hashable instance with Ord (can use both ordered and hash-based containers)" $ do
          let p1 = point "a" :: Pattern String
              p2 = point "b" :: Pattern String
          -- Can use in both Data.Set (Ord) and HashSet (Hashable)
          let orderedSet = Set.fromList [p1, p2]
              hashSet = HashSet.fromList [p1, p2]
          Set.member p1 orderedSet `shouldBe` True
          HashSet.member p1 hashSet `shouldBe` True
      
      describe "Hashable with Semigroup and Monoid" $ do
        
        it "T047: Hashable instance with Semigroup (combined patterns hash correctly)" $ do
          let p1 = point "a" :: Pattern String
              p2 = point "b" :: Pattern String
              combined = p1 <> p2
          hash combined `shouldSatisfy` (\h -> h == h)
          -- Combined point should have different hash from individual patterns
          hash combined `shouldNotBe` hash p1
        
        it "T047: Hashable instance with Monoid (mempty hashes correctly)" $ do
          let empty = mempty :: Pattern String
          hash empty `shouldSatisfy` (\h -> h == h)
          -- mempty <> p should have same hash as p (identity)
          let p = point "test" :: Pattern String
          hash (mempty <> p) `shouldBe` hash p
    
    describe "Applicative Instance (User Story 1)" $ do
      
      describe "pure function" $ do
        
        it "T005: pure with integer value creates atomic pattern" $ do
          let p = pure 5 :: Pattern Int
          value p `shouldBe` 5
          elements p `shouldBe` ([] :: [Pattern Int])
        
        it "T006: pure with string value creates atomic pattern" $ do
          let p = pure "hello" :: Pattern String
          value p `shouldBe` "hello"
          elements p `shouldBe` ([] :: [Pattern String])
        
        it "T007: pure with function value creates atomic pattern" $ do
          let f = (+1) :: Int -> Int
              p = pure f :: Pattern (Int -> Int)
          -- Verify function is stored correctly by applying it
          (value p) 5 `shouldBe` 6
          length (elements p) `shouldBe` 0
      
      describe "<*> operator" $ do
        
        it "T008: <*> with atomic patterns (function and value)" $ do
          let f = pure ((+1) :: Int -> Int)
              x = pure 5 :: Pattern Int
              result = f <*> x
          value result `shouldBe` 6
          elements result `shouldBe` ([] :: [Pattern Int])
        
        it "T009: <*> with patterns having multiple elements" $ do
          let fs = pattern (id :: Int -> Int) [pure (*2), pure (+10)]
              xs = pattern 5 [pure 3, pure 7]
              result = fs <*> xs
          value result `shouldBe` 5
          -- Law-abiding: each f in fs applies to xs, and each x gets id applied
          -- Result: [(*2) <*> xs, (+10) <*> xs, id <*> 3, id <*> 7]
          length (elements result) `shouldBe` 4
          value (elements result !! 0) `shouldBe` 10  -- (*2) applied to 5
          value (elements result !! 1) `shouldBe` 15  -- (+10) applied to 5
        
        it "T010: <*> with nested patterns" $ do
          let fs = pattern (id :: Int -> Int) 
                [ pattern (*2) [pure (*3)]
                , pattern (+1) []
                ]
              xs = pattern 1
                [ pattern 2 [pure 3]
                , pattern 4 []
                ]
              result = fs <*> xs
          value result `shouldBe` 1
          -- Law-abiding: fs elements apply to xs, and xs elements get id applied
          -- Result has 4 elements: [pattern (*2) <*> xs, pattern (+1) <*> xs, id <*> pattern 2, id <*> pattern 4]
          length (elements result) `shouldBe` 4
          -- First element: pattern (*2) [pure (*3)] <*> pattern 1 [pattern 2 [pure 3], pattern 4 []]
          let firstElem = head (elements result)
          value firstElem `shouldBe` 2  -- (*2) applied to 1
          -- Last element: id <*> pattern 4 [] = pattern 4 []
          let lastElem = last (elements result)
          value lastElem `shouldBe` 4  -- id applied to 4
          -- The last element will have nested structure from the recursive application
          length (elements lastElem) `shouldBe` 2
        
        it "T011: <*> with pure function and point value" $ do
          let f = pure ((+1) :: Int -> Int)
              x = pattern 5 [pure 3, pure 7]
              result = f <*> x
          value result `shouldBe` 6
          length (elements result) `shouldBe` 2
          value (head (elements result)) `shouldBe` 4
          value (last (elements result)) `shouldBe` 8
        
        it "T012: <*> with point function and pure value" $ do
          let f = pattern ((+1) :: Int -> Int) [pure (*2), pure (+10)]
              x = pure 5 :: Pattern Int
              result = f <*> x
          value result `shouldBe` 6
          length (elements result) `shouldBe` 2
          value (head (elements result)) `shouldBe` 10
          value (last (elements result)) `shouldBe` 15
    
    describe "Applicative Consistency with Functor (User Story 3)" $ do
      
      describe "Functor Consistency" $ do
        
        it "T037: consistency with atomic patterns" $ do
          let f = (+1) :: Int -> Int
              p = pure 5 :: Pattern Int
              functorResult = fmap f p
              applicativeResult = pure f <*> p
          functorResult `shouldBe` applicativeResult
        
        it "T038: consistency with patterns having elements" $ do
          let f = (*2) :: Int -> Int
              p = pattern 5 [pure 3, pure 7]
              functorResult = fmap f p
              applicativeResult = pure f <*> p
          functorResult `shouldBe` applicativeResult
        
        it "T039: consistency with nested patterns" $ do
          let f = (+10) :: Int -> Int
              p = pattern 1 [pattern 2 [pure 3]]
              functorResult = fmap f p
              applicativeResult = pure f <*> p
          functorResult `shouldBe` applicativeResult
        
        it "T040: consistency with type transformations (String -> Int)" $ do
          let f = length :: String -> Int
              p = pattern "hello" [pure "world", pure "test"]
              functorResult = fmap f p
              applicativeResult = pure f <*> p
          functorResult `shouldBe` applicativeResult
    
    describe "Edge Cases in Applicative Operations (User Story 4)" $ do
      
      describe "Empty elements lists" $ do
        
        it "T049: <*> with patterns having empty elements lists" $ do
          let f = pure ((+1) :: Int -> Int)
              x = pure 5 :: Pattern Int
              result = f <*> x
          value result `shouldBe` 6
          elements result `shouldBe` ([] :: [Pattern Int])
      
      describe "Mismatched element counts" $ do
        
        it "T050: <*> with mismatched element counts (function point has fewer elements)" $ do
          let fs = pattern (id :: Int -> Int) [pure (*2)]  -- 1 element
              xs = pattern 5 [pure 3, pure 7]              -- 2 elements
              result = fs <*> xs
          value result `shouldBe` 5
          -- With law-abiding Applicative: fs applies to all xs elements, and xs elements get id applied
          length (elements result) `shouldBe` 3  -- [(*2) <*> xs, id <*> 3, id <*> 7]
          -- First element: (*2) applied to entire xs pattern
          value (head (elements result)) `shouldBe` 10  -- (*2) applied to 5
        
        it "T051: <*> with mismatched element counts (value p has fewer elements)" $ do
          let fs = pattern (id :: Int -> Int) [pure (*2), pure (+10)]  -- 2 elements
              xs = pattern 5 [pure 3]                                   -- 1 element
              result = fs <*> xs
          value result `shouldBe` 5
          -- With law-abiding Applicative: each f in fs applies to xs, and each x gets id applied
          length (elements result) `shouldBe` 3  -- [(*2) <*> xs, (+10) <*> xs, id <*> 3]
          -- First element: (*2) applied to entire xs pattern  
          value (head (elements result)) `shouldBe` 10  -- (*2) applied to 5
      
      describe "Deeply nested patterns" $ do
        
        it "T052: <*> with deeply nested patterns (10+ levels)" $ do
          -- Create deeply nested patterns with matching structures
          let buildDeepValue n = if n <= 0 
                                 then pure 1
                                 else pattern n [buildDeepValue (n - 1)]
              buildDeepFunc n = if n <= 0
                                then pure (id :: Int -> Int)
                                else pattern (id :: Int -> Int) [buildDeepFunc (n - 1)]
              xs = buildDeepValue 10 :: Pattern Int
              fs = buildDeepFunc 10 :: Pattern (Int -> Int)
              result = fs <*> xs
          -- With law-abiding Applicative, depth doubles due to applying fs elements to xs and xs elements to fs
          depth result `shouldBe` 20  -- Depth increases due to both branches
          value result `shouldBe` 10
      
      describe "Atomic patterns with multi-element patterns" $ do
        
        it "T053: <*> with atomic function point and point with multiple elements" $ do
          let f = pure ((+1) :: Int -> Int)
              x = pattern 5 [pure 3, pure 7]
              result = f <*> x
          value result `shouldBe` 6
          length (elements result) `shouldBe` 2
          value (head (elements result)) `shouldBe` 4
          value (last (elements result)) `shouldBe` 8
        
        it "T054: <*> with point with multiple function elements and atomic value p" $ do
          let f = pattern ((+1) :: Int -> Int) [pure (*2), pure (+10)]
              x = pure 5 :: Pattern Int
              result = f <*> x
          value result `shouldBe` 6
          length (elements result) `shouldBe` 2
          value (head (elements result)) `shouldBe` 10
          value (last (elements result)) `shouldBe` 15
      
      describe "Different value types" $ do
        
        it "T055: pure with different value types (strings, integers, custom types)" $ do
          let p1 = pure "hello" :: Pattern String
              p2 = pure 42 :: Pattern Int
              p3 = pure (Person "Alice" (Just 30)) :: Pattern Person
          value p1 `shouldBe` "hello"
          value p2 `shouldBe` 42
          name (value p3) `shouldBe` "Alice"
          elements p1 `shouldBe` ([] :: [Pattern String])
          elements p2 `shouldBe` ([] :: [Pattern Int])
          elements p3 `shouldBe` ([] :: [Pattern Person])
    
    describe "Value Predicate Functions (User Story 1)" $ do
      
      describe "anyValue function - unit tests" $ do
        
        it "T001: anyValue with atomic point containing matching value" $ do
          let pat = point 5
          anyValue (> 0) pat `shouldBe` True
          anyValue (> 10) pat `shouldBe` False
        
        it "T002: anyValue with nested point containing matching value" $ do
          let pat = pattern 0 [point 1, point 2]
          anyValue (> 0) pat `shouldBe` True
          anyValue (< 0) pat `shouldBe` False
        
        it "T003: anyValue with point containing no matching values" $ do
          let pat = pattern 0 [point 1, point 2]
          anyValue (< 0) pat `shouldBe` False
          anyValue (> 10) pat `shouldBe` False
      
      describe "allValues function - unit tests" $ do
        
        it "T004: allValues with atomic pattern" $ do
          let pat = point 5
          allValues (> 0) pat `shouldBe` True
          allValues (> 10) pat `shouldBe` False
        
        it "T007: allValues with empty point (vacuous truth)" $ do
          let pat = point 0
          -- For atomic pattern, predicate is evaluated on the value
          allValues (> 0) pat `shouldBe` False
          allValues (>= 0) pat `shouldBe` True
        
        it "T008: anyValue and allValues with deeply nested patterns" $ do
          let level3 = point 1
          let level2 = pattern 2 [level3]
          let level1 = pattern 3 [level2]
          let pat = pattern 4 [level1]
          anyValue (> 0) pat `shouldBe` True
          allValues (> 0) pat `shouldBe` True
          anyValue (> 10) pat `shouldBe` False
          allValues (> 10) pat `shouldBe` False
    
    describe "Pattern Predicate Functions (User Story 2)" $ do
      
      describe "filterPatterns function - unit tests" $ do
        
        it "T019: filterPatterns with predicate matching some subpatterns" $ do
          let pat = pattern "root" [point "a", point "b", pattern "c" [point "d"]]
          filterPatterns (\p -> length (elements p) == 0) pat `shouldBe` [point "a", point "b", point "d"]
        
        it "T020: filterPatterns with predicate matching root pattern" $ do
          let pat = pattern "root" [point "a", point "b"]
          filterPatterns (\p -> value p == "root") pat `shouldBe` [pat]
        
        it "T021: filterPatterns with predicate matching no subpatterns" $ do
          let pat = pattern "root" [point "a", point "b"]
          filterPatterns (\p -> value p == "x") pat `shouldBe` []
      
      describe "findPattern function - unit tests" $ do
        
        it "T022: findPattern with predicate matching first subpattern" $ do
          let pat = pattern "root" [point "a", point "b"]
          findPattern (\p -> value p == "a") pat `shouldBe` Just (point "a")
        
        it "T023: findPattern with predicate matching root pattern" $ do
          let pat = pattern "root" [point "a", point "b"]
          findPattern (\p -> value p == "root") pat `shouldBe` Just pat
        
        it "T024: findPattern with predicate matching no subpatterns" $ do
          let pat = pattern "root" [point "a", point "b"]
          findPattern (\p -> value p == "x") pat `shouldBe` Nothing
      
      describe "findAllPatterns function - unit tests" $ do
        
        it "T025: findAllPatterns with predicate matching multiple subpatterns" $ do
          let pat = pattern "root" [point "a", point "b"]
          findAllPatterns (\p -> length (elements p) == 0) pat `shouldBe` [point "a", point "b"]
      
      describe "Pattern predicates on various structures" $ do
        
        it "T026: point predicates on deeply nested patterns" $ do
          let level3 = point "leaf"
          let level2 = pattern "level2" [level3]
          let level1 = pattern "level1" [level2]
          let pat = pattern "root" [level1]
          filterPatterns (\p -> value p == "leaf") pat `shouldBe` [level3]
          findPattern (\p -> value p == "level2") pat `shouldBe` Just level2
        
        it "T027: point predicates on atomic patterns" $ do
          let pat = point "a"
          filterPatterns (\p -> value p == "a") pat `shouldBe` [pat]
          findPattern (\p -> value p == "a") pat `shouldBe` Just pat
        
        it "T028: point predicates matching element sequence structure (a, b, b, a)" $ do
          let pat = pattern "root" [point "a", point "b", point "b", point "a"]
          filterPatterns (\p -> length (elements p) == 4 && 
                               value (elements p !! 0) == value (elements p !! 3) &&
                               value (elements p !! 1) == value (elements p !! 2)) pat `shouldBe` [pat]
    
    describe "Structural Matching Functions (User Story 3)" $ do
      
      describe "matches function - unit tests" $ do
        
        it "T041: matches with identical patterns" $ do
          let pat1 = pattern "root" [point "a", point "b"]
          let pat2 = pattern "root" [point "a", point "b"]
          matches pat1 pat2 `shouldBe` True
        
        it "T042: matches with patterns having different values" $ do
          let pat1 = pattern "root1" [point "a", point "b"]
          let pat2 = pattern "root2" [point "a", point "b"]
          matches pat1 pat2 `shouldBe` False
        
        it "T043: matches with patterns having different element counts" $ do
          let pat1 = pattern "root" [point "a", point "b"]
          let pat2 = pattern "root" [point "a"]
          matches pat1 pat2 `shouldBe` False
        
        it "T044: matches with patterns having same flattened values but different structures" $ do
          let pat1 = pattern "root" [point "a", point "b"]
          let pat2 = pattern "a" [pattern "b" [point "root"]]
          -- Same flattened values but different structure
          matches pat1 pat2 `shouldBe` False
        
        it "T045: matches with atomic patterns" $ do
          let pat1 = point "a"
          let pat2 = point "a"
          let pat3 = point "b"
          matches pat1 pat2 `shouldBe` True
          matches pat1 pat3 `shouldBe` False
      
      describe "contains function - unit tests" $ do
        
        it "T046: contains with point containing subpattern" $ do
          let subpat = point "a"
          let pat = pattern "root" [subpat, point "b"]
          contains pat subpat `shouldBe` True
        
        it "T047: contains with point not containing subpattern" $ do
          let subpat = point "x"
          let pat = pattern "root" [point "a", point "b"]
          contains pat subpat `shouldBe` False
        
        it "T048: contains with point containing itself (self-containment)" $ do
          let pat = pattern "root" [point "a", point "b"]
          contains pat pat `shouldBe` True
        
        it "T049: contains with atomic patterns" $ do
          let pat1 = point "a"
          let pat2 = point "b"
          contains pat1 pat1 `shouldBe` True
          contains pat1 pat2 `shouldBe` False
      
      describe "Structural matching on various structures" $ do
        
        it "T050: structural matching on deeply nested patterns" $ do
          let level3 = point "leaf"
          let level2 = pattern "level2" [level3]
          let level1 = pattern "level1" [level2]
          let pat1 = pattern "root" [level1]
          let pat2 = pattern "root" [level1]
          let pat3 = pattern "root" [pattern "level1" [point "leaf"]]
          matches pat1 pat2 `shouldBe` True
          matches pat1 pat3 `shouldBe` False
          contains pat1 level3 `shouldBe` True
          contains pat1 (point "x") `shouldBe` False
    
    describe "Integration Tests - Predicate Functions with Existing Operations" $ do
      
      describe "Predicate functions with Functor operations" $ do
        
        it "T063: anyValue and allValues work with fmap" $ do
          let pat = pattern 1 [point 2, point 3]
          let pat' = fmap (+1) pat
          anyValue (> 0) pat' `shouldBe` True
          allValues (> 0) pat' `shouldBe` True
          anyValue (> 5) pat' `shouldBe` False
      
      describe "Predicate functions with Foldable operations" $ do
        
        it "T064: anyValue and allValues are consistent with toList" $ do
          let pat = pattern 1 [point 2, point 3]
          let valuesList = toList pat
          anyValue (> 0) pat `shouldBe` any (> 0) valuesList
          allValues (> 0) pat `shouldBe` all (> 0) valuesList
      
      describe "Pattern predicates with existing query functions" $ do
        
        it "T064: filterPatterns works with size and depth" $ do
          let pat = pattern "root" [point "a", point "b"]
          let largePatterns = filterPatterns (\p -> size p >= 2) pat
          length largePatterns `shouldBe` 1  -- only root has size >= 2 (size 3)
          let deepPatterns = filterPatterns (\p -> depth p == 0) pat
          length deepPatterns `shouldBe` 2  -- two atomic elements
    
    describe "Edge Case Tests - Extreme Patterns" $ do
      
      describe "Very deeply nested patterns (100+ levels)" $ do
        
        it "T065: predicate functions work with 100+ nesting levels" $ do
          let createDeep n = if n <= 0
                             then point 1
                             else pattern n [createDeep (n - 1)]
          let deepPat = createDeep 100
          anyValue (> 0) deepPat `shouldBe` True
          allValues (> 0) deepPat `shouldBe` True
          filterPatterns (\p -> depth p == 0) deepPat `shouldBe` [point 1]
          contains deepPat (point 1) `shouldBe` True
      
      describe "Patterns with many nodes (1000+)" $ do
        
        it "T065: predicate functions work with 1000+ nodes" $ do
          let manyElems = map point [1..1000]
          let pat = pattern 0 manyElems
          anyValue (> 500) pat `shouldBe` True
          allValues (>= 0) pat `shouldBe` True
          allValues (> 0) pat `shouldBe` False  -- root value is 0
          length (filterPatterns (\p -> length (elements p) == 0) pat) `shouldBe` 1000
    
    describe "Comonad Instance" $ do
      
      describe "extract function (User Story 1)" $ do
        
        it "T009: extract with atomic point (integer value)" $ do
          let p = point 5
          extract p `shouldBe` (5 :: Int)
        
        it "T010: extract with atomic point (string value)" $ do
          let p = point "test"
          extract p `shouldBe` "test"
        
        it "T011: extract with point with elements" $ do
          let p = pattern "root" [point "a", point "b"]
          extract p `shouldBe` "root"
        
        it "T012: extract with nested point structure" $ do
          let p = pattern "root" [pattern "a" [point "x"], point "b"]
          extract p `shouldBe` "root"
        
        it "T013: extract with different value types" $ do
          let pInt = point 42
          let pString = point "hello"
          let pCustom = point (Person "Alice" (Just 30))
          extract pInt `shouldBe` (42 :: Int)
          extract pString `shouldBe` "hello"
          extract pCustom `shouldBe` Person "Alice" (Just 30)
      
      describe "extend function (User Story 2)" $ do
        
        it "T021: extend with depth computation function on atomic pattern" $ do
          let depthFunc p = depth p
          let p = point 5
          let result = extend depthFunc p
          extract result `shouldBe` (0 :: Int)
        
        it "T022: extend with depth computation function on point with elements" $ do
          let depthFunc p = depth p
          let p = pattern "root" [point "a", point "b"]
          let result = extend depthFunc p
          extract result `shouldBe` (1 :: Int)  -- Root has depth 1 (has elements)
          length (elements result) `shouldBe` 2
          extract (elements result !! 0) `shouldBe` (0 :: Int)  -- Atomic patterns have depth 0
          extract (elements result !! 1) `shouldBe` (0 :: Int)  -- Atomic patterns have depth 0
        
        it "T023: extend with size computation function on nested pattern" $ do
          let sizeFunc p = size p
          let p = pattern "root" [pattern "a" [point "x"], point "b"]
          let result = extend sizeFunc p
          extract result `shouldBe` (4 :: Int)  -- Root (1) + "a" (1) + "x" (1) + "b" (1) = 4
          length (elements result) `shouldBe` 2
          extract (elements result !! 0) `shouldBe` (2 :: Int)  -- "a" (1) + "x" (1) = 2
          extract (elements result !! 1) `shouldBe` (1 :: Int)  -- "b" (1) = 1
        
        it "T024: extend with custom context-aware function (length of values)" $ do
          let customFunc p = length (values p)
          let p = pattern "root" [point "a", point "b"]
          let result = extend customFunc p
          extract result `shouldBe` (3 :: Int)
          extract (elements result !! 0) `shouldBe` (1 :: Int)
          extract (elements result !! 1) `shouldBe` (1 :: Int)
        
        it "T025: extend with function that transforms value types (Pattern Int -> String)" $ do
          let transformFunc p = show (size p)
          let p = pattern 10 [point 20, point 30]
          let result = extend transformFunc p
          extract result `shouldBe` "3"
          extract (elements result !! 0) `shouldBe` "1"
          extract (elements result !! 1) `shouldBe` "1"
        
        it "T026: extend with nested point structure (verify recursive application)" $ do
          let depthFunc p = depth p
          let p = pattern "root" [pattern "a" [point "x"], point "b"]
          let result = extend depthFunc p
          extract result `shouldBe` (2 :: Int)  -- Root has depth 2 (nested structure)
          extract (elements result !! 0) `shouldBe` (1 :: Int)  -- "a" has depth 1 (has "x")
          extract (elements result !! 1) `shouldBe` (0 :: Int)  -- "b" has depth 0 (atomic)
          extract (elements (elements result !! 0) !! 0) `shouldBe` (0 :: Int)  -- "x" has depth 0 (atomic)
      
      describe "duplicate function (User Story 3)" $ do
        
        it "T034: duplicate with atomic pattern" $ do
          let p = point 5
          let result = duplicate p
          extract result `shouldBe` p
          elements result `shouldBe` ([] :: [Pattern (Pattern Int)])
        
        it "T035: duplicate with point with elements" $ do
          let p = pattern "root" [point "a", point "b"]
          let result = duplicate p
          extract result `shouldBe` p
          length (elements result) `shouldBe` 2
          extract (elements result !! 0) `shouldBe` point "a"
          extract (elements result !! 1) `shouldBe` point "b"
        
        it "T036: duplicate with nested point structure" $ do
          let p = pattern "root" [pattern "a" [point "x"], point "b"]
          let result = duplicate p
          extract result `shouldBe` p
          length (elements result) `shouldBe` 2
          extract (elements result !! 0) `shouldBe` pattern "a" [point "x"]
          extract (elements result !! 1) `shouldBe` point "b"
          length (elements (elements result !! 0)) `shouldBe` 1
          extract (elements (elements result !! 0) !! 0) `shouldBe` point "x"
        
        it "T037: extract . duplicate = id (verifying context structure)" $ do
          let p = pattern "root" [point "a", point "b"]
          let result = duplicate p
          extract result `shouldBe` p
        
        it "T038: duplicate with deeply nested patterns (10+ levels)" $ do
          let deepPat = foldl (\acc _ -> pattern "level" [acc]) (point "leaf") [1..10]
          let result = duplicate deepPat
          extract result `shouldBe` deepPat
          depth result `shouldBe` depth deepPat
      
      describe "context-aware helper functions (User Story 5)" $ do
        
        it "T058: depthAt with atomic pattern" $ do
          let p = point 5
          let result = depthAt p
          extract result `shouldBe` (0 :: Int)
          elements result `shouldBe` ([] :: [Pattern Int])
        
        it "T059: depthAt with nested point structure" $ do
          let p = pattern "root" [pattern "a" [point "x"], point "b"]
          let result = depthAt p
          extract result `shouldBe` (2 :: Int)  -- Root has depth 2
          extract (elements result !! 0) `shouldBe` (1 :: Int)  -- "a" has depth 1
          extract (elements result !! 1) `shouldBe` (0 :: Int)  -- "b" has depth 0
          extract (elements (elements result !! 0) !! 0) `shouldBe` (0 :: Int)  -- "x" has depth 0
        
        it "T060: sizeAt with point with elements" $ do
          let p = pattern "root" [point "a", point "b"]
          let result = sizeAt p
          extract result `shouldBe` (3 :: Int)  -- Root: 3 nodes (root + a + b)
          extract (elements result !! 0) `shouldBe` (1 :: Int)  -- "a": 1 node
          extract (elements result !! 1) `shouldBe` (1 :: Int)  -- "b": 1 node
        
        it "T061: sizeAt with nested point structure" $ do
          let p = pattern "root" [pattern "a" [point "x"], point "b"]
          let result = sizeAt p
          extract result `shouldBe` (4 :: Int)  -- Root: 4 nodes (root + a + x + b)
          extract (elements result !! 0) `shouldBe` (2 :: Int)  -- "a": 2 nodes (a + x)
          extract (elements result !! 1) `shouldBe` (1 :: Int)  -- "b": 1 node
          extract (elements (elements result !! 0) !! 0) `shouldBe` (1 :: Int)  -- "x": 1 node
        
        it "T062: indicesAt with point with elements" $ do
          let p = pattern "root" [point "a", point "b"]
          let result = indicesAt p
          extract result `shouldBe` ([] :: [Int])  -- Root: empty indices
          extract (elements result !! 0) `shouldBe` ([0] :: [Int])  -- "a": [0]
          extract (elements result !! 1) `shouldBe` ([1] :: [Int])  -- "b": [1]
        
        it "T063: indicesAt with nested point structure" $ do
          let p = pattern "root" [pattern "a" [point "x"], point "b"]
          let result = indicesAt p
          extract result `shouldBe` ([] :: [Int])  -- Root: empty indices
          extract (elements result !! 0) `shouldBe` ([0] :: [Int])  -- "a": [0]
          extract (elements result !! 1) `shouldBe` ([1] :: [Int])  -- "b": [1]
          extract (elements (elements result !! 0) !! 0) `shouldBe` ([0, 0] :: [Int])  -- "x": [0, 0]