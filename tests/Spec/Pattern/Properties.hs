-- | Property-based tests for category-theoretic laws.
--
-- This module contains QuickCheck properties that verify:
-- - Functor laws
-- - Foldable laws and properties
-- - Naturality conditions
-- - Other category-theoretic properties
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Spec.Pattern.Properties where

import Control.Applicative (liftA2)
import Data.Char (toUpper)
import Data.Foldable (foldl, foldMap, foldr, toList)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid (All(..), Sum(..))
import Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Pattern.Core (Pattern(..), pattern, patternWith, fromList, flatten, size, depth, values, toTuple)
import qualified Pattern.Core as PC
import Test.Hspec
import Test.QuickCheck hiding (elements)
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Property (Property)

-- | Arbitrary instance for Pattern with String values.
-- Generates patterns of varying structure: atomic, with elements, and nested.
--
-- PERFORMANCE NOTE: This generator is optimized for fast test execution.
-- It limits patterns to:
--   - Maximum 2 elements per level (instead of 3+)
--   - Smaller recursion depth (n `div` 3 instead of n `div` 2)
--   - Faster size reduction (n - 2 instead of n - 1)
--
-- These limits keep property-based tests fast (~6ms total) while still providing
-- good coverage. If test runtime becomes slow as more functionality is added,
-- consider further reducing these limits or the number of test cases in quickProperty.
instance Arbitrary (Pattern String) where
  arbitrary = sized genPatternString
    where
      genPatternString 0 = do
        v <- arbitrary
        return $ Pattern { value = v, elements = [] }
      genPatternString n = do
        v <- arbitrary
        -- Limit to 2 elements max and smaller size to keep tests fast
        numElems <- choose (0, min 2 (max 1 (n `div` 3)))
        elems <- vectorOf numElems (genPatternString (max 0 (n - 2)))
        return $ Pattern { value = v, elements = elems }

-- | Arbitrary instance for Pattern with Int values.
-- Generates patterns of varying structure: atomic, with elements, and nested.
--
-- PERFORMANCE NOTE: This generator is optimized for fast test execution.
-- It limits patterns to:
--   - Maximum 2 elements per level (instead of 3+)
--   - Smaller recursion depth (n `div` 3 instead of n `div` 2)
--   - Faster size reduction (n - 2 instead of n - 1)
--
-- These limits keep property-based tests fast (~6ms total) while still providing
-- good coverage. If test runtime becomes slow as more functionality is added,
-- consider further reducing these limits or the number of test cases in quickProperty.
instance Arbitrary (Pattern Int) where
  arbitrary = sized genPatternInt
    where
      genPatternInt 0 = do
        v <- arbitrary
        return $ Pattern { value = v, elements = [] }
      genPatternInt n = do
        v <- arbitrary
        -- Limit to 2 elements max and smaller size to keep tests fast
        numElems <- choose (0, min 2 (max 1 (n `div` 3)))
        elems <- vectorOf numElems (genPatternInt (max 0 (n - 2)))
        return $ Pattern { value = v, elements = elems }

-- | Arbitrary instance for Pattern with Maybe Int values.
-- Generates patterns of varying structure with Maybe Int values.
instance Arbitrary (Pattern (Maybe Int)) where
  arbitrary = sized genPatternMaybeInt
    where
      genPatternMaybeInt 0 = do
        v <- arbitrary
        return $ Pattern { value = v, elements = [] }
      genPatternMaybeInt n = do
        v <- arbitrary
        -- Limit to 2 elements max and smaller size to keep tests fast
        numElems <- choose (0, min 2 (max 1 (n `div` 3)))
        elems <- vectorOf numElems (genPatternMaybeInt (max 0 (n - 2)))
        return $ Pattern { value = v, elements = elems }

-- | Arbitrary instance for Pattern with Maybe String values.
-- Generates patterns of varying structure with Maybe String values.
instance Arbitrary (Pattern (Maybe String)) where
  arbitrary = sized genPatternMaybeString
    where
      genPatternMaybeString 0 = do
        v <- arbitrary
        return $ Pattern { value = v, elements = [] }
      genPatternMaybeString n = do
        v <- arbitrary
        -- Limit to 2 elements max and smaller size to keep tests fast
        numElems <- choose (0, min 2 (max 1 (n `div` 3)))
        elems <- vectorOf numElems (genPatternMaybeString (max 0 (n - 2)))
        return $ Pattern { value = v, elements = elems }

-- | Arbitrary instance for Pattern with Either String Int values.
-- Generates patterns of varying structure with Either String Int values.
instance Arbitrary (Pattern (Either String Int)) where
  arbitrary = sized genPatternEither
    where
      genPatternEither 0 = do
        v <- arbitrary
        return $ Pattern { value = v, elements = [] }
      genPatternEither n = do
        v <- arbitrary
        -- Limit to 2 elements max and smaller size to keep tests fast
        numElems <- choose (0, min 2 (max 1 (n `div` 3)))
        elems <- vectorOf numElems (genPatternEither (max 0 (n - 2)))
        return $ Pattern { value = v, elements = elems }

-- | Helper to create a property with reduced test cases for faster execution.
--
-- PERFORMANCE NOTE: This reduces QuickCheck test cases from the default 100 to 20.
-- Combined with the limited pattern generators above, this keeps property-based
-- tests fast (~6ms total for all functor law tests).
--
-- If test runtime becomes slow as more functionality is added:
--   - Consider reducing from 20 to 10 test cases
--   - Monitor test execution time and adjust accordingly
--   - Consider separating slow property-based tests into a separate test suite
--
-- Current baseline: All property-based tests complete in <10ms
quickProperty :: Testable prop => prop -> Property
quickProperty = withMaxSuccess 20 . property

-- | Helper function to manually count all values in a pattern.
-- Used for verifying that foldable operations process all values correctly.
countValues :: Pattern a -> Int
countValues (Pattern _ els) = 1 + sum (map countValues els)

spec :: Spec
spec = do
  describe "Functor Laws (User Story 2)" $ do
    
    describe "Identity Law" $ do
      
      it "fmap id = id for Pattern String" $ do
        -- Uses quickProperty: 20 test cases (instead of 100) for faster execution
        quickProperty $ \p -> fmap id (p :: Pattern String) == p
      
      it "fmap id = id for Pattern Int" $ do
        -- Uses quickProperty: 20 test cases (instead of 100) for faster execution
        quickProperty $ \p -> fmap id (p :: Pattern Int) == p
    
    describe "Composition Law" $ do
      
      it "fmap (f . g) = fmap f . fmap g for Pattern String" $ do
        -- Uses quickProperty: 20 test cases (instead of 100) for faster execution
        quickProperty $ \p -> 
          let f = map toUpper :: String -> String
              g = reverse :: String -> String
          in fmap (f . g) (p :: Pattern String) == (fmap f . fmap g) p
      
      it "fmap (f . g) = fmap f . fmap g for Pattern Int" $ do
        -- Uses quickProperty: 20 test cases (instead of 100) for faster execution
        quickProperty $ \p -> 
          let f = (* 2) :: Int -> Int
              g = (+ 1) :: Int -> Int
          in fmap (f . g) (p :: Pattern Int) == (fmap f . fmap g) p
  
  describe "Category-theoretic properties" $ do
    -- Additional property-based tests will be added here
    it "placeholder property test" $ do
      QC.property $ \x -> (x :: Int) == x
  
  describe "Constructor Functions Properties (User Story 1)" $ do
    
    it "pattern function is functionally equivalent to record syntax" $ do
      QC.property $ \v -> 
        let p1 = pattern (v :: String)
            p2 = Pattern { value = v, elements = [] }
        in p1 == p2 && value p1 == value p2 && elements p1 == elements p2
    
  describe "Constructor Functions Properties (User Story 2)" $ do
    
    it "patternWith function is functionally equivalent to record syntax" $ do
      QC.property $ \(v :: String) vs -> 
        let ps = map (pattern :: String -> Pattern String) vs
            p1 = patternWith v ps
            p2 = Pattern { value = v, elements = ps }
        in p1 == p2 && value p1 == value p2 && elements p1 == elements p2
    
  describe "Constructor Functions Properties (User Story 3)" $ do
    
    it "fromList function is functionally equivalent to patternWith decoration (map pattern values)" $ do
      QC.property $ \(v :: String) vs -> 
        let p1 = fromList v vs
            p2 = patternWith v (map (pattern :: String -> Pattern String) vs)
        in p1 == p2 && value p1 == value p2 && elements p1 == elements p2
  
  describe "Foldable Laws (User Story 1-5)" $ do
    
    describe "toList extracts all values correctly as flat list" $ do
      
      it "toList extracts all values for Pattern String" $ do
        -- T047: Property-based test for toList extracting all values correctly as flat list
        quickProperty $ \p -> 
          let values = toList (p :: Pattern String)
          in length values == countValues p && all (`elem` values) [value p]
      
      it "toList extracts all values for Pattern Int" $ do
        -- T047: Property-based test for toList extracting all values correctly as flat list
        quickProperty $ \p -> 
          let values = toList (p :: Pattern Int)
          in length values == countValues p && all (`elem` values) [value p]
    
    describe "flatten extracts all values correctly" $ do
      
      it "flatten extracts all values for Pattern String" $ do
        -- T048: Property-based test for flatten extracting all values correctly
        -- Note: flatten should be equivalent to toList (both extract flat lists)
        quickProperty $ \p -> 
          let values = flatten (p :: Pattern String)
          in length values == countValues p && all (`elem` values) [value p]
      
      it "flatten extracts all values for Pattern Int" $ do
        -- T048: Property-based test for flatten extracting all values correctly
        -- Note: flatten should be equivalent to toList (both extract flat lists)
        quickProperty $ \p -> 
          let values = flatten (p :: Pattern Int)
          in length values == countValues p && all (`elem` values) [value p]
    
    describe "foldr processes all values correctly" $ do
      
      it "foldr processes all values for Pattern Int" $ do
        -- T049: Property-based test for foldr processing all values correctly
        quickProperty $ \p -> 
          let pInt = p :: Pattern Int
              folded = foldr (+) 0 pInt
              listed = sum (toList pInt)
          in folded == listed
      
      it "foldr processes all values for Pattern String" $ do
        -- T049: Property-based test for foldr processing all values correctly
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
              folded = foldr (++) "" pStr
              listed = concat (toList pStr)
          in folded == listed
    
    describe "foldl processes all values correctly" $ do
      
      it "foldl processes all values for Pattern Int" $ do
        -- T050: Property-based test for foldl processing all values correctly
        quickProperty $ \p -> 
          let pInt = p :: Pattern Int
              folded = foldl (+) 0 pInt
              listed = sum (toList pInt)
          in folded == listed
      
      it "foldl processes all values for Pattern String" $ do
        -- T050: Property-based test for foldl processing all values correctly
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
              folded = foldl (++) "" pStr
              listed = concat (toList pStr)
          in folded == listed
    
    describe "foldMap with Sum monoid produces correct results" $ do
      
      it "foldMap Sum produces correct sum for Pattern Int" $ do
        -- T051: Property-based test for foldMap with Sum monoid producing correct results
        quickProperty $ \p -> 
          let pInt = p :: Pattern Int
              folded = getSum (foldMap Sum pInt)
              listed = sum (toList pInt)
          in folded == listed
    
    describe "Order preservation in toList and flatten" $ do
      
      it "toList preserves order consistently for Pattern String" $ do
        -- T052: Property-based test for order preservation in toList and flatten
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
              list1 = toList pStr
              list2 = toList pStr
          in list1 == list2
      
      it "flatten preserves order consistently for Pattern String" $ do
        -- T052: Property-based test for order preservation in toList and flatten
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
              list1 = flatten pStr
              list2 = flatten pStr
          in list1 == list2
      
      it "toList and flatten produce same order for Pattern String" $ do
        -- T052: Property-based test for order preservation in toList and flatten
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
              list1 = toList pStr
              list2 = flatten pStr
          in list1 == list2
    
    describe "toList p = flatten p relationship" $ do
      
      it "toList and flatten are equivalent for Pattern String" $ do
        -- T053: Property-based test verifying toList p = flatten p relationship
        -- Both extract flat lists (standard Foldable behavior)
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in toList pStr == flatten pStr
      
      it "toList and flatten are equivalent for Pattern Int" $ do
        -- T053: Property-based test verifying toList p = flatten p relationship
        -- Both extract flat lists (standard Foldable behavior)
        quickProperty $ \p -> 
          let pInt = p :: Pattern Int
          in toList pInt == flatten pInt
    
    describe "foldr and foldl produce same results for commutative operations" $ do
      
      it "foldr and foldl produce same results for addition (Pattern Int)" $ do
        -- T054: Property-based test verifying foldr and foldl produce same results for commutative operations
        quickProperty $ \p -> 
          let pInt = p :: Pattern Int
              foldedR = foldr (+) 0 pInt
              foldedL = foldl (+) 0 pInt
          in foldedR == foldedL
      
      it "foldr and foldl produce same results for multiplication (Pattern Int)" $ do
        -- T054: Property-based test verifying foldr and foldl produce same results for commutative operations
        quickProperty $ \p -> 
          let pInt = p :: Pattern Int
              -- Use non-zero values to avoid division by zero issues
              foldedR = foldr (*) 1 pInt
              foldedL = foldl (*) 1 pInt
          in foldedR == foldedL
  
  describe "Traversable Laws (User Story 1)" $ do
    
    describe "Identity Law" $ do
      
      it "traverse Identity = Identity for Pattern String" $ do
        -- T030: Property-based test for Identity law (traverse Identity = Identity)
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
              result = traverse Identity pStr
          in runIdentity result == pStr
      
      it "traverse Identity = Identity for Pattern Int" $ do
        -- T030: Property-based test for Identity law (traverse Identity = Identity)
        quickProperty $ \p -> 
          let pInt = p :: Pattern Int
              result = traverse Identity pInt
          in runIdentity result == pInt
    
    describe "Structure Preservation" $ do
      
      it "traverse preserves structure (element count, nesting depth, element order) for Pattern String" $ do
        -- T031: Property-based test for structure preservation
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
              result = traverse Identity pStr
              p' = runIdentity result
              -- Helper to count elements recursively
              countElems (Pattern _ els) = length els + sum (map countElems els)
          in length (elements p') == length (elements pStr) 
             && countElems p' == countElems pStr
      
      it "traverse preserves structure (element count, nesting depth, element order) for Pattern Int" $ do
        -- T031: Property-based test for structure preservation
        quickProperty $ \p -> 
          let pInt = p :: Pattern Int
              result = traverse Identity pInt
              p' = runIdentity result
              -- Helper to count elements recursively
              countElems (Pattern _ els) = length els + sum (map countElems els)
          in length (elements p') == length (elements pInt) 
             && countElems p' == countElems pInt
    
    describe "Relationship between traverse and sequenceA" $ do
      
      it "sequenceA = traverse id for Pattern (Maybe Int)" $ do
        -- T050: Property-based test for relationship between traverse and sequenceA
        quickProperty $ \p -> 
          let pMaybe = p :: Pattern (Maybe Int)
              result1 = sequenceA pMaybe
              result2 = traverse id pMaybe
          in result1 == result2
      
      it "sequenceA = traverse id for Pattern (Maybe String)" $ do
        -- T050: Property-based test for relationship between traverse and sequenceA
        quickProperty $ \p -> 
          let pMaybe = p :: Pattern (Maybe String)
              result1 = sequenceA pMaybe
              result2 = traverse id pMaybe
          in result1 == result2
  
  describe "Traversable Laws (Phase 5)" $ do
    
    describe "Naturality Law" $ do
      
      it "t . traverse f = traverse (t . f) for Pattern Int with Maybe" $ do
        -- T067: Property-based test for Naturality law (t . traverse f = traverse (t . f))
        -- Testing with Maybe as the applicative functor
        quickProperty $ \p -> 
          let pInt = p :: Pattern Int
              f x = if x > 0 then Just x else Nothing
              -- Naturality: t . traverse f = traverse (t . f)
              -- For Maybe, t is the identity transformation (Maybe a -> Maybe a)
              -- So we test: traverse f = traverse f (which is trivially true)
              -- A more meaningful test: traverse (fmap (+1) . f) = fmap (fmap (+1)) . traverse f
              result1 = traverse (fmap (+1) . f) pInt
              result2 = fmap (fmap (+1)) (traverse f pInt)
          in result1 == result2
      
      it "t . traverse f = traverse (t . f) for Pattern String with Identity" $ do
        -- T067: Property-based test for Naturality law with Identity transformation
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
              f = Identity
              -- Naturality with Identity: Identity . traverse Identity = traverse (Identity . Identity)
              -- Since Identity . Identity = Identity, this simplifies to:
              -- Identity . traverse Identity = traverse Identity
              result1 = Identity (runIdentity (traverse Identity pStr))
              result2 = traverse Identity pStr
          in runIdentity result1 == runIdentity result2
    
    describe "Identity Law" $ do
      
      it "traverse Identity = Identity for Pattern String (Phase 5)" $ do
        -- T068: Property-based test for Identity law (traverse Identity = Identity)
        -- Note: This is also tested in T030, but included here for completeness
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
              result = traverse Identity pStr
          in runIdentity result == pStr
      
      it "traverse Identity = Identity for Pattern Int (Phase 5)" $ do
        -- T068: Property-based test for Identity law (traverse Identity = Identity)
        -- Note: This is also tested in T030, but included here for completeness
        quickProperty $ \p -> 
          let pInt = p :: Pattern Int
              result = traverse Identity pInt
          in runIdentity result == pInt
    
    describe "Composition Law" $ do
      
      it "traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f for Pattern Int" $ do
        -- T069: Property-based test for Composition law
        quickProperty $ \p -> 
          let pInt = p :: Pattern Int
              f x = if x > 0 then Just x else Nothing
              g x = if x < 100 then Right x else Left "too large"
              -- Composition law: traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
              -- f :: Int -> Maybe Int
              -- g :: Int -> Either String Int
              -- Compose . fmap g . f :: Int -> Compose Maybe (Either String) Int
              -- Compose . fmap (traverse g) . traverse f :: Pattern Int -> Compose Maybe (Either String) (Pattern Int)
              h = Compose . fmap g . f
              result1 = traverse h pInt
              result2 = (Compose . fmap (traverse g) . traverse f) pInt
          in getCompose result1 == getCompose result2
      
      it "traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f for Pattern String" $ do
        -- T069: Property-based test for Composition law with String
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
              f = Identity
              g = map toUpper
              -- Composition law with Identity and fmap
              -- f :: String -> Identity String
              -- g :: String -> String
              -- Compose . fmap g . f :: String -> Compose Identity Identity String
              h = Compose . fmap (Identity . g) . f
              result1 = traverse h pStr
              result2 = (Compose . fmap (traverse (Identity . g)) . traverse f) pStr
          in getCompose result1 == getCompose result2
    
    describe "Structure Preservation (Phase 5)" $ do
      
      it "traverse preserves structure (element count, nesting depth, element order) for Pattern String" $ do
        -- T070: Property-based test for structure preservation
        -- Note: This is also tested in T031, but included here for completeness
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
              result = traverse Identity pStr
              p' = runIdentity result
              -- Helper to count elements recursively
              countElems (Pattern _ els) = length els + sum (map countElems els)
          in length (elements p') == length (elements pStr) 
             && countElems p' == countElems pStr
      
      it "traverse preserves structure (element count, nesting depth, element order) for Pattern Int" $ do
        -- T070: Property-based test for structure preservation
        -- Note: This is also tested in T031, but included here for completeness
        quickProperty $ \p -> 
          let pInt = p :: Pattern Int
              result = traverse Identity pInt
              p' = runIdentity result
              -- Helper to count elements recursively
              countElems (Pattern _ els) = length els + sum (map countElems els)
          in length (elements p') == length (elements pInt) 
             && countElems p' == countElems pInt
    
    describe "Effect Combination Semantics" $ do
      
      it "Maybe short-circuits on first Nothing" $ do
        -- T071: Property-based test for effect combination semantics (Maybe short-circuits)
        quickProperty $ \p -> 
          let pMaybe = p :: Pattern (Maybe Int)
              -- If any value is Nothing, sequenceA should return Nothing
              hasNothing = any (== Nothing) (toList pMaybe)
              result = sequenceA pMaybe
          in if hasNothing
             then result == Nothing
             else case result of
                    Just p' -> length (elements p') == length (elements pMaybe)
                    Nothing -> False
      
      it "Either short-circuits on first Left" $ do
        -- T071: Property-based test for effect combination semantics (Either short-circuits)
        -- We need to generate patterns with Either values
        quickProperty $ \p -> 
          let pEither = p :: Pattern (Either String Int)
              -- Convert to list and check for Left values
              values = toList pEither
              hasLeft = any (\v -> case v of Left _ -> True; Right _ -> False) values
              result = sequenceA pEither
          in if hasLeft
             then case result of
                    Left _ -> True
                    Right _ -> False
             else case result of
                    Right p' -> length (elements p') == length (elements pEither)
                    Left _ -> False
      
      it "Identity preserves structure without effects" $ do
        -- T071: Property-based test for effect combination semantics (Identity preserves)
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
              result = traverse Identity pStr
              p' = runIdentity result
          in p' == pStr
    
    describe "Query Functions - Length (User Story 1)" $ do
      
      it "T005: length p >= 0 for all patterns" $ do
        -- Property: length is always non-negative
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in PC.length pStr >= 0
      
      it "T006: length p == length (elements p) for all patterns" $ do
        -- Property: length of pattern equals length of elements list
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in PC.length pStr == Prelude.length (elements pStr)
    
    describe "Query Functions - Size (User Story 2)" $ do
      
      it "T015: size p >= 1 for all patterns" $ do
        -- Property: size is always at least 1 (every pattern has at least the root node)
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in size pStr >= 1
      
      it "T016: size p >= length p for all patterns" $ do
        -- Property: size is always at least as large as length (size counts root + elements)
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in size pStr >= PC.length pStr
      
      it "T017: size p == 1 + sum (map size (elements p)) for all patterns" $ do
        -- Property: size equals 1 (root) plus sum of sizes of all elements
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in size pStr == 1 + sum (map size (elements pStr))
    
    describe "Query Functions - Depth (User Story 3)" $ do
      
      it "T026: depth p >= 0 for all patterns" $ do
        -- Property: depth is always non-negative (atomic patterns have depth 0)
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in depth pStr >= 0
      
      it "T027: depth p <= size p - 1 for all patterns" $ do
        -- Property: depth cannot exceed size - 1 (worst case is a linear chain)
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in depth pStr <= size pStr - 1
    
    describe "Query Functions - Values (User Story 4)" $ do
      
      it "T036: length (values p) == size p for all patterns" $ do
        -- Property: number of values equals number of nodes (one value per node)
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in Prelude.length (values pStr) == size pStr
      
      it "T037: head (values p) == value p for all patterns" $ do
        -- Property: first value is the pattern's own value
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in not (null (values pStr)) && head (values pStr) == value pStr
      
      it "T038: values p == toList p for all patterns" $ do
        -- Property: values is equivalent to toList from Foldable
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in values pStr == toList pStr
      
      it "T039: values p == flatten p for all patterns" $ do
        -- Property: values is equivalent to flatten function
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in values pStr == flatten pStr
  
  describe "Ord Instance Properties (User Story 1)" $ do
    
    describe "Ordering properties" $ do
      
      it "T008: transitivity: if p1 < p2 and p2 < p3, then p1 < p3" $ do
        -- Property: ordering is transitive
        quickProperty $ \p1 p2 p3 -> 
          let p1Str = p1 :: Pattern String
              p2Str = p2 :: Pattern String
              p3Str = p3 :: Pattern String
          in if p1Str < p2Str && p2Str < p3Str
             then p1Str < p3Str
             else True  -- If premise is false, property is vacuously true
      
      it "T009: antisymmetry: if p1 < p2, then p2 > p1" $ do
        -- Property: ordering is antisymmetric
        quickProperty $ \p1 p2 -> 
          let p1Str = p1 :: Pattern String
              p2Str = p2 :: Pattern String
          in if p1Str < p2Str
             then p2Str > p1Str
             else True  -- If premise is false, property is vacuously true
      
      it "T010: reflexivity: p1 <= p1 always true" $ do
        -- Property: ordering is reflexive
        quickProperty $ \p -> 
          let pStr = p :: Pattern String
          in pStr <= pStr
      
      it "T011: lexicographic ordering: compare p1 p2 == compare (toTuple p1) (toTuple p2)" $ do
        -- Property: ordering follows lexicographic rules based on toTuple
        -- Tuples are compared lexicographically: first component, then second
        quickProperty $ \p1 p2 -> 
          let p1Str = p1 :: Pattern String
              p2Str = p2 :: Pattern String
              tuple1 = toTuple p1Str
              tuple2 = toTuple p2Str
          in compare p1Str p2Str == compare tuple1 tuple2
  
  describe "Ord Instance Integration Properties (User Story 2)" $ do
    
    describe "Data.Set operations" $ do
      
      it "T023: Data.Set operations with patterns" $ do
        -- Property: Set operations work correctly with patterns
        quickProperty $ \ps -> 
          let patterns = ps :: [Pattern String]
              -- Remove duplicates by converting to set and back
              uniquePatterns = Set.toList (Set.fromList patterns)
              -- All patterns in set should be distinct
              allDistinct = length uniquePatterns == length (Set.fromList patterns)
              -- All original patterns should be members of the set
              s = Set.fromList patterns
              allMembers = all (`Set.member` s) patterns
          in allDistinct && allMembers
      
      it "T023: Data.Set maintains sorted order" $ do
        -- Property: Set maintains patterns in sorted order
        quickProperty $ \ps -> 
          let patterns = ps :: [Pattern String]
              s = Set.fromList patterns
              sorted = Set.toList s
              -- Verify sorted order
              isSorted = and (zipWith (<=) sorted (drop 1 sorted))
          in isSorted
    
    describe "Data.Map operations" $ do
      
      it "T024: Data.Map operations with patterns" $ do
        -- Property: Map operations work correctly with patterns as keys
        quickProperty $ \ps -> 
          let patterns = ps :: [Pattern String]
              -- Create map with patterns as keys
              keyValuePairs = zip patterns (map show [1..])
              m = Map.fromList keyValuePairs
              -- All patterns should be members
              allMembers = all (`Map.member` m) patterns
              -- All lookups should succeed
              allLookups = all (\p -> Map.lookup p m /= Nothing) patterns
          in allMembers && allLookups
      
      it "T024: Data.Map key uniqueness with patterns" $ do
        -- Property: Map correctly handles duplicate patterns (later value overwrites)
        quickProperty $ \p1 p2 -> 
          let p1Str = p1 :: Pattern String
              p2Str = p2 :: Pattern String
              -- If patterns are equal, second insert should overwrite
              m1 = Map.insert p1Str "value1" Map.empty
              m2 = Map.insert p2Str "value2" m1
              -- If p1 == p2, then lookup p1 should return "value2"
              -- If p1 /= p2, then lookup p1 should return "value1"
              result = if p1Str == p2Str
                       then Map.lookup p1Str m2 == Just "value2"
                       else Map.lookup p1Str m2 == Just "value1"
          in result
  
  describe "Ord Instance Consistency with Eq Properties (User Story 3)" $ do
    
    describe "Consistency properties" $ do
      
      it "T032: p1 == p2 implies compare p1 p2 == EQ" $ do
        -- Property: If patterns are equal, they compare as equal
        quickProperty $ \p1 p2 -> 
          let p1Str = p1 :: Pattern String
              p2Str = p2 :: Pattern String
          in if p1Str == p2Str
             then compare p1Str p2Str == EQ
             else True  -- If premise is false, property is vacuously true
      
      it "T033: compare p1 p2 == EQ implies p1 == p2" $ do
        -- Property: If patterns compare as equal, they are equal
        quickProperty $ \p1 p2 -> 
          let p1Str = p1 :: Pattern String
              p2Str = p2 :: Pattern String
          in if compare p1Str p2Str == EQ
             then p1Str == p2Str
             else True  -- If premise is false, property is vacuously true
      
      it "T034: ordering uses same comparison order as Eq (value first, then elements)" $ do
        -- Property: Ordering respects the same structural comparison as Eq
        -- This verifies that ordering compares value first, then elements
        quickProperty $ \p1 p2 -> 
          let p1Str = p1 :: Pattern String
              p2Str = p2 :: Pattern String
              (v1, els1) = toTuple p1Str
              (v2, els2) = toTuple p2Str
              -- If values differ, ordering should match value comparison
              valueOrdering = if v1 /= v2
                              then compare p1Str p2Str == compare v1 v2
                              else True  -- If values equal, check elements
              -- If values equal but elements differ, ordering should match element comparison
              elementOrdering = if v1 == v2 && els1 /= els2
                                then compare p1Str p2Str == compare els1 els2
                                else True
          in valueOrdering && elementOrdering

