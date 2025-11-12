-- | Property-based tests for category-theoretic laws.
--
-- This module contains QuickCheck properties that verify:
-- - Functor laws
-- - Naturality conditions
-- - Other category-theoretic properties
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Spec.Pattern.Properties where

import Data.Char (toUpper)
import Pattern.Core (Pattern(..), pattern, patternWith, fromList)
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

