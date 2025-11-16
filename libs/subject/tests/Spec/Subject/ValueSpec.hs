-- | Unit tests for Subject.Value module.
module Spec.Subject.ValueSpec where

import Data.Hashable (hash)
import Data.Map (fromList)
import Test.Hspec
import Subject.Value (RangeValue (..), Value (..))

spec :: Spec
spec = do
  describe "Subject.Value" $ do
    
    describe "Value Types" $ do
      
      describe "Standard Types" $ do
        
        it "creates VInteger value" $ do
          let v = VInteger 42
          v `shouldBe` VInteger 42
        
        it "creates VDecimal value" $ do
          let v = VDecimal 3.14
          v `shouldBe` VDecimal 3.14
        
        it "creates VBoolean value" $ do
          let v = VBoolean True
          v `shouldBe` VBoolean True
          let v2 = VBoolean False
          v2 `shouldBe` VBoolean False
        
        it "creates VString value" $ do
          let v = VString "hello"
          v `shouldBe` VString "hello"
        
        it "creates VSymbol value" $ do
          let v = VSymbol "mySymbol"
          v `shouldBe` VSymbol "mySymbol"
      
      describe "Extended Types" $ do
        
        it "creates VTaggedString value" $ do
          let v = VTaggedString "url" "https://example.com"
          v `shouldBe` VTaggedString "url" "https://example.com"
        
        it "creates VArray value" $ do
          let v = VArray [VInteger 1, VInteger 2, VInteger 3]
          v `shouldBe` VArray [VInteger 1, VInteger 2, VInteger 3]
        
        it "creates VMap value" $ do
          let v = VMap (fromList [("key1", VString "value1"), ("key2", VInteger 42)])
          v `shouldBe` VMap (fromList [("key1", VString "value1"), ("key2", VInteger 42)])
        
        it "creates VRange value" $ do
          let v = VRange (RangeValue (Just 1) (Just 10))
          v `shouldBe` VRange (RangeValue (Just 1) (Just 10))
        
        it "creates VMeasurement value" $ do
          let v = VMeasurement "kg" 5.0
          v `shouldBe` VMeasurement "kg" 5.0
    
    describe "RangeValue" $ do
      
      describe "Range types" $ do
        
        it "creates closed range" $ do
          let r = RangeValue (Just 1) (Just 10)
          lower r `shouldBe` Just 1
          upper r `shouldBe` Just 10
        
        it "creates lower-bound-only range" $ do
          let r = RangeValue (Just 1) Nothing
          lower r `shouldBe` Just 1
          upper r `shouldBe` Nothing
        
        it "creates upper-bound-only range" $ do
          let r = RangeValue Nothing (Just 10)
          lower r `shouldBe` Nothing
          upper r `shouldBe` Just 10
        
        it "creates open range" $ do
          let r = RangeValue Nothing Nothing
          lower r `shouldBe` Nothing
          upper r `shouldBe` Nothing
    
    describe "Nested Values" $ do
      
      describe "Nested arrays" $ do
        
        it "creates array of arrays" $ do
          let v = VArray [VArray [VInteger 1, VInteger 2], VArray [VInteger 3, VInteger 4]]
          v `shouldBe` VArray [VArray [VInteger 1, VInteger 2], VArray [VInteger 3, VInteger 4]]
      
      describe "Nested maps" $ do
        
        it "creates map with array values" $ do
          let v = VMap (fromList [("numbers", VArray [VInteger 1, VInteger 2, VInteger 3])])
          v `shouldBe` VMap (fromList [("numbers", VArray [VInteger 1, VInteger 2, VInteger 3])])
        
        it "creates map with map values" $ do
          let v = VMap (fromList [("nested", VMap (fromList [("key", VString "value")]))])
          v `shouldBe` VMap (fromList [("nested", VMap (fromList [("key", VString "value")]))])
    
    describe "Typeclass Instances" $ do
      
      describe "Eq instance" $ do
        
        it "equal values are equal" $ do
          let v1 = VInteger 42
          let v2 = VInteger 42
          v1 `shouldBe` v2
        
        it "different values are not equal" $ do
          let v1 = VInteger 42
          let v2 = VInteger 43
          v1 `shouldNotBe` v2
        
        it "different value types are not equal" $ do
          let v1 = VInteger 42
          let v2 = VString "42"
          v1 `shouldNotBe` v2
      
      describe "Ord instance" $ do
        
        it "orders values correctly" $ do
          let v1 = VInteger 1
          let v2 = VInteger 2
          v1 < v2 `shouldBe` True
          v2 > v1 `shouldBe` True
      
      describe "Show instance" $ do
        
        it "shows values correctly" $ do
          show (VInteger 42) `shouldContain` "VInteger"
          show (VString "hello") `shouldContain` "VString"
          show (VBoolean True) `shouldContain` "VBoolean"
      
      describe "Hashable instance" $ do
        
        it "hashes values consistently" $ do
          let v = VInteger 42
          hash v `shouldBe` hash v
        
        it "hashes equal values to the same value" $ do
          let v1 = VInteger 42
          let v2 = VInteger 42
          hash v1 `shouldBe` hash v2
    
    describe "Edge Cases" $ do
      
      describe "Empty arrays" $ do
        
        it "creates empty array" $ do
          let v = VArray []
          v `shouldBe` VArray []
      
      describe "Empty maps" $ do
        
        it "creates empty map" $ do
          let v = VMap (fromList [])
          v `shouldBe` VMap (fromList [])
      
      describe "Complex nested structures" $ do
        
        it "creates deeply nested structure" $ do
          let v = VArray
                [ VMap (fromList [("a", VInteger 1)])
                , VMap (fromList [("b", VArray [VString "x", VString "y"])])
                ]
          v `shouldBe` VArray
                [ VMap (fromList [("a", VInteger 1)])
                , VMap (fromList [("b", VArray [VString "x", VString "y"])])
                ]

