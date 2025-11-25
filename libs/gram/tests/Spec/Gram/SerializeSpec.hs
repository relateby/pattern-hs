-- | Unit tests for Gram.Serialize module.
module Spec.Gram.SerializeSpec where

import Test.Hspec
import Gram.Serialize (toGram)
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import Subject.Value (Value(..), RangeValue(..))
import Data.Map (empty, fromList)
import Data.Set (Set)
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "Gram.Serialize" $ do
    
    describe "toGram" $ do
      
      describe "simple subject serialization" $ do
        it "serializes subject with identity and single label" $ do
          let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n:Person)"
        
        it "serializes subject with identity and multiple labels" $ do
          let s = Subject (Symbol "r") (Set.fromList ["KNOWS", "RELATIONSHIP"]) empty
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(r:KNOWS:RELATIONSHIP)"
        
        it "serializes subject with identity only (no labels)" $ do
          let s = Subject (Symbol "n") Set.empty empty
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n)"
        
        it "serializes anonymous subject (empty Symbol) with label" $ do
          let s = Subject (Symbol "") (Set.fromList ["Person"]) empty
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(:Person)"
        
        it "serializes anonymous subject (empty Symbol) without label" $ do
          let s = Subject (Symbol "") Set.empty empty
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "()"
      
      describe "subject with standard value types" $ do
        it "serializes subject with integer property" $ do
          let props = fromList [("age", VInteger 30)]
          let s = Subject (Symbol "n") (Set.fromList ["Person"]) props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n:Person {age:30})"
        
        it "serializes subject with decimal property" $ do
          let props = fromList [("pi", VDecimal 3.14)]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n {pi:3.14})"
        
        it "serializes subject with boolean property" $ do
          let props = fromList [("active", VBoolean True), ("verified", VBoolean False)]
          let s = Subject (Symbol "n") (Set.fromList ["User"]) props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n:User {active:true,verified:false})"
        
        it "serializes subject with string property" $ do
          let props = fromList [("name", VString "Alice")]
          let s = Subject (Symbol "n") (Set.fromList ["Person"]) props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n:Person {name:\"Alice\"})"
        
        it "serializes subject with symbol property" $ do
          let props = fromList [("type", VSymbol "Person")]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n {type:Person})"
        
        it "serializes subject with multiple standard value properties" $ do
          let props = fromList 
                [ ("name", VString "Alice")
                , ("age", VInteger 30)
                , ("active", VBoolean True)
                ]
          let s = Subject (Symbol "n") (Set.fromList ["Person"]) props
          let p = Pattern { value = s, elements = [] }
          let result = toGram p
          -- Property order may vary (Map doesn't preserve order)
          result `shouldContain` "(n:Person {"
          result `shouldContain` "name:\"Alice\""
          result `shouldContain` "age:30"
          result `shouldContain` "active:true"
          result `shouldContain` "})"
        
        it "serializes string with special characters (quotes, escapes)" $ do
          let props = fromList [("quote", VString "He said \"Hello\"")]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n {quote:\"He said \\\"Hello\\\"\"})"
      
      describe "subject with extended value types" $ do
        it "serializes subject with tagged string property" $ do
          let props = fromList [("url", VTaggedString "url" "https://example.com")]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n {url:url`https://example.com`})"
        
        it "serializes subject with array property" $ do
          let props = fromList [("tags", VArray [VString "admin", VString "user"])]
          let s = Subject (Symbol "n") (Set.fromList ["User"]) props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n:User {tags:[\"admin\",\"user\"]})"
        
        it "serializes subject with map property" $ do
          let props = fromList [("metadata", VMap (fromList [("key1", VString "value1"), ("key2", VInteger 42)]))]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n {metadata:{key1:\"value1\",key2:42}})"
        
        it "serializes subject with range property (closed range)" $ do
          let props = fromList [("age", VRange (RangeValue (Just 18) (Just 65)))]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n {age:18..65})"
        
        it "serializes subject with range property (lower bound only)" $ do
          let props = fromList [("age", VRange (RangeValue (Just 18) Nothing))]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n {age:18...})"
        
        it "serializes subject with range property (upper bound only)" $ do
          let props = fromList [("age", VRange (RangeValue Nothing (Just 65)))]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n {age:...65})"
        
        it "serializes subject with measurement property" $ do
          let props = fromList [("weight", VMeasurement "kg" 70.5)]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n {weight:70.5kg})"
        
        it "serializes nested arrays and maps" $ do
          let props = fromList 
                [ ("nested", VArray [VMap (fromList [("key", VString "value")])])
                ]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n {nested:[{key:\"value\"}]})"
      
      describe "nested pattern serialization" $ do
        it "serializes pattern with single nested element" $ do
          let inner = Pattern { value = Subject (Symbol "a") (Set.fromList ["Person"]) empty, elements = [] }
          let outer = Pattern { value = Subject (Symbol "g") Set.empty empty, elements = [inner] }
          toGram outer `shouldBe` "(g (a:Person))"
        
        it "serializes pattern with multiple nested elements" $ do
          let elem1 = Pattern { value = Subject (Symbol "a") (Set.fromList ["Person"]) empty, elements = [] }
          let elem2 = Pattern { value = Subject (Symbol "b") (Set.fromList ["Person"]) empty, elements = [] }
          let outer = Pattern { value = Subject (Symbol "g") Set.empty empty, elements = [elem1, elem2] }
          toGram outer `shouldBe` "(g (a:Person) (b:Person))"
        
        it "serializes deeply nested patterns" $ do
          let level3 = Pattern { value = Subject (Symbol "c") Set.empty empty, elements = [] }
          let level2 = Pattern { value = Subject (Symbol "b") Set.empty empty, elements = [level3] }
          let level1 = Pattern { value = Subject (Symbol "a") Set.empty empty, elements = [level2] }
          let root = Pattern { value = Subject (Symbol "root") Set.empty empty, elements = [level1] }
          toGram root `shouldBe` "(root (a (b (c))))"
      
      describe "relationship pattern serialization" $ do
        it "serializes relationship pattern (source-relationship-target)" $ do
          let source = Pattern { value = Subject (Symbol "a") (Set.fromList ["Person"]) empty, elements = [] }
          let rel = Pattern { value = Subject (Symbol "r") (Set.fromList ["KNOWS"]) empty, elements = [] }
          let target = Pattern { value = Subject (Symbol "b") (Set.fromList ["Person"]) empty, elements = [] }
          let relationship = Pattern { value = Subject (Symbol "g") Set.empty empty, elements = [source, rel, target] }
          toGram relationship `shouldBe` "(g (a:Person) (r:KNOWS) (b:Person))"
        
        it "serializes relationship with properties" $ do
          let source = Pattern { value = Subject (Symbol "a") (Set.fromList ["Person"]) empty, elements = [] }
          let relProps = fromList [("since", VInteger 2024)]
          let rel = Pattern { value = Subject (Symbol "r") (Set.fromList ["KNOWS"]) relProps, elements = [] }
          let target = Pattern { value = Subject (Symbol "b") (Set.fromList ["Person"]) empty, elements = [] }
          let relationship = Pattern { value = Subject (Symbol "g") Set.empty empty, elements = [source, rel, target] }
          toGram relationship `shouldBe` "(g (a:Person) (r:KNOWS {since:2024}) (b:Person))"
      
      describe "edge cases" $ do
        it "serializes pattern with empty property record" $ do
          let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n:Person)"
        
        it "serializes pattern with empty label set" $ do
          let s = Subject (Symbol "n") Set.empty empty
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n)"
        
        it "serializes pattern with empty elements list" $ do
          let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n:Person)"
        
        it "serializes string with Unicode characters" $ do
          let props = fromList [("name", VString "José")]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          toGram p `shouldBe` "(n {name:\"José\"})"
        
        it "serializes negative numbers" $ do
          let props = fromList [("temp", VInteger (-10)), ("ratio", VDecimal (-0.5))]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          let result = toGram p
          -- Property order may vary (Map doesn't preserve order)
          result `shouldContain` "(n {"
          result `shouldContain` "temp:-10"
          result `shouldContain` "ratio:-0.5"
          result `shouldContain` "})"
