-- | Unit tests for Gram.Parse module.
module Spec.Gram.ParseSpec where

import Test.Hspec
import Gram.Parse (fromGram, ParseError(..))
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import Subject.Value (Value(..), RangeValue(..))
import Data.Map (Map, empty, fromList)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "Gram.Parse" $ do
    
    describe "fromGram" $ do
      
      describe "simple subject parsing" $ do
        it "parses subject with identity and single label" $ do
          case fromGram "(n:Person)" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "n") (Set.fromList ["Person"]) empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with identity and multiple labels" $ do
          case fromGram "(r:KNOWS:RELATIONSHIP)" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "r") (Set.fromList ["KNOWS", "RELATIONSHIP"]) empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with identity only (no labels)" $ do
          case fromGram "(n)" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "n") Set.empty empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses anonymous subject (empty Symbol) with label" $ do
          case fromGram "(:Person)" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "") (Set.fromList ["Person"]) empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses anonymous subject (empty Symbol) without label" $ do
          case fromGram "()" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "") Set.empty empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      describe "subject with standard value types parsing" $ do
        it "parses subject with integer property" $ do
          case fromGram "(n:Person {age:30})" of
            Right p -> do
              let props = fromList [("age", VInteger 30)]
              value p `shouldBe` Subject (Symbol "n") (Set.fromList ["Person"]) props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with decimal property" $ do
          case fromGram "(n {pi:3.14})" of
            Right p -> do
              let props = fromList [("pi", VDecimal 3.14)]
              value p `shouldBe` Subject (Symbol "n") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with boolean property" $ do
          case fromGram "(n:User {active:true,verified:false})" of
            Right p -> do
              let props = fromList [("active", VBoolean True), ("verified", VBoolean False)]
              value p `shouldBe` Subject (Symbol "n") (Set.fromList ["User"]) props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with string property" $ do
          case fromGram "(n:Person {name:\"Alice\"})" of
            Right p -> do
              let props = fromList [("name", VString "Alice")]
              value p `shouldBe` Subject (Symbol "n") (Set.fromList ["Person"]) props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with symbol property" $ do
          case fromGram "(n {type:Person})" of
            Right p -> do
              let props = fromList [("type", VSymbol "Person")]
              value p `shouldBe` Subject (Symbol "n") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with multiple standard value properties" $ do
          case fromGram "(n:Person {name:\"Alice\",age:30,active:true})" of
            Right p -> do
              let props = fromList 
                    [ ("name", VString "Alice")
                    , ("age", VInteger 30)
                    , ("active", VBoolean True)
                    ]
              -- Property order may vary, so check individual properties
              let subj = value p
              identity subj `shouldBe` Symbol "n"
              labels subj `shouldBe` Set.fromList ["Person"]
              Map.lookup "name" (properties subj) `shouldBe` Just (VString "Alice")
              Map.lookup "age" (properties subj) `shouldBe` Just (VInteger 30)
              Map.lookup "active" (properties subj) `shouldBe` Just (VBoolean True)
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses string with escaped quotes" $ do
          case fromGram "(n {quote:\"He said \\\"Hello\\\"\"})" of
            Right p -> do
              let props = fromList [("quote", VString "He said \"Hello\"")]
              value p `shouldBe` Subject (Symbol "n") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      describe "subject with extended value types parsing" $ do
        it "parses subject with tagged string property" $ do
          case fromGram "(n {url:url`https://example.com`})" of
            Right p -> do
              let props = fromList [("url", VTaggedString "url" "https://example.com")]
              value p `shouldBe` Subject (Symbol "n") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with array property" $ do
          case fromGram "(n:User {tags:[\"admin\",\"user\"]})" of
            Right p -> do
              let props = fromList [("tags", VArray [VString "admin", VString "user"])]
              value p `shouldBe` Subject (Symbol "n") (Set.fromList ["User"]) props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with map property" $ do
          case fromGram "(n {metadata:{key1:\"value1\",key2:42}})" of
            Right p -> do
              let props = fromList [("metadata", VMap (fromList [("key1", VString "value1"), ("key2", VInteger 42)]))]
              value p `shouldBe` Subject (Symbol "n") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with range property (closed range)" $ do
          case fromGram "(n {age:18..65})" of
            Right p -> do
              let props = fromList [("age", VRange (RangeValue (Just 18) (Just 65)))]
              value p `shouldBe` Subject (Symbol "n") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with range property (lower bound only)" $ do
          case fromGram "(n {age:18...})" of
            Right p -> do
              let props = fromList [("age", VRange (RangeValue (Just 18) Nothing))]
              value p `shouldBe` Subject (Symbol "n") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with range property (upper bound only)" $ do
          case fromGram "(n {age:...65})" of
            Right p -> do
              let props = fromList [("age", VRange (RangeValue Nothing (Just 65)))]
              value p `shouldBe` Subject (Symbol "n") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with measurement property" $ do
          case fromGram "(n {weight:70.5kg})" of
            Right p -> do
              let props = fromList [("weight", VMeasurement "kg" 70.5)]
              value p `shouldBe` Subject (Symbol "n") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses nested arrays and maps" $ do
          case fromGram "(n {nested:[{key:\"value\"}]})" of
            Right p -> do
              let props = fromList [("nested", VArray [VMap (fromList [("key", VString "value")])])]
              value p `shouldBe` Subject (Symbol "n") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      describe "nested pattern parsing" $ do
        it "parses pattern with single nested element" $ do
          case fromGram "(g (a:Person))" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "g") Set.empty empty
              length (elements p) `shouldBe` 1
              value (head (elements p)) `shouldBe` Subject (Symbol "a") (Set.fromList ["Person"]) empty
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses pattern with multiple nested elements" $ do
          case fromGram "(g (a:Person) (b:Person))" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "g") Set.empty empty
              length (elements p) `shouldBe` 2
              value (elements p !! 0) `shouldBe` Subject (Symbol "a") (Set.fromList ["Person"]) empty
              value (elements p !! 1) `shouldBe` Subject (Symbol "b") (Set.fromList ["Person"]) empty
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses deeply nested patterns" $ do
          case fromGram "(root (a (b (c))))" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "root") Set.empty empty
              length (elements p) `shouldBe` 1
              let level1 = head (elements p)
              value level1 `shouldBe` Subject (Symbol "a") Set.empty empty
              length (elements level1) `shouldBe` 1
              let level2 = head (elements level1)
              value level2 `shouldBe` Subject (Symbol "b") Set.empty empty
              length (elements level2) `shouldBe` 1
              let level3 = head (elements level2)
              value level3 `shouldBe` Subject (Symbol "c") Set.empty empty
            Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      describe "relationship pattern parsing" $ do
        it "parses relationship pattern (source-relationship-target)" $ do
          case fromGram "(g (a:Person) (r:KNOWS) (b:Person))" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "g") Set.empty empty
              length (elements p) `shouldBe` 3
              value (elements p !! 0) `shouldBe` Subject (Symbol "a") (Set.fromList ["Person"]) empty
              value (elements p !! 1) `shouldBe` Subject (Symbol "r") (Set.fromList ["KNOWS"]) empty
              value (elements p !! 2) `shouldBe` Subject (Symbol "b") (Set.fromList ["Person"]) empty
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses relationship with properties" $ do
          case fromGram "(g (a:Person) (r:KNOWS {since:2024}) (b:Person))" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "g") Set.empty empty
              length (elements p) `shouldBe` 3
              let relProps = fromList [("since", VInteger 2024)]
              value (elements p !! 1) `shouldBe` Subject (Symbol "r") (Set.fromList ["KNOWS"]) relProps
            Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      describe "parse error handling" $ do
        it "handles unclosed bracket" $ do
          case fromGram "(invalid" of
            Right _ -> expectationFailure "Should have failed"
            Left (ParseError msg) -> msg `shouldContain` "expected"
        
        it "handles mismatched brackets" $ do
          case fromGram "(n:Person}" of
            Right _ -> expectationFailure "Should have failed"
            Left (ParseError msg) -> msg `shouldContain` "expected"
        
        it "handles incomplete property record" $ do
          case fromGram "(n:Person {name" of
            Right _ -> expectationFailure "Should have failed"
            Left (ParseError msg) -> msg `shouldContain` "expected"
        
        it "handles invalid value syntax" $ do
          case fromGram "(n {age:invalid})" of
            Right _ -> expectationFailure "Should have failed"
            Left (ParseError msg) -> msg `shouldContain` "expected"
      
      describe "edge cases" $ do
        it "parses string with Unicode characters" $ do
          case fromGram "(n {name:\"José\"})" of
            Right p -> do
              let props = fromList [("name", VString "José")]
              value p `shouldBe` Subject (Symbol "n") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses negative numbers" $ do
          case fromGram "(n {temp:-10,ratio:-0.5})" of
            Right p -> do
              let props = fromList [("temp", VInteger (-10)), ("ratio", VDecimal (-0.5))]
              -- Property order may vary
              let subj = value p
              Map.lookup "temp" (properties subj) `shouldBe` Just (VInteger (-10))
              Map.lookup "ratio" (properties subj) `shouldBe` Just (VDecimal (-0.5))
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "handles whitespace variations" $ do
          case fromGram "( n : Person { name : \"Alice\" } )" of
            Right p -> do
              let props = fromList [("name", VString "Alice")]
              value p `shouldBe` Subject (Symbol "n") (Set.fromList ["Person"]) props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "strips line comments" $ do
          case fromGram "// This is a comment\n(n:Person)" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "n") (Set.fromList ["Person"]) empty
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "strips end-of-line comments" $ do
          case fromGram "(n:Person) // This is a comment" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "n") (Set.fromList ["Person"]) empty
            Left err -> expectationFailure $ "Parse failed: " ++ show err
