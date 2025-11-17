-- | Unit tests for Gram.Parse module.
-- Tests are based on tree-sitter-gram test corpus examples.
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
      
      describe "node parsing (from corpus: nodes.txt)" $ do
        it "parses empty node" $ do
          case fromGram "()" of
            Right p -> do
              -- Empty node should have empty subject
              value p `shouldBe` Subject (Symbol "") Set.empty empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses node with empty record" $ do
          case fromGram "({})" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "") Set.empty empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses node with record" $ do
          case fromGram "({ k : \"v\" })" of
            Right p -> do
              let props = fromList [("k", VString "v")]
              value p `shouldBe` Subject (Symbol "") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses identified node with record" $ do
          case fromGram "(player1 { named : \"it\" })" of
            Right p -> do
              let props = fromList [("named", VString "it")]
              value p `shouldBe` Subject (Symbol "player1") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses identified, labeled node with record" $ do
          case fromGram "(player1:Player { named : \"it\" })" of
            Right p -> do
              let props = fromList [("named", VString "it")]
              value p `shouldBe` Subject (Symbol "player1") (Set.fromList ["Player"]) props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      describe "subject parsing (from corpus: subjects.txt)" $ do
        it "parses empty subject" $ do
          case fromGram "[ ]" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "") Set.empty empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses named subject" $ do
          case fromGram "[ a ]" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "a") Set.empty empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses named, labeled subject" $ do
          case fromGram "[ a:Subject ]" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "a") (Set.fromList ["Subject"]) empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses named, labeled subject with record" $ do
          case fromGram "[ a:Subject { title: \"Generic Subject\" } ]" of
            Right p -> do
              let props = fromList [("title", VString "Generic Subject")]
              value p `shouldBe` Subject (Symbol "a") (Set.fromList ["Subject"]) props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with nested elements" $ do
          case fromGram "[ devrel:Team {name : \"Developer Relations\"} | abk, adam, alex, alexy ]" of
            Right p -> do
              let props = fromList [("name", VString "Developer Relations")]
              value p `shouldBe` Subject (Symbol "devrel") (Set.fromList ["Team"]) props
              -- Should have nested elements (references)
              length (elements p) `shouldBe` 4
            Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      describe "pattern parsing (from corpus: patterns.txt)" $ do
        it "parses single node pattern" $ do
          case fromGram "()" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "") Set.empty empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses two node members" $ do
          case fromGram "(),()" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "") Set.empty empty
              length (elements p) `shouldBe` 1
              value (head (elements p)) `shouldBe` Subject (Symbol "") Set.empty empty
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses one relationship" $ do
          case fromGram "()-->()" of
            Right p -> do
              -- Relationship should be parsed as a single element
              length (elements p) `shouldBe` 1
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses two-hop path" $ do
          case fromGram "()-->()-->()" of
            Right p -> do
              -- Nested relationship
              length (elements p) `shouldBe` 1
            Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      describe "value types parsing (from corpus)" $ do
        it "parses integer property" $ do
          case fromGram "({ n : 1 })" of
            Right p -> do
              let props = fromList [("n", VInteger 1)]
              value p `shouldBe` Subject (Symbol "") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses string property" $ do
          case fromGram "({ s : \"a\" })" of
            Right p -> do
              let props = fromList [("s", VString "a")]
              value p `shouldBe` Subject (Symbol "") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses range property (closed range)" $ do
          case fromGram "({ i : 1..10 })" of
            Right p -> do
              let props = fromList [("i", VRange (RangeValue (Just 1) (Just 10)))]
              value p `shouldBe` Subject (Symbol "") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses range property (lower bound only)" $ do
          case fromGram "({ i : 1... })" of
            Right p -> do
              let props = fromList [("i", VRange (RangeValue (Just 1) Nothing))]
              value p `shouldBe` Subject (Symbol "") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses range property (upper bound only)" $ do
          case fromGram "({ i : ...100 })" of
            Right p -> do
              let props = fromList [("i", VRange (RangeValue Nothing (Just 100)))]
              value p `shouldBe` Subject (Symbol "") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses map property" $ do
          case fromGram "(a:Person {\n  address: {\n    street: \"123 Main St\",\n    city: \"Anytown\",\n    state: \"CA\",\n    zip: \"12345\"\n  }\n})" of
            Right p -> do
              let addressMap = fromList 
                    [ ("street", VString "123 Main St")
                    , ("city", VString "Anytown")
                    , ("state", VString "CA")
                    , ("zip", VString "12345")
                    ]
              let props = fromList [("address", VMap addressMap)]
              let subj = value p
              identity subj `shouldBe` Symbol "a"
              labels subj `shouldBe` Set.fromList ["Person"]
              Map.lookup "address" (properties subj) `shouldBe` Just (VMap addressMap)
            Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      describe "record parsing (from corpus: records.txt)" $ do
        it "parses empty record" $ do
          case fromGram "{}" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "") Set.empty empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses record with integer property" $ do
          case fromGram "{ n : 1 }" of
            Right p -> do
              let props = fromList [("n", VInteger 1)]
              value p `shouldBe` Subject (Symbol "") Set.empty props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses record followed by node" $ do
          case fromGram "{ s : \"a\" }\n()" of
            Right p -> do
              let props = fromList [("s", VString "a")]
              value p `shouldBe` Subject (Symbol "") Set.empty props
              length (elements p) `shouldBe` 1
            Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      describe "parse error handling" $ do
        it "handles invalid syntax" $ do
          case fromGram "(invalid" of
            Right _ -> expectationFailure "Should have failed"
            Left (ParseError msg) -> msg `shouldContain` "expected"
        
        it "handles nested parentheses in nodes (invalid)" $ do
          -- This should fail - nodes cannot have nested elements
          case fromGram "(g (a:Person))" of
            Right _ -> expectationFailure "Should have failed - nodes cannot nest"
            Left (ParseError _) -> return ()  -- Expected to fail
