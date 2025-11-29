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
              -- Empty node should have generated ID
              let Symbol id = identity (value p)
              take 1 id `shouldBe` "#"
              labels (value p) `shouldBe` Set.empty
              properties (value p) `shouldBe` empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses node with empty record" $ do
          case fromGram "({})" of
            Right p -> do
              let Symbol id = identity (value p)
              take 1 id `shouldBe` "#"
              labels (value p) `shouldBe` Set.empty
              properties (value p) `shouldBe` empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses node with record" $ do
          case fromGram "({ k : \"v\" })" of
            Right p -> do
              let props = fromList [("k", VString "v")]
              let Symbol id = identity (value p)
              take 1 id `shouldBe` "#"
              labels (value p) `shouldBe` Set.empty
              properties (value p) `shouldBe` props
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
              let Symbol id = identity (value p)
              take 1 id `shouldBe` "#"
              labels (value p) `shouldBe` Set.empty
              properties (value p) `shouldBe` empty
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
              let Symbol id = identity (value p)
              take 1 id `shouldBe` "#"
              labels (value p) `shouldBe` Set.empty
              properties (value p) `shouldBe` empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses two node members" $ do
          case fromGram "() ()" of
            Right p -> do
              value p `shouldBe` Subject (Symbol "") (Set.singleton "Gram.Root") empty
              length (elements p) `shouldBe` 2
              let [e1, e2] = elements p
              let Symbol id1 = identity (value e1)
              let Symbol id2 = identity (value e2)
              take 1 id1 `shouldBe` "#"
              take 1 id2 `shouldBe` "#"
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses one relationship" $ do
          case fromGram "()-->()" of
            Right p -> do
              -- Relationship should be parsed as a single element (Edge Pattern)
              -- With correct mapping, the top-level pattern is the Edge Pattern itself
              -- because fromGram parses the first pattern
              
              -- The structure of ()-->() is Pattern relValue [leftNode, rightNode]
              -- NOTE: parseRelationship returns Pattern relValue [left, right]
              -- fromGram wraps it if there are multiple patterns, but here it's a single path
              
              length (elements p) `shouldBe` 2
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses path with edge semantics correctly" $ do
          -- (a)-[r]->(b) should be [r | (a), (b)]
          case fromGram "(a)-[r]->(b)" of
            Right p -> do
              -- Check top-level is the relationship pattern
              -- Relationship identifier "r" IS captured in Subject data
              length (elements p) `shouldBe` 2
              
              -- Verify relationship identity
              value p `shouldBe` Subject (Symbol "r") Set.empty empty

              -- Check elements are nodes a and b
              let [left, right] = elements p
              value left `shouldBe` Subject (Symbol "a") Set.empty empty
              value right `shouldBe` Subject (Symbol "b") Set.empty empty
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        
        it "parses two-hop path" $ do
          case fromGram "()-->()-->()" of
            Right p -> do
              -- The parser now maps walks to a flat sequence of Edge Patterns
              -- (a)->(b)->(c) -> Pattern walk [Pattern r1 [a, b], Pattern r2 [b, c]]
              -- So top level has 2 elements: edge 1 and edge 2
              length (elements p) `shouldBe` 2
              -- Verify the structure of the elements (they should be edges)
              let edges = elements p
              all (\e -> length (elements e) == 2) edges `shouldBe` True
            Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      describe "value types parsing (from corpus)" $ do
        it "parses integer property" $ do
          case fromGram "({ n : 1 })" of
            Right p -> do
              let props = fromList [("n", VInteger 1)]
              let Symbol id = identity (value p)
              take 1 id `shouldBe` "#"
              properties (value p) `shouldBe` props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses string property" $ do
          case fromGram "({ s : \"a\" })" of
            Right p -> do
              let props = fromList [("s", VString "a")]
              let Symbol id = identity (value p)
              take 1 id `shouldBe` "#"
              properties (value p) `shouldBe` props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses range property (closed range)" $ do
          case fromGram "({ i : 1..10 })" of
            Right p -> do
              let props = fromList [("i", VRange (RangeValue (Just 1) (Just 10)))]
              let Symbol id = identity (value p)
              take 1 id `shouldBe` "#"
              properties (value p) `shouldBe` props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses range property (lower bound only)" $ do
          case fromGram "({ i : 1... })" of
            Right p -> do
              let props = fromList [("i", VRange (RangeValue (Just 1) Nothing))]
              let Symbol id = identity (value p)
              take 1 id `shouldBe` "#"
              properties (value p) `shouldBe` props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses range property (upper bound only)" $ do
          case fromGram "({ i : ...100 })" of
            Right p -> do
              let props = fromList [("i", VRange (RangeValue Nothing (Just 100)))]
              let Symbol id = identity (value p)
              take 1 id `shouldBe` "#"
              properties (value p) `shouldBe` props
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
              value p `shouldBe` Subject (Symbol "") (Set.singleton "Gram.Root") empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses record with integer property" $ do
          case fromGram "{ n : 1 }" of
            Right p -> do
              let props = fromList [("n", VInteger 1)]
              value p `shouldBe` Subject (Symbol "") (Set.singleton "Gram.Root") props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses record followed by node" $ do
          case fromGram "{ s : \"a\" }\n()" of
            Right p -> do
              let props = fromList [("s", VString "a")]
              value p `shouldBe` Subject (Symbol "") (Set.singleton "Gram.Root") props
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

      describe "User Story 2: Anonymous Subject Handling" $ do
        
        it "assigns unique IDs to anonymous nodes" $ do
          -- Two anonymous nodes separated by space (parsed as 2 top-level patterns)
          case fromGram "() ()" of
            Right p -> do
              let elems = elements p
              length elems `shouldBe` 2
              let [n1, n2] = elems
              -- Check generated IDs
              let Symbol id1 = identity (value n1)
              let Symbol id2 = identity (value n2)
              
              -- IDs should be non-empty and distinct
              id1 `shouldNotBe` ""
              id2 `shouldNotBe` ""
              id1 `shouldNotBe` id2
              
              -- IDs should follow format #<N>
              take 1 id1 `shouldBe` "#"
              take 1 id2 `shouldBe` "#"
            Left err -> expectationFailure $ "Parse failed: " ++ show err
            
        it "assigns unique IDs to anonymous path elements" $ do
          -- Path with anonymous nodes and relationship: ()-[]->()
          case fromGram "()-[]->()" of
            Right p -> do
              -- Pattern is relationship: [rel | left, right]
              let relSubject = value p
              let [left, right] = elements p
              
              let Symbol relId = identity relSubject
              let Symbol leftId = identity (value left)
              let Symbol rightId = identity (value right)
              
              -- All IDs should be distinct and generated
              relId `shouldNotBe` ""
              leftId `shouldNotBe` ""
              rightId `shouldNotBe` ""
              
              leftId `shouldNotBe` rightId
              relId `shouldNotBe` leftId
              relId `shouldNotBe` rightId
              
              take 1 relId `shouldBe` "#"
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "avoids collision with existing generated-style IDs" $ do
          -- Input has explicit #1. Generator should skip it and use #2 (or higher).
          case fromGram "(`#1`) ()" of
            Right p -> do
              let elems = elements p
              length elems `shouldBe` 2
              let [e1, e2] = elems
              let Symbol id1 = identity (value e1)
              let Symbol id2 = identity (value e2)
              
              id1 `shouldBe` "#1"
              id2 `shouldNotBe` "#1"
              take 1 id2 `shouldBe` "#"
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "re-round-trips generated IDs safely (US3 Collision Prevention)" $ do
          -- () -> #1 -> (#1)
          -- (#1), () -> #1, #2 -> (#1), (#2)
          case fromGram "(`#1`) ()" of
            Right p -> do
              let elems = elements p
              let [e1, e2] = elems
              let Symbol id1 = identity (value e1)
              let Symbol id2 = identity (value e2)
              
              -- Ensure they are distinct
              id1 `shouldNotBe` id2
            Left err -> expectationFailure $ "Parse failed: " ++ show err
