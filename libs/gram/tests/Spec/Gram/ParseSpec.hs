-- | Unit tests for Gram.Parse module.
-- Tests are based on tree-sitter-gram test corpus examples.
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Spec.Gram.ParseSpec where

import Test.Hspec
import Gram.Parse (fromGram, fromGramWithIds, fromGramList, fromGramWithHeader, ParseError(..))
import Gram.Serialize (toGram)
import qualified Gram.Transform as Transform
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
    
    describe "fromGramWithIds" $ do
      it "assigns unique IDs to anonymous subjects" $ do
        case fromGramWithIds "() ()" of
          Right p -> do
            length (elements p) `shouldBe` 2
          Left err -> expectationFailure $ "Parse failed: " ++ show err

    describe "fromGramList" $ do
      it "parses multiple patterns as a list" $ do
        case fromGramList "(a) (b)" of
          Right [p1, p2] -> do
            identity (value p1) `shouldBe` Symbol "a"
            identity (value p2) `shouldBe` Symbol "b"
          _ -> expectationFailure "Should have parsed two patterns"

      it "returns empty list for empty input" $ do
        case fromGramList "" of
          Right [] -> return ()
          _ -> expectationFailure "Should have returned empty list"

    describe "fromGramWithHeader" $ do
      it "separates leading header from patterns" $ do
        case fromGramWithHeader "{v:1} (a)" of
          Right (Just header, [p]) -> do
            Map.lookup "v" header `shouldBe` Just (VInteger 1)
            identity (value p) `shouldBe` Symbol "a"
          _ -> expectationFailure "Should have separated header and pattern"

      it "returns Nothing for header if no leading record" $ do
        case fromGramWithHeader "(a) (b)" of
          Right (Nothing, [p1, p2]) -> do
            identity (value p1) `shouldBe` Symbol "a"
            identity (value p2) `shouldBe` Symbol "b"
          _ -> expectationFailure "Should have returned Nothing for header"

    describe "fromGram" $ do
      
      describe "node parsing (from corpus: nodes.txt)" $ do
        it "parses empty node" $ do
          case fromGram "()" of
            Right p -> do
              -- Empty node should preserve anonymity (Symbol "")
              let Symbol id = identity (value p)
              id `shouldBe` ""
              labels (value p) `shouldBe` Set.empty
              properties (value p) `shouldBe` empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses node with empty record" $ do
          case fromGramWithIds "({})" of
            Right p -> do
              let Symbol id = identity (value p)
              take 1 id `shouldBe` "#"
              labels (value p) `shouldBe` Set.empty
              properties (value p) `shouldBe` empty
              elements p `shouldBe` []
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses node with record" $ do
          case fromGramWithIds "({ k : \"v\" })" of
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
          case fromGramWithIds "[ ]" of
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
        
        it "parses subject with newline after pipe separator" $ do
          -- Test for regression: newlines should be allowed after pipe separator
          case fromGram "[ test:Agent { description: \"test\" } |\n  [tool:Tool {description: \"test\"}] ]" of
            Right p -> do
              let props = fromList [("description", VString "test")]
              value p `shouldBe` Subject (Symbol "test") (Set.fromList ["Agent"]) props
              length (elements p) `shouldBe` 1
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses subject with newline before closing bracket" $ do
          -- Test for regression: newlines should be allowed before closing bracket
          case fromGram "[ test:Agent { description: \"test\" } | [tool:Tool {description: \"test\"}]\n]" of
            Right p -> do
              let props = fromList [("description", VString "test")]
              value p `shouldBe` Subject (Symbol "test") (Set.fromList ["Agent"]) props
              length (elements p) `shouldBe` 1
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses complex nested subject pattern with newlines (reported issue)" $ do
          -- This is the exact example that was reported as not parsing
          let input = "[test:Agent {\n\n\n  description: \"test\"\n\n} |\n  [tool:Tool {description: \"test\"} | (param::Text)==>(::String)]\n]"
          case fromGram input of
            Right p -> do
              let props = fromList [("description", VString "test")]
              value p `shouldBe` Subject (Symbol "test") (Set.fromList ["Agent"]) props
              -- Should have one nested element (the tool subject pattern)
              length (elements p) `shouldBe` 1
              let [nested] = elements p
              -- The nested element should be a subject pattern with tool data
              let toolProps = fromList [("description", VString "test")]
              value nested `shouldBe` Subject (Symbol "tool") (Set.fromList ["Tool"]) toolProps
              -- The nested element should have one element (the path)
              length (elements nested) `shouldBe` 1
            Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      describe "pattern parsing (from corpus: patterns.txt)" $ do
        it "parses single node pattern" $ do
          case fromGram "()" of
            Right p -> do
              let Symbol id = identity (value p)
              id `shouldBe` ""  -- Preserves anonymity
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
              id1 `shouldBe` ""  -- Preserves anonymity
              id2 `shouldBe` ""  -- Preserves anonymity
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
              id `shouldBe` ""  -- Preserves anonymity
              properties (value p) `shouldBe` props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses string property" $ do
          case fromGram "({ s : \"a\" })" of
            Right p -> do
              let props = fromList [("s", VString "a")]
              let Symbol id = identity (value p)
              id `shouldBe` ""  -- Preserves anonymity
              properties (value p) `shouldBe` props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses range property (closed range)" $ do
          case fromGram "({ i : 1..10 })" of
            Right p -> do
              let props = fromList [("i", VRange (RangeValue (Just 1) (Just 10)))]
              let Symbol id = identity (value p)
              id `shouldBe` ""  -- Preserves anonymity
              properties (value p) `shouldBe` props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses range property (lower bound only)" $ do
          case fromGram "({ i : 1... })" of
            Right p -> do
              let props = fromList [("i", VRange (RangeValue (Just 1) Nothing))]
              let Symbol id = identity (value p)
              id `shouldBe` ""  -- Preserves anonymity
              properties (value p) `shouldBe` props
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses range property (upper bound only)" $ do
          case fromGram "({ i : ...100 })" of
            Right p -> do
              let props = fromList [("i", VRange (RangeValue Nothing (Just 100)))]
              let Symbol id = identity (value p)
              id `shouldBe` ""  -- Preserves anonymity
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

      describe "User Story 2: Anonymous Subject Handling (with ID assignment)" $ do
        
        it "assigns unique IDs to anonymous nodes" $ do
          -- Two anonymous nodes separated by space (parsed as 2 top-level patterns)
          -- This test uses explicit ID assignment via fromGramWithIds
          case fromGramWithIds "() ()" of
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
          -- This test uses explicit ID assignment via fromGramWithIds
          case fromGramWithIds "()-[]->()" of
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
          -- This test uses explicit ID assignment via fromGramWithIds
          case fromGramWithIds "(`#1`) ()" of
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
          -- This test uses explicit ID assignment via fromGramWithIds
          case fromGramWithIds "(`#1`) ()" of
            Right p -> do
              let elems = elements p
              let [e1, e2] = elems
              let Symbol id1 = identity (value e1)
              let Symbol id2 = identity (value e2)
              
              -- Ensure they are distinct
              id1 `shouldNotBe` id2
            Left err -> expectationFailure $ "Parse failed: " ++ show err

      describe "Anonymous Subject Preservation (default behavior)" $ do
        
        it "preserves anonymous nodes as empty Symbol" $ do
          -- Two anonymous nodes should both have Symbol ""
          case fromGram "() ()" of
            Right p -> do
              let elems = elements p
              length elems `shouldBe` 2
              let [n1, n2] = elems
              -- Both should have empty Symbol
              let Symbol id1 = identity (value n1)
              let Symbol id2 = identity (value n2)
              
              id1 `shouldBe` ""
              id2 `shouldBe` ""
              
              -- Both patterns are structurally equal (same anonymous structure)
              -- They are separate pattern instances but have identical structure
              value n1 `shouldBe` value n2
            Left err -> expectationFailure $ "Parse failed: " ++ show err
            
        it "preserves anonymous path elements as empty Symbol" $ do
          -- Path with anonymous nodes and relationship: ()-[]->()
          case fromGram "()-[]->()" of
            Right p -> do
              -- Pattern is relationship: [rel | left, right]
              let relSubject = value p
              let [left, right] = elements p
              
              let Symbol relId = identity relSubject
              let Symbol leftId = identity (value left)
              let Symbol rightId = identity (value right)
              
              -- All should have empty Symbol
              relId `shouldBe` ""
              leftId `shouldBe` ""
              rightId `shouldBe` ""
              
              -- Verify structure is preserved correctly
              length (elements p) `shouldBe` 2
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves anonymous in nested patterns" $ do
          -- Nested pattern with anonymous subjects
          -- Use valid syntax: subject patterns with anonymous nodes as paths
          case fromGram "[ | ()-[]->(), ()-[]->() ]" of
            Right p -> do
              -- Outer pattern should have empty Symbol
              let Symbol outerId = identity (value p)
              outerId `shouldBe` ""
              
              -- Inner patterns (paths) should also have empty Symbol for their components
              let elems = elements p
              length elems `shouldBe` 2
              let [e1, e2] = elems
              -- Each element is a relationship pattern [rel | left, right]
              let Symbol relId1 = identity (value e1)
              let Symbol relId2 = identity (value e2)
              
              relId1 `shouldBe` ""
              relId2 `shouldBe` ""
              
              -- Check that the nodes in the paths also have empty identity
              let [left1, right1] = elements e1
              let [left2, right2] = elements e2
              let Symbol leftId1 = identity (value left1)
              let Symbol rightId1 = identity (value right1)
              let Symbol leftId2 = identity (value left2)
              let Symbol rightId2 = identity (value right2)
              
              leftId1 `shouldBe` ""
              rightId1 `shouldBe` ""
              leftId2 `shouldBe` ""
              rightId2 `shouldBe` ""
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves anonymous alongside named subjects" $ do
          -- Mix of named and anonymous subjects
          case fromGram "(a) () (b)" of
            Right p -> do
              let elems = elements p
              length elems `shouldBe` 3
              let [e1, e2, e3] = elems
              
              -- Named subjects keep their IDs
              let Symbol id1 = identity (value e1)
              let Symbol id2 = identity (value e2)
              let Symbol id3 = identity (value e3)
              
              id1 `shouldBe` "a"
              id2 `shouldBe` ""  -- Anonymous
              id3 `shouldBe` "b"
              
              -- All three should be distinct
              e1 `shouldNotBe` e2
              e2 `shouldNotBe` e3
              e1 `shouldNotBe` e3
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "assignIdentities assigns IDs to anonymous only" $ do
          -- Create pattern with mix of named and anonymous subjects
          let named = Pattern (Subject (Symbol "a") Set.empty empty) []
          let anonymous1 = Pattern (Subject (Symbol "") Set.empty empty) []
          let anonymous2 = Pattern (Subject (Symbol "") Set.empty empty) []
          let root = Pattern (Subject (Symbol "") (Set.singleton "Gram.Root") empty) [named, anonymous1, anonymous2]
          
          -- Apply assignIdentities
          let assigned = Transform.assignIdentities root
          
          -- Root should get ID
          let Symbol rootId = identity (value assigned)
          rootId `shouldNotBe` ""
          take 1 rootId `shouldBe` "#"
          
          -- Named subject should remain unchanged
          let [e1, e2, e3] = elements assigned
          let Symbol id1 = identity (value e1)
          let Symbol id2 = identity (value e2)
          let Symbol id3 = identity (value e3)
          
          id1 `shouldBe` "a"  -- Named unchanged
          id2 `shouldNotBe` ""  -- Anonymous gets ID
          id3 `shouldNotBe` ""  -- Anonymous gets ID
          take 1 id2 `shouldBe` "#"
          take 1 id3 `shouldBe` "#"
          
          -- IDs should be distinct
          id2 `shouldNotBe` id3

        it "assignIdentities avoids collisions with existing IDs" $ do
          -- Create pattern with #1 and anonymous subjects
          let existing = Pattern (Subject (Symbol "#1") Set.empty empty) []
          let anonymous1 = Pattern (Subject (Symbol "") Set.empty empty) []
          let anonymous2 = Pattern (Subject (Symbol "") Set.empty empty) []
          let root = Pattern (Subject (Symbol "") (Set.singleton "Gram.Root") empty) [existing, anonymous1, anonymous2]
          
          -- Apply assignIdentities
          let assigned = Transform.assignIdentities root
          
          -- Root should get ID starting after #1
          let Symbol rootId = identity (value assigned)
          rootId `shouldNotBe` ""
          take 1 rootId `shouldBe` "#"
          
          -- Check elements
          let [e1, e2, e3] = elements assigned
          let Symbol id1 = identity (value e1)
          let Symbol id2 = identity (value e2)
          let Symbol id3 = identity (value e3)
          
          id1 `shouldBe` "#1"  -- Existing unchanged
          id2 `shouldNotBe` "#1"  -- Should be #2 or higher
          id3 `shouldNotBe` "#1"  -- Should be #3 or higher
          take 1 id2 `shouldBe` "#"
          take 1 id3 `shouldBe` "#"
          
          -- All IDs should be distinct
          id2 `shouldNotBe` id3

      describe "Integration: End-to-end anonymous preservation workflow" $ do
        
        it "end-to-end anonymous preservation workflow" $ do
          -- Parse anonymous pattern
          case fromGram "()" of
            Right parsed -> do
              -- Verify anonymity preserved
              let Symbol id = identity (value parsed)
              id `shouldBe` ""
              
              -- Serialize to gram
              let serialized = toGram parsed
              serialized `shouldBe` "()"
              
              -- Re-parse
              case fromGram serialized of
                Right reparsed -> do
                  -- Verify structural equality
                  let Symbol id2 = identity (value reparsed)
                  id2 `shouldBe` ""
                  value parsed `shouldBe` value reparsed
                  
                  -- Apply assignIdentities if needed
                  let assigned = Transform.assignIdentities reparsed
                  let Symbol id3 = identity (value assigned)
                  id3 `shouldNotBe` ""
                  take 1 id3 `shouldBe` "#"
                  
                  -- Verify IDs assigned correctly
                  let serialized2 = toGram assigned
                  serialized2 `shouldContain` "#"
                Left err -> expectationFailure $ "Re-parse failed: " ++ show err
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "mixed workflow with explicit ID assignment" $ do
          -- Use fromGramWithIds for patterns needing IDs
          case fromGramWithIds "() ()" of
            Right parsedWithIds -> do
              let [e1, e2] = elements parsedWithIds
              let Symbol id1 = identity (value e1)
              let Symbol id2 = identity (value e2)
              id1 `shouldNotBe` ""
              id2 `shouldNotBe` ""
              take 1 id1 `shouldBe` "#"
              take 1 id2 `shouldBe` "#"
              
              -- Use fromGram for round-trip preservation
              case fromGram "() ()" of
                Right parsedAnonymous -> do
                  let [e3, e4] = elements parsedAnonymous
                  let Symbol id3 = identity (value e3)
                  let Symbol id4 = identity (value e4)
                  id3 `shouldBe` ""
                  id4 `shouldBe` ""
                  
                  -- Verify both workflows work independently
                  length (elements parsedWithIds) `shouldBe` 2
                  length (elements parsedAnonymous) `shouldBe` 2
                Left err -> expectationFailure $ "fromGram failed: " ++ show err
            Left err -> expectationFailure $ "fromGramWithIds failed: " ++ show err

      -- US1: Plain Codefence String Parsing
      describe "codefence string parsing (US1)" $ do
        
        it "parses plain codefence basic" $ do
          -- Basic codefence: ```\nHello World\n```
          case fromGram "({ content: ```\nHello World\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "content" props `shouldBe` Just (VString "Hello World")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses plain codefence with multiline content" $ do
          case fromGram "({ text: ```\nLine 1\nLine 2\nLine 3\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "text" props `shouldBe` Just (VString "Line 1\nLine 2\nLine 3")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses plain codefence empty content" $ do
          case fromGram "({ empty: ```\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "empty" props `shouldBe` Just (VString "")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses plain codefence with backticks in content" $ do
          -- Content containing single and double backticks
          case fromGram "({ code: ```\nconst x = `hello`;\nconst y = ``template``;\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "code" props `shouldBe` Just (VString "const x = `hello`;\nconst y = ``template``;")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "fails on unclosed codefence" $ do
          -- Unclosed codefence should fail parsing
          case fromGram "({ bad: ```\nno closing fence here" of
            Right _ -> expectationFailure "Should have failed on unclosed codefence"
            Left (ParseError _) -> return ()  -- Any parse error is acceptable

      -- US2: Tagged Codefence String Parsing
      describe "tagged codefence string parsing (US2)" $ do
        
        it "parses tagged codefence basic" $ do
          -- Tagged codefence: ```md\n# Title\n```
          case fromGram "({ content: ```md\n# Title\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "content" props `shouldBe` Just (VTaggedString "md" "# Title")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses tagged codefence empty content" $ do
          case fromGram "({ empty: ```json\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "empty" props `shouldBe` Just (VTaggedString "json" "")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses tagged codefence various tags" $ do
          -- Test with html tag
          case fromGram "({ html: ```html\n<div>test</div>\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "html" props `shouldBe` Just (VTaggedString "html" "<div>test</div>")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses tagged codefence with multiline content" $ do
          case fromGram "({ code: ```cypher\nMATCH (n)\nWHERE n.name = 'test'\nRETURN n\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "code" props `shouldBe` Just (VTaggedString "cypher" "MATCH (n)\nWHERE n.name = 'test'\nRETURN n")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "distinguishes between plain and tagged codefence" $ do
          -- Plain codefence should be VString, tagged should be VTaggedString
          case fromGram "({ plain: ```\ntext\n```, tagged: ```md\ntext\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "plain" props `shouldBe` Just (VString "text")
              Map.lookup "tagged" props `shouldBe` Just (VTaggedString "md" "text")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

      -- US3: Integration with Property Records
      describe "codefence integration with property records (US3)" $ do
        
        it "parses node with codefence property" $ do
          -- Node with single codefence property
          case fromGram "(n:Document { content: ```\nDocument content here\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "content" props `shouldBe` Just (VString "Document content here")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses node with multiple codefence properties" $ do
          case fromGram "(:Page { title: ```\nPage Title\n```, body: ```\nPage body content\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "title" props `shouldBe` Just (VString "Page Title")
              Map.lookup "body" props `shouldBe` Just (VString "Page body content")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses node with mixed value types including codefence" $ do
          -- Mix of integer, string, boolean, and codefence
          case fromGram "(:Post { views: 42, draft: true, title: \"Short\", content: ```\nLong content here\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "views" props `shouldBe` Just (VInteger 42)
              Map.lookup "draft" props `shouldBe` Just (VBoolean True)
              Map.lookup "title" props `shouldBe` Just (VString "Short")
              Map.lookup "content" props `shouldBe` Just (VString "Long content here")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses node with tagged codefence in property record" $ do
          case fromGram "(:Template { markup: ```html\n<div>Hello</div>\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "markup" props `shouldBe` Just (VTaggedString "html" "<div>Hello</div>")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses examples/markdown.gram content successfully" $ do
          -- Actual content from examples/markdown.gram
          let gramContent = "(:Example {prompt: ```md\n# Markdown Headline\nThis is a brief example of a tagged codefence that makes it easier\nto support multiline text in a particular format (in this case Markdown).\n```\n})"
          case fromGram gramContent of
            Right p -> do
              let props = properties (value p)
              case Map.lookup "prompt" props of
                Just (VTaggedString tag content) -> do
                  tag `shouldBe` "md"
                  content `shouldContain` "# Markdown Headline"
                  content `shouldContain` "tagged codefence"
                _ -> expectationFailure "Expected VTaggedString for prompt property"
            Left err -> expectationFailure $ "Parse failed: " ++ show err

      -- Phase 7: Edge cases
      describe "codefence edge cases" $ do
        
        it "parses codefence with exactly two consecutive backticks in content" $ do
          -- Content containing `` (two backticks) - shouldn't confuse the parser
          case fromGram "({ code: ```\nconst x = ``template``;\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "code" props `shouldBe` Just (VString "const x = ``template``;")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "preserves // sequences inside codefence content (not stripped as comments)" $ do
          -- CRITICAL: // inside codefence should NOT be treated as comment
          case fromGram "({ url: ```\nhttps://example.com\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "url" props `shouldBe` Just (VString "https://example.com")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "preserves comment-like content inside codefence" $ do
          -- Code with // comments should be preserved in codefence
          case fromGram "({ code: ```\n// This is a comment\nlet x = 1; // inline\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "code" props `shouldBe` Just (VString "// This is a comment\nlet x = 1; // inline")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves // sequences inside TAGGED codefence content" $ do
          -- CRITICAL: // inside tagged codefence should NOT be treated as comment
          -- This tests the fix for endsWithCodefenceOpen detecting ```tag patterns
          case fromGram "({ url: ```md\nhttps://example.com\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "url" props `shouldBe` Just (VTaggedString "md" "https://example.com")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves comment-like content inside TAGGED codefence" $ do
          -- Code with // comments should be preserved in tagged codefence
          case fromGram "({ code: ```js\n// This is a comment\nlet x = 1; // inline\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "code" props `shouldBe` Just (VTaggedString "js" "// This is a comment\nlet x = 1; // inline")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves double quotes inside plain codefence without escaping" $ do
          -- Double quotes should be captured verbatim - no escaping needed
          case fromGram "({ text: ```\nHe said \"hello\" there.\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "text" props `shouldBe` Just (VString "He said \"hello\" there.")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves single quotes inside plain codefence without escaping" $ do
          -- Single quotes should be captured verbatim - no escaping needed
          case fromGram "({ text: ```\nIt's a test with 'quoted' words.\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "text" props `shouldBe` Just (VString "It's a test with 'quoted' words.")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves mixed quotes inside plain codefence without escaping" $ do
          -- All quote types should be captured verbatim together
          case fromGram "({ text: ```\n\"double\" and 'single' and `backtick`\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "text" props `shouldBe` Just (VString "\"double\" and 'single' and `backtick`")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves double quotes inside tagged codefence without escaping" $ do
          -- JSON content with quotes should be captured verbatim
          case fromGram "({ data: ```json\n{\"key\": \"value\", \"name\": \"Alice\"}\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "data" props `shouldBe` Just (VTaggedString "json" "{\"key\": \"value\", \"name\": \"Alice\"}")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves single quotes inside tagged codefence without escaping" $ do
          -- Code with single quotes should be captured verbatim
          case fromGram "({ code: ```python\nprint('hello')\nname = 'world'\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "code" props `shouldBe` Just (VTaggedString "python" "print('hello')\nname = 'world'")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves mixed quotes inside tagged codefence without escaping" $ do
          -- Mix of quote styles in tagged content
          case fromGram "({ script: ```js\nconst msg = \"It's a 'test'\";\nconsole.log(`Value: ${msg}`);\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "script" props `shouldBe` Just (VTaggedString "js" "const msg = \"It's a 'test'\";\nconsole.log(`Value: ${msg}`);")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves inline code backticks (markdown style) without escaping" $ do
          -- Single backticks for inline code should work without any escaping
          case fromGram "({ doc: ```md\nUse the `sayHello` tool to greet users.\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "doc" props `shouldBe` Just (VTaggedString "md" "Use the `sayHello` tool to greet users.")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves multiple inline code spans without escaping" $ do
          -- Multiple inline code spans in markdown content
          case fromGram "({ doc: ```md\nCombine `foo`, `bar`, and `baz` together.\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "doc" props `shouldBe` Just (VTaggedString "md" "Combine `foo`, `bar`, and `baz` together.")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
        
        it "parses codefence with code block syntax in content" $ do
          -- Simulating markdown code block inside codefence
          case fromGram "({ doc: ```md\nHere is code:\n``\nvar x = 1;\n``\n``` })" of
            Right p -> do
              let props = properties (value p)
              case Map.lookup "doc" props of
                Just (VTaggedString "md" content) -> do
                  content `shouldContain` "``"
                  content `shouldContain` "var x = 1"
                _ -> expectationFailure "Expected VTaggedString"
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "strips comments after codefence closes with gram syntax on same line" $ do
          -- CRITICAL: Tests fix for isClosingFence recognizing ``` })" as closing fence
          -- The closing fence followed by }) should properly exit codefence mode
          -- so that // comments on same line are stripped
          let gramWithComment = "({ content: ```\ntext\n``` }) // This comment should be stripped"
          case fromGram gramWithComment of
            Right p -> do
              let props = properties (value p)
              Map.lookup "content" props `shouldBe` Just (VString "text")
            Left err -> expectationFailure $ "Parse failed (comment not stripped after codefence): " ++ show err

        it "strips comments after codefence on following line" $ do
          -- Verify normal comments after codefence block work
          -- Single pattern with comment on following line
          let gramWithComment = "({ a: ```\nvalue\n``` }) // trailing comment"
          case fromGram gramWithComment of
            Right p -> do
              let props = properties (value p)
              Map.lookup "a" props `shouldBe` Just (VString "value")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "strips comments between multiple patterns after codefence" $ do
          -- Multiple patterns with comment between them - tests codefence mode exit
          let gramMulti = "({ x: ```\ndata\n``` })\n// This comment should be stripped\n(:Next)"
          case fromGram gramMulti of
            Right _ -> return ()  -- Parsing succeeds if comment was stripped
            Left err -> expectationFailure $ "Parse failed: " ++ show err

      -- Indented closing fence support
      describe "indented closing fence support" $ do
        
        it "accepts indented closing fence with spaces" $ do
          -- Closing fence can have leading spaces for better readability
          case fromGram "({ text: ```\nHello World\n  ``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "text" props `shouldBe` Just (VString "Hello World")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "accepts indented closing fence with tabs" $ do
          -- Closing fence can have leading tabs
          case fromGram "({ text: ```\nContent\n\t``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "text" props `shouldBe` Just (VString "Content")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "accepts indented closing fence in tagged codefence" $ do
          -- Tagged codefence with indented closing fence
          case fromGram "({ doc: ```md\n# Title\n    ``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "doc" props `shouldBe` Just (VTaggedString "md" "# Title")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "improves readability in nested structures" $ do
          -- Real-world example: node with indented codefence property
          let input = unlines
                [ "(:Agent {"
                , "  name: \"Assistant\","
                , "  instructions: ```md"
                , "When the user greets you, use the `sayHello` tool."
                , "Always be polite and helpful."
                , "  ```"
                , "})"
                ]
          case fromGram input of
            Right p -> do
              let props = properties (value p)
              case Map.lookup "instructions" props of
                Just (VTaggedString "md" content) -> do
                  content `shouldContain` "sayHello"
                  content `shouldContain` "polite"
                _ -> expectationFailure "Expected VTaggedString for instructions"
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "handles empty content with indented closing fence" $ do
          -- Empty codefence with indented closing
          case fromGram "({ empty: ```\n   ``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "empty" props `shouldBe` Just (VString "")
            Left err -> expectationFailure $ "Parse failed: " ++ show err

        it "preserves content when line starts with spaces but not closing fence" $ do
          -- Indented content that doesn't start with ``` should be preserved
          case fromGram "({ code: ```\n  indented line\n  another line\n``` })" of
            Right p -> do
              let props = properties (value p)
              Map.lookup "code" props `shouldBe` Just (VString "  indented line\n  another line")
            Left err -> expectationFailure $ "Parse failed: " ++ show err
