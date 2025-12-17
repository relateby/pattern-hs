-- | Unit tests for Gram.Serialize module.
module Spec.Gram.SerializeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC
import Test.QuickCheck (forAll, listOf, listOf1, Gen)
import Gram.Serialize (toGram)
import Gram.Parse (fromGram)
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import Subject.Value (Value(..), RangeValue(..))
import Data.Map (empty, fromList)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)

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
          -- Subject "" is reserved for Implicit Root, which must have Gram.Root label
          let s = Subject (Symbol "") (Set.singleton "Gram.Root") empty
          let p = Pattern { value = s, elements = [] }
          -- Empty props/elems -> "{}" (Empty Graph Record)
          toGram p `shouldBe` "{}"
      
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
          toGram outer `shouldBe` "[g | (a:Person)]"
        
        it "serializes pattern with multiple nested elements" $ do
          let elem1 = Pattern { value = Subject (Symbol "a") (Set.fromList ["Person"]) empty, elements = [] }
          let elem2 = Pattern { value = Subject (Symbol "b") (Set.fromList ["Person"]) empty, elements = [] }
          let outer = Pattern { value = Subject (Symbol "g") Set.empty empty, elements = [elem1, elem2] }
          -- Note: 2-element patterns are now normalized to edge syntax: (a)-[g]->(b)
          -- This normalization ensures consistent round-trip behavior for edges.
          toGram outer `shouldBe` "(a:Person)-[g]->(b:Person)"
        
        it "serializes deeply nested patterns" $ do
          let level3 = Pattern { value = Subject (Symbol "c") Set.empty empty, elements = [] }
          let level2 = Pattern { value = Subject (Symbol "b") Set.empty empty, elements = [level3] }
          let level1 = Pattern { value = Subject (Symbol "a") Set.empty empty, elements = [level2] }
          let root = Pattern { value = Subject (Symbol "root") Set.empty empty, elements = [level1] }
          toGram root `shouldBe` "[root | [a | [b | c]]]"
      
      describe "relationship pattern serialization" $ do
        it "serializes relationship pattern (source-relationship-target)" $ do
          let source = Pattern { value = Subject (Symbol "a") (Set.fromList ["Person"]) empty, elements = [] }
          let rel = Pattern { value = Subject (Symbol "r") (Set.fromList ["KNOWS"]) empty, elements = [] }
          let target = Pattern { value = Subject (Symbol "b") (Set.fromList ["Person"]) empty, elements = [] }
          -- Edge Pattern structure: Pattern rel [source, target]
          let relationship = Pattern { value = value rel, elements = [source, target] }
          
          -- Should serialize as path notation: (a:Person)-[r:KNOWS]->(b:Person)
          toGram relationship `shouldBe` "(a:Person)-[r:KNOWS]->(b:Person)"
        
        it "serializes relationship with properties" $ do
          let source = Pattern { value = Subject (Symbol "a") (Set.fromList ["Person"]) empty, elements = [] }
          let relProps = fromList [("since", VInteger 2024)]
          let rel = Pattern { value = Subject (Symbol "r") (Set.fromList ["KNOWS"]) relProps, elements = [] }
          let target = Pattern { value = Subject (Symbol "b") (Set.fromList ["Person"]) empty, elements = [] }
          -- Edge Pattern structure: Pattern rel [source, target]
          let relationship = Pattern { value = value rel, elements = [source, target] }
          
          toGram relationship `shouldBe` "(a:Person)-[r:KNOWS {since:2024}]->(b:Person)"

        it "serializes anonymous relationship" $ do
          let source = Pattern { value = Subject (Symbol "a") Set.empty empty, elements = [] }
          let rel = Pattern { value = Subject (Symbol "") Set.empty empty, elements = [] } -- Anonymous
          let target = Pattern { value = Subject (Symbol "b") Set.empty empty, elements = [] }
          let relationship = Pattern { value = value rel, elements = [source, target] }
          
          toGram relationship `shouldBe` "(a)-->(b)"

        it "serializes pattern with 3 elements as standard bracket pattern" $ do
          let e1 = Pattern { value = Subject (Symbol "a") Set.empty empty, elements = [] }
          let e2 = Pattern { value = Subject (Symbol "b") Set.empty empty, elements = [] }
          let e3 = Pattern { value = Subject (Symbol "c") Set.empty empty, elements = [] }
          let p = Pattern { value = Subject (Symbol "g") Set.empty empty, elements = [e1, e2, e3] }
          
          -- Should NOT be treated as an edge because it has 3 elements
          toGram p `shouldBe` "[g | a, b, c]"

      describe "walk pattern serialization" $ do
        it "serializes two-step walk (a)->(b)->(c)" $ do
          let a = Pattern { value = Subject (Symbol "a") Set.empty empty, elements = [] }
          let b = Pattern { value = Subject (Symbol "b") Set.empty empty, elements = [] }
          let c = Pattern { value = Subject (Symbol "c") Set.empty empty, elements = [] }
          
          let rel1 = Pattern { value = Subject (Symbol "r1") Set.empty empty, elements = [] }
          let rel2 = Pattern { value = Subject (Symbol "r2") Set.empty empty, elements = [] }
          
          -- Edge 1: (a)-[r1]->(b)
          let edge1 = Pattern { value = value rel1, elements = [a, b] }
          -- Edge 2: (b)-[r2]->(c)
          let edge2 = Pattern { value = value rel2, elements = [b, c] }
          
          -- Walk structure: [Gram.Walk | edge1, edge2]
          -- Note: Transform.hs uses "Gram.Walk" label for the walk container
          let walk = Pattern { 
                value = Subject (Symbol "") (Set.singleton "Gram.Walk") empty,
                elements = [edge1, edge2] 
              }
          
          toGram walk `shouldBe` "(a)-[r1]->(b)-[r2]->(c)"
          
        it "serializes walk with anonymous relationships" $ do
          let a = Pattern { value = Subject (Symbol "a") Set.empty empty, elements = [] }
          let b = Pattern { value = Subject (Symbol "b") Set.empty empty, elements = [] }
          let c = Pattern { value = Subject (Symbol "c") Set.empty empty, elements = [] }
          
          let rel = Pattern { value = Subject (Symbol "") Set.empty empty, elements = [] }
          
          let edge1 = Pattern { value = value rel, elements = [a, b] }
          let edge2 = Pattern { value = value rel, elements = [b, c] }
          
          let walk = Pattern { 
                value = Subject (Symbol "") (Set.singleton "Gram.Walk") empty,
                elements = [edge1, edge2] 
              }
          
          toGram walk `shouldBe` "(a)-->(b)-->(c)"


      
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

      describe "User Story 1: Round-trip Serialization" $ do
        
        it "handles complex special character escaping" $ do
          let specialStr = "Line 1\nLine 2\tTabbed\rCarriage \"Quote\" \\Backslash"
          let props = fromList [("data", VString specialStr)]
          let s = Subject (Symbol "n") Set.empty props
          let p = Pattern { value = s, elements = [] }
          let serialized = toGram p
          
          -- Verify serialization format
          serialized `shouldContain` "\\n"
          serialized `shouldContain` "\\t"
          serialized `shouldContain` "\\r"
          serialized `shouldContain` "\\\""
          serialized `shouldContain` "\\\\"
          
          -- Verify round-trip
          let parsed = fromGram serialized
          parsed `shouldBe` Right p

      describe "Gram.Root Serialization (Flattening)" $ do
        it "serializes top-level elements as flat sequence" $ do
          let n1 = Pattern (Subject (Symbol "n1") Set.empty empty) []
          let n2 = Pattern (Subject (Symbol "n2") Set.empty empty) []
          let root = Pattern (Subject (Symbol "") (Set.singleton "Gram.Root") empty) [n1, n2]
          
          -- Should be separated by newlines, no wrapping brackets
          toGram root `shouldBe` "(n1)\n(n2)"
        
        it "serializes empty root as {}" $ do
          let root = Pattern (Subject (Symbol "") (Set.singleton "Gram.Root") empty) []
          toGram root `shouldBe` "{}"
          
        it "serializes root with properties correctly" $ do
          let props = fromList [("version", VString "1.0")]
          let n1 = Pattern (Subject (Symbol "n1") Set.empty empty) []
          let root = Pattern (Subject (Symbol "") (Set.singleton "Gram.Root") props) [n1]
          
          -- Properties come first, then elements
          toGram root `shouldBe` "{version:\"1.0\"}\n(n1)"

      describe "Annotated Pattern Serialization" $ do
        it "serializes annotated pattern as subject pattern with properties" $ do
          -- @author("Me") (n) -> [ {author:"Me"} | n]
          let props = fromList [("author", VString "Me")]
          let n = Pattern (Subject (Symbol "n") Set.empty empty) []
          let annotated = Pattern (Subject (Symbol "") Set.empty props) [n]
          
          -- Note: Extra space after [ is due to property record serialization
          -- Note: (n) becomes n because it is a reference (named subject with no other attrs)
          toGram annotated `shouldBe` "[ {author:\"Me\"} | n]"
          
        it "serializes multiple annotations" $ do
          -- @a(1) @b(2) (n) -> [ {a:1, b:2} | n]
          let props = fromList [("a", VInteger 1), ("b", VInteger 2)]
          let n = Pattern (Subject (Symbol "n") Set.empty empty) []
          let annotated = Pattern (Subject (Symbol "") Set.empty props) [n]
          
          let result = toGram annotated
          result `shouldContain` "a:1"
          result `shouldContain` "b:2"
          result `shouldStartWith` "[ {"
          result `shouldEndWith` "} | n]"

          let idStr = "user123"
          let s = Subject (Symbol idStr) Set.empty empty
          let p = Pattern { value = s, elements = [] }
          let serialized = toGram p
          serialized `shouldBe` "(user123)"
          
          let parsed = fromGram serialized
          parsed `shouldBe` Right p
          
        it "preserves explicit IDs requiring quoting" $ do
          -- IDs with spaces require backtick quoting
          let idStr = "user name"
          let s = Subject (Symbol idStr) Set.empty empty
          let p = Pattern { value = s, elements = [] }
          let serialized = toGram p
          serialized `shouldBe` "(`user name`)"
          
          let parsed = fromGram serialized
          parsed `shouldBe` Right p
          
        it "preserves special characters in IDs" $ do
          -- IDs with backticks need escaping
          let idStr = "user`name"
          let s = Subject (Symbol idStr) Set.empty empty
          let p = Pattern { value = s, elements = [] }
          let serialized = toGram p
          serialized `shouldBe` "(`user\\`name`)"
          
          let parsed = fromGram serialized
          parsed `shouldBe` Right p

        prop "serializes and parses back to an equivalent pattern (Round Trip)" $ 
          forAll genPattern $ \p -> do
            let serialized = toGram p
            let parsed = fromGram serialized
            parsed `shouldBe` Right p

      -- US4: Automatic Codefence Serialization
      describe "codefence string serialization (US4)" $ do
        
        it "serializes short VString (<=120 chars) using quotes" $ do
          let shortStr = "This is a short string"  -- 22 chars
          let s = Subject (Symbol "n") Set.empty (fromList [("text", VString shortStr)])
          let p = Pattern { value = s, elements = [] }
          let result = toGram p
          -- Should use double quotes, not codefence
          result `shouldContain` "\"This is a short string\""
          result `shouldNotContain` "```"
        
        it "serializes long VString (>120 chars) using codefence" $ do
          -- Create a string of 121 characters
          let longStr = replicate 121 'x'
          let s = Subject (Symbol "n") Set.empty (fromList [("content", VString longStr)])
          let p = Pattern { value = s, elements = [] }
          let result = toGram p
          -- Should use codefence format
          result `shouldContain` "```"
          result `shouldContain` longStr
        
        it "serializes VString exactly 120 chars using quotes" $ do
          -- Exactly 120 characters - should use quotes (threshold is >120)
          let exactStr = replicate 120 'y'
          let s = Subject (Symbol "n") Set.empty (fromList [("text", VString exactStr)])
          let p = Pattern { value = s, elements = [] }
          let result = toGram p
          -- Should use double quotes, not codefence
          result `shouldNotContain` "```\n"
          result `shouldContain` "\""
        
        it "serializes short VTaggedString using inline format" $ do
          let shortContent = "Short content"  -- 13 chars
          let s = Subject (Symbol "n") Set.empty (fromList [("code", VTaggedString "md" shortContent)])
          let p = Pattern { value = s, elements = [] }
          let result = toGram p
          -- Should use inline tagged format: md`Short content`
          result `shouldContain` "md`Short content`"
          result `shouldNotContain` "```"
        
        it "serializes long VTaggedString using codefence format" $ do
          -- Create a string of 121 characters
          let longContent = replicate 121 'z'
          let s = Subject (Symbol "n") Set.empty (fromList [("doc", VTaggedString "markdown" longContent)])
          let p = Pattern { value = s, elements = [] }
          let result = toGram p
          -- Should use tagged codefence format: ```markdown\ncontent\n```
          result `shouldContain` "```markdown"
          result `shouldContain` longContent

      -- Phase 7: Round-trip property tests
      describe "codefence round-trip tests" $ do
        
        prop "round-trip VString through parse/serialize" $ 
          forAll genShortString $ \str -> do
            let s = Subject (Symbol "test") Set.empty (fromList [("content", VString str)])
            let p = Pattern { value = s, elements = [] }
            let serialized = toGram p
            let parsed = fromGram serialized
            case parsed of
              Right p' -> properties (value p') `shouldBe` properties (value p)
              Left err -> expectationFailure $ "Round-trip failed: " ++ show err
        
        prop "round-trip VTaggedString through parse/serialize" $ 
          forAll genTaggedPair $ \(tag, content) -> do
            let s = Subject (Symbol "test") Set.empty (fromList [("doc", VTaggedString tag content)])
            let p = Pattern { value = s, elements = [] }
            let serialized = toGram p
            let parsed = fromGram serialized
            case parsed of
              Right p' -> properties (value p') `shouldBe` properties (value p)
              Left err -> expectationFailure $ "Round-trip failed: " ++ show err
        
        it "serializer escapes newlines in short strings correctly" $ do
          let shortMultiline = "Line 1\nLine 2"  -- 14 chars, short
          let s = Subject (Symbol "n") Set.empty (fromList [("text", VString shortMultiline)])
          let p = Pattern { value = s, elements = [] }
          let result = toGram p
          -- Should use escaped newline in quoted string, not codefence
          result `shouldContain` "\\n"
          result `shouldNotContain` "```"
        
        it "falls back to quoted format when long string contains closing fence pattern" $ do
          -- Long string (>120 chars) containing \n``` which would break codefence parsing
          let problematic = replicate 100 'x' ++ "\n```" ++ replicate 50 'y'
          length problematic `shouldSatisfy` (> 120)  -- Verify it's long enough
          let s = Subject (Symbol "n") Set.empty (fromList [("content", VString problematic)])
          let p = Pattern { value = s, elements = [] }
          let serialized = toGram p
          -- Should NOT use codefence format due to closing fence pattern in content
          -- Instead should use quoted format with escaped newline
          serialized `shouldNotContain` "```\n"  -- No opening codefence
          -- Verify round-trip works
          let parsed = fromGram serialized
          case parsed of
            Right p' -> properties (value p') `shouldBe` properties (value p)
            Left err -> expectationFailure $ "Round-trip failed: " ++ show err
        
        it "falls back to quoted format when long tagged string contains closing fence pattern" $ do
          -- Long tagged string content containing \n``` which would break codefence parsing
          let problematic = replicate 100 'a' ++ "\n```" ++ replicate 50 'b'
          length problematic `shouldSatisfy` (> 120)
          let s = Subject (Symbol "n") Set.empty (fromList [("doc", VTaggedString "md" problematic)])
          let p = Pattern { value = s, elements = [] }
          let serialized = toGram p
          -- Should NOT use tagged codefence format
          serialized `shouldNotContain` "```md"
          -- Verify round-trip works
          let parsed = fromGram serialized
          case parsed of
            Right p' -> properties (value p') `shouldBe` properties (value p)
            Left err -> expectationFailure $ "Round-trip failed: " ++ show err

-- Generators for Property Tests
genPattern :: Gen (Pattern Subject)
genPattern = do
  val <- genSubject
  -- Limit recursion depth for simple round-trip test
  return $ Pattern val []

genSubject :: Gen Subject
genSubject = do
  idStr <- listOf1 (QC.elements ['a'..'z'])
  lbls <- listOf (listOf1 (QC.elements ['A'..'Z']))
  -- Generate simple string properties to verify property serialization
  k <- listOf1 (QC.elements ['a'..'z'])
  v <- listOf1 (QC.elements ['a'..'z'])
  let props = Map.fromList [(k, VString v)]
  return $ Subject (Symbol idStr) (Set.fromList lbls) props

-- | Generate short strings (avoiding special chars that need escaping)
genShortString :: Gen String
genShortString = do
  len <- QC.choose (1, 50)
  listOf1 (QC.elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,!?")
    >>= return . take len

-- | Generate tag and content pair for tagged strings
genTaggedPair :: Gen (String, String)
genTaggedPair = do
  tag <- listOf1 (QC.elements ['a'..'z'])  -- Simple tag like "md", "json"
  len <- QC.choose (1, 50)
  content <- listOf1 (QC.elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,!?")
    >>= return . take len
  return (tag, content)

