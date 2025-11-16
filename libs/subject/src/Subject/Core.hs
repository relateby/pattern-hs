{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core Subject data type and basic operations.
--
-- This module defines the fundamental Subject type as a self-descriptive object
-- with identity, labels, and property records. Subject is designed to be the
-- primary content type for patterns (i.e., @Pattern Subject@ will be the common
-- use case) and will serialize to gram notation.
--
-- == Conceptual Model: Subject as Self-Descriptive Object
--
-- A Subject is a self-descriptive object that contains:
--
-- * **Identity**: A required symbol identifier that uniquely identifies the subject within a context
-- * **Labels**: A set of label strings that categorize or classify the subject
-- * **Property Record**: A key-value map storing properties with rich value types
--
-- Subjects are "self-descriptive" because they contain all the information needed
-- to understand and work with them: their identity (if any), their classification
-- (labels), and their attributes (properties).
--
-- == Gram Notation Mapping
--
-- Subjects correspond to the "attributes" component in gram notation patterns.
-- Examples:
--
-- * @(n:Person {name:"ABK"})@ →
--   @Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "ABK")])@
--
-- * @(a)-[r:KNOWS {since: 2024}]->(b)@ →
--   @Subject (Symbol "r") (Set.fromList ["KNOWS"]) (fromList [("since", VInteger 2024)])@
--
-- * @[pat:Pattern {k:"v"}]@ →
--   @Subject (Symbol "pat") (Set.fromList ["Pattern"]) (fromList [("k", VString "v")])@
--
-- == Subject Components
--
-- === Identity
--
-- The identity field is a required symbol identifier. The serialization layer will
-- handle converting symbols to the appropriate gram notation syntax (unquoted symbols,
-- quoted strings, or numbers).
--
-- **Note**: While gram notation allows anonymous (unidentified) subjects, the Subject
-- data type requires identity to be mandatory. When implementing serialization (see
-- TODO-later.md Feature 10), we'll need to decide how to handle assigning identity
-- to anonymous subjects during serialization.
--
-- === Labels
--
-- Labels are a set of strings that categorize or classify the subject.
-- The set can be empty (no labels) or contain one or more unique labels.
-- In gram notation, labels are prefixed with @:@ or @::@. Labels are
-- treated as a set (no duplicates, order doesn't matter).
--
-- Examples:
--
-- * @Set.fromList ["Person"]@ - Single label
-- * @Set.fromList ["KNOWS", "RELATIONSHIP"]@ - Multiple labels
-- * @Set.empty@ - No labels
--
-- === Property Record
--
-- The property record is a key-value map where:
--
-- * Keys are identifiers (strings, symbols, or integers converted to strings)
-- * Values are @Value@ types supporting standard and extended types
--
-- Properties store structured data about the subject. The property record can be
-- empty (no properties) or contain any number of key-value pairs.
--
-- == Integration with Pattern
--
-- Subject is designed to work as the value type for Pattern:
--
-- >>> import Pattern.Core (Pattern(..))
-- >>> import Subject.Core (Subject(..))
-- >>> let s = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
-- >>> let p = Pattern { value = s, elements = [] }
--
-- This enables patterns to contain subjects as their decoration values, creating
-- a powerful combination where patterns provide structure and subjects provide
-- self-descriptive content.
--
-- == Examples
--
-- Creating a subject with identity, labels, and properties:
--
-- >>> import Data.Map (fromList)
-- >>> import Subject.Value (VString, VInteger)
-- >>> let s = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice"), ("age", VInteger 30)])
--
-- Creating a subject with only labels:
--
-- >>> let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
--
-- Creating an empty subject:
--
-- >>> let s = Subject (Symbol "") Set.empty empty
module Subject.Core
  ( Subject (..)
  , Symbol (..)
  , PropertyRecord
  ) where

import Data.Hashable (Hashable (..))
import Data.Map (Map, empty, union)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Data.String (IsString (..))

-- Re-export Value for convenience
import Subject.Value (Value)

-- | A symbol identifier for uniquely identifying subjects.
--
-- Symbols are used as the identity component of a Subject. They provide
-- a way to uniquely identify a subject within a context. The serialization
-- layer will handle converting symbols to the appropriate gram notation
-- syntax (unquoted symbols, quoted strings, or numbers).
--
-- === Examples
--
-- >>> Symbol "n"
-- >>> Symbol "myId"
-- >>> Symbol "42"  -- Numbers are represented as symbol strings
--
-- With OverloadedStrings extension:
--
-- >>> identity = "n" :: Symbol
newtype Symbol = Symbol String
  deriving (Eq, Ord, Show, Generic, Hashable)

-- Enable string literals for convenient syntax
instance IsString Symbol where
  fromString = Symbol

-- | A property record mapping keys to values.
--
-- Property records store structured data about a subject. Keys are strings
-- (identifiers from gram notation are converted to strings), and values
-- are @Value@ types supporting standard and extended types.
--
-- Property records can be empty or contain any number of key-value pairs.
-- They are implemented as @Map String Value@ for efficient lookups.
--
-- === Examples
--
-- >>> import Data.Map (fromList, empty)
-- >>> import Subject.Value (VString, VInteger)
-- >>> fromList [("name", VString "Alice"), ("age", VInteger 30)]
-- >>> empty  -- Empty property record
type PropertyRecord = Map String Value

-- | A self-descriptive object with identity, labels, and property record.
--
-- Subject is the primary content type for patterns. It provides a structured
-- way to represent entities with identity, classification (labels), and
-- attributes (properties).
--
-- === Subject Structure
--
-- A Subject contains three components:
--
-- * @identity :: Symbol@ - Required symbol identifier
-- * @labels :: Set String@ - Set of label strings (can be empty)
-- * @properties :: PropertyRecord@ - Key-value property map
--
-- All three components are optional/flexible:
--
-- * Identity is always required (must be provided)
-- * Labels can be empty set @Set.empty@ (no labels) or contain one or more unique labels
-- * Properties can be empty map (no properties) or contain key-value pairs
--
-- === Gram Notation
--
-- Subjects correspond to the "attributes" component in gram notation:
--
-- * @(n:Person {name:"ABK"})@ has identity @n@, label @Person@, property @name:"ABK"@
-- * @(a)-[r:KNOWS {since: 2024}]->(b)@ has identity @r@, label @KNOWS@, property @since:2024@
-- * @[pat:Pattern {k:"v"}]@ has identity @pat@, label @Pattern@, property @k:"v"@
--
-- === Examples
--
-- Subject with all components:
--
-- >>> import Data.Map (fromList)
-- >>> import Subject.Value (VString, VInteger)
-- >>> Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice"), ("age", VInteger 30)])
--
-- Subject with only labels:
--
-- >>> Subject (Symbol "n") (Set.fromList ["Person"]) empty
--
-- Subject with only identity:
--
-- >>> Subject (Symbol "n") Set.empty empty
--
-- === Integration with Pattern
--
-- Subject works as the value type for Pattern:
--
-- >>> import Pattern.Core (Pattern(..))
-- >>> let s = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
-- >>> let p = Pattern { value = s, elements = [] }
data Subject = Subject
    { -- | Symbol identifier that uniquely identifies the subject.
    --
    -- The identity field is always required. In gram notation, identities
    -- appear before labels and properties: @(n:Person {name:"ABK"})@ has identity @n@.
    --
    -- **Note**: While gram notation allows anonymous (unidentified) subjects, the Subject
    -- data type requires identity to be mandatory. When implementing serialization (see
    -- TODO-later.md Feature 10), we'll need to decide how to handle assigning identity
    -- to anonymous subjects during serialization.
    --
    -- === Examples
    --
    -- >>> identity (Subject (Symbol "n") Set.empty empty)
    -- Symbol "n"
    --
    -- >>> identity (Subject "42" Set.empty empty)  -- With OverloadedStrings
    -- Symbol "42"
    identity :: Symbol

    -- | Set of label strings that categorize or classify the subject.
    --
    -- Labels provide classification information about the subject. The set can
    -- be empty (no labels) or contain one or more unique labels. In gram notation,
    -- labels are prefixed with @:@ or @::@ and appear after the identity
    -- (if present) and before properties. Labels are treated as a set (no duplicates,
    -- order doesn't matter).
    --
    -- === Examples
    --
-- >>> labels (Subject (Symbol "n") (Set.fromList ["Person"]) empty)
-- fromList ["Person"]
--
-- >>> labels (Subject (Symbol "r") (Set.fromList ["KNOWS", "RELATIONSHIP"]) empty)
-- fromList ["KNOWS", "RELATIONSHIP"]
--
-- >>> labels (Subject (Symbol "n") Set.empty empty)
-- fromList []
  , labels :: Set String

    -- | Key-value property map storing structured data about the subject.
    --
    -- Properties store attributes and metadata about the subject. The property
    -- record is a map from string keys to @Value@ types, enabling rich,
    -- structured data storage. Properties can include standard types (integers,
    -- decimals, booleans, strings, symbols) and extended types (tagged strings,
    -- arrays, maps, ranges, measurements).
    --
    -- In gram notation, properties appear in curly braces: @{name:"Alice", age:30}@.
    --
    -- === Examples
    --
-- >>> import Data.Map (fromList, empty)
-- >>> import Subject.Value (VString, VInteger)
-- >>> properties (Subject (Symbol "n") Set.empty (fromList [("name", VString "Alice"), ("age", VInteger 30)]))
-- fromList [("age",VInteger 30),("name",VString "Alice")]
--
-- >>> properties (Subject (Symbol "n") Set.empty empty)
-- fromList []
  , properties :: PropertyRecord
  }
  deriving (Eq, Ord)

-- | Show instance for Subject.
--
-- Provides a readable string representation of a Subject for debugging and display.
-- The format shows all three components: identity, labels, and properties.
--
-- === Examples
--
-- >>> show (Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")]))
-- "Subject {identity = Symbol \"n\", labels = fromList [\"Person\"], properties = fromList [(\"name\",VString \"Alice\")]}"
instance Show Subject where
  show (Subject ident lbls props) =
    "Subject {identity = "
      ++ show ident
      ++ ", labels = "
      ++ show lbls
      ++ ", properties = "
      ++ show props
      ++ "}"

-- | Hashable instance for Subject.
--
-- Enables using subjects as keys in `HashMap` and elements in `HashSet` for
-- efficient hash-based lookups and deduplication. The instance provides O(1)
-- average-case performance compared to O(log n) for ordered containers.
--
-- === Hash Semantics
--
-- The `Hashable` instance uses structure-preserving hashing: subjects are hashed
-- based on their structure (identity, labels, and properties), ensuring that equal
-- subjects (according to `Eq`) produce the same hash value while providing good
-- distribution to minimize collisions.
--
-- === Implementation
--
-- The implementation follows standard Haskell conventions:
--
-- @
-- hashWithSalt s (Subject ident lbls props) = s `hashWithSalt` ident `hashWithSalt` lbls `hashWithSalt` props
-- @
--
-- Where each component is hashed recursively using its own `Hashable` instance.
--
-- === Examples
--
-- Hashing with default salt:
--
-- >>> hash (Subject (Symbol "n") (Set.fromList ["Person"]) empty)
-- <hash value>
--
-- Hashing with custom salt:
--
-- >>> hashWithSalt 42 (Subject (Symbol "n") (Set.fromList ["Person"]) empty)
-- <hash value>
instance Hashable Subject where
  hashWithSalt s (Subject ident lbls props) =
    s `hashWithSalt` ident `hashWithSalt` lbls `hashWithSalt` props

-- | Semigroup instance for Subject.
--
-- Combines two subjects by merging their components:
--
-- 1. **Identity**: Takes the first subject's identity, unless it's the default empty identity (Symbol ""), in which case takes the second identity
-- 2. **Labels**: Unions labels from both subjects (set union, no duplicates)
-- 3. **Properties**: Unions property maps (left-biased: first subject's properties take precedence on conflicts)
--
-- This enables incremental subject construction using standard Haskell combinators like `<>`,
-- `sconcat`, and `stimes`.
--
-- === Examples
--
-- Combining subjects with different components:
--
-- >>> s1 = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
-- >>> s2 = Subject (Symbol "m") (Set.fromList ["Employee"]) (fromList [("age", VInteger 30)])
-- >>> s1 <> s2
-- Subject {identity = Symbol "n", labels = fromList ["Employee", "Person"], properties = fromList [("age",VInteger 30),("name",VString "Alice")]}
--
-- When first subject has default empty identity, second identity is used:
--
-- >>> mempty <> Subject (Symbol "n") (Set.fromList ["Person"]) empty
-- Subject {identity = Symbol "n", labels = fromList ["Person"], properties = fromList []}
--
-- === Associativity
--
-- The operation is associative:
--
-- @
-- (s1 <> s2) <> s3 = s1 <> (s2 <> s3)
-- @
--
-- This is verified through property-based testing.
instance Semigroup Subject where
  Subject ident1 lbls1 props1 <> Subject ident2 lbls2 props2 =
    Subject
      (if ident1 == Symbol "" then ident2 else ident1) -- Use second identity if first is default empty, otherwise first
      (lbls1 `Set.union` lbls2) -- Union labels (set union, no duplicates)
      (props1 `union` props2) -- Union properties (left-biased)

-- | Monoid instance for Subject.
--
-- Extends the Semigroup instance by providing an identity element (`mempty`).
-- The identity subject has a default identity (Symbol ""), empty labels, and empty properties,
-- enabling identity-based operations and standard Monoid combinators
-- (e.g., `mconcat`) while preserving the self-descriptive object model.
--
-- === Identity Semantics
--
-- The Monoid instance provides an identity subject:
--
-- * **Identity subject**: `mempty = Subject (Symbol "") Set.empty empty`
--
-- * **Left identity**: `mempty <> s = s` for all subjects `s`
--
-- * **Right identity**: `s <> mempty = s` for all subjects `s`
--
-- === Monoid Laws
--
-- The Monoid instance satisfies the standard monoid laws, which are verified
-- through property-based testing in the test suite:
--
-- **Left Identity Law**: For all subjects @s :: Subject@,
--
-- @
-- mempty <> s = s
-- @
--
-- **Right Identity Law**: For all subjects @s :: Subject@,
--
-- @
-- s <> mempty = s
-- @
--
-- === Examples
--
-- Identity subject:
--
-- >>> mempty :: Subject
-- Subject {identity = Symbol "", labels = fromList [], properties = fromList []}
--
-- Left identity law:
--
-- >>> mempty <> Subject (Symbol "n") (Set.fromList ["Person"]) empty
-- Subject {identity = Symbol "n", labels = fromList ["Person"], properties = fromList []}
--
-- Right identity law:
--
-- >>> Subject (Symbol "n") (Set.fromList ["Person"]) empty <> mempty
-- Subject {identity = Symbol "n", labels = fromList ["Person"], properties = fromList []}
--
-- Using mconcat to combine subjects:
--
-- >>> mconcat [Subject (Symbol "a") (Set.fromList ["A"]) empty, Subject (Symbol "b") (Set.fromList ["B"]) empty, Subject (Symbol "c") (Set.fromList ["C"]) empty]
-- Subject {identity = Symbol "a", labels = fromList ["A", "B", "C"], properties = fromList []}
instance Monoid Subject where
  mempty = Subject (Symbol "") Set.empty empty
  -- Note: <> is inherited from Semigroup instance

