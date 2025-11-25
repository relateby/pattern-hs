-- | Constructor functions and property manipulation for Subject.
--
-- This module provides convenient functions for creating and manipulating
-- Subject instances, including constructor functions and property record
-- operations.
--
-- == Construction Functions
--
-- * @subject@ - Create an empty subject with default identity
-- * @subjectWith@ - Create a subject with all components specified
--
-- == Property Manipulation
--
-- * @addProperty@ - Add a property to a subject's property record
-- * @updateProperty@ - Update an existing property in a subject's property record
-- * @removeProperty@ - Remove a property from a subject's property record
-- * @hasProperty@ - Check if a subject has a specific property
--
-- == Examples
--
-- Creating an empty subject:
--
-- >>> emptySubj = subject
-- >>> identity emptySubj
-- Symbol ""
-- >>> labels emptySubj
-- fromList []
-- >>> properties emptySubj
-- fromList []
--
-- Creating a subject with all components:
--
-- >>> import Data.Map (fromList)
-- >>> import Subject.Value (VString, VInteger)
-- >>> s = subjectWith (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
--
-- Adding a property:
--
-- >>> s' = addProperty "age" (VInteger 30) s
module Subject.Construction
  ( subject
  , subjectWith
  , addProperty
  , updateProperty
  , removeProperty
  , hasProperty
  ) where

import Data.Map (Map, delete, empty, insert, member, (!?))
import Data.Set (Set)
import qualified Data.Set as Set
import Subject.Core (Symbol (..), PropertyRecord, Subject (..))
import Subject.Value (Value)

-- | Create an empty subject with default identity, no labels, and no properties.
--
-- This function provides a convenient way to create an empty subject without
-- using verbose record syntax. The resulting subject uses a default identity
-- (Symbol "") and is functionally identical to @Subject (Symbol "") Set.empty empty@
-- or @mempty@.
--
-- === Examples
--
-- Empty subject:
--
-- >>> s = subject
-- >>> identity s
-- Symbol ""
-- >>> labels s
-- fromList []
-- >>> properties s
-- fromList []
--
-- Using as starting point for building subjects:
--
-- >>> s = addProperty "name" (VString "Alice") (subject { labels = Set.fromList ["Person"] })
subject :: Subject
subject = Subject (Symbol "") Set.empty empty

-- | Create a subject with all components specified.
--
-- This function provides a convenient way to create a subject with identity,
-- labels, and properties without using verbose record syntax. The resulting
-- subject is functionally identical to @Subject ident lbls props@.
--
-- === Examples
--
-- Subject with all components:
--
-- >>> import Data.Map (fromList)
-- >>> import Data.Set (Set)
-- >>> import qualified Data.Set as Set
-- >>> import Subject.Value (VString, VInteger)
-- >>> s = subjectWith (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice"), ("age", VInteger 30)])
--
-- Subject with only labels:
--
-- >>> s = subjectWith (Symbol "n") (Set.fromList ["Person"]) empty
--
-- Subject with only identity:
--
-- >>> s = subjectWith (Symbol "n") Set.empty empty
subjectWith :: Symbol -> Set String -> PropertyRecord -> Subject
subjectWith ident lbls props = Subject ident lbls props

-- | Add a property to a subject's property record.
--
-- If the property key already exists, the new value replaces the old value
-- (equivalent to @updateProperty@). The function returns a new subject with
-- the added/updated property.
--
-- === Examples
--
-- Adding a new property:
--
-- >>> s = subject { labels = Set.fromList ["Person"] }
-- >>> s' = addProperty "name" (VString "Alice") s
-- >>> properties s' !? "name"
-- Just (VString "Alice")
--
-- Adding a property that already exists (replaces old value):
--
-- >>> s = subject { properties = fromList [("name", VString "Bob")] }
-- >>> s' = addProperty "name" (VString "Alice") s
-- >>> properties s' !? "name"
-- Just (VString "Alice")
addProperty :: String -> Value -> Subject -> Subject
addProperty key val (Subject ident lbls props) =
  Subject ident lbls (insert key val props)

-- | Update an existing property in a subject's property record.
--
-- If the property key does not exist, the function returns the subject unchanged.
-- Use @addProperty@ if you want to add a property regardless of whether it exists.
--
-- === Examples
--
-- Updating an existing property:
--
-- >>> s = subject { properties = fromList [("name", VString "Bob")] }
-- >>> s' = updateProperty "name" (VString "Alice") s
-- >>> properties s' !? "name"
-- Just (VString "Alice")
--
-- Updating a non-existent property (no change):
--
-- >>> s = subject { properties = fromList [("name", VString "Bob")] }
-- >>> s' = updateProperty "age" (VInteger 30) s
-- >>> properties s' !? "age"
-- Nothing
updateProperty :: String -> Value -> Subject -> Subject
updateProperty key val s@(Subject ident lbls props) =
  if member key props
    then addProperty key val s
    else s

-- | Remove a property from a subject's property record.
--
-- If the property key does not exist, the function returns the subject unchanged.
-- The function returns a new subject with the property removed.
--
-- === Examples
--
-- Removing an existing property:
--
-- >>> s = subject { properties = fromList [("name", VString "Alice"), ("age", VInteger 30)] }
-- >>> s' = removeProperty "age" s
-- >>> properties s' !? "age"
-- Nothing
-- >>> properties s' !? "name"
-- Just (VString "Alice")
--
-- Removing a non-existent property (no change):
--
-- >>> s = subject { properties = fromList [("name", VString "Alice")] }
-- >>> s' = removeProperty "age" s
-- >>> properties s' == properties s
-- True
removeProperty :: String -> Subject -> Subject
removeProperty key (Subject ident lbls props) =
  Subject ident lbls (delete key props)

-- | Check if a subject has a specific property.
--
-- Returns @True@ if the property key exists in the subject's property record,
-- @False@ otherwise.
--
-- === Examples
--
-- Checking for an existing property:
--
-- >>> s = subject { properties = fromList [("name", VString "Alice")] }
-- >>> hasProperty "name" s
-- True
--
-- Checking for a non-existent property:
--
-- >>> s = subject { properties = fromList [("name", VString "Alice")] }
-- >>> hasProperty "age" s
-- False
hasProperty :: String -> Subject -> Bool
hasProperty key (Subject _ _ props) = member key props

