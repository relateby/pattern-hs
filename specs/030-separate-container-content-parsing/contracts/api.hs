-- Proposed changes to Gram module exports

module Gram
  ( -- * Existing API (Backward Compatible)
    fromGram
  , toGram
    
    -- * New Container-Aware API
    -- ** Parsing
  , fromGramList
  , fromGramWithHeader
    
    -- ** Serialization
  , toGramList
  , toGramWithHeader
    
    -- * Re-exports
  , module Gram.Parse
  , module Gram.Serialize
  ) where

import Pattern.Core (Pattern)
import Subject.Core (Subject, PropertyRecord)
import Gram.Parse (fromGram, fromGramList, fromGramWithHeader, ParseError)
import Gram.Serialize (toGram, toGramList, toGramWithHeader)
