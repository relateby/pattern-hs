# Quickstart: Subject Serialization

## Basic Usage

```haskell
import Gram.Serialize (toGram)
import Gram.Parse (fromGram)
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import Data.Set (fromList)
import Data.Map (empty)

-- 1. Create a Subject
let subj = Subject (Symbol "n") (fromList ["Person"]) empty
let pat = Pattern subj []

-- 2. Serialize
let gram = toGram pat
-- Result: "(n:Person)"

-- 3. Parse
let parsed = fromGram "(n:Person)"
-- Result: Right (Pattern (Subject (Symbol "n") ...))
```

## Handling Anonymous Subjects

When parsing anonymous subjects, unique IDs are automatically assigned:

```haskell
-- Parse anonymous node
let result = fromGram "()"
-- Result: Right (Pattern (Subject (Symbol "_anon_1") ...))

-- Parse anonymous path
let result2 = fromGram "()-[:KNOWS]->()"
-- Result: Pattern with:
--   Left Node: Subject "_anon_1"
--   Right Node: Subject "_anon_2"
--   Relationship: Subject "_anon_3" (if implicit) or named if specified
```

