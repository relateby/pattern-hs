# Quickstart: Updated Gram Serialization

## Serialization

The `toGram` function now produces output compatible with `tree-sitter-gram` 0.2.7.

```haskell
import Gram.Serialize (toGram)
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import qualified Data.Set as Set
import qualified Data.Map as Map

-- 1. Serializing a flat top-level structure
-- (Internally represented as Gram.Root)
let root = Pattern (Subject (Symbol "") (Set.singleton "Gram.Root") Map.empty) 
             [ node1, node2 ]

-- Output:
-- (n1)
-- (n2)
putStrLn (toGram root)

-- 2. Serializing Annotations (Mapped to Properties)
-- Input: @author("Me") (n)
-- Transformed to: [ {author:"Me"} | (n) ]
let props = Map.fromList [("author", VString "Me")]
let node = Pattern (Subject (Symbol "n") Set.empty Map.empty) []
let annotated = Pattern (Subject (Symbol "") Set.empty props) [node]

-- Output:
-- [{author:"Me"} | (n)]
putStrLn (toGram annotated)
```
