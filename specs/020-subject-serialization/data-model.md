# Data Model: Subject Identity and Serialization

## Entities

### Subject

The core data structure representing a node or relationship's content.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| `identity` | `Symbol` | Unique identifier | **Mandatory**. Cannot be empty string in a valid graph (though type allows it). Parsed anonymous subjects receive generated IDs (e.g., `#1`). |
| `labels` | `Set String` | Classification tags | Unique set. |
| `properties` | `Map String Value` | Key-value attributes | Keys are strings. Values are typed. |

### Identity Generation

- **Format**: `#<N>` (e.g., `#1`, `#2`, ...)
- **Scope**: Local to a single `fromGram` parse operation.
- **Counter**: Starts at 1 for each parse.

## Type Definitions

```haskell
-- | Core Subject type
data Subject = Subject
  { identity :: Symbol
  , labels :: Set String
  , properties :: Map String Value
  }

-- | Symbol wrapper
newtype Symbol = Symbol String
```

