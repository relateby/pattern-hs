# Research: Subject Identity and Serialization

**Feature**: Subject Identity and Serialization (`020-subject-serialization`)
**Status**: Complete

## 1. Identity Generation for Anonymous Subjects

**Context**: Gram syntax allows anonymous subjects (e.g., `()`, `()-[]->()`), but the `Subject` data type requires a mandatory `identity` field (Symbol). We need a strategy to assign unique IDs to these subjects during parsing.

**Options Considered**:
1.  **UUIDs**: Generate a random UUID for each anonymous subject.
    *   *Pros*: Globally unique.
    *   *Cons*: Requires `IO` or a random seed, making `fromGram` impure or complex. Harder to test (non-deterministic).
2.  **Sequential IDs (Global)**: Use a global counter.
    *   *Pros*: Simple.
    *   *Cons*: Requires `IO` / `MVar` / global state. Breaks purity.
3.  **Sequential IDs (Local/Deterministic)**: Use a counter scoped to the `fromGram` call (e.g., `#1`, `#2`).
    *   *Pros*: Pure, deterministic, easy to test.
    *   *Cons*: IDs are only unique within that specific parse result. Merging two parsed graphs could cause collisions if not handled (but that's a separate concern; `Subject` semigroup handles merging).

**Decision**: **Option 3: Sequential IDs (Local/Deterministic)**.
We will generate IDs of the form `#<N>` (or similar distinct prefix) using a `State` monad during the transformation phase (`Gram.Transform`).

**Implementation Details**:
- Modify `transformGram` to be `transformGram :: CST.Gram -> P.Pattern S.Subject` (keeping signature pure) but internally use `evalState` with a stateful transformation function.
- State will track the next available ID index.
- `transformIdentifier` will look like:
  ```haskell
  transformIdentifier :: Maybe CST.Identifier -> State Int S.Symbol
  transformIdentifier Nothing = do
    n <- get
    put (n + 1)
    return $ S.Symbol ("#" ++ show n)
  transformIdentifier (Just (CST.IdentSymbol (CST.Symbol s))) = return $ S.Symbol s
  -- ...
  ```

## 2. Round-trip Consistency

**Context**: We want `fromGram . toGram == id` (conceptually).
If we parse `()` -> `Subject "#1"`, then `toGram` will produce `(#1)`.
Parsing `(#1)` -> `Subject "#1"`.
This preserves the data identity.

**Decision**: Accept that anonymous subjects become named subjects after a round-trip. This is consistent with the requirement that `Subject` *has* an identity. The "anonymous" syntax is just a shorthand for "I don't care about the ID, make one up". Once made up, it persists.

## 3. Special Character Escaping

**Context**: `Gram.Serialize` already handles some escaping. We need to ensure it covers all cases (quotes, backslashes) to ensure valid gram output.

**Decision**: Review and enhance `escapeString` in `Gram.Serialize` if necessary. Current implementation looks basic but likely sufficient for standard string types. Will add tests to confirm.

