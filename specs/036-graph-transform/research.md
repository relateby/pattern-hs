# Research & Technical Decisions: Graph Transform

## Overview
This document records key design decisions regarding `GraphView`, structural folding algorithms, and composition semantics. The context and requirements derive directly from `proposals/graph-transform.md` and `specs/036-graph-transform/spec.md`.

### Decision 1: Record-of-Functions vs. Typeclasses for Transformations
- **Decision**: Implemented as record-of-functions combined with `{-# INLINE #-}` pragmas instead of heavy typeclasses.
- **Rationale**: Graph transformations operate over `GraphView` dynamically. While typeclasses abstract implementations well statically, passing higher-order mapping functions into a specific `GraphClassifier` record scales nicely and stays structurally transparent. By adding `{-# INLINE #-}`, GHC specializes the function arguments and eliminates closure overhead for high-performance iteration.
- **Alternatives Considered**: Using a typeclass-based approach for `GraphView`. Rejected because the view abstraction fundamentally involves runtime configuration (e.g., dynamically defined category sets in `GraphClassifier`).

### Decision 2: Context-Aware Mapping (Snapshot Semantics)
- **Decision**: `mapWithContext` explicitly employs snapshot semantics—the context queries run against the unmodified original `GraphView`.
- **Rationale**: Enforces deterministic, predictable outcomes. If mapping functions ran incrementally over partially modified elements, output would depend strictly on internal traversal order, breaking functional commutativity rules established by Pattern HS's category-theoretic constitution.
- **Alternatives Considered**: Incremental map updates (where subsequent elements "see" the mutations of prior elements). Rejected due to non-determinism and ordering dependence, creating correctness risks rather than a stable data structure API.

### Decision 3: Iterative Folding Convergence and Execution Order
- **Decision**: `paraGraphFixed` executes rounds of pattern-centric structural folding (bottom-up containment order: atomic nodes first, then relationships, then walks, then annotations), delegating convergence criteria between rounds to a user-supplied predicate (`r -> r -> Bool`) instead of utilizing strict `Eq r`.
- **Rationale**: By enforcing a pattern-centric execution model, we avoid complex topological sorting algorithms that fail on cycles. The fold naturally progresses up the compositional depth of `Pattern v`. Furthermore, iterative graph algorithms (PageRank, Belief Propagation, Centrality) inherently deal with floating point operations. Floating points seldom reach strict identity (`==`) in converging iterative processes. Requiring a generic predicate allows the caller to implement robust bounds, like `\old new -> abs(old - new) < epsilon`.
- **Alternatives Considered**: Depending on typeclass `Eq`, or attempting to calculate graph-theoretic topological sorts. Rejected due to guaranteed divergence risks with real numbers and impossibility on cyclic graph structures.

### Decision 4: Handling Container Gaps in Filtering
- **Decision**: `filterGraph` will delegate handling of "gap" occurrences in walks (when an internal edge is removed) to an explicit `Substitution` object.
- **Rationale**: There is no one-size-fits-all fallback. Sometimes a walk gap means the walk is entirely invalid; other times, a surrogate placeholder edge should be sutured in. A parameter ensures semantic transparency and follows constitutional principles to explicitly handle missing paths.
- **Alternatives Considered**: Hardcoding a default "segment walk into fragments" behavior. Rejected to force users to reason about filtering consequences directly.
