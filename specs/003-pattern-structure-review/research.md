# Research: Pattern Structure Design Review

**Feature**: 003-pattern-structure-review  
**Date**: 2025-01-27  
**Status**: Complete

## Research Objectives

1. Identify all Pattern definitions across documentation files
2. Identify terminology inconsistencies
3. Document implementation status of features
4. Identify conflicting definitions
5. Establish authoritative definitions

---

## Findings

### 1. Pattern Definition Inconsistencies

**Files Reviewed**:
- `src/Pattern/Core.hs` - Primary implementation
- `DESIGN.md` - Design documentation
- `README.md` - Project overview
- `specs/001-pattern-data-structure/data-model.md` - Data model spec
- `specs/002-basic-pattern-type/data-model.md` - Basic type spec

**Inconsistencies Found**:

1. **Conceptual Model Description**:
   - `Core.hs`: Describes Pattern as "sequence of elements" (conceptual) with "recursive tree structure" (implementation)
   - `DESIGN.md`: Describes Pattern as "recursive data structure" that "can be interpreted as graphs"
   - `data-model.md` (001): Describes Pattern as "recursive tree structure that serves as a generalized representation of graph elements"
   - `data-model.md` (002): Describes Pattern as "recursive tree structure that serves as the foundational building block"
   - `README.md`: Emphasizes "Sequence-Based Semantics" as a key principle

**Decision**: Standardize on **sequence-based conceptual model** with tree implementation. The sequence semantic is primary; the tree structure is the implementation detail that supports sequence semantics.

**Rationale**: User agreed (Q1: A) to standardize on "sequence" as the primary conceptual model. This aligns with README.md's stated design principle and Core.hs's current documentation.

**Alternatives Considered**:
- Tree-only model: Rejected - loses the sequence semantic that's central to the design
- Both equally: Rejected - creates confusion about which is primary

---

### 2. Terminology Inconsistencies

#### 2.1 Value Field Terminology

**Terms Found**:
- "value" (code field name, Core.hs)
- "metadata" (Core.hs documentation, some spec files)
- "value associated with pattern" (various)

**Decision**: Standardize on **"value"** as the primary term.

**Rationale**: User agreed (Q2: A) to standardize on "value" as it matches the actual code field name (`value :: v`). This ensures consistency between code and documentation.

**Alternatives Considered**:
- "metadata": Rejected - doesn't match code field name, creates inconsistency
- Context-dependent: Rejected - adds unnecessary complexity

#### 2.2 Elements Field Terminology

**Terms Found**:
- "elements" (code field name, Core.hs)
- "children" (some documentation)
- "child patterns" (some documentation)
- "sequence elements" (Core.hs)

**Decision**: Standardize on **"elements"** as the primary term.

**Rationale**: User agreed (Q3: A) to standardize on "elements" as it matches the code field name (`elements :: [Pattern v]`) and aligns with the sequence-based conceptual model.

**Alternatives Considered**:
- "children": Rejected - emphasizes tree model over sequence model, doesn't match code
- "child patterns": Rejected - more verbose, doesn't match code field name
- Context-dependent: Rejected - adds unnecessary complexity

---

### 3. Implementation Status Analysis

#### 3.1 Typeclass Instances

**Documented vs Implemented**:

| Typeclass | Documented In | Actually Implemented | Status |
|-----------|---------------|---------------------|--------|
| `Show` | All docs | ✅ Yes (manual instance) | Implemented |
| `Eq` | All docs | ✅ Yes (`deriving Eq`) | Implemented |
| `Functor` | data-model.md (001), contracts | ❌ No | Planned (TODO.md Feature 4) |
| `Foldable` | data-model.md (001), contracts | ❌ No | Planned (TODO.md Feature 5) |
| `Traversable` | data-model.md (001), contracts | ❌ No | Planned (TODO.md Feature 6) |
| `Ord` | contracts/type-signatures.md | ❌ No | Not documented as planned |

**Decision**: Documentation must clearly mark Functor, Foldable, and Traversable as **planned** (not implemented). ~~Remove Ord from contracts if not planned.~~ **REVISED**: Ord is now marked as **planned** because it's required for `Set (Pattern v)` in Graph structures (see Graph type definition).

**Rationale**: Prevents developers from trying to use non-existent instances. Aligns documentation with actual implementation status. **REVISED**: Ord is required for Graph functionality using Sets, so it must be planned.

#### 3.2 Classification Functions

**Documented vs Implemented**:

| Function | Documented In | Actually Implemented | Status |
|----------|---------------|---------------------|--------|
| `isNode` | DESIGN.md, data-model.md (001), contracts | ❌ No | Planned (TODO.md Feature 8-11) |
| `isRelationship` | DESIGN.md, data-model.md (001), contracts | ❌ No | Planned (TODO.md Feature 8-11) |
| `isSubgraph` | DESIGN.md, data-model.md (001), contracts | ❌ No | Planned (TODO.md Feature 8-11) |
| `isPath` | DESIGN.md, data-model.md (001), contracts | ❌ No | Planned (TODO.md Feature 8-11) |
| `isGraphElement` | contracts | ❌ No | Planned (TODO.md Feature 8-11) |

**Decision**: All classification functions must be clearly marked as **planned** in documentation.

**Rationale**: These functions are referenced in multiple places but don't exist in code. Developers need to know they're not available yet.

#### 3.3 Other Functions

**Documented vs Implemented**:

| Function | Documented In | Actually Implemented | Status |
|----------|---------------|---------------------|--------|
| `pattern` | contracts, TODO.md | ❌ No | Planned (TODO.md Feature 3) |
| `patternWith` | contracts, TODO.md | ❌ No | Planned (TODO.md Feature 3) |
| `source` | contracts, DESIGN.md | ❌ No | Planned (TODO.md Feature 12) |
| `target` | contracts, DESIGN.md | ❌ No | Planned (TODO.md Feature 12) |
| `nodes` | contracts, DESIGN.md | ❌ No | Planned (TODO.md Feature 12) |
| `relationships` | contracts, DESIGN.md | ❌ No | Planned (TODO.md Feature 12) |

**Decision**: All navigation and construction functions must be clearly marked as **planned**.

**Rationale**: Consistent with classification functions - these are documented but not implemented.

---

### 4. Pattern Variant Definitions

**Variants Identified**:
- Empty patterns (elements == [])
- Nodes (interpretation of empty patterns)
- Relationships (exactly 2 elements that are nodes)
- Subgraphs (all elements are graph elements)
- Paths (subgraphs with chained relationships)

**Inconsistencies Found**:
- Some docs describe variants as "interpretations" (view-based)
- Some docs describe variants as "structural classifications"
- Some docs mix both concepts

**Decision**: Pattern variants are **structural classifications** based on element structure that can be **interpreted through different graph views**.

**Rationale**: Based on DESIGN.md's GraphView typeclass and the fact that variants are determined by structure (e.g., relationship has exactly 2 elements), but views provide different semantic interpretations (directed vs undirected).

**Alternatives Considered**:
- Pure structural: Rejected - ignores the view-based interpretation aspect
- Pure view-based: Rejected - variants have structural requirements that exist independently of views

---

### 5. Sequence vs Tree Model Relationship

**Current State**:
- Core.hs explains both but relationship could be clearer
- Some docs emphasize tree, some emphasize sequence
- Relationship between the two isn't always explicit

**Decision**: Establish clear explanation: Patterns are **conceptually sequences** (primary semantic) that are **implemented as recursive trees** (implementation detail). The tree structure supports sequence semantics.

**Rationale**: User agreed to sequence as primary model. The tree implementation is how sequences are represented, but the sequence semantic is what developers should think about.

---

## Summary of Decisions

1. **Pattern Definition**: Sequence-based conceptual model (primary), tree-based implementation (supporting detail)
2. **Value Field**: Use "value" consistently (matches code)
3. **Elements Field**: Use "elements" consistently (matches code, aligns with sequence model)
4. **Pattern Variants**: Structural classifications that can be interpreted through views
5. **Implementation Status**: Clearly mark all planned features (Functor, Foldable, Traversable, classification functions, navigation functions)
6. **Sequence-Tree Relationship**: Explicitly document that tree implements sequence semantics

---

## Documentation Files Requiring Updates

1. `src/Pattern/Core.hs` - Update Haddock comments to use consistent terminology
2. `DESIGN.md` - Align with sequence-based model, mark planned features
3. `README.md` - Already emphasizes sequence model, but verify consistency
4. `specs/001-pattern-data-structure/data-model.md` - Update definition, terminology, implementation status
5. `specs/001-pattern-data-structure/contracts/type-signatures.md` - Mark planned features, mark Ord as planned (required for Graph/Set)
6. `specs/002-basic-pattern-type/data-model.md` - Update definition, terminology
7. `specs/002-basic-pattern-type/contracts/type-signatures.md` - Verify consistency

---

## Next Steps

1. Generate authoritative data-model.md with consistent definitions
2. Create terminology standards document
3. Create implementation status tracking document
4. Generate quickstart guide with consistent examples

