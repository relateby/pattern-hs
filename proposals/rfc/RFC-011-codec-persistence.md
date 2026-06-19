# RFC-011: Codec — Pluggable Persistence Adapters for Pattern

**Status:** draft
**Date:** 2026-06-18
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Depends on:** RFC-001 (Frames and Spans), RFC-007 (RepresentationMap, PatternKind), RFC-004 (GraphClassifier kinds)
**Prerequisite:** scoped identity namespaces — a future RFC (or RFC-010 extension) gating implementation; see Open Question 3
**Followed by:** Rust/TypeScript ports (`pattern-rs`) → downstream adoption (`aie-matrix`)
**Related modules:** `Pattern.Core`, `Pattern.RepresentationMap`, `Pattern.Frame`, `Pattern.Span`, `Pattern.Codec` (new), `Gram.JSON`, `Gram.Schema`

## Summary

Introduce `Codec` as the thin device that crosses the boundary between a *normalized*
`Pattern v` and an external store's bytes. Persistence factors into **three independent
axes**: the **target data model** is a `PatternKind` (RFC-007); the **encoding strategy**
that adapts a pattern into that model is a chain of `RepresentationMap`s; and the
**transport** that reads and writes bytes is a separate `Store`. A `Codec` binds one
`PatternKind` to one transport and is *faithful for that kind by construction* — fidelity
is a property of the `RepresentationMap` chain, not a codec flag. Two regimes follow from
where a target model sits in the model **subsumption lattice**: a **faithful** regime — the
**Frame/Span two-table encoding**, which is the relational/columnar realization of
RFC-001's by-reference storage form, where the externality discipline (a Frame does not
hold its cross-Frame edges; the Span does) *is* the node-table/edge-table split — and a
**lossy** regime — projection onto a native graph store's smaller vocabulary, which needs
discriminator conventions or is treated as a one-way projection. Because the model
relationships are themselves `RepresentationMap`s, "alternative representations with
different qualities" become different map chains composed before the same thin codec.

## Motivation

For any data structure to be adopted, it needs a credible story for persisting at scale in
a real database. Pattern has in-memory operations and a canonical JSON encoding
(`Gram.JSON`), but no first-class, fidelity-aware way to land a `Pattern v` in a store, in
a chosen *representation*, and reconstruct it.

### Why a database, not just a blob

Pattern's operations are in-memory: the substrate is a value in RAM. For *durability alone*
that needs nothing elaborate — serialize the whole `Pattern` to a blob (the `Gram.JSON`
document encoding) and read it back. The document codec covers exactly this case and is the
baseline; if everything fits in memory, you are done.

The reason to design for *database* backends is **scale past RAM** — large ≈ more than fits
in memory. When a Pattern is larger than memory you cannot load it whole, so durability stops
being the problem and *partial, indexed, and streaming access* becomes the problem. That
requires a real storage and query engine underneath: Frame/Span tables to fetch one Frame
without materializing the whole graph-of-graphs, a native graph engine to traverse without
loading, columnar scans over Subject components without reading every row. A blob can do none
of these. So the document codec is the honest durability answer *up to* RAM, and the database
backends earn their keep precisely *past* it (see Open Question 7 on streaming).

### A storage engine, not an object mapper

`Pattern Subject` and its operations are the data model and the query space — the conceptual
layer. Codecs and Stores are a pluggable *storage* layer, chosen by which queries a workload
runs, the way Postgres decouples its relational/SQL layer from a pluggable table-access method
(and, beneath both, Codd's logical/physical data independence). A codec is a **physical
encoding, not a semantic translation**: there is no second model to reconcile, and the store
never tries to look like Pattern.

This is the ANSI/SPARC three-schema split:

- **external** — typed views / OO lenses over the model (per-consumer presentation)
- **conceptual** — `Pattern Subject` + operations (the one model and query algebra)
- **internal** — codecs + stores (pluggable physical engines)

The two fidelity regimes are roles within the internal layer: faithful backends are
interchangeable engines (any can hold the whole model; pick by query fit); the lossy
native-graph backend is a query-optimized replica/index, with the logical truth held in a
faithful engine. ORM/OGM, by contrast, collapse the external and internal schemas — objects
*are* the persistence mapping — which is the source of their impedance mismatch (see
Alternatives).

### Three axes, not "a codec per backend"

A naïve design writes "the relational codec" and "the graph codec" as parallel siblings.
That conflates three things that vary independently:

1. **Target data model** — the abstract model a representation targets (Frame/Span tables,
   property-graph, document). *This is a `PatternKind`* — a recognizable shape of
   `Pattern v` (RFC-004/007).
2. **Encoding strategy** — *how* a source pattern is adapted into that model, with
   differing qualities. This is a `RepresentationMap` chain (RFC-007).
3. **Transport** — where the bytes live and how you talk to it (Neo4j driver, Postgres,
   DuckDB/columnar, Mongo, KV). This is a `Store`.

A *representation* is a point in the product of these axes. The same transport can host
several representations (Postgres can hold a faithful JSONB document *and* the Frame/Span
tables); the same target model can be served by several strategies. Collapsing the axes
hides that structure and forces duplication.

### The models subsume each other — and split into two fidelity regimes

The target models form a **subsumption lattice**, with `Pattern` itself at the top.
Faithful *encodings* sit at the top tier (they hold everything a `Pattern Subject` carries);
genuinely less-expressive *models* sit below:

```
Pattern                       ordered nesting + n-ary elements + decoration — most expressive
  │
  ├─ faithful encodings (≈ Pattern):
  │     document / tree        nested JSON; whole-pattern I/O
  │     Frame/Span tables      node table + edge table; by-reference storage form
  │
  └─ subsumed models (below Pattern):
        property-graph         nodes, typed relationships, properties
          └── relational       tables = labels, join tables = relationships, scalar attrs
```

Where a model sits determines its **fidelity regime**:

- **Faithful regime** — models at/near the top (`document`, `Frame/Span tables`) hold
  everything a `Pattern Subject` carries. Reached by a *lossless* `RepresentationMap`; no
  information is dropped.
- **Lossy regime** — the *native* property-graph vocabulary (Neo4j nodes/typed-rels with
  scalar-only properties) sits strictly below `Pattern` (no ordered nesting, no n-ary).
  Projecting an arbitrary pattern onto it loses information unless conventions re-encode
  the residual in-band, or it is accepted as a one-way query projection.

### Why a separate device at all?

`RepresentationMap` is `Pattern v → Pattern v` — pure shape adaptation within
Pattern-space, including the model relationships above. A store is *not* a point in
Pattern-space, so making it a `codomain` of a `RepresentationMap` is a category error: the
boundary is effectful and deals in backend-native bytes. `Codec` is that boundary, and
nothing more. The two compose:

```
Pattern --[RepresentationMap chain: choose/compose a target shape]--> Pattern(targetKind)
        --[Codec: targetKind ⇄ bytes]--> Store
```

## Design

### Fidelity is a consequence of subsumption — and splits cleanly into two regimes

A codec serializes a *known kind* to bytes and back, so it is **faithful for its
`targetKind` by construction**. Fidelity is therefore a property of the
`RepresentationMap` chain that reaches the codec's `targetKind`, not a codec flag:

> A representation is **faithful** iff its target model subsumes its source kind in the
> lattice, *or* the chain's conventions re-encode the residual sufficient for a strict
> round-trip (RFC-007's `roundTrip` witness holds on `domain`). Where neither holds — the
> native graph store as a query-optimized projection with the canonical form held
> elsewhere — the mapping is a one-way **projection**, not a `RepresentationMap`, and
> round-trip is not claimed.

This retires the earlier "lossy RepresentationMap" wording: a `RepresentationMap` is always
invertible on its domain; loss lives either in declared conventions that *restore*
invertibility, or in an explicitly one-way projection that is not a `RepresentationMap`.

### `Codec` — bind a kind to a transport

```haskell
-- | The pure shape↔bytes mapping for ONE target kind against ONE backend model `b`.
data Codec b v = Codec
  { codecName  :: Text
  , targetKind :: PatternKind v   -- the normalized kind this codec faithfully serializes
  , encode     :: Pattern v -> b
  , decode     :: b -> Either DecodeError (Pattern v)
  }

data DecodeError
  = KindViolation   Text   -- stored data does not satisfy `targetKind`
  | MissingConvention Text -- an ambiguous shape lacks the discriminator needed to invert
  | MalformedValue  Text   -- a value column/cell could not be parsed
  | DanglingRef     Text   -- an element/endpoint FK references a row that is absent
  deriving (Eq, Show)
```

The codec performs no I/O; it only translates the shape it owns. This keeps it pure,
property-testable without a database, and faithful for `targetKind`.

### Transport is a separate concern

```haskell
class Monad m => Store b m where
  persist  :: b -> m StoreKey
  retrieve :: StoreQuery b -> m b
```

`Store` is deliberately minimal — single-unit `persist`/`retrieve`. **Batching and
transactionality are a layer above**, not methods here: bulk seeding and multi-statement
atomic writes (`persistMany`, `withTransaction`) are orchestration over a `Store`, transport-
specific and orthogonal to the codec's shape concern. Keeping them out of the interface keeps
the boundary thin and every backend's `Store` instance trivial (Open Question 2).

Conceptually a `Store` is the **persistent analog of an RFC-001 Frame**: a handle over a
persisted collection, with operations over it and simple add/remove of contained elements.
The signature above is the in-RAM baseline — `retrieve` materializes a whole `b`. Past RAM,
where a result or even a single element exceeds memory, retrieval **streams** instead of
materializing (the scale extension; the durability baseline materializes, the streaming `Store`
does not). Streaming changes the shape of both ends — a `retrieve` that yields incrementally
and a `decode` that consumes incrementally — so the whole-`b` signature above is the baseline,
not the final word; the streaming surface is settled when that step is built (Step 5).
Random-access **cursor** navigation — which aligns with Zippers — is deferred beyond that;
near term, streaming only (Open Question 7).

End-to-end save/load are thin compositions of a `RepresentationMap` chain (shape), a
`Codec` (bytes), and a `Store` (transport). Use `idMap` when the source is already of the
codec's `targetKind`:

```haskell
saveVia :: (Store b m, ScopeQuery q v)
        => RepresentationMap v -> Codec b v -> q v -> Pattern v -> m StoreKey
saveVia rmap codec scope = persist . encode codec . forward rmap scope

loadVia :: (Store b m, ScopeQuery q v)
        => Codec b v -> RepresentationMap v -> q v -> StoreQuery b
        -> m (Either DecodeError (Pattern v))
loadVia codec rmap scope q = do
  b <- retrieve q
  pure $ inverse rmap scope <$> decode codec b
```

### The faithful regime: the Frame/Span two-table encoding

RFC-001 (§ What this RFC does not include) defers database-backed Frames to "a layer above
pattern-hs," on the principle that the substrate is in-memory. **RFC-011 is that layer:**
`Store` and `Codec` live in the persistence layer, the substrate gains nothing, and the
in-memory by-value form remains canonical — storage is its by-reference projection.

This is the relational/columnar realization of RFC-001's **by-reference storage form**,
which RFC-001 (§ Inter-Frame topology) names as "the natural serialization-and-storage
form." Its faithfulness is not an accident of schema design — it is the **externality
discipline** of Frames and Spans serialized directly:

- A **Frame** (RFC-001) is a self-contained module: its elements do not reach across to
  other Frames. → a **frame-table** (the *node table*) of rows whose element FKs reference
  only same-frame rows.
- A **Span** (RFC-001) holds the cross-Frame correspondences (a Bundle of pair-elements)
  *externally* — a Frame never holds its cross-Frame edges. → a **span-table** (the *edge
  table*).

"Every table is a label, every join table is a relationship" thus stops being an imposed
convention and becomes the direct image of RFC-001's principle 2. ("Two-table" here means two
*kinds* of table — node and edge — realized physically as `frame`/`frame_row` for the node
side and `span`/`bundle_pair` for the edge side.)

| RFC-001 (in-memory, by-value) | RFC-011 (by-reference storage) |
|---|---|
| Frame (self-contained module) | a `frame_row` row-set under one `frame_id` |
| Frame element (Pattern / Portal) | a row: Subject columns + ordered child-FK array |
| Self-containment (Reading 1) | element FKs reference **only same-frame rows** |
| Span (relates two Frames) + its Bundle | one `span` row (Span/Bundle Subjects on the row) |
| Bundle pair-element (cross-Frame edge) | one `bundle_pair` row: `(span_id, src_ref, tgt_ref, …)` |
| Externality (edges live in the Span) | edges live in `bundle_pair`, never in `frame_row` |

Logical schema — column names and relationships are the contract; physical types are
transport-specific (`array<…>` is `text[]` in Postgres, `LIST` in DuckDB/Arrow; `json` is
`jsonb` or a columnar struct). Subject identity is **frame-scoped** (the same identity may
recur across Frames), so `frame_row`'s key is composite `(frame_id, id)` — which is what
forces endpoint FKs to carry a frame id:

```
-- frame-table : within-module structure, self-contained, faithful
frame(frame_id, root_id)
frame_row(frame_id, id, labels array<text>, properties json, elements array<id>)
  PRIMARY KEY (frame_id, id)              -- identity is frame-scoped
                                          -- elements: ordered, refs stay in-frame; an array, so
                                          -- NOT FK-enforceable — dangling element refs are DanglingRef

-- span-table : the Span's Subject + its (0..1) Bundle's Subject, denormalized
span(span_id, subject_id, labels array<text>, properties json, frame_a, frame_b,
     bundle_id?, bundle_labels array<text>?, bundle_properties json?)
  PRIMARY KEY (span_id)
  UNIQUE (span_id, frame_a), UNIQUE (span_id, frame_b)   -- enable the through-span FKs below
                                          -- Bundle Subject (if any): bundle_* ; bundle_id is an
                                          -- attribute, NOT an FK (there is no bundle table)

-- bundle_pair : the Bundle's pair-elements — one row per inter-frame relationship (the edge table)
bundle_pair(span_id, src_frame, src_ref, tgt_frame, tgt_ref, pair_subject_id?,
            labels array<text>, properties json)
  FOREIGN KEY (span_id)            REFERENCES span(span_id)              -- cascade-on-delete
  FOREIGN KEY (span_id, src_frame) REFERENCES span(span_id, frame_a)    -- src_frame = the span's frame_a
  FOREIGN KEY (span_id, tgt_frame) REFERENCES span(span_id, frame_b)    -- tgt_frame = the span's frame_b
  FOREIGN KEY (src_frame, src_ref) REFERENCES frame_row(frame_id, id)   -- src endpoint exists in that frame
  FOREIGN KEY (tgt_frame, tgt_ref) REFERENCES frame_row(frame_id, id)   -- tgt endpoint exists in that frame
                                          -- keyed by span_id (span:bundle is 1:0..1, so span_id
                                          -- functionally determines the bundle)
```

The endpoint frame ids (`src_frame`/`tgt_frame`) are duplicated from the span deliberately:
under frame-scoped identity they are required for *any* endpoint FK, and the through-span
FKs additionally make the externality invariant declarative — a pair-element may only connect
`frame_a` content to `frame_b` content, enforced by the database rather than by application
code. (A global surrogate key on `frame_row` would avoid the duplication but reduce the
endpoint FKs to "exists somewhere," losing frame-pair correctness; see Alternatives.)

**Decisions baked in (settled in design review):**

- **Span : Bundle is 1:(0..1)**, fixed by RFC-001 (the Bundle is the Span's optional third
  element). Multiple correspondence-sets between the same two Frames are expressed as multiple
  *Spans*, not multiple Bundles. The 1:N lives one level down — **Bundle : pair-elements** —
  and that is the `bundle_pair` table (many rows per span).
- **Span and Bundle identities live on the `span` row**, not as separate entities — flatter
  to query, and valid precisely because span:bundle is 1:(0..1). There is **no `bundle` table**:
  the Bundle's Subject is the `bundle_*` columns, its pair-elements are the `bundle_pair` rows.
- **Pair-elements are keyed by `span_id` and materialized per span**, not shared by
  `bundle_id`. This makes each span self-contained — Frame self-containment applied to the
  edge table — so `ON DELETE CASCADE` works, endpoint FKs are declarable (a bound span has
  a single valid frame pair), and concurrent writers do not contend on shared edge rows.
- **`bundle_id` is a non-enforced attribute** (a column, not an FK — it points at no table),
  retained so the in-memory *shared* Bundle form (RFC-001 allows Bundle sharing) can be
  reconstituted on read *on explicit request* when two spans carry identical pairs under the
  same id — not automatically, and without the database ever depending on it (Open Question 4).
  A third (`bundle`) table would be needed only to reintroduce the rejected shared-bundle
  semantics; see Alternatives.
- **Escape hatch** for a genuinely large, shared correspondence: promote the Bundle to a
  first-class stored entity (it is a `Pattern Subject`, so it gets its own `frame_row`s)
  with an explicit, managed lifecycle. Opt-in, eyes-open sharing — not the implicit default
  that breaks cascade and FK enforcement.

**Faithfulness.** The by-value ↔ by-reference relationship is a lossless
`RepresentationMap` (resolve `FrameRef`/`SpanRef` through an `id → Pattern` map; RFC-001
§ Inter-Frame topology):

```haskell
byReference :: RepresentationMap Subject      -- by-value Pattern  ↔  Frame/Span by-reference
byReference = RepresentationMap
  { name        = "by-value ↔ by-reference (Frame/Span)"
  , domain      = anyPattern
  , codomain    = frameSpanRefKind
  , conventions = [ "Frames stored once; Spans reference Frames by identity"
                  , "cross-Frame edges externalized as bundle_pair rows (RFC-001 principle 2)" ]
  , forward     = toByReference
  , inverse     = resolveByReference
  , roundTrip   = byReferenceRoundTrip       -- strict on anyPattern with identified subjects
  }
```

A **single frame-table** faithfully stores a lone self-contained Frame; **frame-table +
span-table** scales it to the graphs-of-graphs RFC-001 motivates (the airplane modeled as
mechanical / electrical / maintenance Frames related by Spans). This subsumes the looser
"pattern-table" and "normalized-adjacency" sketches considered earlier — they are the
one-Frame and many-narrow-tables points of this same principled encoding.

### The lossy regime, and the two distinct "relational" stories

There are two unrelated things that both touch "relational," and conflating them was a flaw
in an earlier draft:

1. **Pattern faithfully *encoded into* relational/columnar storage** — the Frame/Span tables
   above. Uses an RDBMS or columnar store (Postgres, DuckDB/Parquet) as a *transport* for a
   faithful encoding. This is the faithful regime.
2. **The native relational *model*** (`relationalKind`: tables = labels, join tables = rels,
   scalar attrs) — a *subset of property-graph*, for **interoperating with existing
   relational data**. The relational ↪ property-graph embedding is an ordinary
   `RepresentationMap` whose `conventions` are the table/join-table rules:

```haskell
relationalToGraph :: RepresentationMap Subject   -- relational data ↪ property-graph
relationalToGraph = RepresentationMap
  { name        = "relational ↪ property-graph"
  , domain      = relationalKind
  , codomain    = graphKind
  , conventions = [ "each table T -> nodes labeled T; columns -> node properties"
                  , "foreign-key column -> relationship to the referenced node"
                  , "M:N junction table -> relationship type; non-key columns -> rel properties" ]
  , forward     = embedRelationalAsGraph
  , inverse     = projectGraphAsRelational           -- partial: on the relational sub-kind
  , roundTrip   = relationalRoundTrip
  }
```

Note what *neither* story is: **faithful round-trip of an arbitrary RDBMS.** `relationalToGraph`
is query-interop — it views relational data *as* a graph and makes no claim to reconstruct the
source database. Faithfully ingesting an RDBMS and getting the same RDBMS back is a third,
distinct concern (it requires capturing the schema/catalog — PKs, FKs, types, constraints — as
data, whose natural faithful encoding is the Frame/Span model: table → Frame, foreign key →
Span, catalog → Frame-of-Frames). That is a relational **source adapter**, separate from
"persist Pattern into a store," and out of scope for this RFC (Open Question 1).

The genuinely **lossy** projection — an arbitrary pattern onto the *native* graph store
vocabulary — is also a map, with discriminators emitted only on target shapes whose preimage
is non-singleton (derivable from `domain`); restricting `domain` to an application's schema
minimizes the tags:

```haskell
patternToGraph :: PatternKind Subject -> RepresentationMap Subject
patternToGraph appDomain = RepresentationMap
  { name        = "Pattern ↠ native property-graph"
  , domain      = appDomain
  , codomain    = graphKind
  , conventions = [ "binary pattern -> relationship; tag _pk=binary only where it collides with a native relationship"
                  , "annotation -> self-loop; tag _pk=annotation only where it collides with a self-reference"
                  , "rich Value (VMap/VArray/VRange/VMeasurement/VTaggedString) -> decomposed sub-nodes or property_json" ]
  , forward     = projectPatternAsGraph appDomain
  , inverse     = liftGraphAsPattern appDomain
  , roundTrip   = graphRoundTripOn appDomain          -- strict on appDomain; tags restore invertibility
  }
```

Used as a faithful-with-tags map it satisfies `roundTrip`; used as a query projection with
the canonical form held in Frame/Span storage, round-trip is simply not claimed.

### Target kinds (grounding)

- `frameKind` / `spanKind` — from RFC-001: a Frame is a self-contained `Pattern Subject`;
  a Span is a relationship-shaped `Pattern Subject` (2–3 elements). `frameSpanRefKind` is the
  by-reference composite (Frames once + Spans referencing them).
- `graphKind` — RFC-004's graph classification (`GNode`/`GRelationship`/`GWalk`/`GAnnotation`).
- `anyPattern` — the document model (≈ lattice top).
- `relationalKind` — the native relational model. Its predicate is non-trivial because
  relationality is largely about *references* (foreign keys), which are scope-relative rather
  than locally structural. `relationalKind` is therefore defined operationally as the `domain`
  of `relationalToGraph` — the three-axes story holds either way, since the embedding, not a
  standalone predicate, is what interop needs (Open Question 1, resolved).

### Reference Codecs (thin, one `targetKind` each)

```haskell
documentCodec :: Codec Value Subject          -- document model ≈ Pattern (lattice top)
documentCodec = Codec "document" anyPattern patternToValue decodeJSON
  -- decodeJSON :: Value -> Either DecodeError (Pattern Subject), via patternFromValue

frameSpanCodec :: Codec FrameSpanTables Subject  -- the faithful Frame/Span tables (RDBMS / columnar)
frameSpanCodec = Codec "frame-span" frameSpanRefKind toFrameSpanTables fromFrameSpanTables

graphCodec :: Codec [GraphOp] Subject          -- native node/rel/property model
graphCodec = Codec "neo4j" graphKind toGraphOps fromGraphResult
```

Representations are assembled by composition — *the* point of leaning on RFC-007:

```haskell
saveVia idMap            documentCodec  scope p   -- faithful document archival
saveVia byReference      frameSpanCodec scope p   -- faithful Frame/Span tables (default for scale)
saveVia relationalToGraph graphCodec    scope rel -- existing relational data into a graph store
saveVia (patternToGraph appDomain) graphCodec scope p  -- arbitrary pattern into native Neo4j
```

### Failure modes

`decode` is **total** — it returns `Either DecodeError`, never throws — and classifies
failures via `DecodeError`: `KindViolation` (stored data does not satisfy `targetKind`,
e.g. schema drift), `MissingConvention` (an ambiguous shape under a lossy map lacks its
discriminator), `MalformedValue` (an unparseable cell), and `DanglingRef` (a reference to an
absent row). `bundle_pair` endpoints are FK-enforced where the transport supports composite
FKs, so `DanglingRef` there is a backstop for transports that do not; the `frame_row.elements`
array is *not* FK-enforceable in standard SQL, so dangling element refs are decode-time
`DanglingRef` checks regardless of transport. `persist` is atomic at the single-unit
granularity; multi-statement transactional and bulk writes are a layer above `Store`
(Open Question 2, resolved).

### The seed-then-own use case

Seed a store from gram-authored `Pattern`, then treat the store as the source of truth.
Two postures:

- **Faithful (Frame/Span tables):** `saveVia byReference frameSpanCodec` seeds; runtime reads
  round-trip losslessly. Use when the store must be authoritative without loss.
- **Native graph (Neo4j as SoT):** `saveVia (patternToGraph appDomain) graphCodec` seeds. At
  runtime the decode domain is the application's schema, shrinking discriminators to the few
  app shapes that collide; the canonical form may also be retained in Frame/Span storage,
  making Neo4j a query projection.

"Source of truth" here means the store owns the *data*, never the *identity*. Identity is
owned by `Subject.identity` (next section).

### Identity is owned by `Subject.identity`

`Subject.identity` is the authoritative identity; the store never mints an identity of record.
A `StoreKey` is a *physical locator* (row address, handle) subordinate to `Subject.identity` —
decode resolves store keys back to subject identities, not the reverse. **Upsert keys on
`Subject.identity`**, and write-time conflicts (same identity, differing labels/properties) are
resolved by **RFC-010 reconciliation** — persistence get/put *is* the I/O boundary at which
RFC-010 says reconciliation runs.

This commits the local rule but leaves a larger problem open and **upstream**: scoped
identity namespaces. Ingesting multiple `.gram` files clashes identities, because each file
carries its own id-space (anonymous `#1`/`#2`, or human-chosen ids that collide across files).
Resolving how id-spaces are scoped, qualified, or rewritten on ingest is a cross-cutting
identity concern — it touches parsing, RFC-010 reconciliation, Frame boundaries, and this
layer's upsert/dedup — and is **bigger than this RFC and a prerequisite for it.** Downstream
(`aie-matrix`) is already hitting it. The Frame/Span schema's frame-scoped key `(frame_id, id)`
— Frame as namespace — is this RFC's local manifestation of that scoping; the prerequisite must
formalize how those frame namespaces are assigned so cross-file ingestion is clash-free. It
warrants its own RFC (or an RFC-010 extension), settled before RFC-011 implementation (see
Open Question 3).

### Relationship to Gram.Schema

Long-term, the `PatternKind`s and the embedding `conventions` could be *generated* from the
same schema that already drives `Gram.Schema`'s TypeScript/Rust type generation — one
`label → type` declaration projecting into the typed view, the Frame/Span table layout, and
the graph discriminator set. This RFC specifies the devices and hand-writes them; generation
is **deliberately deferred** — the stack is too immature to automate before we have hand-written
several codecs and kinds and learned what actually recurs (Open Question 6).

### Acceptance criteria

Observable, regardless of file/package layout:

1. `saveVia idMap documentCodec` round-trips any `Pattern Subject` byte-for-byte through
   `Gram.JSON`.
2. Seeding a multi-Frame pattern (e.g. an `aie-matrix` NPC + map + calendar config) via
   `saveVia byReference frameSpanCodec`, then `loadVia`, yields a `Pattern` structurally
   equal to the original — including cross-Frame correspondences, which appear as
   `bundle_pair` rows and never inside any frame.
3. Deleting a `span` row removes exactly its `bundle_pair` rows (cascade) and no `frame_row`s.
4. `decode` of a row set with a dangling endpoint FK returns `Left (DanglingRef …)`, never
   throws.

### Implementation Sequence

Steps 1–2 (faithful encode/decode and round-trip) need no identity scoping and can proceed
immediately. **Upsert and seed-then-own, however, are gated on the scoped-identity-namespace
prerequisite** (Open Question 3) — do not build identity-keyed upsert until that is settled.

**Step 1 — Codec, Store, faithful document baseline.** Define `Codec`, `DecodeError`,
`Store`, `saveVia`/`loadVia`; implement `documentCodec` over `Gram.JSON`; Hedgehog
round-trip; in-memory `Store` fake. **Demo (~15 min, no database):** round-trip
`examples/*.gram` through `documentCodec` against the in-memory `Store` and assert
structural equality.

**Step 2 — Frame/Span tables.** Define `frameKind`/`spanKind`/`frameSpanRefKind` over RFC-001;
implement `byReference` as a `RepresentationMap` and prove its round-trip with RFC-007's
harness; implement `frameSpanCodec`; verify acceptance criteria 2–4 against the in-memory and
a SQLite `Store`.

**Step 3 — Native graph + relational interop + real driver.** Implement `patternToGraph`
(lossy, declared conventions) and `relationalToGraph` (faithful embedding); `graphCodec`;
provide a real `Store [GraphOp] IO` against a Neo4j driver (outside the pure library).

**Step 4 — Ports** (`pattern-rs`, TypeScript). The pure `Codec` and the `RepresentationMap`s
port directly; `Store` is implemented per language against native drivers. The document codec
is the first port — the immediate need in `aie-matrix`.

**Step 5 — Streaming retrieval (the past-RAM scale increment).** A streaming `retrieve`
variant that yields incrementally and an incremental `decode`, for stores whose results or
elements exceed memory; this step settles the streaming surface left open above. It is the
scale increment the motivation calls for, sequenced after the core device lands; random-access
cursors (Zippers) are a later, separate addition (Open Question 7).

## Open Questions

All seven are dispositioned below. The only live *external* dependency is #3 (scoped identity
namespaces), a prerequisite gating implementation; #5 and #6 are deliberate deferrals. RFC-011
may be **accepted as a design** independently of #3, but its identity-dependent implementation
(upsert, seed-then-own) is gated on that prerequisite landing first.

1. **`relationalKind` expressibility, and faithful RDBMS ingestion. — Resolved.** Two
   concerns were conflated here, and separating them resolves both:
   - **Predicate sub-question (closed):** the query-interop path (`relationalToGraph`) needs
     no standalone `relationalKind` *predicate*. `relationalKind` is defined operationally as
     the embedding's `domain` — recognizing relational-shaped patterns is the embedding's
     concern, not a separate classifier (see § Target kinds). Foreign keys being scope-relative
     is precisely why a local structural predicate is the wrong tool.
   - **Faithful RDBMS round-trip (out of scope):** ingesting an arbitrary RDBMS *as* Pattern
     and reconstructing the same RDBMS is a different problem from "persist Pattern into a
     store." It requires capturing the schema/catalog (PKs, FKs, types, constraints) as data,
     and its natural faithful encoding is the Frame/Span model (table → Frame, FK → Span,
     catalog → Frame-of-Frames) — the faithful regime, not `relationalToGraph`. It is a
     relational **source adapter** and belongs in its own future RFC ("RDBMS ⇄ Frame/Span");
     RFC-011 makes no faithful-RDBMS claim (see § two distinct "relational" stories).
2. **Batching / transactionality. — Resolved: a layer above.** `persist` stays
   one-unit-at-a-time; `Store` does not grow a batch/transaction surface. Bulk seeding and
   multi-statement atomic writes (`persistMany`, `withTransaction`) are orchestration *over* a
   `Store` — transport-specific and orthogonal to encoding — so they live above the boundary,
   keeping `Store` instances trivial and the scope tidy (see § Transport is a separate concern).
3. **Identity and upsert. — Decided in principle; blocked on a prerequisite.** Identity is
   owned by `Subject.identity`, not the store; `StoreKey` is a subordinate physical locator;
   upsert keys on identity; write-time conflicts are RFC-010 reconciliation at the I/O boundary
   (see § Identity is owned by `Subject.identity`). What remains open is **bigger than this RFC
   and a prerequisite for it:** scoped identity namespaces for clash-free ingestion of multiple
   `.gram` files (each carrying its own id-space). That is a cross-cutting identity concern
   (parsing, RFC-010, Frame boundaries, persistence) that RFC-011 depends on; it warrants its
   own RFC (or an RFC-010 extension) and should be settled before RFC-011 implementation.
   Downstream (`aie-matrix`) is already hitting it.
4. **Shared-Bundle reconstruction. — Resolved: explicit.** On read, decode produces the
   materialized (per-span) Bundle as stored; identical `bundle_pair` sets under one `bundle_id`
   are *not* automatically coalesced into a shared in-memory Bundle. Reconstituting sharing is
   a deliberate, opt-in operation the caller requests — keeping the read path simple,
   predictable, and free of implicit cross-span coupling.
5. **Representation registry. — Resolved: deferred (deliberate).** A catalog of
   `(model × strategy) → (map chain, codec)` letting callers pick a representation by desired
   qualities is *not* part of this RFC. Until at least two real embeddings exist and the
   composition ergonomics are understood, a registry would be premature abstraction over a
   single example. Representations are assembled explicitly at call sites via `saveVia` /
   `loadVia`; the registry is revisited once there is enough variety to generalize from.
6. **Schema-driven generation. — Resolved: deferred (deliberate).** Generating the kinds,
   table layout, and graph conventions from `Gram.Schema` is premature: the whole stack is too
   immature to automate. The codecs and kinds are hand-written first; only after exercising
   several of them — and seeing what genuinely recurs — is there expertise to generate from.
   Revisit once the hand-written versions are stable.
7. **Streaming vs. cursors. — Resolved: streaming now, cursors (Zippers) later.** *(The
   earlier wording was awkward.)* The concern is that a store holds Patterns too large for
   memory — a query result, or even a single element, may exceed RAM — not that "one Pattern
   won't fit." A `Store` is the persistent analog of an RFC-001 Frame: a handle over a
   persisted collection, exposing operations over it plus simple add/remove of contained
   elements (see § Transport is a separate concern). Because results and elements can exceed
   RAM, the near-term answer is **streaming** retrieval — incremental, never materializing the
   whole value — which is the simpler model and pairs with the scale motivation (durability
   baseline materializes in RAM; the streaming `Store` is the past-RAM extension). Random-access
   **cursors** are deferred: they align closely with **Zippers** (a focus + surrounding context
   over an immutable Pattern), so the cursor surface should follow the in-memory Zipper rather
   than be invented here. Near term: streaming.

## Alternatives

**Extend `RepresentationMap` to target stores directly** — make a database a `codomain`.
Rejected as a category error: a store is not a `Pattern v`, the boundary is effectful, and
serialization deals in backend bytes. This RFC *does* lean on `RepresentationMap` for the
entire shape half — including the by-reference Frame/Span encoding and model subsumption —
and adds only the thin boundary crossing.

**Monolithic per-backend codecs that bake in shape strategy** — "the relational codec", "the
graph codec" as siblings, each embedding its own shape decisions and fidelity flag. Rejected:
it duplicates shape logic across backends (relational-into-a-graph-store would be a whole new
codec instead of `relationalToGraph >>> graphCodec`), cannot reuse the subsumption embeddings,
and hides *where* loss is introduced. Thin codecs + `RepresentationMap` chains keep loss
declared and tested in one place (RFC-007).

**Global surrogate key on `frame_row` (instead of frame-scoped `(frame_id, id)`).** Would let
`bundle_pair` endpoints be single-column FKs with no `src_frame`/`tgt_frame` duplication.
Rejected as the default: Subject identity is frame-scoped (the same identity may recur across
Frames), so a global key requires minting surrogate ids and an identity-resolution layer, and
— more importantly — it reduces endpoint FKs to "exists somewhere," forfeiting FK-level
enforcement of the externality invariant (an endpoint must live in the span's `frame_a`/`frame_b`).
Carrying the two frame columns is cheap and makes that invariant declarative. A global key
remains available to a transport that prefers it, at that cost.

**Shared Bundles persisted by reference (`bundle_id` grouping).** Tempting for storage dedup
and faithful to RFC-001's in-memory sharing. Rejected as the *default* at scale: a shared
Bundle has no single owning Span, so `ON DELETE CASCADE` cannot apply (forcing app-level
refcount/GC), endpoint foreign keys become undeclarable (no single valid frame pair), and
concurrent writers contend on shared edge rows. Storage materializes per span; sharing is
preserved as an *opt-in* read-time reconstruction (`bundle_id` as a non-enforced attribute;
Open Question 4) and an explicit promote-to-entity escape hatch.

**An existing Haskell persistence library as the whole answer** (`persistent`, `beam`,
`esqueleto`, `hasql`). Rejected as a *replacement* for these devices: they map *records* to
tables and do not model a recursive meta-structure or its shape strategy. They remain strong
candidates to *implement* a `Store` (and a backend `Codec`'s encode/decode against SQL) — they
are a transport-layer choice, not a substitute for the kind/representation machinery.

**An ORM/OGM-style model translation** — map `Pattern` to/from a second first-class model
(stateful domain objects with lazy-loading and an identity map, or the store's native model
dressed up as objects) and reconcile the two. Rejected *at the premise*, not just the
mechanics: ORM and OGM exist to make one model *look like* another — the store's `M` like the
application's `O` — and the impedance mismatch is the standing cost of maintaining two models
and a translation between them. This RFC has **one** model, `Pattern Subject` and its
operations, and treats backends as pluggable storage engines beneath it (logical/physical
independence), not as a model to reconcile; a codec is a physical encoding, not a translation.
The secondary symptoms the Haskell ecosystem also rejects — a third source of truth, a mutable
object graph `Pattern` is better than — follow from that premise. OO ergonomics, when wanted,
are an external-schema view/lens over the one model (ANSI/SPARC), never the persistence path.

**One canonical store only** (e.g. "everything is a document"). Simplest, and `documentCodec`
already covers it as the default. Rejected as the *whole* answer because adoption needs native
query power (SQL, Cypher) and scalable storage for some workloads; the axes let each deployment
choose its fidelity/queryability trade-off rather than imposing one.

**Hand-rolled per-application persistence** — the status quo in `aie-matrix`
(`Pattern → OO → Neo4j`, by hand). Rejected: bespoke, untested round-trips, no declared
fidelity, no reuse across ports — exactly the friction this RFC removes.
