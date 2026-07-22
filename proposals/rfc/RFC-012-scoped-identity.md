# RFC-012: Scoped Identity Namespaces

**Status:** draft
**Date:** 2026-07-06
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Depends on:** RFC-001 (Frames are the scope-grounding primitive), RFC-003 (uses gram's leading header-record mechanism), RFC-010 (consumes resolved identity; owns post-hoc merge)
**Followed by:** RFC-011 (Codec) implementation — this RFC is the prerequisite gating its identity-dependent paths (upsert, seed-then-own, clash-free ingest)
**Related modules:** `Subject.Core`, `Pattern.Core`, `Pattern.Reconcile`, `Gram.Parse` (header entry point `fromGramWithHeader`), `Gram.Transform`, `Pattern.Frame` (RFC-001), `Pattern.Codec` (RFC-011)
**Research companion:** [proposals/research/identity-management-survey.md](../research/identity-management-survey.md) — cited prior-art survey (RDF, Datomic, Neo4j, UUIDv5, Matrix, SQL, Wikidata)

## Summary

This RFC defines how a `Subject`'s bare local identity becomes a stable, clash-free identity when patterns are ingested into a shared store. Identity is **`(scope, local)`**: an element's resolved identity is its nearest identified ancestor plus a local discriminator, a chain that bottoms out at a **Frame** whose id *is* the scope. Named elements resolve to `(frame_scope, name)`; anonymous elements are **weak entities** identified by `(nearest named ancestor, ordinal)`, grounded through that ancestor to the Frame. A Frame's scope is declared by a reserved header key **`namespace`** and, when absent, assigned by the store on first ingest. Truly anonymous elements remain permitted everywhere in text and in memory; they are constrained in exactly one place — they may not be the *target* of a by-identity reference (a cross-Frame Span endpoint or a stored foreign key) until promoted, and promotion is anchored to a Frame. The design draws its two load-bearing mechanisms from prior art that maps almost one-to-one onto the problem: RDF's document-scoped blank nodes and skolemization, and Datomic's per-transaction tempids resolved to durable ids at the boundary.

## Motivation

RFC-011 (Codec) can persist a `Pattern v` into a store, but its Open Question 3 flagged a prerequisite it could not resolve within its own scope: **scoped identity namespaces.** A `.gram` file carries its own id-space — human-chosen symbols (`alice`) and parser-assigned anonymous ordinals (`#1`, `#2`) that are only meaningful *within that file*. The moment two files are ingested into one store, those id-spaces collide, and RFC-011's upsert (which keys on identity) has no principled way to know whether two `alice`s are the same entity or two different ones. RFC-011 committed the local rule — identity is owned by `Subject.identity`, the store never mints an identity of record — but explicitly deferred the cross-cutting question of how id-spaces are scoped, qualified, or grounded on ingest to a future RFC. This is that RFC.

The problem is not hypothetical: downstream (`aie-matrix`) is already hitting it, seeding a store from multiple gram-authored files whose ids were never designed to coexist.

### The five requirements

Any solution must satisfy all five together, and two of them pull against each other:

1. **Clash-free ingest.** `(alice)` in `a.gram` and `(alice)` in `b.gram` are different entities *by default*. Per-file id-spaces must not silently merge.
2. **Anonymous identity that travels.** `#1`/`#2` are minted per file. When an anonymous element must be referenced from outside its file (a cross-Frame edge, a stored row), it needs a stable, clash-free identity.
3. **Deliberate cross-file sameness.** The opposite of (1) must remain expressible: the *same* entity legitimately appearing in several files, identified across them on purpose.
4. **Idempotent re-ingest.** Loading the same file twice must upsert, not duplicate.
5. **Frames as namespace boundaries.** RFC-011's faithful encoding already keys the Frame/Span node table on `(frame_id, id)`. The identity model should formalize that composite, not fight it.

**The central tension is (1) vs (4).** Qualifying ids by something unique-per-ingest (a timestamp, a random scope) gives clash-freedom but destroys idempotency. Qualifying by something deterministic (file content hash) gives idempotency but forces a decision about what happens when the file changes. The prior art picks sides — RDF skolemization is per-ingest-unique; UUIDv5 is deterministic — and the resolution here is to let the *author* decide by declaring scope, with a safe store-assigned fallback when they don't.

### Why not just mint global ids?

RDF's IRIs are globally scoped: the same IRI anywhere denotes the same resource. But that works *only* because authors mint identifiers under authority they control ([RDF 1.1 Concepts §1.3](https://www.w3.org/TR/rdf11-concepts/)). Gram's bare `alice` has no such authority — it is a convenience name, not a claim of global identity. Forcing authors to write globally-unique identifiers everywhere is exactly the friction that made RDF's community guidance ("name everything with URIs") hard to follow and left blank nodes pervasive in practice. We want bare local names to stay bare, and to acquire scope at the boundary.

### Why not resolve identity after the fact?

OWL's `owl:sameAs` lets anyone assert that two identifiers denote the same thing — and its real-world use is a cautionary tale: usage "almost always violates" the strict semantics, and because the relation is symmetric and transitive, third parties can attach identity to your entity without permission, producing runaway transitive closures ([Halpin/Herman/Hayes, W3C 2010](https://www.w3.org/2009/12/rdf-ws/papers/ws21)). The lesson is not "never identify across sources" but "make it a deliberate, scoped, key-based act at ingest, not an open global inference." Deliberate cross-file sameness (requirement 3) is served by unique-key upsert, not by equivalence assertions baked into storage identity. Discovering sameness *after* ingest is a distinct problem that belongs to RFC-010 reconciliation (see Open Question 4).

## Design

### Identity is `(scope, local)`, grounded at a Frame

Every element's resolved identity is a pair: a **scope** and a **local discriminator**. The scope is not free-floating — it is the resolved identity of the element's *nearest named ancestor*, which makes identity a chain:

```
element  →  nearest named ancestor  →  … →  Frame
                                              └── scope authority (declared or assigned)
```

The chain bottoms out at a **Frame** (RFC-001), whose own id is the scope authority. Resolving an element means walking up to the Frame and composing the discriminators encountered on the way down. This is the same shape three independent traditions converged on (see the survey): RDF's named-graph-scoped identity, Datomic's per-transaction tempid resolution, and Neo4j's import id-groups.

Two populations sit on this chain:

- **Named elements are strong entities.** Their local discriminator is their author-chosen name, so they resolve to `(frame_scope, name)`. Two files each naming `alice` resolve to `(frame_a, alice)` and `(frame_b, alice)` — distinct by construction, delivering requirement 1.

- **Anonymous elements are weak entities.** They have no name of their own; they are identified by their owner plus a local ordinal — `(nearest named ancestor, ordinal)` — recursively grounded to the Frame. This is the ER *weak entity / identifying relationship* pattern: `alice`'s anonymous address is not identified globally but as *"alice's address."* It is also exactly how a nested RDF blank node works: `:alice :hasAddress _:b1` means "the address *of* alice." The ordinal is used because a Pattern's elements are an ordered sequence (RFC-002), and because RFC-003 fixes that two anonymous patterns are always distinct even when structurally identical — an ordinal preserves that distinctness where a content hash would collapse it.

Because anonymous identity is scoped to the nearest *named* ancestor rather than to the whole file, editing one part of a file cannot perturb the identities of anonymous elements owned by an unrelated named element: inserting an anonymous child under `bob` leaves the identities of `alice`'s anonymous children untouched. The blast radius of the ordinal's edit-fragility shrinks from *file* to *one named parent's direct anonymous children* (see Open Question 1 for the residue).

### The Frame grounds the chain — and is required for storage

"Storage requires a Frame" and "promotion is anchored to a Frame" are the same statement. A relative chain of discriminators (`alice / 0 / 2`) means nothing until grounded against an absolute scope; the Frame is that ground. In memory and in text the chain may dangle — an element's identity is simply its position in the tree, and that is sufficient for structural navigation. To *persist* an element, or to *reference it from outside its own tree*, the chain must terminate at a Frame with a real scope id.

Resolution is therefore a total function of the in-memory `Pattern` alone: an anonymous ancestor bears no id of its own, contributing only its position, so the *nearest named* ancestor is the sole identity-bearing anchor encountered on the way up — this is why "nearest named" and "nearest identified" name the same walk here, and only the former is used. Promotion (below) is a codec *realizing* this function's output as concrete ids at the Frame boundary; those minted ids are outputs, never inputs, and never feed back into resolution. A codec chooses its representation and is faithful iff round-tripping preserves the resolved identities — it does not get to redefine them. This is the general answer to "which codecs must this account for": all of them, by reference to the one resolution defined here, not by enumeration.

This is already latent in RFC-011, which settled that **a `Store` is the persistent analog of an RFC-001 Frame.** If the Store *is* a Frame, then "you cannot store outside a Frame" is not a new constraint — it makes RFC-011's own metaphor load-bearing. Every durable store in the prior art requires such a container: a SQL row cannot exist outside a table (and a foreign key can only target a table's keyed rows); RDF triples are stored and addressed inside named graphs; Datomic datoms live in a database that assigns the permanent id; a Git blob is only *named* once it sits in a tree.

### Anonymous tolerance: positional vs. referential identity

Truly anonymous elements are **permitted everywhere** — in text and in memory — and are the common case, not an edge case. They are constrained in exactly one place, and the boundary is precise:

- **Positional identity** — the element *is* its location ("the second child of `alice`"). The containing structure supplies identity; no name is needed. This suffices for gram text, and for navigating a single in-memory tree.
- **Referential identity** — something points *at* the element from a place that cannot reach it by position. Now it needs a name to be a target.

An anonymous element breaks only when it must be a **reference target**. The canonical case is a stored foreign key, but it is not unique to storage: RFC-011 establishes that a cross-Frame **Span** endpoint is an in-memory foreign key (a Span connects elements across Frames, so it cannot address an endpoint by position — there is no shared tree). Storage is simply where the constraint *always* bites, because storage makes every link referential.

The rule, in one sentence:

> **Truly anonymous elements are always permitted; they carry positional identity only; they may be contained and value-compared freely, but they may not be the target of a by-identity reference (a cross-Frame Span endpoint or a stored foreign key) until promoted to a Frame-grounded identity.**

This matches the prior art without exception: a SQL table may have no primary key and is fully queryable, but nothing can declare a foreign key to it; an XML element without `id` is fine, but `IDREF` can only target elements that have one; an RDF blank node is fine until a query result must be referenced again, at which point engines skolemize it.

Because promotion is anchored to a Frame, the feared impossibility — "an anonymous element that is a cross-Frame Span endpoint" — cannot arise: a Span endpoint is necessarily inside a Frame, and being inside a Frame promotes it.

### The identity gradient (and the best practice it implies)

Three tiers, in increasing durability:

| Tier | Resolved identity | Referenceable? |
|------|-------------------|----------------|
| Frameless anonymous | positional, ungrounded | No — text / in-memory only |
| Framed anonymous | `(frame_scope, positional-path)` | Yes, but **fragile** under edits (ordinal shifts) |
| Named | `(frame_scope, name)` | Yes, **stably** |

Framing makes an element *referenceable*; naming makes it *stably* referenceable. This yields a single best-practice sentence with real teeth: **wrap anonymous elements in a named element inside a Frame.** It is not etiquette — it is the condition under which an element's identity is well-defined, stable, and safe to reference.

### Conventions

Round-tripping and cross-file behavior rely on the following encoded conventions.

**Scope declaration — the `namespace` header key.** A `.gram` file declares its Frame's scope in the leading bare record (gram's document header, parsed today by `fromGramWithHeader` in `Gram.Parse`, which already returns the header as `Maybe (Map String Value)` separately from the pattern list), using the reserved key `namespace`:

```
{namespace: "@neo4j/census2020"}
(alice:Person {name: "Alice"})
...
```

**The file is an anonymous pattern; `namespace` names it.** The header record plus the top-level pattern list *is* an anonymous bracketed pattern — `[ <anon-file> {namespace: "@neo4j/census2020"} | p1, p2, … ]` — the same `[record | elements]` form as `@k(42) (n)` ≡ `[ {k: 42} | (n) ]`, applied to the whole document. Reading the file this way puts it on the identity gradient with everything else, and three behaviors that would otherwise be special file-level rules fall out of mechanisms already defined above:

- **Distinctness is free (requirements 1, 6).** An unnamed file-subject is an anonymous element, so two files each containing `(alice)` are distinct for the same reason any two anonymous patterns are (RFC-003) — no separate store guarantee is invoked.
- **`namespace` is promotion, not a new mechanism.** Declaring it *names* the anonymous file-subject, moving it from framed-anonymous to the gradient's stable **Named** tier. That is the same promotion any anonymous element undergoes on becoming a reference target; the file-subject is simply the outermost one.
- **Frame is a reading, not a partition.** A consumer may wrap *any* pattern at any depth as a Frame (RFC-001's `asFrame`), so there is no meaningful count of "Frames per file." "One file = one Frame" is the common *reading*, not a structural limit — and the file-subject is itself the outermost such reading.

`namespace` is carried as a header *property* rather than as an identity symbol because gram's document root has no identity slot: there is no syntax to name the outermost subject directly, so its name rides in the reserved header key instead. This is the same accommodation that makes the reserved-key collision noted below a bounded cost rather than a reason to add new syntax.

`namespace` is the **idempotency key**: with it declared, re-ingest (requirement 4) resolves to the same Frame every time, with no fragile recognition needed, and other files gain a stable handle to reference into this one (requirement 3). This is Datomic's `:db.unique/identity` upsert applied at the Frame level: a declared scope that already exists resolves to the existing Frame rather than minting a new one.

**`namespace` value form — two-level, reach-marked.** The value is an opaque string following the GitHub/npm two-level convention `@org/sub`. URIs and reverse-DNS were considered and rejected as needlessly verbose (see Alternatives). The `@` sigil on the *value* carries the reach distinction directly in the notation:

- `namespace: "census2020"` — a bare token: **store-local** scope (the store is the enclosing authority).
- `namespace: "@neo4j/census2020"` — an `@`-scoped token: **authority-qualified**, safe to share across stores.

Presence of the value's `@` marks a namespace as federation-safe; its absence marks a local convenience. Uniqueness discipline scales with reach, and the author can *see* which they've chosen.

**Reserved header keys — an explicit registry.** `namespace` is a bare key, not a sigil-prefixed one, and it joins an existing convention: the peer `pattern-rs` project already reserves bare `kind` and `variant` in the document header for its schema-definition work (e.g. `{ kind: "schema", variant: "sample" }`). `namespace` extends that same registry. A sigil prefix (`@namespace`) is deliberately *not* used: in gram, `@` is a structural annotation operator — `@k(42) (n)` is equivalent to `[ {k: 42} | (n) ]` — so overloading it as a header-key prefix would collide with notation that already means something else. System keys are therefore distinguished by an explicit, documented set (`namespace`, `kind`, `variant`, …) rather than by prefix. The cost — a reserved key could in principle collide with author-chosen domain metadata of the same name — is the same one `pattern-rs` already accepts for `kind`/`variant`, and is bounded by keeping the registry small and documented.

**Store-assigned fallback.** When `namespace` is absent, the anonymous file-subject is skolemized at ingest — the same promotion applied at any Frame boundary (below), here to the outermost subject — yielding an assigned scope that isolates the file's ids (requirement 1 still holds). This is the safe default, but the assigned name is minted fresh each ingest, so re-ingest idempotency then depends on the store recognizing the file by some other key (see Open Question 2). Declaring `namespace` is the friction-free path: it names the file-subject up front, so promotion resolves to the same scope every time.

**Skolemization at promotion.** When an anonymous element is promoted (on entering a Frame for storage, or on becoming a Span endpoint), its positional path is resolved against the Frame scope into a stable id. This is RDF's spec-sanctioned skolemization ([Concepts §3.5](https://www.w3.org/TR/rdf11-concepts/)) performed deterministically at the Frame boundary. After promotion it is an ordinary Frame-scoped id; the hierarchy is used only at resolution time (Datomic's tempid→permanent-id move).

### Interaction with RFC-010 reconciliation

RFC-010 keys reconciliation on `Subject.identity`, collecting occurrences into a `Map Symbol [...]`. Today `Subject.identity :: Symbol` is mandatory and the parser represents an anonymous subject as `Symbol ""` (`Gram.Transform.transformIdentifier Nothing = Symbol ""`). Under identity-keyed collection, **every anonymous element would bucket under the single key `""` and be merged into one** — a direct violation of RFC-003's "two anonymous patterns are always distinct." This RFC therefore requires:

> Identity-keyed operations (reconcile, upsert, dedup) MUST treat anonymous elements as **value-distinct** and never merge them by identity. Anonymous elements are excluded from identity collection, not bucketed under a sentinel.

This is the same positional/referential boundary surfacing in the reconciler: anonymous elements have value semantics and are compared structurally, never by a shared identity key. Whether this is expressed by making anonymity a first-class case in the identity type (e.g. `data Identity = Named Symbol | Anonymous` scoped by position) or by filtering empty symbols out of collection is an implementation choice (see Open Question 3).

**Post-hoc merge is out of scope and belongs to RFC-010.** When two independently-ingested, distinctly-scoped ids are *later* discovered to denote the same entity, that resolution is reconciliation, not ingest-time identity. The Wikidata lesson applies: merge must **redirect, not delete** — the merged-away id keeps resolving via a tombstone/redirect so existing references and future re-ingests neither dangle nor silently re-create the entity ([Wikidata Help:Merge](https://www.wikidata.org/wiki/Help:Merge)). This RFC hands that requirement to RFC-010; it does not implement it.

### Acceptance criteria

Observable, regardless of file/package layout:

1. Two files each containing `(alice)`, **both omitting the `namespace` header entirely**, ingest to **two distinct** entities (the store-assigned fallback default).
2. Two files both declaring `namespace: "@org/x"` and both naming `alice` resolve `alice` to **one** entity.
3. Ingesting a file that declares `namespace` twice adds **no new entities** the second time (idempotent re-ingest).
4. A pattern containing two structurally identical anonymous elements reconciles to **two distinct** elements — never merged (RFC-003 distinctness preserved; the `Symbol ""` conflation is gone).
5. A contained-only anonymous element round-trips through a Frame with a `(frame_scope, positional-path)` id; an anonymous element made a cross-Frame Span endpoint is **promoted or rejected**, never silently keyed on `Symbol ""`.
6. A file with no `namespace` still ingests (store-assigned Frame) and its ids clash with no other file's.

### Demo

**Demo (~15 min, in-memory store, no database):** Reuses the RFC-011 in-memory `Store` fake — no persistence backend required. Each step exercises one acceptance criterion above.

1. **Clash-free default (criterion 1).** Create `a.gram` and `b.gram`, each containing `(alice:Person)` and neither declaring `namespace`. Ingest both; assert the store holds **two distinct** `alice` entities.
2. **Deliberate cross-file sameness (criterion 2).** Add `{namespace: "@org/x"}` to the leading header of both files and re-ingest into a fresh store; assert `alice` resolves to **one** entity.
3. **Idempotent re-ingest (criterion 3).** Ingest `a.gram` (with its `namespace`) a second time; assert the entity count is **unchanged**.

Each assertion is on observable store contents (entity identity and count), independent of file or package layout.

### Implementation Sequence

Identity-keyed persistence upsert (RFC-011) must not be built until steps 1–3 land.

1. **Identity model.** Introduce a resolved-identity representation distinguishing named (`(scope, name)`) from anonymous (positional, ungrounded), and the scope-chain resolution that grounds a chain at a Frame. Decide the anonymity representation (Open Question 3).
2. **Header parsing.** Add `namespace` to the reserved-key registry (alongside `pattern-rs`'s `kind`/`variant`) and interpret it from the leading bare record already surfaced by `fromGramWithHeader` (`Gram.Parse`); define the store-assigned fallback when absent.
3. **Reconciliation fix.** Update `Pattern.Reconcile` so identity-keyed collection treats anonymous elements as value-distinct (never merged), removing the `Symbol ""` conflation.
4. **Promotion / skolemization.** Implement deterministic skolemization of anonymous elements at the Frame boundary, producing Frame-grounded ids for elements that become reference targets.
5. **Codec integration (RFC-011).** Wire resolved identity into the Frame/Span `(frame_id, id)` composite key and the identity-keyed upsert path; enable seed-then-own.

### Port

The identity model, `namespace` convention, and skolemization rules are notation- and store-agnostic and are intended to be ported to `pattern-rs` (and the TypeScript surface) alongside RFC-011, so that gram files carry the same scope semantics regardless of runtime.

## Open Questions

1. **Ordinal fragility within a named parent.** Ordinal discriminators for anonymous siblings are stable across re-*parses* but not re-*orderings*: inserting an anonymous child at the front of a named parent shifts its siblings' ordinals, changing their promoted ids. The weak-entity scoping contains the blast radius to one named parent's direct anonymous children, but does not eliminate it within that parent. Options: accept it (framed-anonymous is documented as "fragile", and durable references should use named elements); derive the discriminator from content *plus* a de-duplicating counter (preserving RFC-003 distinctness while surviving reordering of *distinct* siblings); or treat any anonymous element that becomes a reference target as requiring promotion to a name. Leaning toward "accept + best-practice", but the content-plus-counter option deserves evaluation.

2. **Recognition key for the store-assigned fallback.** When a file omits `namespace`, what does the store use to recognize "I have seen this file before" so re-ingest is idempotent? Content hash (breaks when the file is edited — every id churns) and path (breaks when the file moves) are both imperfect; this is precisely the (1)-vs-(4) tension with no author declaration to resolve it. The pragmatic answer may be "no idempotency guarantee without `namespace`" — the fallback isolates ids (satisfying requirement 1) but re-ingest without a declared scope creates a new Frame. Needs a decision.

3. **Representation of anonymity in the identity type.** Should anonymity be a first-class constructor (`data Identity = Named Symbol | Anonymous Position`) that changes `Subject.identity`'s type, or should `Subject.identity` stay `Symbol` with anonymity encoded and filtered out of identity-keyed operations? The former is cleaner and makes the "never merge anonymous" rule unrepresentable-to-violate; the latter is a smaller change to a type used throughout the codebase. Note that `Subject.identity` ships in accepted, *implemented* foundation (RFC-002, RFC-010): changing its type is a foundation-wide migration, and avoiding that migration is part of what motivates the encoded-and-filtered option — reviewers should weigh blast radius, not only cleanliness. This is the highest-impact API decision in the RFC.

4. **Cross-file reference notation.** `namespace` gives a file a stable handle, but this RFC does not define how one file *references into* another's namespace (the analog of RDF's `@base`/prefixed names, or an `imports` header). Deliberate cross-file sameness (requirement 3) is served today by unique-key upsert at ingest; an explicit cross-file reference syntax is a plausible follow-on but is deferred until a concrete need appears. Named as future work, not resolved here.

## Alternatives

**File path as scope.** `scope = "census/people.gram"`. Simple, but every tradition warns against it: Matrix and DIDs make scope an *authority*, not a *location*, precisely because locations move. Rename or re-path the file and identity breaks; re-ingest from a moved file duplicates everything. Rejected on the same grounds RFC-011 rejects Neo4j's `elementId` as identity of record — location is not identity.

**Content hash of the file as scope.** Maximally idempotent for *unchanged* files and maximally clash-free, but it fails the property we actually want: editing one property changes the file hash, so *every* element in the file is re-scoped and the store sees a wholesale delete-and-reinsert. Content-addressing is right for immutable values (Git, Unison) and wrong for mutable documents — the Perkeep lesson is that mutable entities need a stable anchor decoupled from content. Rejected.

**Store-assigned scope only (no declaration).** Clean and clash-free, and it is our fallback — but *as the only mechanism* it destroys idempotency: re-ingesting a file mints a fresh scope and duplicates its contents. It only works paired with a recognition key, which pushes back to the content-hash or path problems. Rejected as a sole mechanism; retained as the fallback (D) behind declared `namespace` (C).

**Global identifiers (RDF IRIs) everywhere.** Make every id globally unique by authority, as RDF requires. This *is* expressible in our model — everyone declares the same `namespace` authority — but forcing it as the *only* option imposes exactly the authoring friction that left RDF blank nodes pervasive and its "name everything" guidance widely unfollowed. We keep bare local names bare and let scope attach at the boundary; global identity remains available as "shared declared scope" for those who want it. Rejected as a mandate, retained as a capability.

**Equivalence assertions (`owl:sameAs`) for cross-file identity.** Assert co-reference after the fact rather than keying on identity at ingest. Rejected for storage identity: unscoped, symmetric, transitive equivalence has documented failure modes (runaway closures, third-party identity capture) and conflates "discovered to be the same" with "declared the same." Deliberate sameness is unique-key upsert at ingest (requirement 3); *discovered* sameness is RFC-010 merge-with-redirect (Open Question 4). Neither is `sameAs`-in-storage.

**URI or reverse-DNS `namespace` values.** Both give authority-rooted global uniqueness, and both proved needlessly long in practice (the survey's XML/RDF experience). The GitHub/npm two-level `@org/sub` form supplies enough authority to be useful while staying short and human-writable, with the `@` sigil marking reach. Rejected in favor of the two-level convention.
